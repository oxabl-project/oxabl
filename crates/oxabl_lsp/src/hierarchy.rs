//! `textDocument/implementation`, the type hierarchy
//! (`textDocument/prepareTypeHierarchy`, `typeHierarchy/supertypes`,
//! `typeHierarchy/subtypes`) and `oxabl/superMethod`. See
//! `docs/design/type-hierarchy.md`.
//!
//! Each request runs on a worker thread, as the references do, and an answer
//! that can be short is never shown as a complete one: a hierarchy answer
//! holds an item named `incomplete` that names every gap, first in the list,
//! and the gaps of the referent come as a `window/showMessage` warning. An
//! implementation answer is a plain list of locations, so its gaps come as the
//! warning only. A supertype that oxabl cannot resolve is an item of kind
//! `Null` that says why.

use std::path::PathBuf;

use lsp_server::{Request, Response};
use lsp_types::request::GotoImplementationParams;
use lsp_types::{
    Location, PositionEncodingKind, SymbolKind, TextDocumentPositionParams, TypeHierarchyItem,
    TypeHierarchyPrepareParams, TypeHierarchySubtypesParams, TypeHierarchySupertypesParams,
};
use oxabl_daemon::canonical_path;
use oxabl_daemon::hierarchy::{
    Form, HierarchyAnswer, HierarchyQuery, ItemKind, TypeItem, TypeKey, answer_hierarchy,
};
use oxabl_pipeline::Gap;
use serde::{Deserialize, Serialize};

use crate::Server;
use crate::path_to_uri;
use crate::references::{Ropes, invalid, with_dropped};

/// The custom request that answers go to super method.
pub(crate) const SUPER_METHOD: &str = "oxabl/superMethod";

/// What a type hierarchy item carries back to the server, in `data`.
#[derive(Serialize, Deserialize)]
struct ItemData {
    root: PathBuf,
    path: PathBuf,
    name: String,
    form: String,
    reason: Option<String>,
}

/// The `data` of the item that says an answer is incomplete.
#[derive(Serialize, Deserialize)]
struct MarkerData {
    incomplete: bool,
}

fn to_data(key: &TypeKey) -> ItemData {
    let (form, reason) = match &key.form {
        Form::Class => ("class", None),
        Form::Builtin => ("builtin", None),
        Form::Unresolved(reason) => ("unresolved", Some(reason.clone())),
    };
    ItemData {
        root: key.root.clone(),
        path: key.path.clone(),
        name: key.name.clone(),
        form: form.to_string(),
        reason,
    }
}

fn from_data(data: ItemData) -> Option<TypeKey> {
    let form = match data.form.as_str() {
        "class" => Form::Class,
        "builtin" => Form::Builtin,
        "unresolved" => Form::Unresolved(data.reason.unwrap_or_default()),
        _ => return None,
    };
    Some(TypeKey {
        root: data.root,
        path: data.path,
        name: data.name,
        form,
    })
}

/// What a request's item is.
enum Asked {
    Item(TypeKey),
    /// The item that says an answer is incomplete: it has no types.
    Marker,
}

fn asked(item: &TypeHierarchyItem) -> Result<Asked, String> {
    let data = item
        .data
        .clone()
        .ok_or_else(|| "the item has no data: it does not come from this server".to_string())?;
    if serde_json::from_value::<MarkerData>(data.clone()).is_ok_and(|marker| marker.incomplete) {
        return Ok(Asked::Marker);
    }
    let data: ItemData = serde_json::from_value(data)
        .map_err(|error| format!("the item's data is not this server's: {error}"))?;
    from_data(data)
        .map(Asked::Item)
        .ok_or_else(|| "the item's data names no known form".to_string())
}

fn symbol_kind(kind: ItemKind) -> SymbolKind {
    match kind {
        ItemKind::Class => SymbolKind::CLASS,
        ItemKind::Interface => SymbolKind::INTERFACE,
        ItemKind::Enum => SymbolKind::ENUM,
        ItemKind::Unresolved => SymbolKind::NULL,
    }
}

fn render_item(
    item: &TypeItem,
    ropes: &mut Ropes<'_>,
    encoding: &PositionEncodingKind,
) -> Option<TypeHierarchyItem> {
    let uri = path_to_uri(&canonical_path(&item.path))?;
    Some(TypeHierarchyItem {
        name: item.key.name.clone(),
        kind: symbol_kind(item.kind),
        tags: None,
        detail: item.detail.clone(),
        uri,
        range: ropes.range(&item.path, item.span, encoding)?,
        selection_range: ropes.range(&item.path, item.selection, encoding)?,
        data: serde_json::to_value(to_data(&item.key)).ok(),
    })
}

/// The item that says the answer for `shown` is incomplete, and why.
fn marker(shown: &TypeHierarchyItem, gaps: &[Gap]) -> TypeHierarchyItem {
    let reasons: Vec<&str> = gaps.iter().map(|gap| gap.reason.as_str()).collect();
    TypeHierarchyItem {
        name: "incomplete".to_string(),
        kind: SymbolKind::NULL,
        tags: None,
        detail: Some(reasons.join("; ")),
        uri: shown.uri.clone(),
        range: shown.range,
        selection_range: shown.selection_range,
        data: Some(serde_json::json!({ "incomplete": true })),
    }
}

const STANDING: &str = "implementations and the type hierarchy";

fn locations(answer: &HierarchyAnswer, encoding: &PositionEncodingKind) -> (Vec<Location>, usize) {
    let mut ropes = Ropes::new(&answer.texts);
    let found: Vec<Location> = answer
        .locations
        .iter()
        .filter_map(|target| {
            let uri = path_to_uri(&canonical_path(&target.path))?;
            let range = ropes.range(&target.path, target.span?, encoding)?;
            Some(Location { uri, range })
        })
        .collect();
    let dropped = answer.locations.len() - found.len();
    (found, dropped)
}

impl Server<'_> {
    pub(crate) fn handle_implementation(&mut self, req: Request) -> anyhow::Result<()> {
        let params: GotoImplementationParams = match serde_json::from_value(req.params) {
            Ok(params) => params,
            Err(error) => return self.respond(invalid(req.id, &error.to_string())),
        };
        let Some((root, file, offset)) =
            self.cursor(&req.id, &params.text_document_position_params)?
        else {
            return Ok(());
        };
        let query = HierarchyQuery::Implementation { file, offset };
        self.spawn_answer(
            req.id,
            root,
            ("the implementations", STANDING),
            move |host, session_root, root, cancel| {
                answer_hierarchy(host, session_root, root, &query, cancel)
            },
            |answer: &HierarchyAnswer| answer.gaps.clone(),
            |answer, encoding| {
                let (found, dropped) = locations(&answer, encoding);
                Ok((serde_json::to_value(found)?, dropped))
            },
        )
    }

    /// `oxabl/superMethod`: the methods that the method at the cursor
    /// overrides or implements, in the nearest supertypes that declare one,
    /// and the reasons the list can be short. The gaps are in the answer, not
    /// a warning, because the client shows them with the locations.
    pub(crate) fn handle_super_method(&mut self, req: Request) -> anyhow::Result<()> {
        let params: TextDocumentPositionParams = match serde_json::from_value(req.params) {
            Ok(params) => params,
            Err(error) => return self.respond(invalid(req.id, &error.to_string())),
        };
        let Some((root, file, offset)) = self.cursor(&req.id, &params)? else {
            return Ok(());
        };
        let query = HierarchyQuery::SuperMethod { file, offset };
        self.spawn_answer(
            req.id,
            root,
            ("the super method", STANDING),
            move |host, session_root, root, cancel| {
                answer_hierarchy(host, session_root, root, &query, cancel)
            },
            |_: &HierarchyAnswer| Vec::new(),
            |answer, encoding| {
                let (found, dropped) = locations(&answer, encoding);
                let gaps: Vec<String> = with_dropped(&answer.gaps, dropped)
                    .into_iter()
                    .map(|gap| gap.reason)
                    .collect();
                Ok((
                    serde_json::json!({ "locations": found, "gaps": gaps }),
                    dropped,
                ))
            },
        )
    }

    pub(crate) fn handle_prepare_type_hierarchy(&mut self, req: Request) -> anyhow::Result<()> {
        let params: TypeHierarchyPrepareParams = match serde_json::from_value(req.params) {
            Ok(params) => params,
            Err(error) => return self.respond(invalid(req.id, &error.to_string())),
        };
        let Some((root, file, offset)) =
            self.cursor(&req.id, &params.text_document_position_params)?
        else {
            return Ok(());
        };
        let query = HierarchyQuery::Prepare { file, offset };
        self.spawn_answer(
            req.id,
            root,
            ("the type hierarchy", STANDING),
            move |host, session_root, root, cancel| {
                answer_hierarchy(host, session_root, root, &query, cancel)
            },
            |answer: &HierarchyAnswer| answer.gaps.clone(),
            |answer, encoding| {
                let mut ropes = Ropes::new(&answer.texts);
                let items: Vec<TypeHierarchyItem> = answer
                    .items
                    .iter()
                    .filter_map(|item| render_item(item, &mut ropes, encoding))
                    .collect();
                let dropped = answer.items.len() - items.len();
                Ok((
                    serde_json::to_value((!items.is_empty()).then_some(items))?,
                    dropped,
                ))
            },
        )
    }

    pub(crate) fn handle_type_supertypes(&mut self, req: Request) -> anyhow::Result<()> {
        let params: TypeHierarchySupertypesParams = match serde_json::from_value(req.params) {
            Ok(params) => params,
            Err(error) => return self.respond(invalid(req.id, &error.to_string())),
        };
        self.spawn_types(
            req.id,
            params.item,
            "the supertypes",
            HierarchyQuery::Supertypes,
        )
    }

    pub(crate) fn handle_type_subtypes(&mut self, req: Request) -> anyhow::Result<()> {
        let params: TypeHierarchySubtypesParams = match serde_json::from_value(req.params) {
            Ok(params) => params,
            Err(error) => return self.respond(invalid(req.id, &error.to_string())),
        };
        self.spawn_types(
            req.id,
            params.item,
            "the subtypes",
            HierarchyQuery::Subtypes,
        )
    }

    /// Answer a supertypes or subtypes request for `shown`.
    fn spawn_types(
        &mut self,
        id: lsp_server::RequestId,
        shown: TypeHierarchyItem,
        what: &'static str,
        query: fn(TypeKey) -> HierarchyQuery,
    ) -> anyhow::Result<()> {
        let key = match asked(&shown) {
            Ok(Asked::Item(key)) => key,
            Ok(Asked::Marker) => {
                return self.respond(Response::new_ok(id, Vec::<()>::new()));
            }
            Err(error) => return self.respond(invalid(id, &error)),
        };
        let root = canonical_path(&key.root);
        let query = query(key);
        self.spawn_answer(
            id,
            root,
            (what, STANDING),
            move |host, session_root, root, cancel| {
                answer_hierarchy(host, session_root, root, &query, cancel)
            },
            |answer: &HierarchyAnswer| answer.gaps.clone(),
            move |answer, encoding| {
                let mut ropes = Ropes::new(&answer.texts);
                let mut items: Vec<TypeHierarchyItem> = answer
                    .items
                    .iter()
                    .filter_map(|item| render_item(item, &mut ropes, encoding))
                    .collect();
                let dropped = answer.items.len() - items.len();
                let gaps = with_dropped(&answer.gaps, dropped);
                if !gaps.is_empty() {
                    items.insert(0, marker(&shown, &gaps));
                }
                Ok((serde_json::to_value(items)?, dropped))
            },
        )
    }
}
