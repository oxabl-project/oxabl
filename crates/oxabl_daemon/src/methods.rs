//! The public `oxabl/*` query surface (R7, R8, R19, R20).

use std::collections::HashMap;
use std::io;
use std::path::{Path, PathBuf};
use std::sync::Arc;
use std::time::Instant;

use oxabl_analyze::unresolved_reason_str;
use oxabl_ast::NodeId;
use oxabl_daemon_protocol::{
    AffectedFile, AffectedGroup, ByteSpan, Cause, Freshness, FreshnessRequest, FreshnessResponse,
    ImpactRequest, ImpactResponse, IndexState, Provenance, ReindexRequest, ReindexResponse,
    SchemaIdentity, Sourced, Subject, SymbolKind, SymbolRow, SymbolSearchRequest,
    SymbolSearchResponse, method,
};
use oxabl_pipeline::{EdgeKind, Expansion, LintPipeline, LintResult, ReverseGraph};
use oxabl_semantic::{SymbolFlags, SymbolKind as SemanticSymbolKind};
use oxabl_workspace::{FileSystem, RealFileSystem, discover_path};

use crate::dispatch::{ClientContext, Dispatch, MethodError};
use crate::session::{FileStamp, SessionHost, WorkspaceSnapshot};

/// Register every non-LSP method. No handler checks the client kind: the daemon
/// exposes the same capability to the editor and desktop clients (KTD5).
pub fn register_methods(dispatch: &mut Dispatch) {
    dispatch.register(method::IMPACT, impact);
    dispatch.register(method::SYMBOL_SEARCH, symbol_search);
    dispatch.register(method::FRESHNESS, freshness);
    dispatch.register(method::REINDEX, reindex);
}

fn impact(
    host: &SessionHost,
    context: &mut ClientContext,
    params: serde_json::Value,
) -> Result<serde_json::Value, MethodError> {
    let started = Instant::now();
    let request: ImpactRequest =
        serde_json::from_value(params).map_err(MethodError::invalid_params)?;
    let root = context.workspace_root()?.to_path_buf();
    let workspace = ensure_workspace(host, &root, false)?;
    let subject = pipeline_subject(&request.subject);
    let dependents = workspace.graph.dependents(&subject);

    let groups = dependents
        .kinds()
        .into_iter()
        .map(|kind| AffectedGroup {
            cause: cause(kind),
            files: dependents
                .of_kind(kind)
                .map(|row| AffectedFile {
                    path: row.file.to_string_lossy().into_owned(),
                    span: row.span.map(span),
                })
                .collect(),
        })
        .collect();
    let unresolved = dependents
        .unresolved()
        .iter()
        .map(|row| oxabl_daemon_protocol::UnresolvedReference {
            file: row.file.to_string_lossy().into_owned(),
            cause: cause(row.reference.kind),
            name: row.reference.name.clone(),
            reason: unresolved_reason_str(row.reference.reason).to_string(),
            span: row.reference.span.map(span),
        })
        .collect();
    let rebuild_set = workspace
        .graph
        .rebuild_set(&subject)
        .into_iter()
        .map(|path| path.to_string_lossy().into_owned())
        .collect();
    let (provenance, freshness) = session_stamps(host, &root, &workspace);
    let response = ImpactResponse {
        subject: request.subject,
        groups,
        unresolved,
        direct_reference_count: dependents.files().len() as u32,
        rebuild_set,
        provenance,
        schema: schema_identity(&workspace),
        freshness,
        estimated_build_seconds: Sourced::unavailable("no build daemon supplies this value"),
        query_millis: started.elapsed().as_millis() as u64,
    };
    serde_json::to_value(response).map_err(MethodError::internal)
}

fn symbol_search(
    host: &SessionHost,
    context: &mut ClientContext,
    params: serde_json::Value,
) -> Result<serde_json::Value, MethodError> {
    let request: SymbolSearchRequest =
        serde_json::from_value(params).map_err(MethodError::invalid_params)?;
    let workspace = ensure_workspace(host, context.workspace_root()?, false)?;
    let needle = request.query.to_ascii_lowercase();
    let mut symbols: Vec<SymbolRow> = workspace
        .symbols
        .iter()
        .filter(|row| row.name.to_ascii_lowercase().contains(&needle))
        .cloned()
        .collect();
    symbols.sort_by(|left, right| {
        let left_prefix = !left.name.to_ascii_lowercase().starts_with(&needle);
        let right_prefix = !right.name.to_ascii_lowercase().starts_with(&needle);
        (
            left_prefix,
            left.name.to_ascii_lowercase(),
            left.id.as_str(),
        )
            .cmp(&(
                right_prefix,
                right.name.to_ascii_lowercase(),
                right.id.as_str(),
            ))
    });
    let total_matches = symbols.len() as u32;
    symbols.truncate(request.limit as usize);
    serde_json::to_value(SymbolSearchResponse {
        symbols,
        total_matches,
    })
    .map_err(MethodError::internal)
}

fn freshness(
    host: &SessionHost,
    context: &mut ClientContext,
    params: serde_json::Value,
) -> Result<serde_json::Value, MethodError> {
    let _: FreshnessRequest =
        serde_json::from_value(params).map_err(MethodError::invalid_params)?;
    let root = context.workspace_root()?;
    let state = host.with(|sessions| {
        let session = sessions
            .get(root)
            .expect("a successful handshake creates its session");
        session.workspace().map(|workspace| {
            let provenance = provenance(session);
            let freshness = workspace_freshness(&workspace);
            (workspace, provenance, freshness)
        })
    });

    let response = match state {
        Some((workspace, provenance, freshness)) => FreshnessResponse {
            freshness,
            schema: schema_identity(&workspace),
            provenance,
        },
        None => FreshnessResponse {
            freshness: Freshness {
                state: IndexState::Indexing {
                    indexed: 0,
                    total: 0,
                },
                indexed_files: 0,
                unanalysed_files: 0,
                unresolved_ratio: 0.0,
                last_pass_millis: Sourced::unavailable("no workspace pass has completed"),
            },
            schema: SchemaIdentity {
                revision: 0,
                table_count: 0,
                loaded: false,
            },
            provenance: Provenance::Disk,
        },
    };
    serde_json::to_value(response).map_err(MethodError::internal)
}

fn reindex(
    host: &SessionHost,
    context: &mut ClientContext,
    params: serde_json::Value,
) -> Result<serde_json::Value, MethodError> {
    let _: ReindexRequest = serde_json::from_value(params).map_err(MethodError::invalid_params)?;
    let workspace = ensure_workspace(host, context.workspace_root()?, true)?;
    serde_json::to_value(ReindexResponse {
        freshness: workspace_freshness(&workspace),
        pass_millis: workspace.pass_millis,
    })
    .map_err(MethodError::internal)
}

/// Return a graph at the current buffer generation. Disk changes never cause an
/// automatic rebuild; they only make the result stale until `oxabl/reindex`.
fn ensure_workspace(
    host: &SessionHost,
    root: &Path,
    force: bool,
) -> Result<WorkspaceSnapshot, MethodError> {
    loop {
        let prepared = host.with(|sessions| {
            let session = sessions.for_root(root);
            if !force
                && let Some(workspace) = session.workspace()
                && workspace.buffer_generation == session.buffer_generation()
            {
                return None;
            }
            Some((
                session.root().to_path_buf(),
                session.buffer_overlay(),
                session.buffer_generation(),
            ))
        });

        let Some((root, overlay, generation)) = prepared else {
            return Ok(host.with(|sessions| {
                sessions
                    .get(root)
                    .and_then(|session| session.workspace())
                    .expect("the checked workspace remains installed")
            }));
        };
        let workspace = build_workspace(&root, overlay, generation)?;
        let installed = host.with(|sessions| {
            let session = sessions.for_root(&root);
            if session.buffer_generation() != generation {
                return false;
            }
            session.install_config((*workspace.config).clone());
            session.install_workspace(workspace.clone());
            true
        });
        if installed {
            return Ok(workspace);
        }
    }
}

fn build_workspace(
    root: &Path,
    overlay: HashMap<PathBuf, Arc<str>>,
    buffer_generation: u64,
) -> Result<WorkspaceSnapshot, MethodError> {
    let started = Instant::now();
    let files = discover_path(root).map_err(MethodError::internal)?;
    let (config, _warnings) =
        oxabl_pipeline::PipelineConfig::resolve(root, &oxabl_pipeline::ConfigOverrides::default());
    let fs = OverlayFileSystem { overlay };
    let pipeline = LintPipeline::new(&config, &fs).with_known_files(&files);
    let mut symbols = file_symbols(&files);
    let graph = ReverseGraph::build_with(&pipeline, &files, |path, expansion, result| {
        collect_symbols(path, expansion, result, &fs, &mut symbols);
    });
    symbols.extend(table_symbols(&config));
    symbols.sort_by(|left, right| left.id.cmp(&right.id));
    symbols.dedup_by(|left, right| left.id == right.id);

    let tracked_files = graph.tracked_files();
    Ok(WorkspaceSnapshot {
        graph: Arc::new(graph),
        symbols: Arc::new(symbols),
        files: Arc::new(tracked_files.into_iter().map(FileStamp::capture).collect()),
        config: Arc::new(config),
        buffer_generation,
        pass_millis: started.elapsed().as_millis() as u64,
    })
}

struct OverlayFileSystem {
    overlay: HashMap<PathBuf, Arc<str>>,
}

impl FileSystem for OverlayFileSystem {
    fn read(&self, path: &Path) -> Result<Arc<str>, io::Error> {
        self.overlay
            .get(path)
            .cloned()
            .map(Ok)
            .unwrap_or_else(|| RealFileSystem.read(path))
    }

    fn exists(&self, path: &Path) -> bool {
        self.overlay.contains_key(path) || RealFileSystem.exists(path)
    }
}

fn collect_symbols(
    path: &Path,
    expansion: &Expansion,
    result: &LintResult,
    fs: &dyn FileSystem,
    rows: &mut Vec<SymbolRow>,
) {
    let (Some(expanded), Some(semantic), Ok(source)) =
        (expansion.expanded(), result.semantic(), fs.read(path))
    else {
        return;
    };
    for (id, symbol) in semantic.symbols.iter() {
        if symbol.declaration == NodeId::DUMMY {
            continue;
        }
        let kind = match symbol.kind {
            SemanticSymbolKind::Class => SymbolKind::Class,
            SemanticSymbolKind::Interface => SymbolKind::Interface,
            SemanticSymbolKind::Procedure => SymbolKind::Procedure,
            SemanticSymbolKind::Function => SymbolKind::Function,
            SemanticSymbolKind::Variable
                if symbol.flags.intersects(
                    SymbolFlags::SHARED | SymbolFlags::NEW_SHARED | SymbolFlags::NEW_GLOBAL_SHARED,
                ) =>
            {
                SymbolKind::SharedVariable
            }
            _ => continue,
        };
        let Some(root_span) = expanded.resolve_root_span(oxabl_ast::Span {
            start: symbol.name_span.start,
            end: symbol.name_span.end,
        }) else {
            continue;
        };
        let name = source
            .get(root_span.start as usize..root_span.end as usize)
            .unwrap_or(symbol.name.as_ref())
            .to_string();
        rows.push(SymbolRow {
            id: format!("{}:{}:{}", path.display(), root_span.start, id.raw()),
            name,
            kind,
            file: Some(path.to_string_lossy().into_owned()),
            span: Some(span(root_span)),
            subject: Subject::File {
                path: path.to_string_lossy().into_owned(),
            },
        });
    }
}

fn file_symbols(files: &[PathBuf]) -> Vec<SymbolRow> {
    files
        .iter()
        .map(|path| SymbolRow {
            id: format!("file:{}", path.display()),
            name: path
                .file_stem()
                .unwrap_or_default()
                .to_string_lossy()
                .into_owned(),
            kind: SymbolKind::File,
            file: Some(path.to_string_lossy().into_owned()),
            span: None,
            subject: Subject::File {
                path: path.to_string_lossy().into_owned(),
            },
        })
        .collect()
}

fn table_symbols(config: &oxabl_pipeline::PipelineConfig) -> Vec<SymbolRow> {
    config
        .schema
        .tables()
        .map(|(_, table)| {
            let name = table.name.to_string();
            SymbolRow {
                id: format!("table:{name}"),
                name: name.clone(),
                kind: SymbolKind::Table,
                file: None,
                span: None,
                subject: Subject::Table { name },
            }
        })
        .collect()
}

fn session_stamps(
    host: &SessionHost,
    root: &Path,
    workspace: &WorkspaceSnapshot,
) -> (Provenance, Freshness) {
    host.with(|sessions| {
        let session = sessions.get(root).expect("the workspace session exists");
        (provenance(session), workspace_freshness(workspace))
    })
}

fn provenance(session: &crate::Session) -> Provenance {
    if session.editor_clients() == 0 {
        Provenance::Disk
    } else {
        Provenance::WorkingTree {
            editor_clients: session.editor_clients(),
            unsaved_buffers: session.open_buffers(),
        }
    }
}

fn workspace_freshness(workspace: &WorkspaceSnapshot) -> Freshness {
    let changed_files = workspace.files.iter().filter(|file| file.changed()).count() as u32;
    Freshness {
        state: if changed_files == 0 {
            IndexState::Ready
        } else {
            IndexState::Stale { changed_files }
        },
        indexed_files: workspace.graph.file_count() as u32,
        unanalysed_files: workspace.graph.unanalysed().len() as u32,
        unresolved_ratio: workspace.graph.unresolved_ratio(),
        last_pass_millis: Sourced::Available {
            value: workspace.pass_millis,
        },
    }
}

fn schema_identity(workspace: &WorkspaceSnapshot) -> SchemaIdentity {
    SchemaIdentity {
        revision: workspace.config.schema.revision().raw(),
        table_count: workspace.config.schema.len() as u32,
        loaded: workspace.config.schema_loaded,
    }
}

fn pipeline_subject(subject: &Subject) -> oxabl_pipeline::Subject {
    match subject {
        Subject::File { path } => oxabl_pipeline::Subject::file(path),
        Subject::Table { name } => oxabl_pipeline::Subject::table(name),
    }
}

fn cause(kind: EdgeKind) -> Cause {
    match kind {
        EdgeKind::DirectInclude => Cause::DirectInclude,
        EdgeKind::TransitiveInclude => Cause::TransitiveInclude,
        EdgeKind::SchemaTable => Cause::SchemaTable,
        EdgeKind::ClassReference => Cause::Class,
        EdgeKind::ProgramReference => Cause::Program,
        EdgeKind::SharedProducer => Cause::SharedProducer,
    }
}

fn span(value: oxabl_ast::Span) -> ByteSpan {
    ByteSpan {
        start: value.start,
        end: value.end,
    }
}
