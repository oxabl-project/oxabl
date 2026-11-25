//! `textDocument/signatureHelp`: the parameters of the routine that the call
//! around the cursor reaches, with the argument the cursor is in.
//!
//! The analysis is the one go to definition runs, on a worker thread with the
//! open buffers over the disk. A call that oxabl does not resolve answers
//! `null`. When the arguments do not pick one overload, the answer lists every
//! overload and sets no `activeSignature`. See `docs/design/signature-help.md`.

use lsp_server::{Request, RequestId, Response};
use lsp_types::{PositionEncodingKind, SignatureHelpParams};
use oxabl_daemon::canonical_path;
use oxabl_daemon::db::NavigationQuery;
use oxabl_pipeline::SignatureHelp;
use serde_json::{Value, json};

use crate::position::position_to_byte;
use crate::{Server, uri_to_path};

impl Server<'_> {
    pub(crate) fn handle_signature_help(&mut self, req: Request) -> anyhow::Result<()> {
        let params: SignatureHelpParams = match serde_json::from_value(req.params) {
            Ok(params) => params,
            Err(error) => return self.respond(invalid(req.id, &error.to_string())),
        };
        let position = params.text_document_position_params;
        let uri = position.text_document.uri;
        let Some(path) = uri_to_path(&uri) else {
            return self.respond(invalid(req.id, &format!("{} is not a file", uri.as_str())));
        };
        let Some(document) = self.documents.get(&uri) else {
            return self.respond(invalid(req.id, &format!("{} is not open", uri.as_str())));
        };
        let offset = position_to_byte(&document.rope, position.position, &self.encoding) as u32;
        let path = canonical_path(&path);
        let root = self
            .contexts
            .selected(&uri)
            .cloned()
            .unwrap_or_else(|| path.clone());
        let query = NavigationQuery::SignatureHelp { file: path, offset };
        self.spawn_navigation(req.id, root, None, None, query, |answer, encoding| {
            Ok(render(answer.signature_help, encoding))
        })
    }
}

fn invalid(id: RequestId, message: &str) -> Response {
    Response::new_err(
        id,
        lsp_server::ErrorCode::InvalidParams as i32,
        format!("oxabl-lsp: {message}"),
    )
}

/// The LSP answer. Built by hand: `activeSignature` is left out when the
/// arguments pick no overload, and a parameter that is not there is `null`,
/// which a typed answer cannot say.
fn render(help: Option<SignatureHelp>, encoding: &PositionEncodingKind) -> Value {
    let Some(help) = help else {
        return Value::Null;
    };
    let units = |label: &str, at: usize| -> usize {
        if *encoding == PositionEncodingKind::UTF8 {
            at
        } else {
            label[..at].encode_utf16().count()
        }
    };
    let signatures: Vec<Value> = help
        .signatures
        .iter()
        .map(|signature| {
            let parameters: Vec<Value> = signature
                .parameters
                .iter()
                .map(|range| {
                    json!({"label": [
                        units(&signature.label, range.start),
                        units(&signature.label, range.end),
                    ]})
                })
                .collect();
            let mut out = json!({
                "label": signature.label,
                "parameters": parameters,
                "activeParameter": signature.active_parameter,
            });
            if let Some(documentation) = &signature.documentation {
                out["documentation"] = json!({"kind": "markdown", "value": documentation});
            }
            out
        })
        .collect();
    let active = help.active_signature.and_then(|at| help.signatures.get(at));
    let mut out = json!({
        "signatures": signatures,
        "activeParameter": active.and_then(|s| s.active_parameter),
    });
    if let Some(at) = help.active_signature {
        out["activeSignature"] = json!(at);
    }
    out
}

#[cfg(test)]
mod tests {
    use super::*;
    use oxabl_pipeline::SignatureInfo;

    fn help() -> SignatureHelp {
        let label = "f(é, zweit)".to_string();
        let (first, second) = (label.find('é').unwrap(), label.find("zweit").unwrap());
        SignatureHelp {
            signatures: vec![SignatureInfo {
                parameters: vec![first..first + 'é'.len_utf8(), second..second + 5],
                label,
                active_parameter: Some(1),
                documentation: None,
            }],
            active_signature: None,
        }
    }

    #[test]
    fn the_parameter_offsets_follow_the_position_encoding() {
        let utf16 = render(Some(help()), &PositionEncodingKind::UTF16);
        assert_eq!(
            utf16["signatures"][0]["parameters"][1]["label"],
            json!([5, 10])
        );
        let utf8 = render(Some(help()), &PositionEncodingKind::UTF8);
        assert_eq!(
            utf8["signatures"][0]["parameters"][1]["label"],
            json!([6, 11])
        );
    }

    #[test]
    fn no_active_signature_is_left_out_and_no_parameter_is_null() {
        let mut none = help();
        none.signatures[0].active_parameter = None;
        let answer = render(Some(none), &PositionEncodingKind::UTF16);
        assert!(answer.get("activeSignature").is_none(), "{answer}");
        assert!(answer["activeParameter"].is_null(), "{answer}");
        assert!(
            answer["signatures"][0]["activeParameter"].is_null(),
            "{answer}"
        );
        assert_eq!(render(None, &PositionEncodingKind::UTF16), Value::Null);
    }
}
