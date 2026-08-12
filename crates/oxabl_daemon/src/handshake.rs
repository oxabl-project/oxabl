//! Contract validation at the daemon boundary (R11).

use oxabl_daemon_protocol::{
    CONTRACT_VERSION, ClientKind, ContractMismatch, HandshakeRequest, HandshakeResponse, method,
};

use crate::{DAEMON_VERSION, Dispatch, MethodError, SessionHost};

/// Build the method table every socket daemon starts with.
///
/// U8 adds the query methods to this table. The handshake exists first because a
/// client must prove that it can read every later response before it sends a query.
pub fn default_dispatch() -> Dispatch {
    let mut dispatch = Dispatch::new();
    register_handshake(&mut dispatch);
    crate::methods::register_methods(&mut dispatch);
    dispatch
}

/// Register the contract handshake on `dispatch`.
pub fn register_handshake(dispatch: &mut Dispatch) {
    dispatch.register(method::HANDSHAKE, |host: &SessionHost, context, params| {
        let request: HandshakeRequest =
            serde_json::from_value(params).map_err(MethodError::invalid_params)?;
        if request.contract_version != CONTRACT_VERSION {
            return Err(MethodError {
                code: -32600,
                message: ContractMismatch {
                    client_version: request.contract_version,
                    daemon_version: CONTRACT_VERSION,
                }
                .to_string(),
            });
        }

        let (root, clients) = host.with(|sessions| {
            let session = sessions.for_root(&request.workspace_root);
            session.attach(matches!(request.client, ClientKind::Editor));
            (session.root().to_path_buf(), session.clients())
        });
        if let Err(error) = context.bind(root, request.client) {
            host.with(|sessions| {
                sessions
                    .for_root(&request.workspace_root)
                    .detach(matches!(request.client, ClientKind::Editor));
            });
            return Err(error);
        }
        serde_json::to_value(HandshakeResponse {
            contract_version: CONTRACT_VERSION,
            workspace_root: request.workspace_root,
            daemon_version: DAEMON_VERSION.to_string(),
            connected_clients: clients,
        })
        .map_err(MethodError::internal)
    });
}

#[cfg(test)]
mod tests {
    use oxabl_daemon_protocol::{ClientKind, HandshakeRequest};

    use super::*;

    #[test]
    fn a_contract_mismatch_names_both_versions_before_creating_a_session() {
        let dispatch = default_dispatch();
        let host = SessionHost::new();
        let request = HandshakeRequest {
            contract_version: CONTRACT_VERSION + 3,
            client: ClientKind::Desktop,
            workspace_root: "/proj/mismatch".to_string(),
        };

        let error = dispatch
            .call(
                &host,
                &mut crate::ClientContext::default(),
                method::HANDSHAKE,
                serde_json::to_value(request).unwrap(),
            )
            .expect_err("the mismatch is refused");

        assert!(error.message.contains(&(CONTRACT_VERSION + 3).to_string()));
        assert!(error.message.contains(&CONTRACT_VERSION.to_string()));
        assert_eq!(host.with(|sessions| sessions.len()), 0);
    }
}
