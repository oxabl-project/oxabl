//! One dispatch table for every method the daemon serves.
//!
//! The language server used to route requests through an inline `if method == …`
//! chain, which is fine for two methods and wrong for a surface that grows: an
//! `oxabl/*` method added to such a chain sits in a different branch of the same
//! `if`, and the two surfaces slowly acquire different error handling, different
//! logging, and different guarantees about what happens when a handler panics.
//!
//! A table makes registration the only way in, so an LSP method and an `oxabl/*`
//! method are the same kind of thing: a name, a handler, and one shared wrapper
//! that contains a panic and reports it as *that request's* failure.

use std::collections::HashMap;

use serde_json::Value;

use crate::session::SessionHost;

/// Why a request could not be answered.
///
/// The code is the JSON-RPC one the caller reports; the message is for a human
/// reading a log. A failed request is a value, never an unwind out of the loop —
/// one request's failure must not take down the daemon or any other client.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MethodError {
    pub code: i32,
    pub message: String,
}

impl MethodError {
    /// JSON-RPC `MethodNotFound`.
    pub fn unknown_method(method: &str) -> Self {
        MethodError {
            code: -32601,
            message: format!("oxabl: unsupported request `{method}`"),
        }
    }

    /// JSON-RPC `InvalidParams`.
    pub fn invalid_params(detail: impl std::fmt::Display) -> Self {
        MethodError {
            code: -32602,
            message: format!("oxabl: invalid params: {detail}"),
        }
    }

    /// JSON-RPC `InternalError`.
    pub fn internal(detail: impl std::fmt::Display) -> Self {
        MethodError {
            code: -32603,
            message: format!("oxabl: {detail}"),
        }
    }
}

impl std::fmt::Display for MethodError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} ({})", self.message, self.code)
    }
}

/// What a handler is given and what it must return.
pub type MethodResult = Result<Value, MethodError>;

/// One method's implementation.
///
/// Takes the [`SessionHost`] rather than one session, for two reasons. Which session
/// a request belongs to is the request's own business — a client names its workspace
/// root at the handshake and the daemon holds one session per root (KTD21). And the
/// host makes the locking rule visible in the handler: take the lock to write or to
/// clone a snapshot, then release it before querying, or one client's slow answer
/// stalls every other client.
///
/// `Send + Sync` because the table is shared by a thread per connected client.
pub type Handler = Box<dyn Fn(&SessionHost, Value) -> MethodResult + Send + Sync>;

/// Every method the daemon serves, by name.
#[derive(Default)]
pub struct Dispatch {
    handlers: HashMap<String, Handler>,
}

impl Dispatch {
    pub fn new() -> Self {
        Dispatch::default()
    }

    /// Register `handler` under `method`.
    ///
    /// Panics on a duplicate name. A silently shadowed method would be a routing
    /// bug that produces plausible answers from the wrong handler, which is far
    /// worse than failing at startup — and this runs once, before any client
    /// connects, so the panic can only fire in a developer's own build.
    pub fn register(
        &mut self,
        method: &str,
        handler: impl Fn(&SessionHost, Value) -> MethodResult + Send + Sync + 'static,
    ) {
        let previous = self.handlers.insert(method.to_string(), Box::new(handler));
        assert!(
            previous.is_none(),
            "method `{method}` is registered twice; one of the two would never be reached"
        );
    }

    /// Whether a method is registered.
    pub fn handles(&self, method: &str) -> bool {
        self.handlers.contains_key(method)
    }

    /// Every registered method name, sorted — for a capability report and for a
    /// test that pins the surface.
    pub fn methods(&self) -> Vec<&str> {
        let mut names: Vec<&str> = self.handlers.keys().map(String::as_str).collect();
        names.sort_unstable();
        names
    }

    /// Answer one request.
    ///
    /// A panic inside a handler is contained here and reported as *this request's*
    /// failure, which is the guarantee that lets one bad file cost one query instead
    /// of the daemon and every client on it. A `salsa::Cancelled` must never reach
    /// this guard — the queries catch their own, because converting a cancellation
    /// into a reported error would turn every concurrent edit into a visible failure.
    pub fn call(&self, host: &SessionHost, method: &str, params: Value) -> MethodResult {
        let Some(handler) = self.handlers.get(method) else {
            return Err(MethodError::unknown_method(method));
        };
        match oxabl_common::catch_panic(std::panic::AssertUnwindSafe(|| handler(host, params))) {
            Ok(result) => result,
            Err(panic) => Err(MethodError::internal(format!(
                "request `{method}` panicked: {panic}"
            ))),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use serde_json::json;

    fn host() -> SessionHost {
        SessionHost::new()
    }

    #[test]
    fn a_registered_method_answers() {
        let mut dispatch = Dispatch::new();
        dispatch.register("oxabl/echo", |_, params| Ok(params));
        assert!(dispatch.handles("oxabl/echo"));
        assert_eq!(
            dispatch.call(&host(), "oxabl/echo", json!({"a": 1})),
            Ok(json!({"a": 1}))
        );
    }

    #[test]
    fn an_unknown_method_is_reported_not_ignored() {
        let dispatch = Dispatch::new();
        let error = dispatch
            .call(&host(), "oxabl/nope", Value::Null)
            .expect_err("an unknown method must be reported");
        assert_eq!(error.code, -32601);
        assert!(error.message.contains("oxabl/nope"), "got {error}");
    }

    /// The containment guarantee: one handler's panic fails one request, and the
    /// table keeps serving.
    #[test]
    fn a_panicking_handler_fails_only_its_own_request() {
        let mut dispatch = Dispatch::new();
        dispatch.register("oxabl/boom", |_, _| panic!("deliberate"));
        dispatch.register("oxabl/fine", |_, _| Ok(json!("ok")));

        let previous = std::panic::take_hook();
        std::panic::set_hook(Box::new(|_| {}));
        let host = host();
        let error = dispatch
            .call(&host, "oxabl/boom", Value::Null)
            .expect_err("a panicking handler must fail its request");
        std::panic::set_hook(previous);

        assert_eq!(error.code, -32603);
        assert!(error.message.contains("deliberate"), "got {error}");
        assert_eq!(
            dispatch.call(&host, "oxabl/fine", Value::Null),
            Ok(json!("ok")),
            "the table must keep serving after a contained panic"
        );
    }

    #[test]
    #[should_panic(expected = "registered twice")]
    fn registering_one_method_twice_fails_loudly() {
        let mut dispatch = Dispatch::new();
        dispatch.register("oxabl/one", |_, _| Ok(Value::Null));
        dispatch.register("oxabl/one", |_, _| Ok(Value::Null));
    }
}
