//! A tenant with a cache, a roster and one member of each role, behind the
//! proxy, and the environment of a workspace that builds against it.

use std::fs;
use std::net::TcpListener;

use serde_json::json;

use super::member_cli::{Member, Owner};
use super::proxy::Proxy;
use super::server::Server;
use super::{admin, http};
use crate::common::built::Built;

/// A member and the token that the server knows it by.
pub struct Account {
    pub member: Member,
    pub token: String,
}

pub struct World {
    pub server: Server,
    pub proxy: Proxy,
    pub tenant: String,
    pub cache: String,
    pub owner: Owner,
    pub ci: Account,
    pub developer: Account,
    pub reader: Account,
}

/// Every variable that the client reads and that a developer's shell may set.
const AMBIENT: &[&str] = &[
    "OXABL_CACHE_TOKEN",
    "OXABL_CACHE_IDENTITY",
    "OXABL_CACHE_IDENTITY_FILE",
    "OXABL_CACHE_IDENTITY_PASSPHRASE",
    "OXABL_CACHE_SERVER",
    "OXABL_CACHE_ID_TOKEN",
    "OXABL_CACHE_TENANT",
    "OXABL_CACHE_CACHE",
    "OXABL_CACHE_OWNER_KEYS",
    "OXABL_CACHE_MIN_ROSTER_VERSION",
    "ACTIONS_ID_TOKEN_REQUEST_URL",
    "ACTIONS_ID_TOKEN_REQUEST_TOKEN",
];

impl World {
    pub fn start() -> World {
        let listener = TcpListener::bind("127.0.0.1:0").expect("a free port");
        let port = listener.local_addr().expect("an address").port();
        let server = Server::start(Some(&format!("http://127.0.0.1:{port}")));
        let proxy = Proxy::start(listener, &server.public);
        let (tenant, tenant_admin) = admin::tenant(&server, "acme");
        let owner = Owner::new();
        let created = http::json(
            &server.public,
            "POST",
            "/v1/tenant/caches",
            &tenant_admin,
            &json!({"display_name": "codebase", "owner_keys": [owner.key_hex]}),
            201,
        );
        let cache = created["id"].as_str().expect("a cache id").to_string();
        let token = |name: &str, scopes: &[&str]| {
            let created = http::json(
                &server.public,
                "POST",
                "/v1/tenant/tokens",
                &tenant_admin,
                &json!({"name": name, "scopes": scopes, "caches": [&cache]}),
                201,
            );
            created["token"].as_str().expect("a token").to_string()
        };
        let cache_admin = token("owner", &["read", "cache:admin"]);
        owner.init(
            &format!("http://{}", server.public),
            &tenant,
            &cache,
            &cache_admin,
        );
        let account = |label: &'static str, role: &'static str, scopes: &[&str]| Account {
            member: owner.add_member(label, role),
            token: token(label, scopes),
        };
        let ci = account("ci", "ci", &["read", "write:trusted"]);
        let developer = account("developer", "developer", &["read", "write:personal"]);
        let reader = account("reader", "reader", &["read"]);
        World {
            server,
            proxy,
            tenant,
            cache,
            owner,
            ci,
            developer,
            reader,
        }
    }

    /// The content of `.oxabl/cache.toml`, with `extra` lines added.
    pub fn cache_toml(&self, extra: &str) -> String {
        format!(
            "server = \"{}\"\ntenant = \"{}\"\ncache = \"{}\"\nowner_keys = [\"{}\"]\nmin_roster_version = 1\n{extra}",
            self.proxy.url(),
            self.tenant,
            self.cache,
            self.owner.key,
        )
    }

    /// Makes `workspace` a client of the cache: its `.oxabl/cache.toml`, the
    /// credentials of `account` and the cache mode `mode`.
    pub fn attach(&self, workspace: &Built, account: &Account, mode: &str) {
        workspace.write(".oxabl/cache.toml", &self.cache_toml(""));
        for name in AMBIENT {
            workspace.env(name, None);
        }
        workspace.env("OXABL_CACHE_MODE", Some(mode));
        workspace.env("OXABL_CACHE_TOKEN", Some(&account.token));
        workspace.env(
            "OXABL_CACHE_IDENTITY",
            Some(&account.member.identity_string),
        );
        let state = workspace.path(".state");
        fs::create_dir_all(&state).expect("a state directory");
        workspace.env("XDG_STATE_HOME", Some(&state.to_string_lossy()));
    }
}

/// Four units: a procedure with an include, two classes and a user of them.
pub fn project(extra: &str) -> Built {
    let workspace = Built::with("include_paths = [\"src\"]", extra);
    workspace.write(
        "src/inc/ledger.i",
        "DEFINE VARIABLE gl AS INTEGER NO-UNDO.\n",
    );
    workspace.write("src/ar/post.p", "{inc/ledger.i}\nMESSAGE \"post\".\n");
    workspace.write("src/pkg/Base.cls", "CLASS pkg.Base:\nEND CLASS.\n");
    workspace.write(
        "src/pkg/User.cls",
        "CLASS pkg.User INHERITS pkg.Base:\nEND CLASS.\n",
    );
    workspace.write(
        "src/main.p",
        "DEFINE VARIABLE o AS pkg.User NO-UNDO.\no = NEW pkg.User().\n",
    );
    workspace
}

/// The units of [`project`].
pub const UNITS: usize = 4;
