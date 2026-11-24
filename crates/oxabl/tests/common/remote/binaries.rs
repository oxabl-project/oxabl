//! The server, the admin tool and the member command line, built at the pinned revision by the caller.

use std::path::PathBuf;

fn required(variable: &str, what: &str) -> PathBuf {
    let value = std::env::var_os(variable).unwrap_or_else(|| {
        panic!(
            "{variable} is not set. The remote-cache tests need the {what} binary built at the \
             revision that Cargo.toml pins: `cargo install --git \
             https://github.com/oxabl-project/oxabl-cache --rev <rev> --locked --root <dir> \
             oxabl-cache-server oxabl-cache-admin oxabl-cache-cli`, then {variable}=<dir>/bin/..."
        )
    });
    let path = PathBuf::from(value);
    assert!(
        path.is_file(),
        "{variable} names {}, which is not a file",
        path.display()
    );
    path
}

pub fn server() -> PathBuf {
    required("OXABL_CACHE_SERVER_BIN", "oxabl-cache-server")
}

pub fn admin() -> PathBuf {
    required("OXABL_CACHE_ADMIN_BIN", "oxabl-cache-admin")
}

pub fn member_cli() -> PathBuf {
    required("OXABL_CACHE_BIN", "oxabl-cache member command line")
}
