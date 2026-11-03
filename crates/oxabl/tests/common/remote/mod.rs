//! A real `oxabl-cache-server` with a tenant, a cache and a roster, a proxy
//! in front of it, and workspaces that build against it. Nothing here knows
//! the code of `oxabl build`.

#![allow(dead_code)]

pub mod admin;
pub mod binaries;
pub mod http;
pub mod proxy;
pub mod roster;
pub mod server;
pub mod world;
