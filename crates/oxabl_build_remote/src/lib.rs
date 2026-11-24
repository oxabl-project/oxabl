//! The remote cache of `oxabl build`: an [`ArtifactCache`] over the
//! oxabl-cache client.
//!
//! The client owns lookup ids, compression, encryption, signing, transfer,
//! verification and atomic installation. This crate maps its types to the
//! seam of `oxabl_build` and turns every failure into a miss or a set that
//! was not stored.
//!
//! [`ArtifactCache`]: oxabl_build::cache::ArtifactCache

mod convert;
mod fetch;
mod notes;
mod open;
mod remote;
mod upload;

pub use open::RemoteCacheOpener;
pub use remote::RemoteCache;
