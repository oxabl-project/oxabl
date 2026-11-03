//! The seam types of `oxabl_build` and the types of the client, one into the
//! other.

use base64::Engine as _;
use base64::engine::general_purpose::STANDARD_NO_PAD;
use oxabl_build::cache as seam;
use oxabl_cache_client as client;

pub fn client_key(key: seam::CompileKey) -> client::CompileKey {
    client::CompileKey(key.0)
}

pub fn seam_key(key: client::CompileKey) -> seam::CompileKey {
    seam::CompileKey(key.0)
}

pub fn client_path(path: &seam::ArtifactPath) -> Option<client::ArtifactPath> {
    client::ArtifactPath::new(&path.0).ok()
}

pub fn seam_path(path: &client::ArtifactPath) -> seam::ArtifactPath {
    seam::ArtifactPath(path.as_str().to_string())
}

pub fn client_file(file: &seam::ArtifactFileRef) -> Option<client::ArtifactFileRef> {
    Some(client::ArtifactFileRef {
        path: client_path(&file.path)?,
        disk_path: file.disk_path.clone(),
        size: file.size,
        blake3: file.blake3,
    })
}

pub fn origin(provenance: &client::Provenance) -> seam::Origin {
    seam::Origin::Remote(seam::RemoteOrigin {
        tier: match provenance.tier {
            client::Tier::Trusted => "trusted",
            client::Tier::Personal => "personal",
        }
        .to_string(),
        signer: format!("ed25519:{}", STANDARD_NO_PAD.encode(provenance.signer)),
    })
}

/// A reason the client may add later reads as `Unavailable`: the key is a miss
/// and the build compiles it.
pub fn miss_reason(reason: client::MissReason) -> seam::MissReason {
    match reason {
        client::MissReason::NotFound => seam::MissReason::NotFound,
        client::MissReason::Untrusted => seam::MissReason::Untrusted,
        client::MissReason::Corrupt => seam::MissReason::Corrupt,
        client::MissReason::PathMismatch => seam::MissReason::PathMismatch,
        client::MissReason::Timeout => seam::MissReason::Timeout,
        client::MissReason::Install(_) => seam::MissReason::InstallFailed,
        client::MissReason::Unavailable => seam::MissReason::Unavailable,
        _ => seam::MissReason::Unavailable,
    }
}

/// An outcome the client may add later reads as not stored.
pub fn store_outcome(outcome: client::StoreOutcome) -> seam::StoreOutcome {
    use seam::NotStoredReason as Reason;
    match outcome {
        client::StoreOutcome::Committed => seam::StoreOutcome::Stored,
        client::StoreOutcome::Existed => seam::StoreOutcome::AlreadyPresent,
        client::StoreOutcome::Changed => seam::StoreOutcome::NotStored(Reason::ChangedOnDisk),
        client::StoreOutcome::Failed(failure) => seam::StoreOutcome::NotStored(match failure {
            client::StoreFailure::Rejected => Reason::Rejected,
            client::StoreFailure::TooLarge => Reason::TooLarge,
            client::StoreFailure::Unreadable => Reason::Unreadable,
            client::StoreFailure::QuotaExceeded => Reason::QuotaExceeded,
            client::StoreFailure::Unavailable => Reason::Unavailable,
            _ => Reason::Unavailable,
        }),
        _ => seam::StoreOutcome::NotStored(Reason::Unavailable),
    }
}
