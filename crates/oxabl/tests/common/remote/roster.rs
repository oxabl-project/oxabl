//! Identities and the first roster of a cache: what a cache owner does before
//! any member can use it.

use base64::Engine as _;
use base64::engine::general_purpose::STANDARD_NO_PAD;
use oxabl_cache_crypto::{ExposeSecret, Identity, MemberEntry, RosterDraft};
use oxabl_cache_format::{CacheId, Role};

/// A member of a cache: its identity string and its public signing key.
pub struct Member {
    pub label: &'static str,
    pub role: Role,
    pub identity_string: String,
    identity: Identity,
}

impl Member {
    pub fn new(label: &'static str, role: Role) -> Member {
        let identity = Identity::generate().expect("an identity");
        Member {
            label,
            role,
            identity_string: identity
                .to_secret_string()
                .expect("an identity string")
                .expose_secret()
                .to_owned(),
            identity,
        }
    }

    fn entry(&self) -> MemberEntry {
        MemberEntry::from_descriptor(
            self.label,
            self.role,
            &self.identity.descriptor().expect("a descriptor"),
        )
        .expect("a roster member")
    }

    pub fn public_key(&self) -> [u8; 32] {
        self.identity.signing_public()
    }

    /// `ed25519:<base64>`: how `.oxabl/cache.toml` and the signer of an entry
    /// spell a public key.
    pub fn key_text(&self) -> String {
        format!("ed25519:{}", STANDARD_NO_PAD.encode(self.public_key()))
    }
}

pub fn hex(key: &[u8; 32]) -> String {
    key.iter().map(|byte| format!("{byte:02x}")).collect()
}

/// The signed first roster of `cache`, owned by `owner`.
pub fn genesis(cache: &str, owner: &Member, members: &[&Member]) -> Vec<u8> {
    let entries = std::iter::once(owner)
        .chain(members.iter().copied())
        .map(Member::entry)
        .collect();
    RosterDraft::genesis(CacheId::from_text(cache).expect("a cache id"), entries)
        .expect("a roster")
        .sign(&[&owner.identity])
        .expect("a signed roster")
}
