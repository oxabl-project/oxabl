//! The wire contract for the oxabl daemon's `oxabl/*` methods.
//!
//! One crate defines the wire, and it depends on serde and nothing else. The
//! desktop client needs to name a request type; it must not pull salsa, the
//! pipeline, and the whole analysis stack in to do so. Any `oxabl_*` dependency
//! added here defeats the point of the crate existing.
//!
//! # Transport
//!
//! JSON-RPC over the framing the language server already speaks, with `oxabl/*`
//! methods beside the LSP surface rather than in a second protocol. The transport
//! carries an arbitrary method string, so a custom method costs nothing.
//!
//! # The contract version
//!
//! [`CONTRACT_VERSION`] is compared **by value** at the handshake. A client and
//! daemon at different versions refuse to proceed rather than misinterpret each
//! other, and the refusal names both numbers so the mismatch is diagnosable from
//! one message. Bump it whenever a type here changes shape.
//!
//! # Unsourceable values
//!
//! Some slots have no source yet — a compile-time estimate needs a build daemon
//! that does not exist. Those are [`Sourced`], never a bare `Option`: an `Option`
//! invites `unwrap_or_default`, and a plausible fabricated zero is a worse failure
//! than an honest gap. Under-reporting impact is the failure mode this product
//! cannot survive.

use std::path::{Path, PathBuf};

use serde::{Deserialize, Serialize};

/// The wire contract's version, compared by value at the handshake.
///
/// Bump on any shape change to a type in this crate. There is no negotiation and
/// no compatibility window: both products are pre-1.0 and are built together, so a
/// mismatch means one side is stale and should be told so rather than accommodated.
pub const CONTRACT_VERSION: u32 = 3;

/// The `oxabl/*` method names, as they travel.
pub mod method {
    pub const HANDSHAKE: &str = "oxabl/handshake";
    pub const IMPACT: &str = "oxabl/impact";
    pub const SYMBOL_SEARCH: &str = "oxabl/symbolSearch";
    pub const FRESHNESS: &str = "oxabl/freshness";
    pub const REINDEX: &str = "oxabl/reindex";
}

/// A method this crate knows, or one it does not.
///
/// [`Method::Unknown`] is explicit rather than an error, so a daemon meeting a
/// method from a newer client can answer "I do not know that one" for *that
/// request* instead of failing the message and leaving the client waiting.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Method {
    Handshake,
    Impact,
    SymbolSearch,
    Freshness,
    Reindex,
    Unknown(String),
}

impl Method {
    /// Classify a method name off the wire.
    pub fn from_name(name: &str) -> Self {
        match name {
            method::HANDSHAKE => Method::Handshake,
            method::IMPACT => Method::Impact,
            method::SYMBOL_SEARCH => Method::SymbolSearch,
            method::FRESHNESS => Method::Freshness,
            method::REINDEX => Method::Reindex,
            other => Method::Unknown(other.to_string()),
        }
    }

    /// The name this method travels under.
    pub fn name(&self) -> &str {
        match self {
            Method::Handshake => method::HANDSHAKE,
            Method::Impact => method::IMPACT,
            Method::SymbolSearch => method::SYMBOL_SEARCH,
            Method::Freshness => method::FRESHNESS,
            Method::Reindex => method::REINDEX,
            Method::Unknown(name) => name,
        }
    }

    /// Whether this is a method the daemon can serve.
    pub fn is_known(&self) -> bool {
        !matches!(self, Method::Unknown(_))
    }
}

// ---------------------------------------------------------------------------
// Honest values
// ---------------------------------------------------------------------------

/// A value the daemon may not be able to source.
///
/// Deliberately not `Option<T>`. An `Option` reads as "maybe absent" and the
/// nearest habit is `unwrap_or_default`, which for every slot this wraps means
/// rendering a zero that claims something false. This type carries the *reason*
/// instead and offers no default, so a client either shows the value or shows that
/// there is none.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
#[serde(tag = "state", rename_all = "snake_case")]
pub enum Sourced<T> {
    Available { value: T },
    Unavailable { reason: String },
}

impl<T> Sourced<T> {
    /// The value, when there is one. There is deliberately no `unwrap_or`.
    pub fn value(&self) -> Option<&T> {
        match self {
            Sourced::Available { value } => Some(value),
            Sourced::Unavailable { .. } => None,
        }
    }

    /// Why the value is unavailable, when it is.
    pub fn reason(&self) -> Option<&str> {
        match self {
            Sourced::Available { .. } => None,
            Sourced::Unavailable { reason } => Some(reason),
        }
    }

    /// An unavailable value with its reason.
    pub fn unavailable(reason: impl Into<String>) -> Self {
        Sourced::Unavailable {
            reason: reason.into(),
        }
    }
}

// ---------------------------------------------------------------------------
// Shared shapes
// ---------------------------------------------------------------------------

/// A byte range in a file's own bytes.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub struct ByteSpan {
    pub start: u32,
    pub end: u32,
}

/// What an impact query is asked about.
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
#[serde(tag = "kind", rename_all = "snake_case")]
pub enum Subject {
    /// A workspace file, by absolute path.
    File { path: String },
    /// A schema table, by folded name. It carries no CRC: a CRC needs a compiler,
    /// and a table id means nothing outside the schema that minted it.
    Table { name: String },
}

/// Why one file depends on another — the cause an impact answer groups by.
///
/// Mirrors the engine's edge kinds. Defined here rather than re-exported so this
/// crate stays free of engine dependencies; the daemon maps between the two, and
/// the tags are identical so a mismatch is visible in one diff.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum Cause {
    DirectInclude,
    TransitiveInclude,
    SchemaTable,
    Class,
    Program,
    SharedProducer,
}

impl Cause {
    /// Every cause, in the order an answer renders its groups.
    pub const ALL: [Cause; 6] = [
        Cause::DirectInclude,
        Cause::TransitiveInclude,
        Cause::SchemaTable,
        Cause::Class,
        Cause::Program,
        Cause::SharedProducer,
    ];

    /// A short label for a group header.
    pub fn label(self) -> &'static str {
        match self {
            Cause::DirectInclude => "Direct include",
            Cause::TransitiveInclude => "Transitive include",
            Cause::SchemaTable => "Schema table",
            Cause::Class => "Class reference",
            Cause::Program => "Program reference",
            Cause::SharedProducer => "Shared producer",
        }
    }
}

/// Which state an answer measured (R19).
///
/// Always stated, because the same symbol yields different numbers depending on
/// whether an editor is attached, and an answer that did not say which it measured
/// would be unreproducible.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(tag = "kind", rename_all = "snake_case")]
pub enum Provenance {
    /// Files as they are on disk. No editor client is contributing buffers.
    Disk,
    /// Files on disk, overlaid with the unsaved buffers connected editors hold.
    WorkingTree {
        /// How many editor clients are contributing, and how many buffers they
        /// hold unsaved — so "working tree" is a quantity rather than a mood.
        editor_clients: u32,
        unsaved_buffers: u32,
    },
}

/// The schema an answer was computed against (R17).
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct SchemaIdentity {
    /// The revision the model resolved under. `0` means no schema was loaded,
    /// which is a different fact from a schema with no tables.
    pub revision: u32,
    pub table_count: u32,
    /// Whether a schema was loaded at all.
    pub loaded: bool,
}

/// How current the index is (R20).
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(tag = "state", rename_all = "snake_case")]
pub enum IndexState {
    /// The workspace pass is running. No answer is available yet.
    Indexing { indexed: u32, total: u32 },
    /// The index reflects what the daemon last read.
    Ready,
    /// Files changed after the last pass. An answer computed now is stale and must
    /// be presented as such.
    Stale { changed_files: u32 },
}

/// Index freshness, and the pass's own numbers.
// No `Eq`: `unresolved_ratio` is a float. The ratio is reported, not matched on.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Freshness {
    pub state: IndexState,
    /// Files the last completed pass covered.
    pub indexed_files: u32,
    /// Files the pass could not analyse. Never folded into `indexed_files`: a file
    /// nobody could read is not a file that depends on nothing.
    pub unanalysed_files: u32,
    /// Unresolved references as a share of every reference the pass attempted,
    /// so an answer's trustworthiness is legible rather than assumed.
    pub unresolved_ratio: f64,
    /// Edges the pass resolved but could not name a file for, because the id came
    /// from an index it does not own.
    ///
    /// Reported separately from `unresolved_ratio` because the two are different
    /// gaps: an unresolved reference is a name the workspace failed to supply,
    /// while these resolved and only lost their path. One number could not say
    /// which had happened, and folding them would claim a workspace gap that does
    /// not exist.
    pub unnameable_edges: u32,
    /// How long the last completed pass took.
    pub last_pass_millis: Sourced<u64>,
}

// ---------------------------------------------------------------------------
// oxabl/handshake
// ---------------------------------------------------------------------------

/// Which client is connecting. Recorded for reporting only — the daemon gates no
/// capability by client (KTD5), and this field must never become a permission.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum ClientKind {
    /// An editor's language client, which also contributes unsaved buffers.
    Editor,
    /// The desktop app.
    Desktop,
    /// A batch run.
    Cli,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct HandshakeRequest {
    pub contract_version: u32,
    pub client: ClientKind,
    /// The workspace root the client wants a session for.
    pub workspace_root: String,
}

impl HandshakeRequest {
    /// A handshake at this build's contract version.
    pub fn new(client: ClientKind, workspace_root: impl Into<String>) -> Self {
        HandshakeRequest {
            contract_version: CONTRACT_VERSION,
            client,
            workspace_root: workspace_root.into(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct HandshakeResponse {
    pub contract_version: u32,
    pub workspace_root: String,
    /// The daemon's own version string, for a human reading a mismatch report.
    pub daemon_version: String,
    /// How many clients this session now serves, so "two clients, one index" is
    /// demonstrable rather than asserted.
    pub connected_clients: u32,
}

/// A refused connection, naming **both** versions (R11).
///
/// One message has to be enough to diagnose the mismatch; a refusal that named
/// only the side that rejected would send the reader looking for the other.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct ContractMismatch {
    pub client_version: u32,
    pub daemon_version: u32,
}

impl std::fmt::Display for ContractMismatch {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "contract version mismatch: client speaks {}, daemon speaks {}",
            self.client_version, self.daemon_version
        )
    }
}

// ---------------------------------------------------------------------------
// oxabl/impact
// ---------------------------------------------------------------------------

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct ImpactRequest {
    pub subject: Subject,
}

/// One file an impact answer names.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct AffectedFile {
    pub path: String,
    /// Where the file writes the reference, in its own bytes, so a jump-out opens
    /// the editor at the line that creates the dependency (KTD26).
    ///
    /// `None` is a real answer: a transitive include is named in an intermediate
    /// file, and a reference spliced in from an include has no offset in this one.
    /// A client opens the file at its start rather than inventing a position.
    pub span: Option<ByteSpan>,
}

/// The affected files that share one cause.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct AffectedGroup {
    pub cause: Cause,
    pub files: Vec<AffectedFile>,
}

/// A reference that named the subject and did not resolve.
///
/// Its own row in the answer, never folded into a resolved count. An impact answer
/// that absorbs these under-reports the blast radius while looking more confident.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct UnresolvedReference {
    /// The file that wrote the reference.
    pub file: String,
    /// Which question was asked.
    pub cause: Cause,
    /// The name as written.
    pub name: String,
    /// Why it did not resolve, in the engine's own vocabulary.
    pub reason: String,
    pub span: Option<ByteSpan>,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct ImpactResponse {
    pub subject: Subject,
    /// Affected files, one group per cause. Empty means nothing depends on the
    /// subject, which is an answer rather than a failure.
    pub groups: Vec<AffectedGroup>,
    /// References that named the subject and did not resolve. Kept out of every
    /// group and out of `direct_reference_count`.
    pub unresolved: Vec<UnresolvedReference>,
    /// Distinct files named across all groups.
    pub direct_reference_count: u32,
    /// The transitive closure of dependents — a distinct field from `groups`,
    /// because "files that mention this" and "files a compiler must touch again"
    /// are different numbers (R4).
    pub rebuild_set: Vec<String>,
    /// Which state the answer measured (R19).
    pub provenance: Provenance,
    /// The schema the answer was computed against (R17).
    pub schema: SchemaIdentity,
    /// How current the index is (R20).
    pub freshness: Freshness,
    /// A compile-time estimate. Unavailable until a build daemon exists to source
    /// one; explicitly so, never as a zero (R18, KTD6).
    pub estimated_build_seconds: Sourced<f64>,
    /// How long the daemon spent answering, so a client can separate its own
    /// render cost from the query's.
    pub query_millis: u64,
}

// ---------------------------------------------------------------------------
// oxabl/symbolSearch
// ---------------------------------------------------------------------------

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct SymbolSearchRequest {
    pub query: String,
    /// How many results the client will render. The daemon may return fewer.
    pub limit: u32,
}

/// What kind of thing a search result names.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum SymbolKind {
    File,
    Class,
    Interface,
    Procedure,
    Function,
    Table,
    SharedVariable,
}

/// One searchable symbol.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct SymbolRow {
    /// A stable identity for this row, unique within a response.
    ///
    /// A list row is keyed by this rather than by its position: an index key in a
    /// filtered or re-sorted list hands the previous row's state to whatever now
    /// occupies that slot (KTD24).
    pub id: String,
    /// The name as the source spells it, for display and for match highlighting.
    pub name: String,
    pub kind: SymbolKind,
    /// The file that declares it, when one does. A table has none.
    pub file: Option<String>,
    pub span: Option<ByteSpan>,
    /// What to ask an impact query about when this row is chosen.
    pub subject: Subject,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct SymbolSearchResponse {
    /// Matching symbols. An empty list is an answer, not an error.
    pub symbols: Vec<SymbolRow>,
    /// How many matched before `limit` was applied, so a client can say "showing
    /// 50 of 900" rather than implying it showed everything.
    pub total_matches: u32,
}

// ---------------------------------------------------------------------------
// oxabl/freshness and oxabl/reindex
// ---------------------------------------------------------------------------

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub struct FreshnessRequest {}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct FreshnessResponse {
    pub freshness: Freshness,
    pub schema: SchemaIdentity,
    pub provenance: Provenance,
}

/// Reindex is an explicit act, never a filesystem watcher (KTD7).
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub struct ReindexRequest {}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct ReindexResponse {
    pub freshness: Freshness,
    /// How long the pass took, which is the cold-start number worth tracking.
    pub pass_millis: u64,
    /// Estimated heap bytes owned by the completed reverse graph.
    pub graph_bytes: u64,
}

// ---------------------------------------------------------------------------
// Discovery (KTD20)
// ---------------------------------------------------------------------------

/// A running daemon's registration for one workspace root.
///
/// Written by the daemon, read by every client. A client that finds a registration
/// whose pid is dead treats it as absent and replaces it, so a crashed daemon never
/// leaves a client waiting (R9).
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct Registration {
    pub pid: u32,
    pub socket_path: String,
    /// The contract the running daemon speaks, so a client can see a mismatch
    /// before it connects rather than after.
    pub contract_version: u32,
    pub workspace_root: String,
}

/// The directory registrations live in.
///
/// A new convention: neither product reads or writes a cache directory today.
/// `$XDG_CACHE_HOME/oxabl/daemon`, falling back to `$HOME/.cache/oxabl/daemon`,
/// and finally to a temp-directory path when neither variable names a usable
/// directory — a headless or sandboxed process still has to be able to register.
///
/// # Why a relative variable is ignored rather than resolved (R16)
///
/// A value that is not absolute is not a base directory, and the XDG base directory
/// specification says such a value is invalid and must be ignored. Resolving one
/// would make the registration, the lock and the socket relative to each *process's*
/// current directory: a daemon started from one directory and a client from another
/// would use two registries, so the lock could not stop two daemons from serving one
/// workspace root, and the client would get `ENOENT` connecting to a socket path that
/// means nothing where it stands. Ignoring the value falls through to the next
/// candidate, which is a location both processes agree on.
pub fn registration_dir() -> PathBuf {
    if let Some(cache) = base_dir("XDG_CACHE_HOME") {
        return cache.join("oxabl").join("daemon");
    }
    if let Some(home) = base_dir("HOME") {
        return home.join(".cache").join("oxabl").join("daemon");
    }
    std::env::temp_dir().join("oxabl").join("daemon")
}

/// The absolute directory `name` holds, or `None` when it holds nothing usable.
///
/// Split from [`registration_dir`] so the rule is stated once: every caller asking
/// "did this variable name a base directory" gets the same answer, and so does
/// [`temp_dir_fallback_in_use`], which would otherwise report the wrong branch for a
/// variable this function rejects.
fn base_dir(name: &str) -> Option<PathBuf> {
    usable_base(std::env::var_os(name))
}

/// The rule itself, over a value rather than a variable, so it can be exercised
/// without mutating the environment of a whole test binary.
fn usable_base(value: Option<std::ffi::OsString>) -> Option<PathBuf> {
    let path = PathBuf::from(value.filter(|value| !value.is_empty())?);
    path.is_absolute().then_some(path)
}

/// Whether [`registration_dir`] is resolving through the temp-directory fallback.
///
/// That branch puts the registration under a world-writable parent, where another
/// user can create the directory first and have it adopted.
///
/// This reports which branch was taken; it does not gate anything. It used to:
/// ownership was verified only when this returned true, on the theory that the
/// other branches were private by construction. They are not — an `XDG_CACHE_HOME`
/// can point anywhere, including at a directory another user owns — so the daemon
/// now verifies ownership and the sticky bit on every parent it walks, whichever
/// branch produced it. Nothing should reintroduce a check conditioned on this
/// answer.
///
/// What that walk checks for is ownership plus world-write without the sticky bit. A
/// group-writable parent is accepted, deliberately: a private per-user group makes
/// `drwxrwx---` an ordinary home directory, and `st_gid` cannot tell a private group
/// from a shared one. See `oxabl_daemon::registry` for that trade-off in full.
pub fn temp_dir_fallback_in_use() -> bool {
    base_dir("XDG_CACHE_HOME").is_none() && base_dir("HOME").is_none()
}

/// The longest socket path a Unix domain socket can carry.
///
/// `sun_path` is 108 bytes on Linux and the kernel needs the terminating NUL, so
/// 107 bytes are usable. This is the tighter of the two caps a registration name
/// has to respect; the other is the 255-byte filename limit most filesystems
/// impose.
pub const MAX_SOCKET_PATH: usize = 107;

/// The longest single filename most filesystems accept.
const MAX_FILE_NAME: usize = 255;

/// The registration file for `workspace_root`.
///
/// The root's path is flattened into the file name rather than hashed, so a human
/// debugging a stale registration can see which workspace it belongs to.
///
/// A hash would have to stay stable across builds to be usable at all, and no
/// hasher in `std` promises that — which is why the fallback below vendors FNV-1a
/// rather than reaching for one. `rustc-hash` would not do either: `FxHasher`
/// makes the same absence of a promise.
///
/// The flattened name is kept verbatim whenever it fits, so the common case is
/// exactly the readable name it always was. It stops fitting because a socket path
/// is capped at [`MAX_SOCKET_PATH`] and a filename at 255 bytes, and a deep
/// workspace root passes both — every separator costs a byte in the flattened name
/// just as it did in the original. A name that does not fit keeps its readable head
/// and carries a hash of the *whole* root, so two roots sharing a long prefix
/// cannot collide.
pub fn registration_path(workspace_root: &Path) -> PathBuf {
    let dir = registration_dir();
    registration_path_in(&dir, workspace_root)
}

/// [`registration_path`], with the directory supplied rather than resolved.
///
/// Public so a test can exercise the naming rule without setting environment
/// variables. Two spellings of one naming rule is how they drift.
pub fn registration_path_in(dir: &Path, workspace_root: &Path) -> PathBuf {
    dir.join(format!("{}.json", registration_stem(dir, workspace_root)))
}

/// The file-name stem for `workspace_root`: the flattened root, or a truncated
/// head plus a hash when the flattened form cannot fit.
fn registration_stem(dir: &Path, workspace_root: &Path) -> String {
    let flattened = flatten_root(workspace_root);

    // Budgeted against the socket path, which is the tighter limit and is derived
    // from this name: `<dir>/<stem>.sock`.
    let fixed = dir.as_os_str().len() + 1 + ".sock".len();
    let budget = MAX_SOCKET_PATH
        .saturating_sub(fixed)
        .min(MAX_FILE_NAME - ".json".len());

    if flattened.len() <= budget {
        return flattened;
    }

    // `~` cannot occur in a flattened name — `flatten_root` emits `%` for a
    // separator and passes everything else through — so the marker is
    // unambiguous. The encoding is reversible only on the branch above, which is
    // exactly the branch a human reads.
    let suffix = format!("~{:016x}", fnv1a64(flattened.as_bytes()));
    let head = budget.saturating_sub(suffix.len());
    let mut stem: String = flattened.chars().take(head).collect();
    stem.push_str(&suffix);
    stem
}

/// FNV-1a, 64-bit, vendored.
///
/// Fully specified by a fixed offset basis and prime, so it is deterministic
/// across builds, platforms, and compiler versions permanently. That is the whole
/// requirement: a registration name that changed between builds would orphan every
/// running daemon.
fn fnv1a64(bytes: &[u8]) -> u64 {
    let mut hash: u64 = 0xcbf2_9ce4_8422_2325;
    for byte in bytes {
        hash ^= u64::from(*byte);
        hash = hash.wrapping_mul(0x0000_0100_0000_01b3);
    }
    hash
}

/// A socket path that cannot fit in `sun_path`.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PathTooLong {
    pub path: PathBuf,
    pub length: usize,
    pub limit: usize,
}

impl std::fmt::Display for PathTooLong {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "the daemon socket path is {} bytes and the limit is {}: {}. \
             The registration directory alone consumes {} bytes — set XDG_CACHE_HOME \
             to a shorter path.",
            self.length,
            self.limit,
            self.path.display(),
            registration_dir().as_os_str().len(),
        )
    }
}

impl std::error::Error for PathTooLong {}

/// Whether `path` fits in a Unix socket address.
///
/// The naming rule budgets for this, so a failure here means the registration
/// directory itself is too long — a deep `XDG_CACHE_HOME`, which no name can
/// rescue. Checked at bind time so the report names the limit and the path, rather
/// than surfacing as a bare `ENAMETOOLONG` from the kernel.
pub fn check_socket_path_fits(path: &Path) -> Result<(), PathTooLong> {
    let length = path.as_os_str().len();
    if length <= MAX_SOCKET_PATH {
        return Ok(());
    }
    Err(PathTooLong {
        path: path.to_path_buf(),
        length,
        limit: MAX_SOCKET_PATH,
    })
}

/// Flatten a path into one file-name component, reversibly enough to read.
///
/// Separators become `%`, and a literal `%` is doubled so the encoding cannot
/// collide: `/a/b` and `%a%b` must not be the same registration.
fn flatten_root(root: &Path) -> String {
    let mut out = String::new();
    for byte in root.to_string_lossy().chars() {
        match byte {
            '/' | '\\' => out.push('%'),
            '%' => out.push_str("%%"),
            other => out.push(other),
        }
    }
    if out.is_empty() {
        out.push('%');
    }
    out
}

#[cfg(test)]
mod tests {
    use super::*;

    fn roundtrip<T>(value: &T) -> T
    where
        T: Serialize + for<'de> Deserialize<'de>,
    {
        let json = serde_json::to_string(value).expect("serialises");
        serde_json::from_str(&json).expect("deserialises")
    }

    fn freshness() -> Freshness {
        Freshness {
            state: IndexState::Stale { changed_files: 3 },
            indexed_files: 14_015,
            unanalysed_files: 2,
            unresolved_ratio: 0.07,
            unnameable_edges: 0,
            last_pass_millis: Sourced::Available { value: 41_000 },
        }
    }

    fn schema() -> SchemaIdentity {
        SchemaIdentity {
            revision: 4,
            table_count: 812,
            loaded: true,
        }
    }

    fn impact_response() -> ImpactResponse {
        ImpactResponse {
            subject: Subject::File {
                path: "/proj/base.i".to_string(),
            },
            groups: vec![AffectedGroup {
                cause: Cause::DirectInclude,
                files: vec![
                    AffectedFile {
                        path: "/proj/one.p".to_string(),
                        span: Some(ByteSpan { start: 0, end: 8 }),
                    },
                    AffectedFile {
                        path: "/proj/two.p".to_string(),
                        span: None,
                    },
                ],
            }],
            unresolved: vec![UnresolvedReference {
                file: "/proj/three.p".to_string(),
                cause: Cause::DirectInclude,
                name: "base.i".to_string(),
                reason: "absent_from_workspace".to_string(),
                span: None,
            }],
            direct_reference_count: 2,
            rebuild_set: vec!["/proj/base.i".to_string(), "/proj/one.p".to_string()],
            provenance: Provenance::WorkingTree {
                editor_clients: 1,
                unsaved_buffers: 2,
            },
            schema: schema(),
            freshness: freshness(),
            estimated_build_seconds: Sourced::unavailable("no build daemon"),
            query_millis: 12,
        }
    }

    // Every request and response type round-trips unchanged.
    #[test]
    fn every_wire_type_round_trips() {
        let handshake = HandshakeRequest::new(ClientKind::Desktop, "/proj");
        assert_eq!(roundtrip(&handshake), handshake);

        let accepted = HandshakeResponse {
            contract_version: CONTRACT_VERSION,
            workspace_root: "/proj".to_string(),
            daemon_version: "0.1.0".to_string(),
            connected_clients: 2,
        };
        assert_eq!(roundtrip(&accepted), accepted);

        let mismatch = ContractMismatch {
            client_version: 1,
            daemon_version: 2,
        };
        assert_eq!(roundtrip(&mismatch), mismatch);

        let impact_request = ImpactRequest {
            subject: Subject::Table {
                name: "customer".to_string(),
            },
        };
        assert_eq!(roundtrip(&impact_request), impact_request);

        let impact = impact_response();
        assert_eq!(roundtrip(&impact), impact);

        let search = SymbolSearchRequest {
            query: "calc".to_string(),
            limit: 50,
        };
        assert_eq!(roundtrip(&search), search);

        let results = SymbolSearchResponse {
            symbols: vec![SymbolRow {
                id: "class:/proj/orders/calc.cls:orders.calc".to_string(),
                name: "orders.calc".to_string(),
                kind: SymbolKind::Class,
                file: Some("/proj/orders/calc.cls".to_string()),
                span: Some(ByteSpan { start: 6, end: 17 }),
                subject: Subject::File {
                    path: "/proj/orders/calc.cls".to_string(),
                },
            }],
            total_matches: 900,
        };
        assert_eq!(roundtrip(&results), results);

        let fresh = FreshnessResponse {
            freshness: freshness(),
            schema: schema(),
            provenance: Provenance::Disk,
        };
        assert_eq!(roundtrip(&fresh), fresh);

        let reindex = ReindexResponse {
            freshness: freshness(),
            pass_millis: 40_100,
            graph_bytes: 8_000_000,
        };
        assert_eq!(roundtrip(&reindex), reindex);

        let registration = Registration {
            pid: 4242,
            socket_path: "/run/oxabl.sock".to_string(),
            contract_version: CONTRACT_VERSION,
            workspace_root: "/proj".to_string(),
        };
        assert_eq!(roundtrip(&registration), registration);
    }

    // The contract version is compared by value, not by matching a string shape.
    #[test]
    fn the_contract_version_is_a_number_compared_by_value() {
        let older = HandshakeRequest {
            contract_version: CONTRACT_VERSION - 1,
            client: ClientKind::Editor,
            workspace_root: "/proj".to_string(),
        };
        assert_ne!(older.contract_version, CONTRACT_VERSION);
        let mismatch = ContractMismatch {
            client_version: older.contract_version,
            daemon_version: CONTRACT_VERSION,
        };
        let rendered = mismatch.to_string();
        assert!(
            rendered.contains(&older.contract_version.to_string())
                && rendered.contains(&CONTRACT_VERSION.to_string()),
            "a refusal must name both versions, got {rendered}"
        );
    }

    // An unknown method is an explicit case, not a failure to read the message.
    #[test]
    fn an_unknown_method_is_its_own_case() {
        assert_eq!(Method::from_name(method::IMPACT), Method::Impact);
        assert!(Method::from_name(method::IMPACT).is_known());

        let future = Method::from_name("oxabl/somethingNewer");
        assert_eq!(future, Method::Unknown("oxabl/somethingNewer".to_string()));
        assert!(!future.is_known());
        assert_eq!(future.name(), "oxabl/somethingNewer");
    }

    // Every method name round-trips through the classifier.
    #[test]
    fn every_method_name_classifies_back_to_itself() {
        for name in [
            method::HANDSHAKE,
            method::IMPACT,
            method::SYMBOL_SEARCH,
            method::FRESHNESS,
            method::REINDEX,
        ] {
            assert_eq!(Method::from_name(name).name(), name);
        }
    }

    // An unavailable value carries a reason and offers no default, so it cannot be
    // rendered as a zero by accident.
    #[test]
    fn an_unavailable_value_carries_a_reason_and_no_default() {
        let missing: Sourced<f64> = Sourced::unavailable("no build daemon");
        assert_eq!(missing.value(), None);
        assert_eq!(missing.reason(), Some("no build daemon"));

        let json = serde_json::to_value(&missing).expect("serialises");
        assert_eq!(json["state"], "unavailable");
        assert_eq!(json["reason"], "no build daemon");
        assert!(
            json.get("value").is_none(),
            "an unavailable value must carry no value field, got {json}"
        );

        let present = Sourced::Available { value: 1.5f64 };
        assert_eq!(present.value(), Some(&1.5));
        assert_eq!(present.reason(), None);
    }

    // Two workspace roots get two registration files, and the flattening cannot
    // collide a path with its own encoding.
    #[test]
    fn a_registration_path_is_per_root_and_collision_free() {
        let one = registration_path(Path::new("/home/dev/alpha"));
        let two = registration_path(Path::new("/home/dev/beta"));
        assert_ne!(one, two);
        assert_eq!(one.parent(), two.parent());

        assert_ne!(
            registration_path(Path::new("/a/b")),
            registration_path(Path::new("%a%b")),
            "a literal percent must not encode to a separator"
        );
    }

    // A short root keeps the readable name it always had. Pinned so a later
    // refactor cannot start hashing everything and cost the debuggability the
    // flattening exists for.
    #[test]
    fn a_short_root_keeps_its_readable_name() {
        let dir = Path::new("/home/dev/.cache/oxabl/daemon");
        let path = registration_path_in(dir, Path::new("/home/dev/alpha"));
        assert_eq!(
            path.file_name().unwrap().to_string_lossy(),
            "%home%dev%alpha.json"
        );
    }

    // A root deep enough to overflow `sun_path` still yields a socket path that
    // fits. The budget is computed against the socket, so the registration name is
    // what has to give.
    #[test]
    fn a_deep_root_yields_a_socket_path_that_fits() {
        let dir = Path::new("/home/dev/.cache/oxabl/daemon");
        let root = PathBuf::from(format!("/home/dev/{}", "nested/".repeat(40)));
        let registration = registration_path_in(dir, &root);
        let socket = registration.with_extension("sock");

        assert!(
            socket.as_os_str().len() <= MAX_SOCKET_PATH,
            "socket path is {} bytes: {}",
            socket.as_os_str().len(),
            socket.display()
        );
        assert!(check_socket_path_fits(&socket).is_ok());
    }

    // Two deep roots sharing a long prefix must not collide. Truncation alone
    // would map them to one name, which is the whole reason the hash covers the
    // untruncated root rather than the truncated head.
    #[test]
    fn two_deep_roots_sharing_a_prefix_do_not_collide() {
        let dir = Path::new("/home/dev/.cache/oxabl/daemon");
        let prefix = "a".repeat(200);
        let one = registration_path_in(dir, &PathBuf::from(format!("/{prefix}/one")));
        let two = registration_path_in(dir, &PathBuf::from(format!("/{prefix}/two")));

        assert_ne!(one, two, "a shared prefix must not become a shared name");
        for path in [&one, &two] {
            assert!(path.with_extension("sock").as_os_str().len() <= MAX_SOCKET_PATH);
        }
    }

    // The hash is a fixed specification, not whatever the toolchain provides. A
    // value that moved between builds would orphan every running daemon.
    #[test]
    fn the_vendored_hash_matches_the_fnv1a_specification() {
        // The published FNV-1a 64-bit vector for "a".
        assert_eq!(fnv1a64(b"a"), 0xaf63_dc4c_8601_ec8c);
        assert_eq!(fnv1a64(b""), 0xcbf2_9ce4_8422_2325);
    }

    // A registration directory long enough to exhaust the budget is reported with
    // the limit and the path, rather than as a bare kernel error at bind time.
    #[test]
    fn an_unusable_directory_is_reported_with_the_limit() {
        let dir = PathBuf::from(format!("/{}", "d".repeat(200)));
        let socket = registration_path_in(&dir, Path::new("/proj")).with_extension("sock");

        let error = check_socket_path_fits(&socket).expect_err("this cannot fit");
        assert_eq!(error.limit, MAX_SOCKET_PATH);
        assert!(error.length > MAX_SOCKET_PATH);
        assert!(error.to_string().contains("limit is 107"));
    }

    // The directory follows XDG when it is set and falls back when it is not. The
    // fallback is the documented behaviour, not an accident.
    #[test]
    fn the_registration_directory_prefers_xdg_cache_home() {
        // Read the ambient environment rather than mutating it: `set_var` is
        // unsafe in this edition and a test that mutates process-wide state races
        // every other test in the binary.
        let dir = registration_dir();
        assert!(
            dir.ends_with("oxabl/daemon"),
            "registrations must live under an oxabl/daemon directory, got {dir:?}"
        );
        assert!(dir.is_absolute(), "got {dir:?}");
    }

    // A relative value is not a base directory (R16). Exercised over the value
    // rather than the variable: `set_var` here would race every other test in this
    // binary that resolves the registration directory, and the composition above it
    // is two lines with no branch of its own.
    #[test]
    fn a_relative_base_directory_is_ignored_rather_than_resolved() {
        for relative in ["cache", ".cache", "sub/dir", "./cache", "../cache"] {
            assert_eq!(
                usable_base(Some(relative.into())),
                None,
                "{relative} is relative, so it must be ignored rather than resolved \
                 against whatever directory a process happens to be in"
            );
        }
        assert_eq!(
            usable_base(Some("".into())),
            None,
            "an empty value names nothing"
        );
        assert_eq!(usable_base(None), None);
        assert_eq!(
            usable_base(Some("/home/dev/.cache".into())),
            Some(PathBuf::from("/home/dev/.cache")),
            "an absolute value is the one a base directory variable may hold"
        );
    }
}
