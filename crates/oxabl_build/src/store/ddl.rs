/// The format of the store. Any change to the tables, to a blob encoding or to
/// the meaning of a column increments it; a store of another format is
/// replaced, never migrated.
pub const STORE_FORMAT: i32 = 1;

/// `OXBL`: marks a file as an oxabl store.
pub const APPLICATION_ID: i32 = 0x4F58_424C;

pub const SCHEMA: &str = "
CREATE TABLE meta (
    key   TEXT PRIMARY KEY,
    value BLOB NOT NULL
) WITHOUT ROWID;

CREATE TABLE paths (
    id   INTEGER PRIMARY KEY,
    path TEXT NOT NULL UNIQUE
);

CREATE TABLE files (
    path_id     INTEGER PRIMARY KEY REFERENCES paths(id),
    present     INTEGER NOT NULL,
    size        INTEGER,
    mtime_ns    INTEGER,
    ctime_ns    INTEGER,
    ino         INTEGER,
    dev         INTEGER,
    hash        BLOB,
    observed_ns INTEGER NOT NULL,
    changed_gen INTEGER NOT NULL,
    tracked_by  INTEGER NOT NULL
);

CREATE TABLE listings (
    path_id     INTEGER PRIMARY KEY REFERENCES paths(id),
    mtime_ns    INTEGER NOT NULL,
    ctime_ns    INTEGER NOT NULL,
    ino         INTEGER NOT NULL,
    observed_ns INTEGER NOT NULL,
    digest      BLOB NOT NULL,
    changed_gen INTEGER NOT NULL
);

CREATE TABLE analyses (
    path_id      INTEGER PRIMARY KEY REFERENCES paths(id),
    analysed_gen INTEGER NOT NULL,
    content_hash BLOB NOT NULL,
    completeness BLOB NOT NULL,
    edges        BLOB NOT NULL,
    unresolved   BLOB NOT NULL,
    observations BLOB NOT NULL
);

CREATE TABLE results (
    unit_id      INTEGER PRIMARY KEY REFERENCES paths(id),
    status       INTEGER NOT NULL,
    key          BLOB,
    components   BLOB NOT NULL,
    compiled_gen INTEGER NOT NULL,
    origin       INTEGER NOT NULL,
    duration_ms  INTEGER NOT NULL,
    finished_at  INTEGER NOT NULL,
    note         TEXT
);

CREATE TABLE outputs (
    unit_id  INTEGER NOT NULL REFERENCES paths(id),
    path     TEXT NOT NULL,
    size     INTEGER NOT NULL,
    mtime_ns INTEGER NOT NULL,
    ino      INTEGER NOT NULL,
    hash     BLOB NOT NULL,
    PRIMARY KEY (unit_id, path)
) WITHOUT ROWID;

CREATE UNIQUE INDEX outputs_by_path ON outputs(path);

CREATE TABLE environments (
    identity   BLOB PRIMARY KEY,
    record     TEXT NOT NULL,
    first_seen INTEGER NOT NULL
) WITHOUT ROWID;
";
