use std::sync::Mutex;
use std::sync::atomic::{AtomicU32, Ordering};

use rustc_hash::{FxHashMap, FxHasher};
use std::hash::{Hash, Hasher};

const SHARDS: usize = 64;

/// Path ids, minted on any thread. A path keeps its id for the life of the
/// store; ids are never reused.
pub struct Interner {
    shards: Vec<Mutex<FxHashMap<Box<str>, u32>>>,
    stored: Vec<String>,
    minted: Mutex<FxHashMap<u32, Box<str>>>,
    next: AtomicU32,
    committed: u32,
}

impl Interner {
    /// An interner that holds the stored `names`; index 0 is unused.
    pub fn from_names(names: Vec<String>) -> Self {
        let committed = names.len().max(1) as u32;
        let interner = Interner {
            shards: (0..SHARDS).map(|_| Mutex::default()).collect(),
            stored: Vec::new(),
            minted: Mutex::default(),
            next: AtomicU32::new(committed),
            committed,
        };
        for (id, name) in names.iter().enumerate().skip(1) {
            if !name.is_empty() {
                interner.shard(name).insert(name.as_str().into(), id as u32);
            }
        }
        Interner {
            stored: names,
            ..interner
        }
    }

    fn shard(&self, name: &str) -> std::sync::MutexGuard<'_, FxHashMap<Box<str>, u32>> {
        let mut hasher = FxHasher::default();
        name.hash(&mut hasher);
        self.shards[hasher.finish() as usize % SHARDS]
            .lock()
            .unwrap_or_else(|e| e.into_inner())
    }

    /// The id of `name`, minting one when it has none.
    pub fn intern(&self, name: &str) -> u32 {
        let mut shard = self.shard(name);
        if let Some(id) = shard.get(name) {
            return *id;
        }
        let id = self.next.fetch_add(1, Ordering::Relaxed);
        shard.insert(name.into(), id);
        drop(shard);
        self.minted
            .lock()
            .unwrap_or_else(|e| e.into_inner())
            .insert(id, name.into());
        id
    }

    /// The name of path id `id`.
    pub fn name_of(&self, id: u32) -> Option<String> {
        match self.stored.get(id as usize) {
            Some(name) if !name.is_empty() => Some(name.clone()),
            _ => self
                .minted
                .lock()
                .unwrap_or_else(|e| e.into_inner())
                .get(&id)
                .map(|name| name.to_string()),
        }
    }

    /// The id of `name`, if it has one.
    pub fn lookup(&self, name: &str) -> Option<u32> {
        self.shard(name).get(name).copied()
    }

    /// Every name by id, and the first id that is not stored yet.
    pub fn into_names(self) -> (Vec<String>, u32) {
        let mut names = self.stored;
        names.resize(self.next.load(Ordering::Relaxed) as usize, String::new());
        for (id, name) in self.minted.into_inner().unwrap_or_else(|e| e.into_inner()) {
            names[id as usize] = name.into_string();
        }
        (names, self.committed)
    }
}
