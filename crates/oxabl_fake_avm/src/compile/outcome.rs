//! What one compile reads and reports.

use std::collections::HashSet;

/// One file the compiler opened, with the SHA-256 of its bytes.
#[derive(Clone)]
pub struct Read {
    pub shown: String,
    pub sha256: String,
}

/// One compiler message.
pub struct Message {
    pub kind: &'static str,
    pub number: u32,
    pub file: String,
    pub row: usize,
    pub column: usize,
    pub text: String,
}

/// The result of compiling one unit.
pub struct Outcome {
    pub ok: bool,
    pub messages: Vec<Message>,
    pub reads: Vec<Read>,
    /// Each reference the XREF names: its type and identifier, as written.
    pub references: Vec<(&'static str, String)>,
    /// The text of the unit itself.
    pub source: String,
    pub slow: bool,
    /// Report success and write no outputs.
    pub no_output: bool,
    seen: HashSet<String>,
}

impl Outcome {
    pub fn new() -> Outcome {
        Outcome {
            ok: true,
            messages: Vec::new(),
            reads: Vec::new(),
            references: Vec::new(),
            source: String::new(),
            slow: false,
            no_output: false,
            seen: HashSet::new(),
        }
    }

    /// Whether a file of that name was read already.
    pub fn has_read(&self, shown: &str) -> bool {
        self.seen.contains(shown)
    }

    pub fn record(&mut self, read: Read) {
        if self.seen.insert(read.shown.clone()) {
            self.reads.push(read);
        }
    }

    pub fn fail(&mut self, file: &str, row: usize, number: u32, text: &str) {
        self.ok = false;
        self.messages.push(Message {
            kind: "error",
            number,
            file: file.to_string(),
            row,
            column: 1,
            text: text.to_string(),
        });
    }

    pub fn warn(&mut self, file: &str, row: usize, text: &str) {
        self.messages.push(Message {
            kind: "warning",
            number: 2345,
            file: file.to_string(),
            row,
            column: 1,
            text: text.to_string(),
        });
    }

    /// Takes over what a class compiled on the side read and said.
    pub fn merge(&mut self, other: Outcome) {
        self.ok &= other.ok;
        self.messages.extend(other.messages);
        for read in other.reads {
            self.record(read);
        }
        self.references.extend(other.references);
    }
}
