/// A file that `--files` named, and the way from a unit to it.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Named {
    pub file: String,
    /// The units and files from the unit to the named file, both included;
    /// just the file when the unit is the file.
    pub via: Vec<String>,
}

/// Why a unit is in the rebuild set.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Reason {
    /// The store had not seen the unit.
    New,
    /// `--files` named a file in its closure.
    Named(Vec<Named>),
    /// The unit's own bytes changed.
    SourceChanged,
    /// Files that stayed in its closure changed.
    DependencyChanged(Vec<String>),
    /// Its closure gained or lost files.
    ClosureChanged {
        gained: Vec<String>,
        lost: Vec<String>,
    },
}

const SHOWN: usize = 5;

fn list(items: &[String]) -> String {
    let mut out = items
        .iter()
        .take(SHOWN)
        .cloned()
        .collect::<Vec<_>>()
        .join(", ");
    if items.len() > SHOWN {
        out.push_str(&format!(", +{} more", items.len() - SHOWN));
    }
    out
}

impl Reason {
    /// The name of the reason.
    pub fn kind(&self) -> &'static str {
        match self {
            Reason::New => "new",
            Reason::Named(_) => "named",
            Reason::SourceChanged => "source-changed",
            Reason::DependencyChanged(_) => "dependency-changed",
            Reason::ClosureChanged { .. } => "closure-changed",
        }
    }

    /// The reason as one line: its name and its details.
    pub fn describe(&self) -> String {
        match self {
            Reason::New | Reason::SourceChanged => self.kind().to_string(),
            Reason::Named(named) => {
                let files: Vec<String> = named
                    .iter()
                    .map(|n| match n.via.len() {
                        0..=2 => n.file.clone(),
                        _ => format!("{} ({})", n.file, n.via.join(" > ")),
                    })
                    .collect();
                format!("named: {}", list(&files))
            }
            Reason::DependencyChanged(files) => format!("dependency-changed: {}", list(files)),
            Reason::ClosureChanged { gained, lost } => {
                let changes: Vec<String> = gained
                    .iter()
                    .map(|f| format!("+{f}"))
                    .chain(lost.iter().map(|f| format!("-{f}")))
                    .collect();
                format!("closure-changed: {}", list(&changes))
            }
        }
    }
}
