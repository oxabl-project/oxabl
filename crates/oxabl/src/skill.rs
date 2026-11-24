//! `oxabl skill`: the agent skill that ships inside the binary.
//!
//! The skill is an Agent Skills folder (`SKILL.md` plus reference files) that
//! teaches a coding agent to use oxabl. The source lives in `crates/oxabl/skill/`
//! and says `dev` for its version; `show` and `install` write the binary's own.
//!
//! `install` keeps one copy of the skill, the store, in oxabl's data directory
//! and links it from the folders that coding agents read.

use std::fs;
use std::io;
use std::path::{Path, PathBuf};
use std::process::ExitCode;
use std::sync::atomic::{AtomicU32, Ordering};

use clap::{Args, Subcommand, ValueEnum};

use crate::paths;

/// Every file of the skill, by path inside the skill directory. A file added
/// to `skill/` must be added here; a test compares the list with the directory.
const FILES: &[(&str, &str)] = &[
    ("SKILL.md", include_str!("../skill/SKILL.md")),
    (
        "references/build.md",
        include_str!("../skill/references/build.md"),
    ),
    (
        "references/check.md",
        include_str!("../skill/references/check.md"),
    ),
    (
        "references/explain.md",
        include_str!("../skill/references/explain.md"),
    ),
    (
        "references/rules.md",
        include_str!("../skill/references/rules.md"),
    ),
];

const ENTRY: &str = "SKILL.md";
const SOURCE_VERSION: &str = "oxabl-version: dev";
const SKILL_NAME: &str = "oxabl";

#[derive(Subcommand)]
pub enum Command {
    /// Print the skill's SKILL.md, or another file of the skill.
    ///
    /// The version in the front matter is the binary's. `--list` names every
    /// file of the skill, one per line. Exit 2 for a file the skill does not
    /// have.
    Show {
        /// File to print, as `--list` names it (default: SKILL.md)
        #[arg(conflicts_with = "list")]
        file: Option<String>,

        /// List the files of the skill instead of printing one
        #[arg(long)]
        list: bool,
    },
    /// Install the skill for your user, where coding agents look for it.
    ///
    /// oxabl keeps one copy, the store, in
    /// `${XDG_DATA_HOME:-~/.local/share}/oxabl/skills/oxabl/`, and links it
    /// from `~/.agents/skills/oxabl` (read by Codex, pi, OpenCode, Copilot and
    /// others) and `~/.claude/skills/oxabl` (Claude Code). One line for the
    /// store and one per link say its path and state. Every project then sees
    /// the skill, and a new oxabl version updates the one copy.
    ///
    /// oxabl owns the store and replaces it as a whole: it writes a staging
    /// directory beside it and swaps it in, so a failed install leaves the old
    /// store whole. A link is created when missing and left alone when it
    /// already points to the store. Anything else at a link's path (a real
    /// directory, a file, a symlink to somewhere else) is foreign: it is
    /// refused, and nothing is written anywhere. `--force` moves it out of the
    /// skills folders (agents would load a backup there as a second skill) to
    /// `${XDG_DATA_HOME:-~/.local/share}/oxabl/skill-backups/<agents|claude|store>-<UTC timestamp>`,
    /// never deletes or copies it, and links. When the move fails (another
    /// filesystem), nothing else is written and the error names the entry.
    ///
    /// Where a symlink cannot be made (Windows without the privilege), oxabl
    /// copies the store to the link's path instead and says so. Such a copy
    /// counts as oxabl's: `--refresh` and a new install replace it whole.
    ///
    /// Exit codes: `0` on success, `1` when `--check` finds the store missing
    /// or stale, or a selected link missing, foreign or pointing elsewhere, `2`
    /// on a usage or I/O error, a refused entry, or no home directory.
    Install(InstallArgs),
}

#[derive(Args)]
pub struct InstallArgs {
    /// Which links to create
    #[arg(long, value_enum, default_value_t = Agent::All)]
    agent: Agent,

    /// Write nothing; exit 1 when the store or a selected link is not in place
    #[arg(long, conflicts_with_all = ["refresh", "force"])]
    check: bool,

    /// Update the store and the copies that exist; create nothing
    #[arg(long, conflicts_with = "force")]
    refresh: bool,

    /// Move a foreign entry to the `skill-backups` data folder and install
    #[arg(long)]
    force: bool,
}

#[derive(Clone, Copy, ValueEnum)]
enum Agent {
    /// `~/.agents/skills/oxabl`
    Agents,
    /// `~/.claude/skills/oxabl`
    Claude,
    /// Both
    All,
}

impl Agent {
    fn links(self) -> &'static [&'static str] {
        match self {
            Agent::Agents => &[".agents/skills/oxabl"],
            Agent::Claude => &[".claude/skills/oxabl"],
            Agent::All => &[".agents/skills/oxabl", ".claude/skills/oxabl"],
        }
    }
}

pub fn run(command: Command) -> ExitCode {
    let result = match command {
        Command::Show { file, list } => show(file.as_deref(), list),
        Command::Install(args) => install(&args),
    };
    match result {
        Ok(code) => code,
        Err(message) => {
            eprintln!("error: {message}");
            ExitCode::from(2)
        }
    }
}

/// The skill as installed: the source with the binary's version filled in.
fn files() -> Vec<(&'static str, String)> {
    FILES
        .iter()
        .map(|&(name, text)| {
            let text = if name == ENTRY {
                text.replacen(
                    SOURCE_VERSION,
                    &format!("oxabl-version: {}", env!("CARGO_PKG_VERSION")),
                    1,
                )
            } else {
                text.to_string()
            };
            (name, text)
        })
        .collect()
}

fn show(file: Option<&str>, list: bool) -> Result<ExitCode, String> {
    if list {
        for (name, _) in FILES {
            println!("{name}");
        }
        return Ok(ExitCode::SUCCESS);
    }
    let wanted = file.unwrap_or(ENTRY);
    let wanted = wanted.strip_prefix("./").unwrap_or(wanted);
    match files().into_iter().find(|(name, _)| *name == wanted) {
        Some((_, text)) => {
            print!("{text}");
            Ok(ExitCode::SUCCESS)
        }
        None => Err(format!(
            "the skill has no file {wanted:?}; its files are: {}",
            FILES
                .iter()
                .map(|(name, _)| *name)
                .collect::<Vec<_>>()
                .join(", ")
        )),
    }
}

/// Whether a SKILL.md says it is the oxabl skill that oxabl installed.
fn is_oxabl_copy(text: &str) -> bool {
    let mut lines = text.lines();
    if lines.next().map(str::trim_end) != Some("---") {
        return false;
    }
    let (mut name, mut version) = (false, false);
    for line in lines {
        if line.trim_end() == "---" {
            break;
        }
        if let Some(value) = line.strip_prefix("name:") {
            name = value.trim().trim_matches(['"', '\'']) == SKILL_NAME;
        } else if let Some(value) = line.trim_start().strip_prefix("oxabl-version:") {
            version = !value.trim().is_empty();
        }
    }
    name && version
}

fn io_error(path: &Path, error: &io::Error) -> String {
    format!("{}: {error}", path.display())
}

fn not_found(error: &io::Error) -> bool {
    matches!(error.kind(), io::ErrorKind::NotFound)
}

/// What sits at the path of the store or of a link.
enum Entry {
    Missing,
    /// A symlink to the store.
    Linked,
    /// A symlink to somewhere else.
    Elsewhere(PathBuf),
    /// An oxabl copy of the skill (the store, or a link's copy where symlinks
    /// are not possible); what differs from the current skill, empty when it
    /// is current.
    Copy(Vec<String>),
    /// Anything oxabl did not make, and why it looks so.
    Foreign(String),
}

/// Relative paths (with `/`) of the files under `root`.
fn list_files(root: &Path, prefix: &Path, out: &mut Vec<String>) -> Result<(), String> {
    let here = root.join(prefix);
    let entries = fs::read_dir(&here).map_err(|e| io_error(&here, &e))?;
    for entry in entries {
        let entry = entry.map_err(|e| io_error(&here, &e))?;
        let relative = prefix.join(entry.file_name());
        let kind = entry.file_type().map_err(|e| io_error(&entry.path(), &e))?;
        if kind.is_dir() {
            list_files(root, &relative, out)?;
        } else {
            out.push(
                relative
                    .components()
                    .map(|c| c.as_os_str().to_string_lossy())
                    .collect::<Vec<_>>()
                    .join("/"),
            );
        }
    }
    Ok(())
}

/// What differs between the directory `dir` and the skill.
fn differences(dir: &Path, files: &[(&str, String)]) -> Result<Vec<String>, String> {
    let mut problems = Vec::new();
    for (name, expected) in files {
        match fs::read(dir.join(name)) {
            Ok(bytes) if bytes == expected.as_bytes() => {}
            Ok(_) => problems.push(format!("{name} differs")),
            Err(error) if not_found(&error) => problems.push(format!("{name} is missing")),
            Err(error) => return Err(io_error(&dir.join(name), &error)),
        }
    }
    let mut present = Vec::new();
    list_files(dir, Path::new(""), &mut present)?;
    present.sort();
    for name in present {
        if !files.iter().any(|(expected, _)| *expected == name) {
            problems.push(format!("{name} is not part of the skill"));
        }
    }
    Ok(problems)
}

/// Whether the symlink `link`, which points to `target`, resolves to `store`.
fn points_to(link: &Path, target: &Path, store: &Path) -> bool {
    let joined = link
        .parent()
        .map_or_else(|| target.to_path_buf(), |p| p.join(target));
    match (fs::canonicalize(&joined), fs::canonicalize(store)) {
        (Ok(a), Ok(b)) => a == b,
        _ => joined == store,
    }
}

/// `store` is the store's path when `path` may be a symlink to it (a link of
/// an agent); the store itself may not be a symlink.
fn inspect(path: &Path, store: Option<&Path>, files: &[(&str, String)]) -> Result<Entry, String> {
    let meta = match fs::symlink_metadata(path) {
        Ok(meta) => meta,
        Err(error) if not_found(&error) => return Ok(Entry::Missing),
        Err(error) => return Err(io_error(path, &error)),
    };
    if meta.file_type().is_symlink() {
        let target = fs::read_link(path).map_err(|e| io_error(path, &e))?;
        return Ok(match store {
            Some(store) if points_to(path, &target, store) => Entry::Linked,
            Some(_) => Entry::Elsewhere(target),
            None => Entry::Foreign("a symlink".to_string()),
        });
    }
    if !meta.is_dir() {
        return Ok(Entry::Foreign("a file".to_string()));
    }
    let entry = path.join(ENTRY);
    let text = match fs::read(&entry) {
        Ok(bytes) => String::from_utf8_lossy(&bytes).into_owned(),
        Err(error) if not_found(&error) => {
            return Ok(Entry::Foreign("a directory without SKILL.md".to_string()));
        }
        Err(error) => return Err(io_error(&entry, &error)),
    };
    if !is_oxabl_copy(&text) {
        return Ok(Entry::Foreign(
            "a directory whose SKILL.md oxabl did not install".to_string(),
        ));
    }
    Ok(Entry::Copy(differences(path, files)?))
}

fn unique() -> String {
    static COUNTER: AtomicU32 = AtomicU32::new(0);
    let nanos = std::time::SystemTime::now()
        .duration_since(std::time::UNIX_EPOCH)
        .map_or(0, |d| d.subsec_nanos());
    format!(
        "{:x}{:x}{}",
        std::process::id(),
        nanos,
        COUNTER.fetch_add(1, Ordering::Relaxed)
    )
}

/// `YYYYMMDDTHHMMSSZ` for a time in seconds since the epoch.
fn utc_stamp(seconds: u64) -> String {
    let (days, rest) = ((seconds / 86_400) as i64, seconds % 86_400);
    // Civil date from a day count (proleptic Gregorian, days since 1970-01-01).
    let z = days + 719_468;
    let era = z.div_euclid(146_097);
    let day_of_era = z.rem_euclid(146_097);
    let year_of_era =
        (day_of_era - day_of_era / 1_460 + day_of_era / 36_524 - day_of_era / 146_096) / 365;
    let day_of_year = day_of_era - (365 * year_of_era + year_of_era / 4 - year_of_era / 100);
    let month_index = (5 * day_of_year + 2) / 153;
    let day = day_of_year - (153 * month_index + 2) / 5 + 1;
    let month = if month_index < 10 {
        month_index + 3
    } else {
        month_index - 9
    };
    let year = year_of_era + era * 400 + i64::from(month <= 2);
    format!(
        "{year:04}{month:02}{day:02}T{:02}{:02}{:02}Z",
        rest / 3_600,
        rest % 3_600 / 60,
        rest % 60
    )
}

/// `<backups>/<label>-<UTC timestamp>`, one that does not exist yet.
fn backup_path(backups: &Path, label: &str) -> PathBuf {
    let seconds = std::time::SystemTime::now()
        .duration_since(std::time::UNIX_EPOCH)
        .map_or(0, |d| d.as_secs());
    let stamp = utc_stamp(seconds);
    let mut candidate = backups.join(format!("{label}-{stamp}"));
    let mut n = 1;
    while fs::symlink_metadata(&candidate).is_ok() {
        candidate = backups.join(format!("{label}-{stamp}-{n}"));
        n += 1;
    }
    candidate
}

/// `agents` or `claude` for `~/.agents/skills/oxabl` or `~/.claude/skills/oxabl`.
fn link_label(link: &Path) -> String {
    link.parent()
        .and_then(Path::parent)
        .and_then(Path::file_name)
        .map_or_else(
            || "link".to_string(),
            |name| name.to_string_lossy().trim_start_matches('.').to_string(),
        )
}

/// Renames each foreign entry of `all` into `backups`, in order. A rename that
/// fails (another filesystem, no permission) puts the earlier ones back and
/// returns an error that names the entry; nothing is copied or deleted.
fn move_away(
    backups: &Path,
    all: &[(&Path, &Entry)],
    labels: &[String],
) -> Result<Vec<Option<PathBuf>>, String> {
    let mut moved: Vec<Option<PathBuf>> = Vec::new();
    for ((path, entry), label) in all.iter().zip(labels) {
        if !matches!(entry, Entry::Foreign(_) | Entry::Elsewhere(_)) {
            moved.push(None);
            continue;
        }
        let target = backup_path(backups, label);
        let result = fs::create_dir_all(backups)
            .map_err(|e| io_error(backups, &e))
            .and_then(|()| {
                fs::rename(path, &target).map_err(|e| {
                    format!(
                        "{} could not be moved to {}: {e}; nothing else was written; move it out of the skills folder yourself, then run the install again",
                        path.display(),
                        target.display()
                    )
                })
            });
        if let Err(error) = result {
            for (earlier, back) in all.iter().zip(&moved) {
                if let Some(back) = back {
                    let _ = fs::rename(back, earlier.0);
                }
            }
            return Err(error);
        }
        moved.push(Some(target));
    }
    Ok(moved)
}

/// The nearest ancestor of `path` that exists must be a directory this process
/// can create files in; fails with the reason otherwise.
fn writable_above(path: &Path) -> Result<(), String> {
    for parent in path.ancestors().skip(1) {
        match fs::metadata(parent) {
            Ok(meta) if meta.is_dir() => {
                let probe = parent.join(format!(".oxabl-skill-probe-{}", unique()));
                fs::File::create_new(&probe).map_err(|e| io_error(&probe, &e))?;
                return fs::remove_file(&probe).map_err(|e| io_error(&probe, &e));
            }
            Ok(_) => return Err(format!("{} is not a directory", parent.display())),
            Err(error) if not_found(&error) => {
                if fs::symlink_metadata(parent).is_ok() {
                    return Err(format!(
                        "{} is a symlink that cannot be followed",
                        parent.display()
                    ));
                }
            }
            Err(error) => return Err(io_error(parent, &error)),
        }
    }
    Err(format!("{} has no directory above it", path.display()))
}

fn write_file(path: &Path, text: &str) -> Result<(), String> {
    use std::io::Write;
    let parent = path.parent().ok_or("a skill file has no directory")?;
    fs::create_dir_all(parent).map_err(|e| io_error(parent, &e))?;
    fs::File::create(path)
        .and_then(|mut file| {
            file.write_all(text.as_bytes())?;
            file.sync_all()
        })
        .map_err(|e| io_error(path, &e))
}

/// Puts the skill at `dest` as a whole: it writes a staging directory beside
/// `dest`, moves what is at `dest` aside (it deletes that after the swap), and
/// renames the staging directory to `dest`. A failure before the swap leaves
/// `dest` as it was.
fn replace_tree(dest: &Path, files: &[(&str, String)]) -> Result<(), String> {
    let parent = dest
        .parent()
        .ok_or("the skill has no directory to live in")?;
    fs::create_dir_all(parent).map_err(|e| io_error(parent, &e))?;
    let staging = parent.join(format!(".oxabl-skill-staging-{}", unique()));
    let staged = files
        .iter()
        .try_for_each(|(name, text)| write_file(&staging.join(name), text));
    if let Err(error) = staged {
        let _ = fs::remove_dir_all(&staging);
        return Err(error);
    }
    if fs::symlink_metadata(dest).is_err() {
        return fs::rename(&staging, dest).map_err(|e| {
            let _ = fs::remove_dir_all(&staging);
            io_error(dest, &e)
        });
    }
    let old = parent.join(format!(".oxabl-skill-old-{}", unique()));
    if let Err(error) = fs::rename(dest, &old) {
        let _ = fs::remove_dir_all(&staging);
        return Err(io_error(dest, &error));
    }
    if let Err(error) = fs::rename(&staging, dest) {
        let _ = fs::rename(&old, dest);
        let _ = fs::remove_dir_all(&staging);
        return Err(io_error(dest, &error));
    }
    let _ = fs::remove_dir_all(&old);
    Ok(())
}

#[cfg(unix)]
fn symlink(store: &Path, link: &Path) -> io::Result<()> {
    std::os::unix::fs::symlink(store, link)
}

#[cfg(windows)]
fn symlink(store: &Path, link: &Path) -> io::Result<()> {
    std::os::windows::fs::symlink_dir(store, link)
}

/// Links `link` to `store`; where that is not possible, copies the skill
/// there. Returns the state word.
fn link_or_copy(link: &Path, store: &Path, files: &[(&str, String)]) -> Result<String, String> {
    let parent = link.parent().ok_or("a link has no directory to live in")?;
    fs::create_dir_all(parent).map_err(|e| io_error(parent, &e))?;
    match symlink(store, link) {
        Ok(()) => Ok("linked".to_string()),
        Err(error) => {
            replace_tree(link, files)?;
            Ok(format!("copied (a symlink was not possible: {error})"))
        }
    }
}

enum Mode {
    Install { force: bool },
    Check,
    Refresh,
}

/// The store: `<data dir>/skills/oxabl`, absolute.
fn store_dir() -> Result<PathBuf, String> {
    let data = paths::data_dir()
        .ok_or("neither XDG_DATA_HOME nor HOME is set, so there is no place for the skill")?;
    let data = std::path::absolute(&data).map_err(|e| io_error(&data, &e))?;
    Ok(data.join("skills").join(SKILL_NAME))
}

fn install(args: &InstallArgs) -> Result<ExitCode, String> {
    let home =
        paths::home().ok_or("HOME is not set, so there is no home directory to install in")?;
    let store = store_dir()?;
    let links: Vec<PathBuf> = args
        .agent
        .links()
        .iter()
        .map(|rel| home.join(rel))
        .collect();
    let mode = if args.check {
        Mode::Check
    } else if args.refresh {
        Mode::Refresh
    } else {
        Mode::Install { force: args.force }
    };
    let files = files();

    let store_entry = inspect(&store, None, &files)?;
    let link_entries = links
        .iter()
        .map(|link| inspect(link, Some(&store), &files))
        .collect::<Result<Vec<_>, _>>()?;

    match mode {
        Mode::Check => Ok(check(&store, &store_entry, &links, &link_entries)),
        Mode::Refresh => refresh(&store, &store_entry, &links, &link_entries, &files),
        Mode::Install { force } => {
            install_all(force, &store, &store_entry, &links, &link_entries, &files)
        }
    }
}

fn describe(entry: &Entry) -> String {
    match entry {
        Entry::Missing => "missing".to_string(),
        Entry::Linked => "current".to_string(),
        Entry::Elsewhere(target) => format!("foreign (a symlink to {})", target.display()),
        Entry::Copy(problems) if problems.is_empty() => "current".to_string(),
        Entry::Copy(problems) => format!("stale ({})", problems.join(", ")),
        Entry::Foreign(what) => format!("foreign ({what})"),
    }
}

fn check(store: &Path, store_entry: &Entry, links: &[PathBuf], entries: &[Entry]) -> ExitCode {
    let ok = |entry: &Entry| {
        matches!(entry, Entry::Linked)
            || matches!(entry, Entry::Copy(problems) if problems.is_empty())
    };
    let mut failed = !ok(store_entry);
    println!("{}: {}", store.display(), describe(store_entry));
    for (link, entry) in links.iter().zip(entries) {
        let copy = matches!(entry, Entry::Copy(_));
        failed |= !ok(entry);
        let word = describe(entry);
        println!(
            "{}: {word}{}",
            link.display(),
            if copy { " (a copy)" } else { "" }
        );
    }
    if failed {
        ExitCode::from(1)
    } else {
        ExitCode::SUCCESS
    }
}

fn refresh(
    store: &Path,
    store_entry: &Entry,
    links: &[PathBuf],
    entries: &[Entry],
    files: &[(&str, String)],
) -> Result<ExitCode, String> {
    let stale = |entry: &Entry| matches!(entry, Entry::Copy(problems) if !problems.is_empty());
    for path in
        std::iter::once((store, store_entry)).chain(links.iter().map(|l| l.as_path()).zip(entries))
    {
        if stale(path.1) {
            writable_above(path.0)?;
        }
    }
    let word = |entry: &Entry| match entry {
        Entry::Copy(problems) if problems.is_empty() => "current",
        Entry::Copy(_) => "updated",
        Entry::Linked => "current",
        Entry::Missing => "skipped (not installed)",
        Entry::Elsewhere(_) | Entry::Foreign(_) => "skipped (not oxabl's)",
    };
    for (path, entry) in
        std::iter::once((store, store_entry)).chain(links.iter().map(|l| l.as_path()).zip(entries))
    {
        if stale(entry) {
            replace_tree(path, files)?;
        }
        println!("{}: {}", path.display(), word(entry));
    }
    Ok(ExitCode::SUCCESS)
}

fn install_all(
    force: bool,
    store: &Path,
    store_entry: &Entry,
    links: &[PathBuf],
    entries: &[Entry],
    files: &[(&str, String)],
) -> Result<ExitCode, String> {
    let all: Vec<(&Path, &Entry)> = std::iter::once((store, store_entry))
        .chain(links.iter().map(PathBuf::as_path).zip(entries))
        .collect();

    let foreign: Vec<String> = all
        .iter()
        .filter(|(_, entry)| matches!(entry, Entry::Foreign(_) | Entry::Elsewhere(_)))
        .map(|(path, entry)| {
            let what = match entry {
                Entry::Elsewhere(target) => format!("a symlink to {}", target.display()),
                Entry::Foreign(what) => what.clone(),
                _ => String::new(),
            };
            format!("{} ({what})", path.display())
        })
        .collect();
    if !foreign.is_empty() && !force {
        return Err(format!(
            "not installed by oxabl: {}; nothing was written (use --force to move it to the skill-backups folder of oxabl's data directory and install)",
            foreign.join(", ")
        ));
    }
    for (path, entry) in &all {
        let current = matches!(entry, Entry::Linked)
            || matches!(entry, Entry::Copy(problems) if problems.is_empty());
        if !current {
            writable_above(path)?;
        }
    }

    // Foreign entries leave first: a failed move writes nothing else.
    let backups = store
        .parent()
        .and_then(Path::parent)
        .ok_or("the store has no data directory")?
        .join("skill-backups");
    let labels: Vec<String> = std::iter::once("store".to_string())
        .chain(links.iter().map(|link| link_label(link)))
        .collect();
    let moved = move_away(&backups, &all, &labels)?;
    let kept = |index: usize| {
        moved[index]
            .as_ref()
            .map(|path| format!(" (the foreign entry was moved to {})", path.display()))
            .unwrap_or_default()
    };

    // The store first: a link that fell back to a copy needs it written.
    let word = match store_entry {
        Entry::Copy(problems) if problems.is_empty() => "current".to_string(),
        Entry::Copy(_) => {
            replace_tree(store, files)?;
            "updated".to_string()
        }
        Entry::Missing => {
            replace_tree(store, files)?;
            "installed".to_string()
        }
        _ => {
            replace_tree(store, files)?;
            format!("installed{}", kept(0))
        }
    };
    println!("{}: {word}", store.display());

    for (index, (link, entry)) in links.iter().zip(entries).enumerate() {
        let word = match entry {
            Entry::Linked => "current".to_string(),
            Entry::Copy(problems) if problems.is_empty() => "current (a copy)".to_string(),
            Entry::Copy(_) => {
                replace_tree(link, files)?;
                "updated (a copy)".to_string()
            }
            Entry::Missing => link_or_copy(link, store, files)?,
            Entry::Elsewhere(_) | Entry::Foreign(_) => {
                let word = link_or_copy(link, store, files)?;
                format!("{word}{}", kept(index + 1))
            }
        };
        println!("{}: {word}", link.display());
    }
    Ok(ExitCode::SUCCESS)
}
