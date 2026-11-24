//! `oxabl skill`: the agent skill that ships inside the binary.
//!
//! The skill is an Agent Skills folder (`SKILL.md` plus reference files) that
//! teaches a coding agent to use oxabl. The source lives in `crates/oxabl/skill/`
//! and says `dev` for its version; `show` and `install` write the binary's own.

use std::fs;
use std::io;
use std::path::{Path, PathBuf};
use std::process::ExitCode;
use std::sync::atomic::{AtomicU32, Ordering};

use clap::{Args, Subcommand, ValueEnum};
use serde::{Deserialize, Serialize};

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
const MANIFEST: &str = ".oxabl-skill.json";

/// What an install wrote: the version and the files oxabl owns in the target.
#[derive(Serialize, Deserialize)]
struct Manifest {
    #[serde(rename = "oxabl-version")]
    version: String,
    files: Vec<String>,
}

fn manifest_text(files: &[(&str, String)]) -> String {
    let manifest = Manifest {
        version: env!("CARGO_PKG_VERSION").to_string(),
        files: files.iter().map(|(name, _)| name.to_string()).collect(),
    };
    let mut text = serde_json::to_string_pretty(&manifest).expect("a manifest serializes");
    text.push('\n');
    text
}

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
    /// Copy the skill where coding agents look for it.
    ///
    /// Writes `.agents/skills/oxabl/` (read by Codex, pi, OpenCode, Copilot and
    /// others) and `.claude/skills/oxabl/` (Claude Code) under the base
    /// directory: the current directory, `--dir DIR`, or with `--user` the home
    /// directory. One line per target says its path and state.
    ///
    /// Each target gets a `.oxabl-skill.json` that lists the files oxabl
    /// wrote. Updating a copy removes only the files that an earlier install
    /// listed and the skill no longer has; any other file in the directory
    /// stays. A symlinked target is written through.
    ///
    /// A target whose SKILL.md oxabl did not install (no `oxabl-version`, or
    /// another `name`) is foreign: it is refused without `--force`, and nothing
    /// is written. `--force` writes the skill's files over such a directory and
    /// lists only them, so its other files stay and later updates never remove
    /// them. A target that cannot be written (a dangling symlink, a file where
    /// a directory belongs, no permission) is an error before anything is
    /// written.
    ///
    /// Exit codes: `0` on success, `1` when `--check` finds a target missing,
    /// stale or foreign, `2` on a usage or I/O error, or a refused target.
    Install(InstallArgs),
}

#[derive(Args)]
pub struct InstallArgs {
    /// Install under the home directory (`HOME`) instead of the current one
    #[arg(long, conflicts_with = "dir")]
    user: bool,

    /// Base directory to install under (default: the current directory)
    #[arg(long, value_name = "DIR")]
    dir: Option<PathBuf>,

    /// Which target to write
    #[arg(long, value_enum, default_value_t = Agent::All)]
    agent: Agent,

    /// Write nothing; exit 1 when a target is missing, stale or foreign
    #[arg(long, conflicts_with_all = ["refresh", "force"])]
    check: bool,

    /// Update only the targets that already hold an oxabl copy; create none
    #[arg(long, conflicts_with = "force")]
    refresh: bool,

    /// Write the skill over a target whose SKILL.md oxabl did not install
    #[arg(long)]
    force: bool,
}

#[derive(Clone, Copy, ValueEnum)]
enum Agent {
    /// `.agents/skills/oxabl`
    Agents,
    /// `.claude/skills/oxabl`
    Claude,
    /// Both
    All,
}

impl Agent {
    fn dirs(self) -> &'static [&'static str] {
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

/// What a target directory holds.
enum State {
    /// No SKILL.md.
    Missing,
    /// A SKILL.md that oxabl did not install.
    Foreign,
    /// An oxabl copy.
    Oxabl(Copy),
}

struct Copy {
    /// What differs from the current skill; empty when the copy is current.
    problems: Vec<String>,
    /// Files an earlier install wrote that the skill no longer has.
    remove: Vec<String>,
    /// Files in the directory that oxabl did not write.
    others: Vec<String>,
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

/// The directory that `dir` stands for, or why nothing can be written there:
/// a symlink that points nowhere, or a file where a directory belongs.
fn resolve(dir: &Path) -> Result<PathBuf, String> {
    match fs::symlink_metadata(dir) {
        Ok(meta) if meta.file_type().is_symlink() => fs::canonicalize(dir)
            .map_err(|e| {
                format!(
                    "{} is a symlink that cannot be followed: {e}",
                    dir.display()
                )
            })
            .and_then(|real| directory(dir, real)),
        Ok(_) => directory(dir, dir.to_path_buf()),
        Err(error) if matches!(error.kind(), io::ErrorKind::NotFound) => {
            for parent in dir.ancestors().skip(1) {
                match fs::symlink_metadata(parent) {
                    Ok(meta) if meta.file_type().is_symlink() => {
                        let real = fs::canonicalize(parent).map_err(|e| {
                            format!(
                                "{} is a symlink that cannot be followed: {e}",
                                parent.display()
                            )
                        })?;
                        directory(parent, real)?;
                        break;
                    }
                    Ok(_) => {
                        directory(parent, parent.to_path_buf())?;
                        break;
                    }
                    Err(e) if matches!(e.kind(), io::ErrorKind::NotFound) => {}
                    Err(e) => return Err(io_error(parent, &e)),
                }
            }
            Ok(dir.to_path_buf())
        }
        Err(error) => Err(io_error(dir, &error)),
    }
}

fn directory(shown: &Path, real: PathBuf) -> Result<PathBuf, String> {
    if real.is_dir() {
        Ok(real)
    } else {
        Err(format!("{} is not a directory", shown.display()))
    }
}

/// Fails when a file cannot be created where the install would write.
fn writable(dir: &Path) -> Result<(), String> {
    let existing = dir
        .ancestors()
        .find(|path| path.is_dir())
        .ok_or_else(|| format!("{} has no directory above it", dir.display()))?;
    let probe = existing.join(format!(".oxabl-skill-probe-{}", unique()));
    fs::File::create_new(&probe).map_err(|e| io_error(&probe, &e))?;
    fs::remove_file(&probe).map_err(|e| io_error(&probe, &e))
}

/// The files that an earlier install wrote, from the manifest. A directory
/// with a SKILL.md of oxabl's and no readable manifest is a copy from before
/// manifests: the files that bear the skill's names are oxabl's, no others.
fn owned(dir: &Path, files: &[(&str, String)]) -> Vec<String> {
    fs::read(dir.join(MANIFEST))
        .ok()
        .and_then(|bytes| serde_json::from_slice::<Manifest>(&bytes).ok())
        .map(|manifest| manifest.files)
        .unwrap_or_else(|| files.iter().map(|(name, _)| name.to_string()).collect())
}

fn inspect(dir: &Path, files: &[(&str, String)]) -> Result<State, String> {
    let entry = dir.join(ENTRY);
    let text = match fs::read(&entry) {
        Ok(bytes) => String::from_utf8_lossy(&bytes).into_owned(),
        Err(error) if matches!(error.kind(), io::ErrorKind::NotFound) => {
            return Ok(State::Missing);
        }
        Err(error) => return Err(io_error(&entry, &error)),
    };
    if !is_oxabl_copy(&text) {
        return Ok(State::Foreign);
    }
    let mut problems = Vec::new();
    for (name, expected) in files {
        match fs::read(dir.join(name)) {
            Ok(bytes) if bytes == expected.as_bytes() => {}
            Ok(_) => problems.push(format!("{name} differs")),
            Err(error) if matches!(error.kind(), io::ErrorKind::NotFound) => {
                problems.push(format!("{name} is missing"));
            }
            Err(error) => return Err(io_error(&dir.join(name), &error)),
        }
    }
    match fs::read(dir.join(MANIFEST)) {
        Ok(bytes) if bytes == manifest_text(files).as_bytes() => {}
        Ok(_) => problems.push(format!("{MANIFEST} differs")),
        Err(error) if matches!(error.kind(), io::ErrorKind::NotFound) => {
            problems.push(format!("{MANIFEST} is missing"));
        }
        Err(error) => return Err(io_error(&dir.join(MANIFEST), &error)),
    }
    let mut present = Vec::new();
    list_files(dir, Path::new(""), &mut present)?;
    let owned = owned(dir, files);
    let (mut remove, mut others) = (Vec::new(), Vec::new());
    for name in present {
        if files.iter().any(|(expected, _)| *expected == name) || name == MANIFEST {
            continue;
        }
        // Only files found in the directory are listed, so a manifest cannot
        // name a path outside it.
        if owned.contains(&name) {
            problems.push(format!("{name} is no longer part of the skill"));
            remove.push(name);
        } else {
            others.push(name);
        }
    }
    Ok(State::Oxabl(Copy {
        problems,
        remove,
        others,
    }))
}

/// Relative paths (with `/`) of the regular files under `dir`.
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

fn base(args: &InstallArgs) -> Result<PathBuf, String> {
    if args.user {
        return std::env::var_os("HOME")
            .or_else(|| std::env::var_os("USERPROFILE"))
            .filter(|home| !home.is_empty())
            .map(PathBuf::from)
            .ok_or_else(|| "--user needs the home directory, and HOME is not set".to_string());
    }
    match &args.dir {
        Some(dir) => Ok(dir.clone()),
        None => {
            std::env::current_dir().map_err(|e| format!("cannot find the current directory: {e}"))
        }
    }
}

fn install(args: &InstallArgs) -> Result<ExitCode, String> {
    let base = base(args)?;
    let files = files();
    let targets: Vec<PathBuf> = args
        .agent
        .dirs()
        .iter()
        .map(|relative| base.join(relative))
        .collect();
    let reals = targets
        .iter()
        .map(|dir| resolve(dir))
        .collect::<Result<Vec<_>, _>>()?;
    let states = reals
        .iter()
        .map(|dir| inspect(dir, &files))
        .collect::<Result<Vec<_>, _>>()?;

    if !(args.check || args.refresh || args.force) {
        let paths: Vec<_> = targets
            .iter()
            .zip(&states)
            .filter(|(_, state)| matches!(state, State::Foreign))
            .map(|(dir, _)| dir.display().to_string())
            .collect();
        if !paths.is_empty() {
            return Err(format!(
                "{} holds a SKILL.md that oxabl did not install; nothing was written (use --force to write the skill over it)",
                paths.join(", ")
            ));
        }
    }

    let writes: Vec<bool> = states
        .iter()
        .map(|state| match state {
            State::Oxabl(copy) => !args.check && !copy.problems.is_empty(),
            State::Missing | State::Foreign => !args.check && !args.refresh,
        })
        .collect();
    for (real, _) in reals.iter().zip(&writes).filter(|(_, write)| **write) {
        writable(real)?;
    }

    let mut failed = false;
    for ((dir, real), (state, write)) in targets.iter().zip(&reals).zip(states.iter().zip(&writes))
    {
        let mut word = match (state, args.check, args.refresh) {
            (State::Oxabl(copy), _, _) if copy.problems.is_empty() => "current".to_string(),
            (State::Oxabl(copy), true, _) => {
                failed = true;
                format!("stale ({})", copy.problems.join(", "))
            }
            (State::Missing, true, _) => {
                failed = true;
                "missing".to_string()
            }
            (State::Foreign, true, _) => {
                failed = true;
                "foreign (SKILL.md was not installed by oxabl)".to_string()
            }
            (State::Missing, false, true) => "skipped (not installed)".to_string(),
            (State::Foreign, false, true) => "skipped (foreign SKILL.md)".to_string(),
            (State::Oxabl(_), false, _) => "updated".to_string(),
            (State::Missing, false, false) => "installed".to_string(),
            (State::Foreign, false, false) => "replaced (foreign SKILL.md)".to_string(),
        };
        if *write {
            let remove = match state {
                State::Oxabl(copy) => copy.remove.as_slice(),
                _ => &[],
            };
            write_copy(real, &files, remove)?;
        }
        if let State::Oxabl(copy) = state
            && !copy.others.is_empty()
        {
            word.push_str(&format!(
                "; other files left alone: {}",
                copy.others.join(", ")
            ));
        }
        println!("{}: {word}", dir.display());
    }
    Ok(if failed {
        ExitCode::from(1)
    } else {
        ExitCode::SUCCESS
    })
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

fn write_file(path: &Path, text: &str) -> Result<(), String> {
    use std::io::Write;
    let parent = path.parent().ok_or("a skill file has no directory")?;
    fs::create_dir_all(parent).map_err(|e| io_error(parent, &e))?;
    let temp = parent.join(format!(".oxabl-skill-{}", unique()));
    let written = fs::File::create(&temp).and_then(|mut file| {
        file.write_all(text.as_bytes())?;
        file.sync_all()
    });
    if let Err(error) = written {
        let _ = fs::remove_file(&temp);
        return Err(io_error(&temp, &error));
    }
    fs::rename(&temp, path).map_err(|e| {
        let _ = fs::remove_file(&temp);
        io_error(path, &e)
    })
}

/// Writes the skill into `dir`, one file at a time (each through a temporary
/// file and a rename), SKILL.md after the others so an interrupted install
/// never leaves a half-written one. Then it removes the files in `remove` and
/// writes the manifest. No other file in `dir` is touched.
fn write_copy(dir: &Path, files: &[(&str, String)], remove: &[String]) -> Result<(), String> {
    let (entry, rest): (Vec<_>, Vec<_>) = files.iter().partition(|(name, _)| *name == ENTRY);
    for (name, text) in rest.into_iter().chain(entry) {
        write_file(&dir.join(name), text)?;
    }
    for name in remove {
        let path = dir.join(name);
        match fs::remove_file(&path) {
            Ok(()) => {}
            Err(error) if matches!(error.kind(), io::ErrorKind::NotFound) => {}
            Err(error) => return Err(io_error(&path, &error)),
        }
        // Drops a directory that the removal emptied; a full one stays.
        let mut parent = path.parent();
        while let Some(up) = parent.filter(|up| *up != dir) {
            if fs::remove_dir(up).is_err() {
                break;
            }
            parent = up.parent();
        }
    }
    write_file(&dir.join(MANIFEST), &manifest_text(files))
}
