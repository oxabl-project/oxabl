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
    /// Copy the skill where coding agents look for it.
    ///
    /// Writes `.agents/skills/oxabl/` (read by Codex, pi, OpenCode, Copilot and
    /// others) and `.claude/skills/oxabl/` (Claude Code) under the base
    /// directory: the current directory, `--dir DIR`, or with `--user` the home
    /// directory. One line per target says its path and state.
    ///
    /// A target whose SKILL.md oxabl did not install (no `oxabl-version`, or
    /// another `name`) is foreign: it is refused without `--force`, and nothing
    /// is written. Replacing an oxabl copy also removes the files in that
    /// directory that the skill does not have.
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

    /// Replace a target whose SKILL.md oxabl did not install
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
    /// An oxabl copy; the problems are empty when it is current.
    Oxabl(Vec<String>),
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
    let mut present = Vec::new();
    list_files(dir, Path::new(""), &mut present)?;
    for name in present {
        if !files.iter().any(|(expected, _)| *expected == name) {
            problems.push(format!("{name} is not part of the skill"));
        }
    }
    Ok(State::Oxabl(problems))
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
    let states = targets
        .iter()
        .map(|dir| inspect(dir, &files))
        .collect::<Result<Vec<_>, _>>()?;

    if !(args.check || args.refresh || args.force) {
        let foreign: Vec<_> = targets
            .iter()
            .zip(&states)
            .filter(|(_, state)| matches!(state, State::Foreign))
            .collect();
        if !foreign.is_empty() {
            let paths: Vec<_> = foreign
                .iter()
                .map(|(dir, _)| dir.display().to_string())
                .collect();
            return Err(format!(
                "{} holds a SKILL.md that oxabl did not install; nothing was written (use --force to replace it)",
                paths.join(", ")
            ));
        }
    }

    let mut failed = false;
    for (dir, state) in targets.iter().zip(&states) {
        let label = dir.display();
        let word = match (state, args.check, args.refresh) {
            (State::Oxabl(problems), _, _) if problems.is_empty() => "current".to_string(),
            (State::Oxabl(problems), true, _) => {
                failed = true;
                format!("stale ({})", problems.join(", "))
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
            (State::Oxabl(_), false, _) => {
                write_copy(dir, &files, true)?;
                "updated".to_string()
            }
            (State::Missing, false, false) => {
                write_copy(dir, &files, false)?;
                "installed".to_string()
            }
            (State::Foreign, false, false) => {
                write_copy(dir, &files, false)?;
                "replaced (foreign SKILL.md)".to_string()
            }
        };
        println!("{label}: {word}");
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

/// A directory removed on drop unless it was renamed away.
struct Staged(PathBuf);

impl Drop for Staged {
    fn drop(&mut self) {
        let _ = fs::remove_dir_all(&self.0);
    }
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

/// Writes the skill into `dir`. A directory that does not exist, or that holds
/// an oxabl copy, is built beside it and renamed into place, so a failure
/// leaves the old copy or nothing and a replaced copy keeps no stale file. Any
/// other directory (a foreign skill, a symlink) gets the files one by one,
/// SKILL.md last, and nothing else in it is touched.
fn write_copy(dir: &Path, files: &[(&str, String)], oxabl_copy: bool) -> Result<(), String> {
    let existing = fs::symlink_metadata(dir);
    let in_place = match &existing {
        Ok(meta) => meta.file_type().is_symlink() || !oxabl_copy,
        Err(error) if matches!(error.kind(), io::ErrorKind::NotFound) => false,
        Err(error) => return Err(io_error(dir, error)),
    };
    if in_place {
        let (entry, rest): (Vec<_>, Vec<_>) = files.iter().partition(|(name, _)| *name == ENTRY);
        for (name, text) in rest.into_iter().chain(entry) {
            write_file(&dir.join(name), text)?;
        }
        return Ok(());
    }
    let parent = dir.parent().ok_or("a skill directory has no parent")?;
    fs::create_dir_all(parent).map_err(|e| io_error(parent, &e))?;
    let staged = Staged(parent.join(format!(".oxabl-skill-{}", unique())));
    for (name, text) in files {
        write_file(&staged.0.join(name), text)?;
    }
    if existing.is_err() {
        return fs::rename(&staged.0, dir).map_err(|e| io_error(dir, &e));
    }
    let backup = Staged(parent.join(format!(".oxabl-skill-old-{}", unique())));
    fs::rename(dir, &backup.0).map_err(|e| io_error(dir, &e))?;
    if let Err(error) = fs::rename(&staged.0, dir) {
        let _ = fs::rename(&backup.0, dir);
        return Err(io_error(dir, &error));
    }
    Ok(())
}
