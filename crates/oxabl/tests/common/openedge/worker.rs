//! Running ABL programs through the worker command of a real AVM.

use std::io::{BufRead, BufReader, Write};
use std::path::Path;
use std::process::{Child, ChildStdin, Command, Stdio};
use std::sync::mpsc::{Receiver, channel};
use std::time::Duration;

use super::Target;

/// The command line of `target` with `{program}` and `{root}` filled in.
pub fn expand(target: &Target, program: &str, root: &str) -> Vec<String> {
    target
        .command
        .iter()
        .map(|arg| arg.replace("{program}", program).replace("{root}", root))
        .collect()
}

/// Writes `source` as `check/check.p` below `host`, runs it through the
/// worker command in place of the worker program and returns what it printed
/// on stdout, then on stderr.
pub fn run_program(target: &Target, host: &Path, worker: &str, source: &str) -> String {
    let program = write_program(host, worker, source);
    let argv = expand(target, &program, worker);
    let output = Command::new(&argv[0])
        .args(&argv[1..])
        .current_dir(host)
        .stdin(Stdio::null())
        .output()
        .expect("the worker command runs");
    format!(
        "{}{}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr)
    )
}

fn write_program(host: &Path, worker: &str, source: &str) -> String {
    std::fs::create_dir_all(host.join("check")).unwrap();
    std::fs::write(host.join("check/check.p"), source).unwrap();
    format!("{worker}/check/check.p")
}

/// A program that runs through the worker command with its stdin and stdout
/// open: lines go in, lines come out.
pub struct Session {
    child: Child,
    stdin: Option<ChildStdin>,
    lines: Receiver<String>,
}

impl Session {
    pub fn start(target: &Target, host: &Path, worker: &str, source: &str) -> Session {
        let program = write_program(host, worker, source);
        let argv = expand(target, &program, worker);
        let mut child = Command::new(&argv[0])
            .args(&argv[1..])
            .current_dir(host)
            .stdin(Stdio::piped())
            .stdout(Stdio::piped())
            .stderr(Stdio::null())
            .spawn()
            .expect("the worker command starts");
        let stdout = child.stdout.take().unwrap();
        let (sender, lines) = channel();
        std::thread::spawn(move || {
            for line in BufReader::new(stdout).split(b'\n') {
                let Ok(line) = line else { break };
                if sender
                    .send(String::from_utf8_lossy(&line).into_owned())
                    .is_err()
                {
                    break;
                }
            }
        });
        let stdin = child.stdin.take();
        Session {
            child,
            stdin,
            lines,
        }
    }

    /// The next line of stdout, or `None` after `wait` with no line.
    pub fn line(&self, wait: Duration) -> Option<String> {
        self.lines.recv_timeout(wait).ok()
    }

    pub fn send(&mut self, line: &str) {
        let stdin = self.stdin.as_mut().expect("stdin is open");
        stdin.write_all(line.as_bytes()).unwrap();
        stdin.write_all(b"\n").unwrap();
        stdin.flush().unwrap();
    }

    /// Closes stdin and waits for the process; returns its exit code.
    pub fn finish(mut self) -> Option<i32> {
        self.stdin = None;
        self.child.wait().ok().and_then(|status| status.code())
    }
}

impl Drop for Session {
    fn drop(&mut self) {
        let _ = self.child.kill();
        let _ = self.child.wait();
    }
}

/// An ABL program that prints one line `WORKER <pid-file>` for each process
/// whose command line holds `<root>/.oxabl/`: the workers of a build that
/// runs in `root`. The pattern does not match its own command line.
pub fn list_workers_program(worker: &str) -> String {
    format!(
        "DEFINE VARIABLE line AS CHARACTER NO-UNDO.\n\
         OS-COMMAND SILENT VALUE(\"grep -la -e '{worker}/[.]oxabl/' /proc/[0-9]*/cmdline > {worker}/check/workers.txt 2>/dev/null\").\n\
         INPUT FROM VALUE(\"{worker}/check/workers.txt\").\n\
         REPEAT:\n\
             IMPORT UNFORMATTED line.\n\
             PUT UNFORMATTED \"WORKER \" line SKIP.\n\
         END.\n\
         PUT UNFORMATTED \"LISTED\" SKIP.\n\
         QUIT.\n"
    )
}

/// The `/proc/<pid>/cmdline` files of the workers that run in `worker`.
pub fn live_workers(target: &Target, host: &Path, worker: &str) -> Vec<String> {
    let printed = run_program(target, host, worker, &list_workers_program(worker));
    assert!(
        printed.contains("LISTED"),
        "the process list did not run:\n{printed}"
    );
    printed
        .lines()
        .filter_map(|line| line.strip_prefix("WORKER "))
        .filter(|name| name.starts_with("/proc/"))
        .map(String::from)
        .collect()
}

/// An ABL program that compiles each `(source, r-code)` member below `libsrc/`
/// and adds its r-code to `libs/acme.pl`, which it creates first when `create`.
/// It prints `LIBRARY DONE`.
pub fn library_program(root: &str, members: &[(&str, &str)], create: bool) -> String {
    let library = format!("{root}/libs/acme.pl");
    let mut program = format!(
        "OS-CREATE-DIR VALUE(\"{root}/libsrc/r\").\nOS-CREATE-DIR VALUE(\"{root}/libs\").\nPROPATH = \"{root}/libsrc,\" + PROPATH.\n"
    );
    for (source, _) in members {
        program.push_str(&format!(
            "COMPILE VALUE(\"{root}/libsrc/{source}\") SAVE INTO VALUE(\"{root}/libsrc/r\").\n"
        ));
    }
    if create {
        program.push_str(&format!(
            "OS-COMMAND SILENT VALUE(OS-GETENV(\"DLC\") + \"/bin/prolib {library} -create\").\n"
        ));
    }
    for (_, rcode) in members {
        program.push_str(&format!(
            "OS-COMMAND SILENT VALUE(\"cd {root}/libsrc/r && \" + OS-GETENV(\"DLC\") + \"/bin/prolib {library} -add {rcode}\").\n"
        ));
    }
    program.push_str("PUT UNFORMATTED \"LIBRARY DONE\" SKIP.\nQUIT.\n");
    program
}
