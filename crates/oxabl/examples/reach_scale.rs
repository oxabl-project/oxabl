//! Time entry-point reachability on a large generated project.
//!
//! Writes a synthetic web application to a temporary directory: handlers
//! (each an entry point) that run library programs, which run more library
//! programs and call service classes; a third of the handlers check access
//! first, and some libraries keep session state. Then times the graph build
//! and a lint of every file in taint mode, with and without the entry points.
//!
//! `OXABL_REACH_HANDLERS` (default 2000) and `OXABL_REACH_LIBRARIES` (default
//! 4000) set the size.
use std::fmt::Write as _;
use std::path::{Path, PathBuf};
use std::time::Instant;

use oxabl_common::SecurityMode;
use oxabl_pipeline::{ConfigOverrides, LintPipeline, PipelineConfig};
use oxabl_workspace::{RealFileSystem, discover_path};

fn size(var: &str, default: usize) -> usize {
    std::env::var(var)
        .ok()
        .and_then(|v| v.parse().ok())
        .unwrap_or(default)
}

fn write(path: &Path, text: &str) {
    std::fs::create_dir_all(path.parent().unwrap()).unwrap();
    std::fs::write(path, text).unwrap();
}

fn generate(root: &Path, handlers: usize, libraries: usize) {
    let classes = (libraries / 40).max(1);
    let mut entries = String::from("{\"version\": 1, \"entries\": [\n");
    for h in 0..handlers {
        let mut p = String::new();
        writeln!(p, "/* GET /h{h} */").unwrap();
        writeln!(p, "USING app.*.").unwrap();
        writeln!(p, "DEFINE VARIABLE cKey AS CHARACTER NO-UNDO.").unwrap();
        writeln!(p, "DEFINE VARIABLE oSvc AS Svc{} NO-UNDO.", h % classes).unwrap();
        if h % 3 == 0 {
            writeln!(p, "RUN auth/check.p (INPUT \"h{h}\").").unwrap();
        }
        writeln!(p, "cKey = get-value(\"key\").").unwrap();
        writeln!(p, "IF get-value(\"role\") = \"admin\" THEN cKey = \"all\".").unwrap();
        for j in 0..3 {
            let l = (h * 7 + j * 13) % libraries;
            writeln!(p, "RUN lib/l{l}.p (INPUT cKey).").unwrap();
        }
        writeln!(p, "oSvc = NEW Svc{}().", h % classes).unwrap();
        writeln!(p, "oSvc:Handle(cKey).").unwrap();
        write(&root.join(format!("src/web/h{h}.p")), &p);
        let sep = if h + 1 == handlers { "" } else { "," };
        writeln!(
            entries,
            "  {{\"kind\": \"REST\", \"name\": \"GET /h{h}\", \"program\": \"web/h{h}.p\"}}{sep}"
        )
        .unwrap();
    }
    entries.push_str("]}\n");
    write(&root.join("entries.json"), &entries);
    for l in 0..libraries {
        let mut p = String::new();
        writeln!(p, "DEFINE INPUT PARAMETER pcKey AS CHARACTER NO-UNDO.").unwrap();
        writeln!(p, "DEFINE VARIABLE hQ AS HANDLE NO-UNDO.").unwrap();
        if l % 10 == 0 {
            writeln!(
                p,
                "DEFINE NEW GLOBAL SHARED VARIABLE gc{} AS CHARACTER NO-UNDO.",
                l % 50
            )
            .unwrap();
            writeln!(p, "IF gc{} = \"\" THEN gc{} = pcKey.", l % 50, l % 50).unwrap();
        }
        writeln!(p, "CREATE QUERY hQ.").unwrap();
        writeln!(
            p,
            "hQ:QUERY-PREPARE(\"FOR EACH Item WHERE Item.Code = '\" + pcKey + \"'\")."
        )
        .unwrap();
        writeln!(p, "CREATE ItemLog.").unwrap();
        writeln!(p, "ItemLog.Code = pcKey.").unwrap();
        for j in 1..3 {
            let next = (l * 3 + j * 17 + 1) % libraries;
            if next > l {
                writeln!(p, "RUN lib/l{next}.p (INPUT pcKey).").unwrap();
            }
        }
        writeln!(p, "PROCEDURE log-it PRIVATE:").unwrap();
        writeln!(p, "    DEFINE INPUT PARAMETER pc AS CHARACTER NO-UNDO.").unwrap();
        writeln!(p, "    OUTPUT TO VALUE(\"/var/log/\" + pc).").unwrap();
        writeln!(p, "    OUTPUT CLOSE.").unwrap();
        writeln!(p, "END PROCEDURE.").unwrap();
        writeln!(p, "RUN log-it (INPUT pcKey).").unwrap();
        write(&root.join(format!("src/lib/l{l}.p")), &p);
    }
    for c in 0..classes {
        let text = format!(
            "CLASS app.Svc{c}:\n    METHOD PUBLIC VOID Handle(INPUT pc AS CHARACTER):\n        \
             RUN lib/l{}.p (INPUT pc).\n        CREATE SvcLog.\n    END METHOD.\nEND CLASS.\n",
            (c * 11) % libraries
        );
        write(&root.join(format!("src/app/Svc{c}.cls")), &text);
    }
    write(
        &root.join("src/auth/check.p"),
        "DEFINE INPUT PARAMETER pc AS CHARACTER NO-UNDO.\nIF NOT CAN-DO(\"x\", pc) THEN RETURN ERROR.\n",
    );
    write(
        &root.join("oxabl.toml"),
        "[workspace]\nname = \"scale\"\n[workspace.sources]\ninclude_paths = [\"src\"]\n\
         [workspace.lint]\nenable = [\"security\"]\n[workspace.lint.security]\nmode = \"taint\"\n\
         [workspace.lint.security.reach]\nentry-points = \"entries.json\"\n\
         auth-routines = [\"auth/check.p\"]\n",
    );
}

fn lint(
    root: &Path,
    files: &[PathBuf],
    reach: bool,
    graph_rules: bool,
) -> (std::time::Duration, [usize; 4]) {
    let (mut config, warnings) = PipelineConfig::resolve(root, &ConfigOverrides::default());
    assert!(warnings.is_empty(), "{warnings:?}");
    if !graph_rules {
        config
            .lint_severities
            .base_mut()
            .set_rule("ABLSEC011", None);
        config
            .lint_severities
            .base_mut()
            .set_rule("ABLSEC012", None);
    }
    assert_eq!(config.lint_severities.security().mode, SecurityMode::Taint);
    if !reach {
        let mut options = config.lint_severities.security().clone();
        options.reach = Default::default();
        config.lint_severities.set_security(options);
    }
    let fs = RealFileSystem;
    let start = Instant::now();
    let mut pipeline = LintPipeline::new(&config, &fs);
    if reach {
        let built = pipeline.build_security_reach(None);
        eprintln!(
            "graph: {} files read in {:?}",
            built.files_read,
            start.elapsed()
        );
        pipeline = pipeline.with_security_reach(std::sync::Arc::new(built));
    }
    let mut counts = [0usize; 4];
    for path in files {
        let text = std::fs::read_to_string(path).unwrap();
        let result = pipeline.with_file(path).run(&text);
        for d in result.all() {
            match d.diagnostic.code.0 {
                "ABLSEC010" => counts[0] += 1,
                "ABLSEC011" => counts[1] += 1,
                "ABLSEC012" => counts[2] += 1,
                c if c.starts_with("ABLSEC") => counts[3] += 1,
                _ => {}
            }
        }
    }
    (start.elapsed(), counts)
}

fn main() {
    let handlers = size("OXABL_REACH_HANDLERS", 2000);
    let libraries = size("OXABL_REACH_LIBRARIES", 4000);
    let dir = tempfile::TempDir::new().unwrap();
    let root = dir.path();
    generate(root, handlers, libraries);
    let mut files = discover_path(root).expect("discover");
    files.sort();
    println!(
        "{} files ({handlers} handlers, {libraries} libraries)",
        files.len()
    );
    let (without, c0) = lint(root, &files, false, true);
    println!(
        "taint, no entry points: {without:?}; ABLSEC010 {} ABLSEC011 {} ABLSEC012 {} other {}",
        c0[0], c0[1], c0[2], c0[3]
    );
    let (labels, c2) = lint(root, &files, true, false);
    println!(
        "taint, labels only:     {labels:?}; ABLSEC010 {} ABLSEC011 {} ABLSEC012 {} other {}",
        c2[0], c2[1], c2[2], c2[3]
    );
    let (with, c1) = lint(root, &files, true, true);
    println!(
        "taint, entry points:    {with:?}; ABLSEC010 {} ABLSEC011 {} ABLSEC012 {} other {}",
        c1[0], c1[1], c1[2], c1[3]
    );
}
