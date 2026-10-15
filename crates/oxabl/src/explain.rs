use std::path::{Path, PathBuf};
use std::process::ExitCode;

use oxabl_pipeline::{ConfigOverrides, LintPipeline, PipelineConfig};
use oxabl_workspace::RealFileSystem;

pub fn run(file: &str, json: bool, include_paths: &[PathBuf], schema: Option<&Path>) -> ExitCode {
    let (file, requested_line) = match file.rsplit_once(':') {
        Some((path, suffix)) if suffix.bytes().all(|b| b.is_ascii_digit()) => {
            match suffix.parse::<usize>() {
                Ok(line) if line > 0 => (path, Some(line)),
                _ => {
                    eprintln!("error: line must be a positive integer");
                    return ExitCode::from(2);
                }
            }
        }
        _ => (file, None),
    };
    let path = Path::new(file);
    let source = match std::fs::read_to_string(path) {
        Ok(s) => s,
        Err(e) => {
            eprintln!("error: cannot read {file}: {e}");
            return ExitCode::from(2);
        }
    };
    let root_offset = if let Some(line) = requested_line {
        let offset = std::iter::once(0)
            .chain(source.match_indices('\n').map(|(i, _)| i + 1))
            .nth(line - 1);
        let Some(offset) = offset else {
            eprintln!("error: line is outside the file");
            return ExitCode::from(2);
        };
        let indent = source.as_bytes()[offset..]
            .iter()
            .take_while(|b| matches!(b, b' ' | b'\t' | b'\r'))
            .count();
        Some((offset + indent) as u32)
    } else {
        None
    };
    let (config, warnings) = PipelineConfig::resolve(
        path,
        &ConfigOverrides {
            include_paths: include_paths.to_vec(),
            schema_path: schema.map(Path::to_path_buf),
            style: None,
        },
    );
    for warning in warnings {
        eprintln!("warning: {warning}");
    }
    let result = oxabl_common::catch_panic(|| {
        let fs = RealFileSystem;
        let run = LintPipeline::new(&config, &fs);
        let handle = run.with_file(path);
        let expansion = handle.expand(&source);
        if expansion.expanded().is_none() {
            return Err((3, "preprocessing failed".to_string()));
        }
        let summaries = handle.build_effect_summaries(&expansion, &[]);
        let Some(effects) = summaries.file(path) else {
            return Err((3, "no effects were produced".into()));
        };
        effects
            .explain(file, root_offset, json)
            .map_err(|e| (6, e.to_string()))
    });
    match result {
        Ok(Ok(output)) => {
            print!("{output}");
            ExitCode::SUCCESS
        }
        Ok(Err((code, message))) => {
            eprintln!("error: {message}");
            ExitCode::from(code)
        }
        Err(panic) => {
            eprintln!("error: analysis panicked: {panic}");
            ExitCode::from(4)
        }
    }
}
