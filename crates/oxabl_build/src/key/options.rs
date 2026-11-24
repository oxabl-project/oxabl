//! The compile options and the requested outputs, as the key holds them.

use oxabl_workspace::{BuildConfig, XrefFormat};

/// `[build.compile]` and `[build.outputs]`, lowered to the pairs and kinds the
/// key encodes.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CompileSettings {
    pairs: Vec<(&'static str, String)>,
    outputs: Vec<&'static str>,
}

fn flag(value: bool) -> String {
    value.to_string()
}

impl CompileSettings {
    pub fn lower(build: &BuildConfig) -> Self {
        let compile = &build.compile;
        let mut options: Vec<String> = compile
            .options
            .iter()
            .map(|option| option.to_ascii_lowercase())
            .collect();
        options.sort();
        options.dedup();
        let pairs = vec![
            ("attr-space", flag(compile.attr_space)),
            ("generate-md5", flag(compile.generate_md5)),
            ("languages", compile.languages.join(":")),
            ("min-size", flag(compile.min_size)),
            ("options", options.join(",")),
            ("stream-io", flag(compile.stream_io)),
            ("v6frame", flag(compile.v6frame)),
        ];
        let outputs = &build.outputs;
        let mut kinds = vec!["rcode"];
        if BuildConfig::directory(&outputs.debug_list).is_some() {
            kinds.push("debug-list");
        }
        if BuildConfig::directory(&outputs.listing).is_some() {
            kinds.push("listing");
        }
        if BuildConfig::directory(&outputs.xref).is_some() {
            kinds.push(match outputs.xref_format {
                XrefFormat::Xml => "xref-xml",
                XrefFormat::Text => "xref",
            });
        }
        kinds.sort_unstable();
        CompileSettings {
            pairs,
            outputs: kinds,
        }
    }

    /// The option pairs, sorted by name.
    pub fn pairs(&self) -> &[(&'static str, String)] {
        &self.pairs
    }

    /// The requested output kinds, sorted by bytes.
    pub fn outputs(&self) -> &[&'static str] {
        &self.outputs
    }
}
