//! Where the outputs of a unit go.

use std::path::{Component, Path};

use oxabl_workspace::{BuildConfig, WorkspaceConfig, XrefFormat};

use crate::error::BuildError;

/// One output file of a unit: where the worker writes it in its stage, and
/// where it belongs in the workspace.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Expected {
    /// Relative to the stage directory.
    pub staged: String,
    /// Relative to the workspace root.
    pub dest: String,
}

/// The output directories of `[build]` and `[build.outputs]`, relative to the
/// workspace root.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct OutputLayout {
    pub rcode: String,
    pub xref: Option<(String, XrefFormat)>,
    pub debug_list: Option<String>,
    pub listing: Option<String>,
}

/// `directory` as a normalised root-relative path with `/` separators.
fn relative(root: &Path, key: &str, directory: &Path) -> Result<String, BuildError> {
    let below = if directory.is_absolute() {
        directory
            .strip_prefix(root)
            .map_err(|_| outside(key, directory))?
    } else {
        directory
    };
    let mut parts = Vec::new();
    for component in below.components() {
        match component {
            Component::Normal(part) => parts.push(part.to_string_lossy().into_owned()),
            Component::CurDir => {}
            _ => return Err(outside(key, directory)),
        }
    }
    if parts.is_empty() {
        return Err(BuildError::Usage(format!(
            "{key} must be a directory below the workspace root, not the root itself"
        )));
    }
    Ok(parts.join("/"))
}

fn outside(key: &str, directory: &Path) -> BuildError {
    BuildError::Usage(format!(
        "{key} = \"{}\" must be a directory under the workspace root",
        directory.display()
    ))
}

impl OutputLayout {
    /// The layout `config` asks for. `oxabl build` needs `[build].output`.
    pub fn lower(config: &WorkspaceConfig, root: &Path) -> Result<OutputLayout, BuildError> {
        let build = &config.build;
        let rcode = BuildConfig::directory(&build.output).ok_or_else(|| {
            BuildError::Usage(
                "oxabl build needs [build].output, the directory for the r-code".to_string(),
            )
        })?;
        let optional = |key: &str, directory: &Path| {
            BuildConfig::directory(directory)
                .map(|directory| relative(root, key, directory))
                .transpose()
        };
        let layout = OutputLayout {
            rcode: relative(root, "[build].output", rcode)?,
            xref: optional("[build.outputs].xref", &build.outputs.xref)?
                .map(|directory| (directory, build.outputs.xref_format)),
            debug_list: optional("[build.outputs].debug_list", &build.outputs.debug_list)?,
            listing: optional("[build.outputs].listing", &build.outputs.listing)?,
        };
        layout.check_apart()?;
        Ok(layout)
    }

    /// Every output directory, the r-code directory first.
    pub fn directories(&self) -> Vec<&str> {
        let mut directories = vec![self.rcode.as_str()];
        directories.extend(self.xref.as_ref().map(|(directory, _)| directory.as_str()));
        directories.extend(self.debug_list.as_deref());
        directories.extend(self.listing.as_deref());
        directories
    }

    fn check_apart(&self) -> Result<(), BuildError> {
        let directories = self.directories();
        for (at, a) in directories.iter().enumerate() {
            for b in &directories[at + 1..] {
                let inside = |outer: &str, inner: &str| {
                    inner == outer
                        || inner
                            .strip_prefix(outer)
                            .is_some_and(|r| r.starts_with('/'))
                };
                if inside(a, b) || inside(b, a) {
                    return Err(BuildError::Usage(format!(
                        "the output directories `{a}` and `{b}` overlap; each must be separate"
                    )));
                }
            }
        }
        Ok(())
    }

    /// The files the compile of `name` must produce.
    pub fn expected(&self, name: &str) -> Vec<Expected> {
        let stem = match name.rsplit_once('.') {
            Some((stem, extension)) if !extension.contains('/') => stem,
            _ => name,
        };
        let mut files = vec![Expected {
            staged: format!("r/{stem}.r"),
            dest: format!("{}/{stem}.r", self.rcode),
        }];
        if let Some((directory, format)) = &self.xref {
            let extension = match format {
                XrefFormat::Xml => "xref.xml",
                XrefFormat::Text => "xref",
            };
            files.push(Expected {
                staged: format!("xref/{name}.{extension}"),
                dest: format!("{directory}/{name}.{extension}"),
            });
        }
        if let Some(directory) = &self.debug_list {
            files.push(Expected {
                staged: format!("dbg/{name}.dbg"),
                dest: format!("{directory}/{name}.dbg"),
            });
        }
        if let Some(directory) = &self.listing {
            files.push(Expected {
                staged: format!("lst/{name}.lst"),
                dest: format!("{directory}/{name}.lst"),
            });
        }
        files
    }

    /// The `xref` field of a `COMPILE` request.
    pub fn xref_mode(&self) -> &'static str {
        match &self.xref {
            None => "none",
            Some((_, XrefFormat::Xml)) => "xref-xml",
            Some((_, XrefFormat::Text)) => "xref",
        }
    }
}
