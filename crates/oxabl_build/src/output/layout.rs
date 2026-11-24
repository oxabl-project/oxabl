//! Where the outputs of a unit go.

use std::path::{Component, Path};

use oxabl_workspace::{BuildConfig, RcodePlace, WorkspaceConfig, XrefFormat};

use crate::error::BuildError;

/// One output file of a unit: where the worker writes it in its stage, its
/// path in a cache entry, and where it belongs in the workspace.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Expected {
    /// Relative to the worker's stage directory, by compile name.
    pub staged: String,
    /// Relative to a cache entry and to the stage of a fetch, by output name,
    /// so it is unique in a build.
    pub artifact: String,
    /// Relative to the workspace root.
    pub dest: String,
    /// `dest` is beside the source: its directory is the unit's own, which
    /// oxabl never creates.
    pub in_place: bool,
}

/// Where the r-code of a unit goes.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Rcode {
    /// Below this directory, relative to the workspace root: `$OUT`.
    Directory(String),
    /// Beside the source of the unit.
    BesideSource,
}

/// The output directories of `[build]` and `[build.outputs]`, relative to the
/// workspace root.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct OutputLayout {
    pub rcode: Rcode,
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

/// `name` without its extension.
fn stem(name: &str) -> &str {
    match name.rsplit_once('.') {
        Some((stem, extension)) if !extension.contains('/') => stem,
        _ => name,
    }
}

fn outside(key: &str, directory: &Path) -> BuildError {
    BuildError::Usage(format!(
        "{key} = \"{}\" must be a directory under the workspace root",
        directory.display()
    ))
}

impl OutputLayout {
    /// The layout `config` asks for. `oxabl build` needs `[build].output`,
    /// unless `[build].rcode` is `beside-source`.
    pub fn lower(config: &WorkspaceConfig, root: &Path) -> Result<OutputLayout, BuildError> {
        let build = &config.build;
        let rcode = match build.rcode {
            RcodePlace::Directory => {
                let directory = BuildConfig::directory(&build.output).ok_or_else(|| {
                    BuildError::Usage(
                        "oxabl build needs [build].output, the directory for the r-code, or [build].rcode = \"beside-source\"".to_string(),
                    )
                })?;
                Rcode::Directory(relative(root, "[build].output", directory)?)
            }
            RcodePlace::BesideSource => Rcode::BesideSource,
        };
        let optional = |key: &str, directory: &Path| {
            BuildConfig::directory(directory)
                .map(|directory| relative(root, key, directory))
                .transpose()
        };
        let layout = OutputLayout {
            rcode,
            xref: optional("[build.outputs].xref", &build.outputs.xref)?
                .map(|directory| (directory, build.outputs.xref_format)),
            debug_list: optional("[build.outputs].debug_list", &build.outputs.debug_list)?,
            listing: optional("[build.outputs].listing", &build.outputs.listing)?,
        };
        layout.check_apart()?;
        Ok(layout)
    }

    /// The r-code directory, `$OUT`; `None` when the r-code goes beside the
    /// source.
    pub fn rcode_directory(&self) -> Option<&str> {
        match &self.rcode {
            Rcode::Directory(directory) => Some(directory),
            Rcode::BesideSource => None,
        }
    }

    /// Whether the r-code goes beside the source.
    pub fn beside_source(&self) -> bool {
        self.rcode == Rcode::BesideSource
    }

    /// Every output directory that oxabl owns, the r-code directory first.
    pub fn directories(&self) -> Vec<&str> {
        let mut directories: Vec<&str> = self.rcode_directory().into_iter().collect();
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

    /// The files the compile of `name` must produce. The worker writes them
    /// under `name`; they belong under `output`, the unit's output name,
    /// which is the unit path when the r-code goes beside the source.
    pub fn expected(&self, name: &str, output: &str) -> Vec<Expected> {
        let mut files = vec![Expected {
            staged: format!("r/{}.r", stem(name)),
            artifact: format!("r/{}.r", stem(output)),
            dest: match &self.rcode {
                Rcode::Directory(directory) => format!("{directory}/{}.r", stem(output)),
                Rcode::BesideSource => format!("{}.r", stem(output)),
            },
            in_place: self.beside_source(),
        }];
        if let Some((directory, format)) = &self.xref {
            let extension = match format {
                XrefFormat::Xml => "xref.xml",
                XrefFormat::Text => "xref",
            };
            files.push(Expected {
                staged: format!("xref/{name}.{extension}"),
                artifact: format!("xref/{output}.{extension}"),
                dest: format!("{directory}/{output}.{extension}"),
                in_place: false,
            });
        }
        if let Some(directory) = &self.debug_list {
            files.push(Expected {
                staged: format!("dbg/{name}.dbg"),
                artifact: format!("dbg/{output}.dbg"),
                dest: format!("{directory}/{output}.dbg"),
                in_place: false,
            });
        }
        if let Some(directory) = &self.listing {
            files.push(Expected {
                staged: format!("lst/{name}.lst"),
                artifact: format!("lst/{output}.lst"),
                dest: format!("{directory}/{output}.lst"),
                in_place: false,
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
