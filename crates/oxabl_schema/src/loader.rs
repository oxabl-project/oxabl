//! Multi-file schema loader with merge and conflict diagnostics.

use std::path::{Path, PathBuf};

use oxabl_ast::Span;
use oxabl_common::{Diagnostic, FileId, FileSpan};
use oxabl_workspace::{FileSystem, RealFileSystem};

use crate::diagnostics::{SCHEMA0001, SCHEMA0010, SCHEMA0011, SCHEMA0012, SCHEMA0030, SCHEMA0031};
use crate::parser::{PendingField, PendingIndex, parse_df};
use crate::schema::{Field, Schema, SchemaType};

/// Hard upper bound on table count before `SCHEMA0031` fires. Guards against
/// crafted `.df` files growing the global `OxablAtom` pool unboundedly.
pub const LOAD_TABLE_CAP: usize = 100_000;
/// Hard upper bound on fields per table before `SCHEMA0031` fires.
pub const LOAD_FIELD_CAP: usize = 10_000;

/// Merges one or more `.df` files into a single `Schema`.
///
/// Last-write-wins for duplicate `ADD TABLE` records (with a `SCHEMA0010`
/// warning); duplicate fields within a table warn with `SCHEMA0011`; a
/// field re-declared with an incompatible `SchemaType` across files emits
/// `SCHEMA0012` (error) and poisons the field to `SchemaType::Error`.
pub struct SchemaLoader;

impl SchemaLoader {
    /// Load every `.df` file in `paths`, merging into a single `Schema`.
    ///
    /// `workspace_root` is the directory that schema paths must not escape;
    /// use `None` when the caller is not enforcing containment (CLI
    /// `--schema <path>`). When `Some`, any path in `paths` that resolves
    /// outside the root is rejected with `SCHEMA0030`.
    pub fn load_files(paths: &[PathBuf], fs: &dyn FileSystem) -> (Schema, Vec<Diagnostic>) {
        Self::load_files_with_root(paths, fs, None)
    }

    pub fn load_files_with_root(
        paths: &[PathBuf],
        fs: &dyn FileSystem,
        workspace_root: Option<&Path>,
    ) -> (Schema, Vec<Diagnostic>) {
        let mut schema = Schema::empty();
        let mut diagnostics = Vec::new();

        for (idx, path) in paths.iter().enumerate() {
            let file_id = FileId::new(idx as u32 + 1);

            if let Some(root) = workspace_root
                && !path_within(root, path)
            {
                diagnostics.push(Diagnostic::error(
                    SCHEMA0030,
                    format!(
                        "schema path `{}` escapes workspace root `{}`",
                        path.display(),
                        root.display()
                    ),
                    FileSpan {
                        file: file_id,
                        span: Span { start: 0, end: 0 },
                    },
                ));
                continue;
            }

            let source = match fs.read(path) {
                Ok(s) => s,
                Err(err) => {
                    diagnostics.push(Diagnostic::error(
                        SCHEMA0001,
                        format!("cannot read schema file `{}`: {err}", path.display()),
                        FileSpan {
                            file: file_id,
                            span: Span { start: 0, end: 0 },
                        },
                    ));
                    continue;
                }
            };

            let mut outcome = parse_df(&source, file_id);
            diagnostics.append(&mut outcome.diagnostics);

            for table in outcome.tables {
                if schema.len() >= LOAD_TABLE_CAP {
                    diagnostics.push(Diagnostic::error(
                        SCHEMA0031,
                        format!(
                            "schema table cap exceeded (> {LOAD_TABLE_CAP} tables); refusing to load further tables"
                        ),
                        table.source,
                    ));
                    break;
                }
                merge_table(&mut schema, table, &mut diagnostics);
            }

            for pending in outcome.fields {
                merge_field(&mut schema, pending, &mut diagnostics);
            }

            for pending in outcome.indexes {
                merge_index(&mut schema, pending, &mut diagnostics);
            }
        }

        if !schema.is_empty() {
            schema.bump_revision();
        }

        (schema, diagnostics)
    }
}

fn merge_table(
    schema: &mut Schema,
    table: crate::schema::Table,
    diagnostics: &mut Vec<Diagnostic>,
) {
    if let Some(existing_id) = schema.table_id(&table.name) {
        let previous = schema
            .get_by_id(existing_id)
            .map(|t| t.source)
            .unwrap_or(table.source);
        diagnostics.push(
            Diagnostic::warning(
                SCHEMA0010,
                format!(
                    "duplicate table `{}`; last definition wins",
                    table.display_name
                ),
                table.source,
            )
            .with_label(previous, "previously defined here".to_string()),
        );
        schema.replace_table(existing_id, table);
    } else {
        schema.insert_table(table);
    }
}

fn merge_field(schema: &mut Schema, pending: PendingField, diagnostics: &mut Vec<Diagnostic>) {
    let PendingField {
        table,
        table_display,
        field,
    } = pending;
    let Some(table_id) = schema.table_id(&table) else {
        diagnostics.push(Diagnostic::error(
            SCHEMA0001,
            format!(
                "field `{}` declared `OF \"{}\"` but no such table has been added",
                field.display_name, table_display
            ),
            field.source,
        ));
        return;
    };

    let t = schema.table_mut(table_id);
    if t.fields.len() >= LOAD_FIELD_CAP {
        diagnostics.push(Diagnostic::error(
            SCHEMA0031,
            format!(
                "field cap exceeded on table `{}` (> {LOAD_FIELD_CAP} fields)",
                t.display_name
            ),
            field.source,
        ));
        return;
    }

    if let Some(existing_idx) = t.fields.iter().position(|f| f.name == field.name) {
        let existing = &t.fields[existing_idx];
        if !types_compatible(&existing.data_type, &field.data_type) {
            let previous = existing.source;
            diagnostics.push(
                Diagnostic::error(
                    SCHEMA0012,
                    format!(
                        "field `{}` of table `{}` redeclared with incompatible type",
                        field.display_name, t.display_name
                    ),
                    field.source,
                )
                .with_label(previous, "previously declared here".to_string()),
            );
            let poisoned = Field {
                data_type: SchemaType::Error,
                ..field
            };
            t.fields[existing_idx] = poisoned;
        } else {
            let previous = existing.source;
            diagnostics.push(
                Diagnostic::warning(
                    SCHEMA0011,
                    format!(
                        "duplicate field `{}` in table `{}`; last definition wins",
                        field.display_name, t.display_name
                    ),
                    field.source,
                )
                .with_label(previous, "previously declared here".to_string()),
            );
            t.fields[existing_idx] = field;
        }
    } else {
        t.fields.push(field);
    }
}

fn merge_index(schema: &mut Schema, pending: PendingIndex, diagnostics: &mut Vec<Diagnostic>) {
    let PendingIndex {
        table,
        table_display,
        index,
    } = pending;
    let Some(table_id) = schema.table_id(&table) else {
        diagnostics.push(Diagnostic::error(
            SCHEMA0001,
            format!(
                "index `{}` declared `ON \"{}\"` but no such table has been added",
                index.display_name, table_display
            ),
            index.source,
        ));
        return;
    };
    let t = schema.table_mut(table_id);
    if let Some(existing_idx) = t.indexes.iter().position(|i| i.name == index.name) {
        let previous = t.indexes[existing_idx].source;
        diagnostics.push(
            Diagnostic::warning(
                SCHEMA0011,
                format!(
                    "duplicate index `{}` on table `{}`; last definition wins",
                    index.display_name, t.display_name
                ),
                index.source,
            )
            .with_label(previous, "previously declared here".to_string()),
        );
        t.indexes[existing_idx] = index;
    } else {
        t.indexes.push(index);
    }
}

/// Two schema types are compatible if they are equal, or either side is
/// `Unknown` (round-tripped from an unrecognised spelling) — we do not
/// poison a field just because a newer `.df` uses a type name we don't yet
/// classify.
fn types_compatible(a: &SchemaType, b: &SchemaType) -> bool {
    if a == b {
        return true;
    }
    matches!(
        (a, b),
        (SchemaType::Unknown(_), _) | (_, SchemaType::Unknown(_))
    )
}

fn path_within(root: &Path, candidate: &Path) -> bool {
    // Reject absolute paths and `..` traversals relative to the root.
    if candidate.is_absolute() {
        // Allow absolute paths that are rooted under the workspace.
        return candidate.starts_with(root);
    }
    let normalized = root.join(candidate);
    // Walk components; any `..` that pops above root rejects.
    let mut depth: i32 = 0;
    for c in candidate.components() {
        use std::path::Component;
        match c {
            Component::Normal(_) | Component::CurDir => {}
            Component::ParentDir => depth -= 1,
            Component::RootDir | Component::Prefix(_) => return false,
        }
        if depth < 0 {
            return false;
        }
    }
    normalized.starts_with(root) || depth >= 0
}

/// The `.df` files directly inside `dir` (non-recursive), in sorted path order so
/// any merge/conflict diagnostic downstream is deterministic.
///
/// Returns the read error rather than an empty list, because "this directory
/// holds no `.df`" and "this directory could not be read" are different facts a
/// caller may need to report differently: the pipeline resolver turns each into
/// its own configuration warning, where a single empty `Vec` would have made both
/// silent (A2).
///
/// The extension match is case-insensitive, matching the rest of the loader.
pub fn df_files_in_dir(dir: impl AsRef<Path>) -> std::io::Result<Vec<PathBuf>> {
    let mut paths: Vec<PathBuf> = std::fs::read_dir(dir.as_ref())?
        .filter_map(|e| e.ok())
        .map(|e| e.path())
        .filter(|p| {
            p.extension()
                .is_some_and(|ext| ext.eq_ignore_ascii_case("df"))
        })
        .collect();
    paths.sort();
    Ok(paths)
}

impl Schema {
    /// Load every `.df` file directly inside `dir` (non-recursive) into a single
    /// merged schema, using the real filesystem. Files are loaded in sorted path
    /// order so merge/conflict diagnostics are deterministic. A directory with
    /// no `.df` files — or one that cannot be read — yields an empty schema and
    /// no diagnostics.
    ///
    /// A caller that needs to *distinguish* those two silent cases should use
    /// [`df_files_in_dir`] and load the result itself.
    ///
    /// This is the explicit-path convenience; workspace-config auto-discovery of
    /// the schema directory is intentionally deferred.
    pub fn from_df_dir(dir: impl AsRef<Path>) -> (Schema, Vec<Diagnostic>) {
        let Ok(paths) = df_files_in_dir(dir) else {
            return (Schema::empty(), Vec::new());
        };
        SchemaLoader::load_files(&paths, &RealFileSystem)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::atom::fold_atom;
    use oxabl_workspace::InMemoryFileSystem;

    fn fs(files: &[(&str, &str)]) -> (InMemoryFileSystem, Vec<PathBuf>) {
        let mut fs = InMemoryFileSystem::new();
        let mut paths = Vec::new();
        for (p, content) in files {
            let path = PathBuf::from(p);
            fs.insert(path.clone(), content.to_string());
            paths.push(path);
        }
        (fs, paths)
    }

    #[test]
    fn empty_input_yields_empty_schema() {
        let (fs, paths) = fs(&[]);
        let (schema, diags) = SchemaLoader::load_files(&paths, &fs);
        assert!(schema.is_empty());
        assert!(diags.is_empty());
    }

    #[test]
    fn single_file_populates_tables_and_fields() {
        let (fs, paths) = fs(&[(
            "/schema/a.df",
            r#"
ADD TABLE "Customer"
  AREA "Main"

ADD FIELD "CustNum" OF "Customer" AS integer
  FORMAT ">>9"
  MANDATORY
ADD FIELD "Name" OF "Customer" AS character
"#,
        )]);
        let (schema, diags) = SchemaLoader::load_files(&paths, &fs);
        assert_eq!(schema.len(), 1);
        let t = schema.get(&fold_atom("customer")).unwrap();
        assert_eq!(t.fields.len(), 2);
        assert_eq!(t.fields[0].data_type, SchemaType::Integer);
        assert!(t.fields[0].mandatory);
        assert_eq!(t.fields[1].data_type, SchemaType::Character);
        assert!(diags.is_empty());
    }

    #[test]
    fn duplicate_table_emits_warning_last_wins() {
        let (fs, paths) = fs(&[
            ("/a.df", "ADD TABLE \"T\"\n  AREA \"First\"\n"),
            ("/b.df", "ADD TABLE \"T\"\n  AREA \"Second\"\n"),
        ]);
        let (schema, diags) = SchemaLoader::load_files(&paths, &fs);
        assert_eq!(schema.len(), 1);
        assert_eq!(
            schema.get(&fold_atom("t")).unwrap().area.as_deref(),
            Some("Second")
        );
        assert_eq!(diags.len(), 1);
        assert_eq!(diags[0].code.0, SCHEMA0010);
    }

    #[test]
    fn duplicate_field_within_table_warns() {
        let (fs, paths) = fs(&[(
            "/a.df",
            r#"
ADD TABLE "T"
ADD FIELD "f" OF "T" AS integer
ADD FIELD "f" OF "T" AS integer
"#,
        )]);
        let (_, diags) = SchemaLoader::load_files(&paths, &fs);
        assert!(diags.iter().any(|d| d.code.0 == SCHEMA0011));
    }

    #[test]
    fn field_type_conflict_emits_schema0012_and_poisons_field() {
        let (fs, paths) = fs(&[
            (
                "/a.df",
                "ADD TABLE \"T\"\nADD FIELD \"f\" OF \"T\" AS character\n",
            ),
            ("/b.df", "ADD FIELD \"f\" OF \"T\" AS integer\n"),
        ]);
        let (schema, diags) = SchemaLoader::load_files(&paths, &fs);
        let t = schema.get(&fold_atom("t")).unwrap();
        assert_eq!(t.fields[0].data_type, SchemaType::Error);
        assert!(diags.iter().any(|d| d.code.0 == SCHEMA0012));
    }

    #[test]
    fn field_on_unknown_table_errors() {
        let (fs, paths) = fs(&[("/a.df", "ADD FIELD \"stray\" OF \"Missing\" AS integer\n")]);
        let (schema, diags) = SchemaLoader::load_files(&paths, &fs);
        assert!(schema.is_empty());
        assert!(diags.iter().any(|d| d.code.0 == SCHEMA0001));
    }

    #[test]
    fn read_failure_emits_schema0001() {
        let fs = InMemoryFileSystem::new();
        let paths = vec![PathBuf::from("/missing.df")];
        let (schema, diags) = SchemaLoader::load_files(&paths, &fs);
        assert!(schema.is_empty());
        assert_eq!(diags.len(), 1);
        assert_eq!(diags[0].code.0, SCHEMA0001);
    }

    #[test]
    fn workspace_root_containment_rejects_traversal() {
        let (fs, _) = fs(&[("/root/ok.df", "ADD TABLE \"X\"\n")]);
        let root = Path::new("/root");
        let paths = vec![PathBuf::from("../escape.df")];
        let (_, diags) = SchemaLoader::load_files_with_root(&paths, &fs, Some(root));
        assert_eq!(diags.len(), 1);
        assert_eq!(diags[0].code.0, SCHEMA0030);
    }

    #[test]
    fn workspace_root_allows_absolute_inside_root() {
        let (fs, _) = fs(&[("/root/a.df", "ADD TABLE \"X\"\n")]);
        let root = Path::new("/root");
        let paths = vec![PathBuf::from("/root/a.df")];
        let (schema, diags) = SchemaLoader::load_files_with_root(&paths, &fs, Some(root));
        assert_eq!(schema.len(), 1);
        assert!(diags.is_empty());
    }

    #[test]
    fn revision_bumps_once_per_load() {
        let (fs, paths) = fs(&[("/a.df", "ADD TABLE \"T\"\n")]);
        let (schema, _) = SchemaLoader::load_files(&paths, &fs);
        assert_eq!(schema.revision().raw(), 1);
    }

    #[test]
    fn empty_df_keeps_schema_empty_no_revision_bump() {
        let (fs, paths) = fs(&[("/a.df", "# only comments here\n")]);
        let (schema, diags) = SchemaLoader::load_files(&paths, &fs);
        assert!(schema.is_empty());
        assert_eq!(schema.revision().raw(), 0);
        assert!(diags.is_empty());
    }

    #[test]
    fn multi_file_merges_into_one_schema() {
        let (fs, paths) = fs(&[
            (
                "/a.df",
                "ADD TABLE \"A\"\nADD FIELD \"f\" OF \"A\" AS integer\n",
            ),
            (
                "/b.df",
                "ADD TABLE \"B\"\nADD FIELD \"g\" OF \"B\" AS character\n",
            ),
        ]);
        let (schema, diags) = SchemaLoader::load_files(&paths, &fs);
        assert_eq!(schema.len(), 2);
        assert!(diags.is_empty());
    }

    #[test]
    fn same_type_duplicate_field_across_files_warns_not_errors() {
        let (fs, paths) = fs(&[
            (
                "/a.df",
                "ADD TABLE \"T\"\nADD FIELD \"f\" OF \"T\" AS integer\n",
            ),
            ("/b.df", "ADD FIELD \"f\" OF \"T\" AS integer\n"),
        ]);
        let (_, diags) = SchemaLoader::load_files(&paths, &fs);
        assert!(diags.iter().any(|d| d.code.0 == SCHEMA0011));
        assert!(!diags.iter().any(|d| d.code.0 == SCHEMA0012));
    }
}
