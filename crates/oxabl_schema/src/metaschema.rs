//! The OpenEdge metaschema: the dictionary and virtual system tables that
//! every OpenEdge database carries.
//!
//! `_file`, `_field`, `_index`, `_db`, `_sequence`, `_user` and their kin are
//! real, queryable tables, and ABL reads them directly to introspect the
//! database at runtime — dynamic field lookup, generic maintenance screens,
//! permission mapping, dictionary-driven UI. They never appear in a `.df`
//! dump, because a `.df` describes the *application* schema rather than the
//! dictionary that stores it. A downstream consumer therefore cannot supply
//! them; they belong to oxabl the same way built-in functions do.
//!
//! The catalog ships as a generated `.df`
//! (`resources/metaschema.df`) and is parsed through the ordinary
//! [`SchemaLoader`](crate::SchemaLoader) on first use. Using the same text
//! format and the same parser as user schema is the point: there is no second
//! model to keep in step, and a metaschema table is a [`Table`](crate::Table)
//! indistinguishable from any other.
//!
//! It is parsed once per process and then *borrowed*, never copied: a
//! [`Schema`] that has
//! [`enable_metaschema`](crate::Schema::enable_metaschema) set hands out
//! [`TableId`](crate::TableId)s pointing into this one static catalog, so
//! loading a `.df` costs nothing per metaschema table.
//!
//! See `METASCHEMA.md` for the layout it targets and how to regenerate it.

use std::sync::LazyLock;

use oxabl_common::FileId;

use crate::loader::schema_from_source;
use crate::schema::Schema;

/// The generated `.df` source for the metaschema catalog.
///
/// Exposed so a consumer can render or diff the catalog without reaching into
/// the crate's file layout.
pub const METASCHEMA_DF: &str = include_str!("../resources/metaschema.df");

/// Parsed once per process. Parsing costs a couple of milliseconds; every
/// schema that enables the catalog then borrows this one allocation, so a
/// long-lived client pays it once no matter how often it reloads.
static METASCHEMA: LazyLock<Schema> = LazyLock::new(|| {
    let (schema, diagnostics) = schema_from_source(METASCHEMA_DF, FileId::UNKNOWN);
    // A `debug_assert` because a release build should not pay for it on a
    // path that cannot vary at runtime. What actually keeps a broken catalog
    // out of a release is `the_shipped_catalog_parses_without_diagnostics`
    // below, which asserts the same thing in CI -- do not delete one without
    // the other.
    debug_assert!(
        diagnostics.is_empty(),
        "the shipped metaschema .df must parse cleanly: {diagnostics:?}"
    );
    schema
});

/// The metaschema catalog as a [`Schema`].
///
/// A caller that wants the catalog *behind* a user schema should let
/// [`SchemaLoader`](crate::SchemaLoader) enable it, which gets the precedence
/// right; this accessor exists for inspection and for tests.
pub fn metaschema() -> &'static Schema {
    &METASCHEMA
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::atom::fold_atom;
    use crate::schema::{FieldResolution, SchemaType};

    #[test]
    fn the_shipped_catalog_parses_without_diagnostics() {
        let (schema, diagnostics) = schema_from_source(METASCHEMA_DF, FileId::UNKNOWN);
        assert!(
            diagnostics.is_empty(),
            "metaschema.df must load cleanly: {diagnostics:#?}"
        );
        assert!(!schema.is_empty());
    }

    #[test]
    fn the_shipped_catalog_is_not_materially_smaller_than_the_one_reviewed() {
        // The named-table tests below would all pass on a catalog that lost
        // most of its contents, so this is the one that actually backs the
        // claim that a truncated regeneration fails CI. A metaschema only
        // grows across OpenEdge releases, so a floor is safe; raise it when a
        // regeneration legitimately adds tables.
        let m = metaschema();
        let tables = m.len();
        let fields: usize = m.tables().map(|(_, t)| t.fields.len()).sum();
        assert!(tables >= 120, "only {tables} tables in the catalog");
        assert!(fields >= 1600, "only {fields} fields in the catalog");
    }

    #[test]
    fn catalog_tables_do_not_claim_to_come_from_a_user_file() {
        // Every table's `source` span is what a "defined at" answer would
        // point to. The catalog is not in any of the user's `.df` files, so
        // it must not name one -- `FileId::UNKNOWN` is the honest answer.
        for (_, table) in metaschema().tables() {
            assert_eq!(table.source.file, FileId::UNKNOWN, "{}", table.display_name);
            for field in &table.fields {
                assert_eq!(field.source.file, FileId::UNKNOWN);
            }
        }
    }

    #[test]
    fn the_headline_dictionary_tables_are_present() {
        let m = metaschema();
        for name in [
            "_file",
            "_field",
            "_index",
            "_index-field",
            "_db",
            "_sequence",
            "_user",
            "_file-trig",
            "_field-trig",
            "_view",
            "_area",
        ] {
            assert!(m.table(name).is_some(), "missing metaschema table `{name}`");
        }
    }

    #[test]
    fn the_headline_virtual_system_tables_are_present() {
        let m = metaschema();
        // VSTs are not stored records but they are queryable from ABL in
        // every database, and code that reads them is exactly the code that
        // reads `_file` — so they belong in the same catalog.
        for name in ["_connect", "_lock", "_trans", "_myconnection", "_dbstatus"] {
            assert!(m.table(name).is_some(), "missing system table `{name}`");
        }
    }

    #[test]
    fn a_table_carries_its_fields_not_just_its_name() {
        let file = metaschema().table("_file").expect("_file");
        assert!(
            file.fields.len() > 20,
            "_file should carry its full field list, got {}",
            file.fields.len()
        );
        assert!(file.field("_file-name").is_some());
        assert!(file.field("_file-number").is_some());
        assert!(file.field("_prime-index").is_some());
        assert!(file.field("_dump-name").is_some());
    }

    #[test]
    fn field_types_come_through_the_df() {
        let file = metaschema().table("_file").expect("_file");
        assert_eq!(
            file.field("_file-name").unwrap().data_type,
            SchemaType::Character
        );
        assert_eq!(
            file.field("_file-number").unwrap().data_type,
            SchemaType::Integer
        );
        assert_eq!(
            file.field("_prime-index").unwrap().data_type,
            SchemaType::Recid
        );
        assert_eq!(
            file.field("_hidden").unwrap().data_type,
            SchemaType::Logical
        );
    }

    #[test]
    fn sql_only_types_round_trip_as_unknown_rather_than_failing() {
        // A handful of catalog fields carry SQL-layer spellings (`fixchar`,
        // `timestamp`, ...) that the `.df` primitive list does not name. The
        // loader's documented behaviour is to round-trip them; assert that,
        // so a future `SchemaType` addition is a deliberate change.
        let unknown = metaschema()
            .tables()
            .flat_map(|(_, t)| t.fields.iter())
            .filter(|f| matches!(f.data_type, SchemaType::Unknown(_)))
            .count();
        assert!(unknown > 0, "expected some SQL-layer type spellings");
        let poisoned = metaschema()
            .tables()
            .flat_map(|(_, t)| t.fields.iter())
            .filter(|f| f.data_type == SchemaType::Error)
            .count();
        assert_eq!(poisoned, 0, "no metaschema field should be type-poisoned");
    }

    #[test]
    fn extents_survive_the_round_trip() {
        let file = metaschema().table("_file").expect("_file");
        assert_eq!(file.field("_fil-misc1").unwrap().extent, Some(8));
        assert_eq!(file.field("_file-name").unwrap().extent, None);
    }

    #[test]
    fn indexes_survive_the_round_trip() {
        let file = metaschema().table("_file").expect("_file");
        assert!(
            !file.indexes.is_empty(),
            "_file should carry its index definitions"
        );
        assert!(file.get_index(&fold_atom("_file-name")).is_some());
    }

    #[test]
    fn abbreviated_field_references_resolve_against_the_catalog() {
        // ABL's unique-prefix abbreviation rule applies to metaschema fields
        // like any others; this is free, but assert it so the catalog's
        // shape is understood to feed it.
        let db = metaschema().table("_db").expect("_db");
        assert!(matches!(
            db.resolve_field_by_name("_db-name"),
            FieldResolution::Unique(_)
        ));
    }

    #[test]
    fn every_table_name_starts_with_an_underscore() {
        // The dictionary reserves the leading underscore, which is what makes
        // merging the catalog beneath user tables safe in practice.
        for (_, table) in metaschema().tables() {
            assert!(
                table.display_name.starts_with('_'),
                "unexpected metaschema table `{}`",
                table.display_name
            );
        }
    }

    /// A user schema built straight from `.df` text, with the catalog off.
    fn user_schema(df: &str) -> Schema {
        let (schema, diagnostics) = schema_from_source(df, FileId::UNKNOWN);
        assert!(diagnostics.is_empty(), "{diagnostics:#?}");
        schema
    }

    #[test]
    fn an_enabled_schema_borrows_the_catalog_rather_than_copying_it() {
        // The whole point of the borrow: enabling the catalog must not change
        // how many tables the schema itself holds, because `len`/`is_empty`
        // are read elsewhere as "how much user schema is configured".
        let mut schema = user_schema("ADD TABLE \"Customer\"\n");
        assert_eq!(schema.len(), 1);
        assert!(schema.table("_file").is_none());

        schema.enable_metaschema();
        assert_eq!(schema.len(), 1, "the catalog must not inflate the count");
        assert!(!schema.is_empty());
        assert_eq!(schema.tables().count(), 1, "iteration stays user-only");

        // ...but lookups see it, and the borrowed table is the very same
        // allocation the static catalog holds.
        let borrowed = schema.table("_file").expect("_file via the catalog");
        let canonical = metaschema().table("_file").expect("_file");
        assert!(std::ptr::eq(borrowed, canonical));
    }

    #[test]
    fn a_disabled_schema_does_not_see_the_catalog() {
        let mut schema = Schema::empty();
        assert!(!schema.metaschema_enabled());
        assert!(schema.table("_file").is_none());
        schema.enable_metaschema();
        assert!(schema.table("_file").is_some());
    }

    #[test]
    fn a_user_table_shadows_the_catalog_entry() {
        let mut schema =
            user_schema("ADD TABLE \"_file\"\nADD FIELD \"mine\" OF \"_file\" AS character\n");
        schema.enable_metaschema();
        let t = schema.table("_file").expect("_file");
        assert!(t.field("mine").is_some(), "the user's table must win");
        assert!(t.field("_file-name").is_none());
    }

    #[test]
    fn a_borrowed_id_round_trips_through_get_by_id() {
        // Everything downstream holds a `TableId` and resolves it later, so
        // the id a catalog lookup mints has to come back to the same table.
        let mut schema = Schema::empty();
        schema.enable_metaschema();
        let id = schema.table_id(&fold_atom("_field")).expect("_field id");
        assert_eq!(
            schema.get_by_id(id).expect("resolves back").display_name,
            "_Field"
        );

        // A user id and a catalog id must not collide.
        let mut mixed = user_schema("ADD TABLE \"Customer\"\n");
        mixed.enable_metaschema();
        let user = mixed.table_id(&fold_atom("customer")).unwrap();
        let builtin = mixed.table_id(&fold_atom("_field")).unwrap();
        assert_ne!(user, builtin);
        assert_eq!(mixed.get_by_id(user).unwrap().display_name, "Customer");
        assert_eq!(mixed.get_by_id(builtin).unwrap().display_name, "_Field");
    }

    #[test]
    fn a_user_df_never_patches_the_shared_catalog() {
        // An `ADD FIELD ... OF "_file"` with no `ADD TABLE` of its own names a
        // table only the catalog has. The catalog is immutable and shared by
        // every schema in the process, so patching it would corrupt them all:
        // the field has to be dropped with the usual unknown-table
        // diagnostic instead.
        let (mut schema, diagnostics) = schema_from_source(
            "ADD FIELD \"mine\" OF \"_file\" AS character\n",
            FileId::UNKNOWN,
        );
        assert_eq!(diagnostics.len(), 1, "expected an unknown-table diagnostic");
        schema.enable_metaschema();
        assert!(
            metaschema().table("_file").unwrap().field("mine").is_none(),
            "the catalog must be untouched"
        );
        assert!(schema.table("_file").unwrap().field("mine").is_none());
    }
}
