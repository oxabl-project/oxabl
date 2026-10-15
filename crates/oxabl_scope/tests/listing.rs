//! The LISTING parser and the comparison with the model, on synthetic
//! listings (`tests/listings/*.lst`, written by hand from the remembered
//! format, not by a compiler).

use std::path::Path;

use oxabl_scope::listing::{self, Disagreement};

fn dir() -> std::path::PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR")).join("tests")
}

fn compare(name: &str) -> listing::Comparison {
    let df = std::fs::read_to_string(dir().join("fixtures/schema.df")).expect("schema.df");
    let schema = oxabl_schema::test_support::schema_from_df(&df);
    let source = std::fs::read_to_string(dir().join("listings").join(name)).expect("source");
    let lst = std::fs::read_to_string(dir().join("listings").join(format!("{name}.lst")))
        .expect("listing");
    let tokens = oxabl_lexer::tokenize(&source);
    let program = oxabl_parser::Parser::new(&tokens, &source).parse_program();
    assert!(program.errors.is_empty(), "{:?}", program.errors);
    let scopes = oxabl_scope::analyze_file(&program.statements, &source, Some(&schema));
    let line_of = |at: u32| source[..at as usize].matches('\n').count() as u32 + 1;
    let model = listing::model_blocks(
        &scopes,
        |span| Some((name.to_string(), line_of(span.start))),
        name,
    );
    listing::compare(&listing::parse(&lst), &model)
}

#[test]
fn a_listing_that_agrees_has_no_disagreement() {
    let c = compare("basic.p");
    assert_eq!(c.disagreements, Vec::new());
    assert_eq!(c.listing_blocks, 6);
    assert_eq!(c.model_blocks, 6);
    assert_eq!(c.matched, 6);
    assert_eq!(c.kind_agree, 6);
    assert_eq!(c.tran_agree, 6);
    assert_eq!(c.buffers_agree, 6);
    // The REPEAT's DO and the procedure are transaction blocks of their own
    // unit only, so the outermost reading agrees too.
    assert_eq!(c.tran_outermost_agree, 6);
}

#[test]
fn every_disagreement_shows_both_sides() {
    let c = compare("disagree.p");
    assert_eq!(c.matched, 6);
    assert_eq!(c.tran_agree, 5);
    assert_eq!(c.buffers_agree, 5);
    assert_eq!(c.disagreements.len(), 2, "{:#?}", c.disagreements);
    let tran = c
        .disagreements
        .iter()
        .find_map(|d| match d {
            Disagreement::Transaction(l, m) => Some((l, m)),
            _ => None,
        })
        .expect("a transaction disagreement");
    assert_eq!((tran.0.line, tran.0.transaction), (14, true));
    assert_eq!(tran.1.transaction, Some(false));
    let buffers = c
        .disagreements
        .iter()
        .find_map(|d| match d {
            Disagreement::Buffers(l, m) => Some((l, m)),
            _ => None,
        })
        .expect("a buffer disagreement");
    assert_eq!(buffers.0.line, 5);
    assert_eq!(buffers.0.buffers, ["sports.Customer", "sports.Order"]);
    assert_eq!(
        buffers.1.buffers.as_deref(),
        Some(&["customer".to_string()][..])
    );
}

#[test]
fn the_parser_reads_pages_lists_and_spaced_file_names() {
    let text = std::fs::read_to_string(dir().join("listings/format.lst")).expect("listing");
    let l = listing::parse(&text);
    assert_eq!(l.blocks.len(), 3, "{:#?}", l.blocks);
    let main = &l.blocks[0];
    assert_eq!(
        (main.file.as_str(), main.line, main.transaction),
        ("a.p", 0, true)
    );
    assert_eq!(main.buffers, ["sports.Customer", "sports.Order"]);
    assert_eq!(main.frames, ["Unnamed"]);
    let inc = &l.blocks[1];
    assert_eq!(
        (inc.file.as_str(), inc.line, inc.kind.as_str()),
        ("inc/b.i", 12, "For")
    );
    assert_eq!(inc.label, "outer");
    assert_eq!(inc.buffers, ["ttLine", "sports.Item"]);
    let ip = &l.blocks[2];
    assert_eq!(ip.file, "C:\\My Work\\a.p");
    assert_eq!((ip.line, ip.label.as_str()), (40, "Procedure ip-name"));
}

#[test]
fn a_listing_without_a_block_table_has_no_blocks() {
    assert_eq!(
        listing::parse("{} Line Blk\n-- ---- ---\n      1     x = 1.\n")
            .blocks
            .len(),
        0
    );
}
