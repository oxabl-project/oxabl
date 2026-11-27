//! Dumps all tokens from the benchmark file for inspection.
//!
//! Run with: cargo run -p oxabl_lexer --example dump_tokens
//!
//! Options:
//!   --errors   Only show tokens with potential parse issues
//!   --summary  Only show the summary

use oxabl_common::SourceMap;
use oxabl_lexer::{Kind, Token, tokenize};
use std::collections::HashMap;
use std::fs;

fn main() {
    let args: Vec<String> = std::env::args().collect();
    let errors_only = args.iter().any(|a| a == "--errors");
    let summary_only = args.iter().any(|a| a == "--summary");

    let source = match fs::read_to_string("resources/bench_keywords.abl") {
        Ok(text) => text,
        Err(err) => {
            eprintln!("Failed to read resources/bench_keywords.abl: {err}");
            std::process::exit(1);
        }
    };
    let source_map = SourceMap::new(&source);
    let tokens = tokenize(&source);

    println!("=== Token Stats ===");
    println!("File size: {} bytes", source.len());
    println!("Total tokens: {}\n", tokens.len());

    if !summary_only {
        print_tokens(&source, &source_map, &tokens, errors_only);
    }
    print_summary(&tokens);
}

fn print_tokens(source: &str, source_map: &SourceMap, tokens: &[Token], errors_only: bool) {
    if errors_only {
        println!("=== Tokens with Potential Parse Issues ===\n");
    } else {
        println!("=== Token Dump ===\n");
    }

    for (i, token) in tokens.iter().enumerate() {
        let slice = token.slice(source);
        let is_numeric_kind = matches!(
            token.kind,
            Kind::IntegerLiteral | Kind::BigIntLiteral | Kind::DecimalLiteral
        );
        let has_no_value = match token.kind {
            Kind::IntegerLiteral | Kind::BigIntLiteral => token.integer_value(source).is_none(),
            Kind::DecimalLiteral => token.decimal_value(source).is_none(),
            _ => true,
        };
        let looks_like_number = slice.chars().next().is_some_and(|c| c.is_ascii_digit());
        let is_suspicious = is_numeric_kind && has_no_value && !looks_like_number;

        if errors_only && !is_suspicious {
            continue;
        }

        let display_slice: String = if slice.len() > 40 {
            format!("{}...", &slice[..40])
        } else {
            slice.replace('\n', "\\n")
        };
        let flag = if is_suspicious { " <-- ISSUE" } else { "" };
        let (line_number, column_number) = source_map.lookup(token.start as usize);

        let value_display = match token.kind {
            Kind::IntegerLiteral | Kind::BigIntLiteral => {
                format!("{:?}", token.integer_value(source))
            }
            Kind::DecimalLiteral => format!("{:?}", token.decimal_value(source)),
            Kind::StringLiteral | Kind::DateLiteral | Kind::IncludeReference => {
                format!("Text({:?})", token.text(source))
            }
            Kind::IncludeArgReference => format!("{:?}", token.include_arg_index(source)),
            Kind::KwTrue | Kind::KwFalse => format!("{:?}", token.boolean_value()),
            _ => "None".to_string(),
        };

        println!(
            "{:5} | {:20} | {:5}:{:<5} | {:?} | {}{}",
            i,
            format!("{:?}", token.kind),
            line_number,
            column_number,
            display_slice,
            value_display,
            flag
        );
    }
}

fn print_summary(tokens: &[Token]) {
    println!("\n=== Token Summary by Kind ===");
    let mut counts: HashMap<Kind, usize> = HashMap::new();
    for token in tokens {
        *counts.entry(token.kind).or_insert(0) += 1;
    }

    let mut counts_vec: Vec<_> = counts.into_iter().collect();
    counts_vec.sort_by_key(|b| std::cmp::Reverse(b.1));

    for (kind, count) in counts_vec {
        println!("  {:30} : {}", format!("{:?}", kind), count);
    }
}
