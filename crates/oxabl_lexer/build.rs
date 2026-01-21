use std::env;
use std::path::Path;

fn main() {
    string_cache_codegen::AtomType::new("oxabl_atom::OxablAtom", "atom!")
        .atoms(&[
            "+", "-", "*", "/", "%", "=", "!=", "<", ">", "<=", ">=", "+=", "-=", "*=", "/=", "if",
            "do", "while", "for",
        ]) // Your static strings
        .write_to_file(&Path::new(&env::var("OUT_DIR").unwrap()).join("oxabl_atom.rs"))
        .unwrap()
}
