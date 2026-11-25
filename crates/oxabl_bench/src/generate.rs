//! A deterministic synthetic ABL workspace.
//!
//! The same [`Knobs`] always give the same files, byte for byte. Every file is
//! synthetic and parses with no error. The shape is set by the knobs: the
//! number of programs and classes, the size of each kind of file, the include
//! fan-in (a few "hot" includes that most programs read), the include fan-out
//! (programs that read hundreds of includes), chains of nested includes, the
//! density of preprocessor directives, the temp-tables and datasets of the
//! includes, and a `.df` schema.
//!
//! Layout of a workspace:
//!
//! - `oxabl.toml`: include paths `src` and `.`, the schema `schema/app.df`.
//! - `schema/app.df`: tables `tbl0000`, `tbl0001`, ...
//! - `src/app/m<M>/p<N>.p`: the programs (roots).
//! - `src/cls/pk<P>/C<N>.cls`: the classes (roots).
//! - `inc/hot/h<H>.i`: includes that many programs read.
//! - `inc/lib/l<N>.i`: includes that a few programs read each.
//! - `inc/chain/ch<C>/d<D>.i`: chains; `d<D>` includes `d<D+1>`.

use std::collections::HashMap;
use std::fmt::Write;
use std::io;
use std::path::{Path, PathBuf};

/// The knobs of a workspace.
#[derive(Debug, Clone, PartialEq)]
pub struct Knobs {
    /// The seed of every random choice.
    pub seed: u64,
    /// Procedure files (`.p`), each a root.
    pub programs: usize,
    /// Class files (`.cls`), each a root.
    pub classes: usize,
    /// Classes in one package directory.
    pub classes_per_package: usize,
    /// Lines of a program or class, smallest and largest. The sizes are spread
    /// on a log scale, so most files are small and a few are large.
    pub root_lines: (usize, usize),
    /// Includes in the shared pool that programs read a few of.
    pub lib_includes: usize,
    /// Lines of a pool include, smallest and largest.
    pub lib_lines: (usize, usize),
    /// Pool includes that an ordinary program reads, smallest and largest.
    pub includes_per_program: (usize, usize),
    /// Includes that many programs read.
    pub hot_includes: usize,
    /// The share of programs that read the first hot include. Each next hot
    /// include is read by 0.8 times the share of the one before it.
    pub hot_share: f64,
    /// Lines of a hot include.
    pub hot_lines: usize,
    /// Programs that read [`fan_out`](Self::fan_out) pool includes each.
    pub fan_out_programs: usize,
    /// Pool includes of a fan-out program.
    pub fan_out: usize,
    /// Chains of nested includes.
    pub chains: usize,
    /// Includes in one chain: the depth of the nesting.
    pub chain_depth: usize,
    /// Lines of one include of a chain.
    pub chain_lines: usize,
    /// Programs that read the head of a chain.
    pub chain_programs: usize,
    /// One preprocessor block (`&SCOPED-DEFINE`, `&IF`, `{&name}`) for each
    /// this many blocks of code. 0 for none.
    pub preprocessor_every: usize,
    /// Temp-tables defined in each pool, hot and chain include.
    pub temp_tables_per_include: usize,
    /// A dataset over the temp-tables of every this many includes. 0 for none.
    pub dataset_every: usize,
    /// Tables of the schema.
    pub tables: usize,
    /// Fields of each table, at least 2.
    pub fields: usize,
}

/// A named set of knobs.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Preset {
    /// About 300 small roots, for a CodSpeed bench.
    Ci,
    /// About a tenth of [`Preset::Erp`], for a quick local run.
    Medium,
    /// The shape of a large ERP code base: about 14,000 files.
    Erp,
}

impl Preset {
    /// Every preset, with its name.
    pub const ALL: [(&'static str, Preset); 3] = [
        ("ci", Preset::Ci),
        ("medium", Preset::Medium),
        ("erp", Preset::Erp),
    ];

    /// The preset of `name`.
    pub fn named(name: &str) -> Option<Preset> {
        Self::ALL
            .iter()
            .find(|(n, _)| *n == name)
            .map(|(_, preset)| *preset)
    }

    /// The knobs of the preset.
    pub fn knobs(self) -> Knobs {
        match self {
            Preset::Ci => Knobs {
                seed: 499,
                programs: 270,
                classes: 30,
                classes_per_package: 10,
                root_lines: (20, 120),
                lib_includes: 60,
                lib_lines: (10, 40),
                includes_per_program: (0, 3),
                hot_includes: 2,
                hot_share: 0.9,
                hot_lines: 40,
                fan_out_programs: 2,
                fan_out: 30,
                chains: 2,
                chain_depth: 4,
                chain_lines: 60,
                chain_programs: 10,
                preprocessor_every: 6,
                temp_tables_per_include: 1,
                dataset_every: 5,
                tables: 40,
                fields: 8,
            },
            Preset::Medium => Knobs {
                programs: 600,
                classes: 100,
                lib_includes: 660,
                fan_out_programs: 5,
                chains: 4,
                chain_programs: 20,
                tables: 200,
                ..Preset::Erp.knobs()
            },
            Preset::Erp => Knobs {
                seed: 499,
                programs: 6_000,
                classes: 1_000,
                classes_per_package: 40,
                root_lines: (30, 3_000),
                lib_includes: 6_590,
                lib_lines: (20, 400),
                includes_per_program: (1, 8),
                hot_includes: 6,
                hot_share: 0.85,
                hot_lines: 300,
                fan_out_programs: 50,
                fan_out: 300,
                chains: 40,
                chain_depth: 10,
                chain_lines: 3_200,
                chain_programs: 200,
                preprocessor_every: 8,
                temp_tables_per_include: 2,
                dataset_every: 10,
                tables: 1_500,
                fields: 20,
            },
        }
    }
}

/// One generated file.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct GenFile {
    /// The path below the workspace directory, with `/`.
    pub path: String,
    pub text: String,
    /// Indexes into [`Workspace::files`] of the files it includes, in order.
    pub includes: Vec<usize>,
    pub root: bool,
}

/// A generated workspace.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Workspace {
    pub files: Vec<GenFile>,
}

/// The measured shape of a [`Workspace`].
#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct Stats {
    pub files: usize,
    pub roots: usize,
    pub includes: usize,
    pub bytes: usize,
    pub lines: usize,
    /// Roots that read each include, through any depth: the largest five.
    pub top_fan_in: Vec<usize>,
    /// Includes with a fan-in of 1,000 or more.
    pub includes_over_1000_roots: usize,
    /// The most includes one root reads directly.
    pub max_fan_out: usize,
    /// The deepest nesting of includes below a root.
    pub max_depth: usize,
    /// The lines of each root with its includes expanded: the largest.
    pub max_expanded_lines: usize,
    /// Roots of more than 30,000 expanded lines.
    pub roots_over_30000_lines: usize,
}

/// A small, fast, deterministic random source (SplitMix64).
struct Rng(u64);

impl Rng {
    fn next(&mut self) -> u64 {
        self.0 = self.0.wrapping_add(0x9E37_79B9_7F4A_7C15);
        let mut z = self.0;
        z = (z ^ (z >> 30)).wrapping_mul(0xBF58_476D_1CE4_E5B9);
        z = (z ^ (z >> 27)).wrapping_mul(0x94D0_49BB_1331_11EB);
        z ^ (z >> 31)
    }

    fn below(&mut self, n: usize) -> usize {
        (self.next() % n.max(1) as u64) as usize
    }

    fn unit(&mut self) -> f64 {
        (self.next() >> 11) as f64 / (1u64 << 53) as f64
    }

    fn between(&mut self, (lo, hi): (usize, usize)) -> usize {
        lo + self.below(hi.saturating_sub(lo) + 1)
    }

    /// A size in `lo..=hi` on a log scale.
    fn log_between(&mut self, (lo, hi): (usize, usize)) -> usize {
        let (lo, hi) = (lo.max(1) as f64, hi.max(lo.max(1)) as f64);
        (lo * (hi / lo).powf(self.unit())).round() as usize
    }
}

/// What a block of code may name.
struct Scope<'a> {
    /// The prefix of its variables and procedures.
    prefix: String,
    temp_tables: Vec<String>,
    procedures: bool,
    /// Programs it may `RUN`.
    programs: &'a [String],
    /// Classes it may `NEW`.
    classes: &'a [String],
    /// Functions of the hot includes it reads.
    functions: Vec<String>,
}

struct Writer<'k> {
    knobs: &'k Knobs,
    rng: Rng,
}

impl Writer<'_> {
    fn table(&mut self) -> String {
        format!("tbl{:04}", self.rng.below(self.knobs.tables))
    }

    fn variables(out: &mut String, prefix: &str, indent: &str) {
        for (name, ty) in [
            ("i", "INTEGER"),
            ("total", "DECIMAL"),
            ("text", "CHARACTER"),
        ] {
            writeln!(
                out,
                "{indent}DEFINE VARIABLE {prefix}-{name} AS {ty} NO-UNDO."
            )
            .unwrap();
        }
    }

    fn temp_table(out: &mut String, name: &str, access: &str) {
        writeln!(
            out,
            "DEFINE {access}TEMP-TABLE {name} NO-UNDO\n    \
             FIELD id AS INTEGER\n    \
             FIELD label AS CHARACTER\n    \
             FIELD amount AS DECIMAL\n    \
             INDEX pk IS PRIMARY UNIQUE id."
        )
        .unwrap();
    }

    fn dataset(out: &mut String, name: &str, parent: &str, child: &str) {
        writeln!(
            out,
            "DEFINE DATASET {name} FOR {parent}, {child}\n    \
             DATA-RELATION r-{name} FOR {parent}, {child}\n    \
             RELATION-FIELDS (id, id)."
        )
        .unwrap();
    }

    /// Blocks of code until `out` has `lines` more lines.
    fn blocks(&mut self, out: &mut String, scope: &Scope<'_>, lines: usize, indent: &str) {
        let mut written = 0;
        let mut k = 0usize;
        while written < lines {
            let start = out.len();
            self.block(out, scope, k, indent);
            written += out[start..].bytes().filter(|&b| b == b'\n').count();
            k += 1;
        }
        let p = &scope.prefix;
        writeln!(out, "{indent}MESSAGE {p}-i {p}-total {p}-text.").unwrap();
    }

    fn block(&mut self, out: &mut String, scope: &Scope<'_>, k: usize, ind: &str) {
        let p = &scope.prefix;
        let every = self.knobs.preprocessor_every;
        if every > 0 && k % every == every - 1 {
            writeln!(
                out,
                "&SCOPED-DEFINE {p}-step-{k} {k}\n\
                 &IF DEFINED(TRACE-ON) &THEN\n\
                 {ind}MESSAGE \"trace\" {{&{p}-step-{k}}}.\n\
                 &ELSE\n\
                 {ind}{p}-total = {p}-total + {{&{p}-step-{k}}}.\n\
                 &ENDIF"
            )
            .unwrap();
            return;
        }
        match self.rng.below(9) {
            0 => writeln!(
                out,
                "{ind}DO {p}-i = 1 TO {n}:\n\
                 {ind}    ASSIGN {p}-total = {p}-total + {p}-i * {k}\n\
                 {ind}           {p}-text = {p}-text + STRING({p}-i).\n\
                 {ind}END.",
                n = 2 + k % 7
            ),
            1 | 2 if self.knobs.tables > 0 => {
                let t = self.table();
                writeln!(
                    out,
                    "{ind}FOR EACH {t} NO-LOCK WHERE {t}.fld01 > {k}:\n\
                     {ind}    {p}-total = {p}-total + {t}.fld01.\n\
                     {ind}END.\n\
                     {ind}FIND FIRST {t} WHERE {t}.fld01 = {k} NO-LOCK NO-ERROR.\n\
                     {ind}IF AVAILABLE {t} THEN {p}-text = {t}.fld02."
                )
            }
            3 if !scope.temp_tables.is_empty() => {
                let tt = &scope.temp_tables[self.rng.below(scope.temp_tables.len())];
                writeln!(
                    out,
                    "{ind}CREATE {tt}.\n\
                     {ind}ASSIGN {tt}.id = {k}\n\
                     {ind}       {tt}.label = \"row {k}\".\n\
                     {ind}FOR EACH {tt} WHERE {tt}.id > 0:\n\
                     {ind}    {p}-total = {p}-total + {tt}.amount.\n\
                     {ind}END."
                )
            }
            4 if scope.procedures => writeln!(
                out,
                "PROCEDURE {p}-proc-{k}:\n    \
                 DEFINE INPUT PARAMETER pi-value AS INTEGER NO-UNDO.\n    \
                 DEFINE OUTPUT PARAMETER po-text AS CHARACTER NO-UNDO.\n    \
                 po-text = STRING(pi-value * 2).\n    \
                 IF pi-value > 10 THEN po-text = po-text + \"+\".\n\
                 END PROCEDURE.\n\
                 RUN {p}-proc-{k} (INPUT {k}, OUTPUT {p}-text)."
            ),
            5 if !scope.functions.is_empty() => {
                let f = &scope.functions[self.rng.below(scope.functions.len())];
                writeln!(out, "{ind}{p}-text = {f}({p}-total).")
            }
            6 if !scope.classes.is_empty() => {
                let c = &scope.classes[self.rng.below(scope.classes.len())];
                writeln!(
                    out,
                    "{ind}DEFINE VARIABLE {p}-obj-{k} AS {c} NO-UNDO.\n\
                     {ind}{p}-obj-{k} = NEW {c}().\n\
                     {ind}{p}-total = {p}-total + {p}-obj-{k}:Compute({k}).\n\
                     {ind}DELETE OBJECT {p}-obj-{k}."
                )
            }
            7 if !scope.programs.is_empty() => {
                let r = &scope.programs[self.rng.below(scope.programs.len())];
                writeln!(out, "{ind}RUN {r} (INPUT {p}-text).")
            }
            _ => writeln!(
                out,
                "{ind}CASE {p}-i:\n\
                 {ind}    WHEN 1 THEN {p}-text = \"one\".\n\
                 {ind}    WHEN 2 THEN {p}-text = \"two\".\n\
                 {ind}    OTHERWISE {p}-text = SUBSTRING({p}-text, 1, 10).\n\
                 {ind}END CASE."
            ),
        }
        .unwrap();
    }

    /// The temp-tables (and a dataset) of an include, with names from `prefix`.
    fn include_tables(&mut self, out: &mut String, prefix: &str, ordinal: usize) -> Vec<String> {
        let names: Vec<String> = (0..self.knobs.temp_tables_per_include)
            .map(|t| format!("tt-{prefix}-{t}"))
            .collect();
        for name in &names {
            Self::temp_table(out, name, "");
        }
        let every = self.knobs.dataset_every;
        if every > 0 && ordinal.is_multiple_of(every) && names.len() >= 2 {
            Self::dataset(out, &format!("ds-{prefix}"), &names[0], &names[1]);
        }
        names
    }
}

fn schema(knobs: &Knobs) -> String {
    const TYPES: [(&str, &str); 4] = [
        ("decimal", "->>,>>9.99"),
        ("character", "x(20)"),
        ("logical", "yes/no"),
        ("date", "99/99/9999"),
    ];
    let mut df = String::new();
    for t in 0..knobs.tables {
        let table = format!("tbl{t:04}");
        writeln!(
            df,
            "ADD TABLE \"{table}\"\n  AREA \"Data\"\n  DUMP-NAME \"{table}\"\n"
        )
        .unwrap();
        for f in 1..=knobs.fields.max(2) {
            let (ty, format) = match f {
                1 => ("integer", ">>>>>9"),
                2 => ("character", "x(30)"),
                _ => TYPES[f % TYPES.len()],
            };
            writeln!(
                df,
                "ADD FIELD \"fld{f:02}\" OF \"{table}\" AS {ty}\n  FORMAT \"{format}\"\n  ORDER {}\n",
                f * 10
            )
            .unwrap();
        }
        writeln!(
            df,
            "ADD INDEX \"pk\" ON \"{table}\"\n  AREA \"Index\"\n  UNIQUE\n  PRIMARY\n  INDEX-FIELD \"fld01\" ASCENDING\n"
        )
        .unwrap();
    }
    df.push_str(".\nPSC\ncpstream=UTF-8\n.\n0000000000\n");
    df
}

/// The name a program is `RUN` by, below `src`.
fn program_name(n: usize) -> String {
    format!("app/m{}/p{n}.p", n / 100)
}

fn class_name(knobs: &Knobs, n: usize) -> String {
    format!("cls.pk{}.C{n}", n / knobs.classes_per_package.max(1))
}

/// Generate the workspace of `knobs`.
pub fn generate(knobs: &Knobs) -> Workspace {
    let mut w = Writer {
        knobs,
        rng: Rng(knobs.seed),
    };
    let mut files: Vec<GenFile> = Vec::new();
    let add = |files: &mut Vec<GenFile>, path: String, text: String, includes, root| {
        files.push(GenFile {
            path,
            text,
            includes,
            root,
        });
        files.len() - 1
    };

    add(
        &mut files,
        "oxabl.toml".into(),
        "[workspace]\nname = \"bench\"\n\n\
         [workspace.sources]\ninclude_paths = [\"src\", \".\"]\n\n\
         [workspace.schema]\nfiles = [\"schema/app.df\"]\n"
            .into(),
        Vec::new(),
        false,
    );
    add(
        &mut files,
        "schema/app.df".into(),
        schema(knobs),
        Vec::new(),
        false,
    );

    let programs: Vec<String> = (0..knobs.programs).map(program_name).collect();
    let classes: Vec<String> = (0..knobs.classes).map(|n| class_name(knobs, n)).collect();

    // Hot includes: shared definitions and a function each.
    let mut hot = Vec::new();
    for h in 0..knobs.hot_includes {
        let prefix = format!("h{h}");
        let mut out = format!("/* Hot include {h}. */\n&GLOBAL-DEFINE {prefix}-app \"bench\"\n");
        let temp_tables = w.include_tables(&mut out, &prefix, h);
        writeln!(
            out,
            "FUNCTION {prefix}-fmt RETURNS CHARACTER (INPUT pd-value AS DECIMAL):\n    \
             RETURN STRING(pd-value, \">>>,>>9.99\").\n\
             END FUNCTION."
        )
        .unwrap();
        Writer::variables(&mut out, &prefix, "");
        let scope = Scope {
            prefix: prefix.clone(),
            temp_tables,
            procedures: true,
            programs: &[],
            classes: &[],
            functions: Vec::new(),
        };
        w.blocks(&mut out, &scope, knobs.hot_lines, "");
        let id = add(
            &mut files,
            format!("inc/hot/{prefix}.i"),
            out,
            Vec::new(),
            false,
        );
        hot.push((id, format!("{prefix}-fmt")));
    }

    // Pool includes.
    let mut pool = Vec::new();
    for l in 0..knobs.lib_includes {
        let prefix = format!("l{l}");
        let mut out = format!("/* Pool include {l}. */\n");
        let temp_tables = w.include_tables(&mut out, &prefix, l);
        Writer::variables(&mut out, &prefix, "");
        let scope = Scope {
            prefix: prefix.clone(),
            temp_tables,
            procedures: true,
            programs: &[],
            classes: &[],
            functions: Vec::new(),
        };
        let lines = w.rng.log_between(knobs.lib_lines);
        w.blocks(&mut out, &scope, lines, "");
        pool.push(add(
            &mut files,
            format!("inc/lib/{prefix}.i"),
            out,
            Vec::new(),
            false,
        ));
    }

    // Chains, written from the deepest include up so each knows its child.
    let mut chain_heads = Vec::new();
    for c in 0..knobs.chains {
        let mut child: Option<usize> = None;
        for d in (0..knobs.chain_depth).rev() {
            let prefix = format!("c{c}d{d}");
            let mut out = format!("/* Chain {c}, depth {d}, argument \"{{1}}\". */\n");
            let temp_tables = w.include_tables(&mut out, &prefix, c * knobs.chain_depth + d);
            Writer::variables(&mut out, &prefix, "");
            writeln!(
                out,
                "&SCOPED-DEFINE {prefix}-label \"{{1}}\"\n{prefix}-text = {{&{prefix}-label}}."
            )
            .unwrap();
            let scope = Scope {
                prefix: prefix.clone(),
                temp_tables,
                procedures: true,
                programs: &programs,
                classes: &[],
                functions: Vec::new(),
            };
            w.blocks(&mut out, &scope, knobs.chain_lines, "");
            let mut includes = Vec::new();
            if let Some(child) = child {
                writeln!(out, "{{inc/chain/ch{c}/d{}.i \"level{}\"}}", d + 1, d + 1).unwrap();
                includes.push(child);
            }
            child = Some(add(
                &mut files,
                format!("inc/chain/ch{c}/d{d}.i"),
                out,
                includes,
                false,
            ));
        }
        chain_heads.extend(child);
    }

    // Programs.
    for (n, name) in programs.iter().enumerate() {
        let mut out =
            format!("/* Program {n}. */\nDEFINE INPUT PARAMETER pi-text AS CHARACTER NO-UNDO.\n");
        let mut includes = Vec::new();
        let mut functions = Vec::new();
        let mut share = knobs.hot_share;
        for (id, function) in &hot {
            if w.rng.unit() < share {
                writeln!(out, "{{{}}}", files[*id].path).unwrap();
                includes.push(*id);
                functions.push(function.clone());
            }
            share *= 0.8;
        }
        if n < knobs.chain_programs && !chain_heads.is_empty() {
            let head = chain_heads[n % chain_heads.len()];
            writeln!(out, "{{{} \"level0\"}}", files[head].path).unwrap();
            includes.push(head);
        }
        // The fan-out programs are spread over the list, not the first ones.
        let fan_out = knobs.fan_out_programs > 0
            && n % (knobs.programs / knobs.fan_out_programs).max(1) == 1
            && n / (knobs.programs / knobs.fan_out_programs).max(1) < knobs.fan_out_programs;
        let count = if fan_out {
            knobs.fan_out
        } else {
            w.rng.between(knobs.includes_per_program)
        }
        .min(pool.len());
        let start = w.rng.below(pool.len());
        for i in 0..count {
            let id = pool[(start + i) % pool.len()];
            writeln!(out, "{{{}}}", files[id].path).unwrap();
            includes.push(id);
        }
        let prefix = "r".to_string();
        Writer::variables(&mut out, &prefix, "");
        out.push_str("r-text = pi-text.\n");
        let temp_tables = vec![format!("tt-{prefix}-0")];
        Writer::temp_table(&mut out, &temp_tables[0], "");
        let scope = Scope {
            prefix,
            temp_tables,
            procedures: true,
            programs: &programs,
            classes: &classes,
            functions,
        };
        let lines = w.rng.log_between(knobs.root_lines);
        w.blocks(&mut out, &scope, lines, "");
        add(&mut files, format!("src/{name}"), out, includes, true);
    }

    // Classes: runs of classes in a package inherit from the one before.
    for (n, name) in classes.iter().enumerate() {
        let per = knobs.classes_per_package.max(1);
        let base = n % per == 0 || n % 4 == 0;
        let mut out = format!("/* Class {n}. */\nCLASS {name}");
        if !base {
            write!(out, " INHERITS {}", classes[n - 1]).unwrap();
        }
        out.push_str(":\n");
        writeln!(
            out,
            "    DEFINE PUBLIC PROPERTY Label{n} AS CHARACTER NO-UNDO GET. SET."
        )
        .unwrap();
        let tt = format!("tt-c{n}");
        Writer::temp_table(&mut out, &tt, "PRIVATE ");
        if base {
            writeln!(
                out,
                "    DEFINE PRIVATE VARIABLE mv-count{n} AS INTEGER NO-UNDO.\n    \
                 METHOD PUBLIC INTEGER Compute(INPUT pi-value AS INTEGER):\n        \
                 mv-count{n} = mv-count{n} + pi-value.\n        \
                 RETURN mv-count{n} * 2.\n    \
                 END METHOD."
            )
            .unwrap();
        }
        let lines = w.rng.log_between(knobs.root_lines);
        let mut written = 0;
        let mut m = 0;
        while written < lines {
            let start = out.len();
            writeln!(
                out,
                "    METHOD PUBLIC CHARACTER Step{m}(INPUT pi-value AS INTEGER):"
            )
            .unwrap();
            Writer::variables(&mut out, "m", "        ");
            out.push_str("        m-i = pi-value.\n");
            let scope = Scope {
                prefix: "m".into(),
                temp_tables: vec![tt.clone()],
                procedures: false,
                programs: &[],
                classes: &[],
                functions: Vec::new(),
            };
            w.blocks(&mut out, &scope, 12, "        ");
            out.push_str("        RETURN m-text.\n    END METHOD.\n");
            written += out[start..].bytes().filter(|&b| b == b'\n').count();
            m += 1;
        }
        out.push_str("END CLASS.\n");
        let path = format!("src/{}.cls", name.replace('.', "/"));
        add(&mut files, path, out, Vec::new(), true);
    }

    Workspace { files }
}

impl Workspace {
    /// The roots, in order.
    pub fn roots(&self) -> impl Iterator<Item = &GenFile> {
        self.files.iter().filter(|f| f.root)
    }

    /// The file at `path`, below the workspace directory.
    pub fn file(&self, path: &str) -> Option<&GenFile> {
        self.files.iter().find(|f| f.path == path)
    }

    /// Write every file below `dir`.
    pub fn write(&self, dir: &Path) -> io::Result<()> {
        for file in &self.files {
            let path = dir.join(&file.path);
            if let Some(parent) = path.parent() {
                std::fs::create_dir_all(parent)?;
            }
            std::fs::write(path, &file.text)?;
        }
        Ok(())
    }

    /// The absolute path of `file` below `dir`.
    pub fn path_in(dir: &Path, file: &GenFile) -> PathBuf {
        dir.join(&file.path)
    }

    /// The measured shape.
    pub fn stats(&self) -> Stats {
        let lines: Vec<usize> = self.files.iter().map(|f| f.text.lines().count()).collect();
        // Expanded lines and depth of each file, from its includes.
        let mut expanded: HashMap<usize, (usize, usize)> = HashMap::new();
        fn walk(
            w: &Workspace,
            lines: &[usize],
            memo: &mut HashMap<usize, (usize, usize)>,
            id: usize,
        ) -> (usize, usize) {
            if let Some(found) = memo.get(&id) {
                return *found;
            }
            let mut total = lines[id];
            let mut depth = 0;
            for &child in &w.files[id].includes {
                let (l, d) = walk(w, lines, memo, child);
                total += l;
                depth = depth.max(d + 1);
            }
            memo.insert(id, (total, depth));
            (total, depth)
        }
        let mut fan_in: HashMap<usize, usize> = HashMap::new();
        let mut stats = Stats {
            files: self.files.len(),
            bytes: self.files.iter().map(|f| f.text.len()).sum(),
            lines: lines.iter().sum(),
            ..Stats::default()
        };
        for (id, file) in self.files.iter().enumerate() {
            if !file.root {
                if file.path.ends_with(".i") {
                    stats.includes += 1;
                }
                continue;
            }
            stats.roots += 1;
            stats.max_fan_out = stats.max_fan_out.max(file.includes.len());
            let (total, depth) = walk(self, &lines, &mut expanded, id);
            stats.max_expanded_lines = stats.max_expanded_lines.max(total);
            stats.max_depth = stats.max_depth.max(depth);
            if total > 30_000 {
                stats.roots_over_30000_lines += 1;
            }
            let mut seen = Vec::new();
            let mut stack = file.includes.clone();
            while let Some(include) = stack.pop() {
                if !seen.contains(&include) {
                    seen.push(include);
                    stack.extend(&self.files[include].includes);
                }
            }
            for include in seen {
                *fan_in.entry(include).or_default() += 1;
            }
        }
        let mut counts: Vec<usize> = fan_in.into_values().collect();
        counts.sort_unstable_by(|a, b| b.cmp(a));
        stats.includes_over_1000_roots = counts.iter().filter(|&&c| c >= 1_000).count();
        counts.truncate(5);
        stats.top_fan_in = counts;
        stats
    }
}
