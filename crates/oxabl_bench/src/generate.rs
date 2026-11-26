//! A deterministic synthetic ABL workspace.
//!
//! The same [`Knobs`] always give the same files, byte for byte. Every file is
//! synthetic and parses with no error. The shape is set by the knobs: the
//! number of programs and classes, the size of each kind of file, the include
//! fan-in (a few "hot" includes that most programs read), the include fan-out
//! (programs that read dozens of includes, and programs that are nothing but
//! includes), nested includes, chains of nested includes, the density of
//! preprocessor directives, the mix of statements, the temp-tables and
//! datasets of the files, the `SHARED` producers and consumers, and a `.df`
//! schema.
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
//!
//! No root reads an include twice, through any depth, so no root defines a
//! name two times.

use std::collections::{BTreeSet, HashMap};
use std::fmt::Write;
use std::io;
use std::path::{Path, PathBuf};

/// A distribution of sizes or counts. One draw takes at most one number from
/// the random source, and [`Dist::Fixed`] takes none.
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Dist {
    /// Always this value.
    Fixed(usize),
    /// Evenly between the two values, both included.
    Between(usize, usize),
    /// On a log scale between the two values: most are small, a few are large.
    Log(usize, usize),
    /// Measured quantiles, with a log scale between two neighbours. The top
    /// 1% leans to its small end (the fourth power), so few values reach `max`.
    Quantiles {
        min: usize,
        p50: usize,
        p90: usize,
        p99: usize,
        max: usize,
    },
}

impl Dist {
    fn draw(self, rng: &mut Rng) -> usize {
        match self {
            Dist::Fixed(n) => n,
            Dist::Between(lo, hi) => lo + rng.below(hi.saturating_sub(lo) + 1),
            Dist::Log(lo, hi) => log_scale(lo, hi, rng.unit()),
            Dist::Quantiles {
                min,
                p50,
                p90,
                p99,
                max,
            } => {
                let u = rng.unit();
                let (lo, hi, t) = if u < 0.5 {
                    (min, p50, u / 0.5)
                } else if u < 0.9 {
                    (p50, p90, (u - 0.5) / 0.4)
                } else if u < 0.99 {
                    (p90, p99, (u - 0.9) / 0.09)
                } else {
                    (p99, max, ((u - 0.99) / 0.01).powi(4))
                };
                log_scale(lo, hi, t)
            }
        }
    }
}

/// `lo` to `hi` on a log scale, at `t` in `0..1`.
fn log_scale(lo: usize, hi: usize, t: f64) -> usize {
    let (lo, hi) = (lo.max(1) as f64, hi.max(lo.max(1)) as f64);
    (lo * (hi / lo).powf(t)).round() as usize
}

/// An include that many programs read.
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Hot {
    /// Lines of its own text, without the includes it reads.
    pub lines: usize,
    /// The share of programs that read it directly.
    pub share: f64,
}

impl Hot {
    /// `count` includes of `lines` lines. The first is read by the `share` of
    /// programs, each next one by 0.8 times the share of the one before.
    pub fn geometric(count: usize, share: f64, lines: usize) -> Vec<Hot> {
        let mut share = share;
        (0..count)
            .map(|_| {
                let hot = Hot { lines, share };
                share *= 0.8;
                hot
            })
            .collect()
    }
}

/// The weight of each kind of block of code in a file. A block is a few lines.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Mix {
    /// A `DO` loop.
    pub do_loop: u32,
    /// A `FOR EACH` and a `FIND` of a table of the schema.
    pub database: u32,
    /// A `FIND` of a table of the schema.
    pub find: u32,
    /// A `CREATE` and a `FOR EACH` of a temp-table.
    pub temp_table: u32,
    /// An internal procedure and a `RUN` of it.
    pub procedure: u32,
    /// A call of a function of a hot include.
    pub function: u32,
    /// A function of the file, and a call of it.
    pub function_def: u32,
    /// A `NEW` class.
    pub new_object: u32,
    /// A `RUN` of another program.
    pub run: u32,
    /// A `CASE`. Also the block that stands in for a kind that the file cannot
    /// hold.
    pub case: u32,
}

impl Mix {
    /// One block in nine of each kind, with the database block twice.
    pub const EVEN: Mix = Mix {
        do_loop: 1,
        database: 2,
        find: 0,
        temp_table: 1,
        procedure: 1,
        function: 1,
        function_def: 0,
        new_object: 1,
        run: 1,
        case: 1,
    };

    fn total(&self) -> u32 {
        self.do_loop
            + self.database
            + self.find
            + self.temp_table
            + self.procedure
            + self.function
            + self.function_def
            + self.new_object
            + self.run
            + self.case
    }

    /// The kind of block for the draw `r` below [`total`](Self::total).
    fn kind(&self, mut r: u32) -> Kind {
        for (weight, kind) in [
            (self.do_loop, Kind::DoLoop),
            (self.database, Kind::Database),
            (self.find, Kind::Find),
            (self.temp_table, Kind::TempTable),
            (self.procedure, Kind::Procedure),
            (self.function, Kind::Function),
            (self.function_def, Kind::FunctionDef),
            (self.new_object, Kind::NewObject),
            (self.run, Kind::Run),
        ] {
            if r < weight {
                return kind;
            }
            r -= weight;
        }
        Kind::Case
    }
}

#[derive(Clone, Copy)]
enum Kind {
    DoLoop,
    Database,
    Find,
    TempTable,
    Procedure,
    Function,
    FunctionDef,
    NewObject,
    Run,
    Case,
}

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
    /// Lines of a program or class.
    pub root_lines: Dist,
    /// Lines of code in one method of a class.
    pub method_lines: usize,
    /// Includes in the shared pool that programs read a few of.
    pub lib_includes: usize,
    /// Lines of a pool include.
    pub lib_lines: Dist,
    /// Pool includes that an ordinary program reads.
    pub includes_per_program: Dist,
    /// How the pool includes are picked. 0 reads a window of neighbours. From
    /// 1 on, a pick leans to the first includes by this power: the higher, the
    /// fewer includes carry the fan-in. Also the pick of the includes that an
    /// include reads.
    pub pool_skew: f64,
    /// Lines of a pool include that another include reads: the small headers
    /// that make the closure of a root large in files and not in lines.
    pub child_lines: Dist,
    /// Includes that many programs read, each read by its own share.
    pub hot: Vec<Hot>,
    /// How much the programs differ in how many hot includes they read, from 0
    /// (each hot include is read on its own chance) to 1. A program that reads
    /// more of them reads the big ones too, as a few real programs do. The
    /// share of a hot include over all programs does not change.
    pub hot_spread: f64,
    /// Pool includes that a hot include reads.
    pub hot_children: Dist,
    /// Pool includes that a pool include reads. They are earlier in the pool.
    pub pool_children: Dist,
    /// The most levels of nesting inside the pool: a pool include with no
    /// child has 0.
    pub nest_depth: usize,
    /// Programs that read [`fan_out`](Self::fan_out) pool includes each.
    pub fan_out_programs: usize,
    /// Pool includes of a fan-out program.
    pub fan_out: usize,
    /// The share of programs (the last ones) that are nothing but includes:
    /// no code of their own, and none of them is a `RUN` target.
    pub launcher_share: f64,
    /// Pool includes that a launcher reads, besides the hot ones.
    pub launcher_includes: Dist,
    /// Chains of nested includes.
    pub chains: usize,
    /// Includes in one chain: the depth of the nesting.
    pub chain_depth: usize,
    /// Lines of one include of a chain.
    pub chain_lines: usize,
    /// Programs that read the head of a chain.
    pub chain_programs: usize,
    /// One preprocessor block (`&SCOPED-DEFINE`, `&IF`, `{&name}`) for each
    /// this many blocks of code of a file. A file shorter than this has none.
    /// 0 for none.
    pub preprocessor_every: usize,
    /// One `{&name}` reference to a definition of a hot include for each this
    /// many blocks of code of a program that reads one. 0 for none.
    pub ref_every: usize,
    /// The share of classes with a `&SCOPED-DEFINE` and its `{&name}`.
    pub class_directive_share: f64,
    /// The weight of each kind of block of code.
    pub mix: Mix,
    /// Bytes of the comment at the end of the first line of each block of
    /// code, with its delimiters. 0 for none.
    pub comment_bytes: usize,
    /// How the `NEW` classes and the `RUN` targets are picked: 1 evenly, more
    /// leans to the first ones by this power.
    pub reference_skew: f64,
    /// Temp-tables of a file that has any.
    pub temp_tables: Dist,
    /// The share of files (programs, hot, pool and chain includes) that have
    /// temp-tables. Classes have one each.
    pub temp_table_share: f64,
    /// A dataset over the temp-tables of every this many includes. 0 for none.
    pub dataset_every: usize,
    /// Programs that define `NEW SHARED` variables and a temp-table: an upper
    /// bound, since a producer needs a runnable program of its own
    /// (`--stats` prints the count written).
    pub shared_producers: usize,
    /// `NEW SHARED` variables of a producer.
    pub shared_vars: usize,
    /// Programs that define `SHARED` what a producer defines: an upper bound,
    /// and 0 when there is no producer.
    pub shared_consumers: usize,
    /// Tables of the schema.
    pub tables: usize,
    /// Fields of each table, at least 2.
    pub fields: usize,
    /// Findings in each hot include: this many unused variables. Every root
    /// that reads the include reports them. 0 for none.
    pub include_findings: usize,
}

/// A named set of knobs.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Preset {
    /// About 300 small roots, for a CodSpeed bench.
    Ci,
    /// About a tenth of [`Preset::Erp`], for a quick local run.
    Medium,
    /// The shape of a large real ERP code base: about 14,800 files.
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
                root_lines: Dist::Log(20, 120),
                method_lines: 12,
                lib_includes: 60,
                lib_lines: Dist::Log(10, 40),
                includes_per_program: Dist::Between(0, 3),
                pool_skew: 0.0,
                child_lines: Dist::Fixed(0),
                hot: Hot::geometric(2, 0.9, 40),
                hot_spread: 0.0,
                hot_children: Dist::Fixed(0),
                pool_children: Dist::Fixed(0),
                nest_depth: 0,
                fan_out_programs: 2,
                fan_out: 30,
                launcher_share: 0.0,
                launcher_includes: Dist::Fixed(0),
                chains: 2,
                chain_depth: 4,
                chain_lines: 60,
                chain_programs: 10,
                preprocessor_every: 6,
                ref_every: 0,
                class_directive_share: 0.0,
                mix: Mix::EVEN,
                comment_bytes: 0,
                reference_skew: 1.0,
                temp_tables: Dist::Fixed(1),
                temp_table_share: 1.0,
                dataset_every: 5,
                shared_producers: 0,
                shared_vars: 0,
                shared_consumers: 0,
                tables: 40,
                fields: 8,
                include_findings: 0,
            },
            Preset::Medium => Knobs {
                programs: 820,
                classes: 220,
                lib_includes: 420,
                chains: 3,
                chain_programs: 20,
                shared_producers: 45,
                shared_consumers: 12,
                tables: 200,
                ..Preset::Erp.knobs()
            },
            // The numbers are measured on a large real workspace: file sizes,
            // include fan-in, fan-out and depth, the size of the closure of a
            // root, the directive and temp-table counts, the statement mix and
            // the SHARED producers. `docs/performance.md` compares them.
            Preset::Erp => Knobs {
                seed: 499,
                programs: 8_210,
                classes: 2_176,
                classes_per_package: 40,
                root_lines: Dist::Quantiles {
                    min: 5,
                    p50: 168,
                    p90: 700,
                    p99: 3_100,
                    max: 28_315,
                },
                method_lines: 85,
                lib_includes: 3_700,
                lib_lines: Dist::Quantiles {
                    min: 5,
                    p50: 166,
                    p90: 770,
                    p99: 3_300,
                    max: 28_366,
                },
                includes_per_program: Dist::Quantiles {
                    min: 1,
                    p50: 1,
                    p90: 4,
                    p99: 10,
                    max: 27,
                },
                pool_skew: 4.0,
                child_lines: Dist::Quantiles {
                    min: 3,
                    p50: 22,
                    p90: 100,
                    p99: 350,
                    max: 1_200,
                },
                hot: erp_hot(),
                hot_spread: 1.0,
                hot_children: Dist::Between(7, 14),
                pool_children: Dist::Between(0, 2),
                nest_depth: 2,
                fan_out_programs: 0,
                fan_out: 0,
                launcher_share: 0.03,
                launcher_includes: Dist::Quantiles {
                    min: 5,
                    p50: 12,
                    p90: 22,
                    p99: 32,
                    max: 40,
                },
                chains: 45,
                chain_depth: 6,
                chain_lines: 120,
                chain_programs: 150,
                preprocessor_every: 165,
                ref_every: 17,
                class_directive_share: 0.75,
                mix: Mix {
                    do_loop: 400,
                    database: 15,
                    find: 63,
                    temp_table: 30,
                    procedure: 35,
                    function: 30,
                    function_def: 7,
                    new_object: 11,
                    run: 44,
                    case: 365,
                },
                comment_bytes: 40,
                reference_skew: 4.0,
                temp_tables: Dist::Quantiles {
                    min: 1,
                    p50: 2,
                    p90: 7,
                    p99: 20,
                    max: 92,
                },
                temp_table_share: 0.32,
                dataset_every: 4,
                shared_producers: 447,
                shared_vars: 4,
                shared_consumers: 110,
                tables: 1_230,
                fields: 25,
                include_findings: 0,
            },
        }
    }
}

/// The hot includes of [`Preset::Erp`]: the sizes and the shares of programs
/// of the includes that most programs read, as measured.
fn erp_hot() -> Vec<Hot> {
    [
        (580, 0.66),
        (215, 0.56),
        (300, 0.56),
        (1_100, 0.22),
        (2_200, 0.20),
        (6_800, 0.016),
        (6_200, 0.011),
        (350, 0.19),
        (150, 0.18),
        (400, 0.17),
        (250, 0.12),
        (180, 0.12),
        (320, 0.11),
        (200, 0.105),
        (260, 0.08),
        (170, 0.065),
        (300, 0.06),
        (220, 0.05),
        (140, 0.05),
        (380, 0.05),
        (260, 0.05),
        (190, 0.05),
    ]
    .into_iter()
    .map(|(lines, share)| Hot { lines, share })
    .collect()
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

/// The mean, the 50th, 90th and 99th percentile and the largest of a list of
/// numbers.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub struct Spread {
    pub mean: usize,
    pub p50: usize,
    pub p90: usize,
    pub p99: usize,
    pub max: usize,
}

impl Spread {
    fn of(mut values: Vec<usize>) -> Spread {
        if values.is_empty() {
            return Spread::default();
        }
        values.sort_unstable();
        let at =
            |p: f64| values[((values.len() as f64 * p).ceil() as usize).clamp(1, values.len()) - 1];
        Spread {
            mean: values.iter().sum::<usize>() / values.len(),
            p50: at(0.5),
            p90: at(0.9),
            p99: at(0.99),
            max: values[values.len() - 1],
        }
    }
}

/// What the text of the files holds: occurrences of a statement or a
/// directive, as the first words of a line or a string.
#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct Counts {
    pub scoped_define: usize,
    pub global_define: usize,
    pub if_directive: usize,
    /// `{&name}` references.
    pub name_refs: usize,
    /// `{1}` arguments.
    pub arg_refs: usize,
    pub temp_tables: usize,
    pub files_with_temp_tables: usize,
    pub datasets: usize,
    pub classes: usize,
    pub inherits: usize,
    pub procedures: usize,
    pub functions: usize,
    pub methods: usize,
    pub find: usize,
    pub for_each: usize,
    pub new_object: usize,
    pub run: usize,
    pub new_shared: usize,
    /// Files with a `DEFINE NEW SHARED` line.
    pub producers: usize,
    pub define_shared: usize,
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
    /// Lines of every file with source text (not `oxabl.toml` or the schema).
    pub lines_per_file: Spread,
    pub root_lines: Spread,
    pub include_lines: Spread,
    /// Includes a root reads directly.
    pub direct_includes: Spread,
    /// Include files below a root, through any depth, each counted once.
    pub closure: Spread,
    /// Levels of nesting below a root.
    pub depth: Spread,
    /// Lines of a root with its includes expanded.
    pub expanded_lines: Spread,
    /// Files that include an include directly, for each include.
    pub direct_fan_in: Spread,
    /// The 20 largest direct fan-ins.
    pub top_direct_fan_in: Vec<usize>,
    /// Includes that no file includes.
    pub never_included: usize,
    /// Temp-tables of a file that has any.
    pub temp_tables_per_file: Spread,
    pub counts: Counts,
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

    /// An index below `n`. With `skew` above 1 it leans to the first ones.
    fn pick(&mut self, n: usize, skew: f64) -> usize {
        if skew <= 1.0 {
            self.below(n)
        } else {
            (((n as f64) * self.unit().powf(skew)) as usize).min(n.saturating_sub(1))
        }
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
    /// `&GLOBAL-DEFINE` names of the hot includes it reads.
    defines: Vec<String>,
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

    fn temp_table(out: &mut String, name: &str, access: &str, ind: &str) {
        writeln!(
            out,
            "{ind}DEFINE {access}TEMP-TABLE {name} NO-UNDO\n{ind}    \
             FIELD id AS INTEGER\n{ind}    \
             FIELD label AS CHARACTER\n{ind}    \
             FIELD amount AS DECIMAL\n{ind}    \
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
        let start = out.len();
        self.block_text(out, scope, k, ind);
        let n = self.knobs.comment_bytes;
        if n > 0
            && !out[start..].trim_start().starts_with('&')
            && let Some(end) = out[start..].find('\n')
        {
            out.insert_str(start + end, &comment(n, k));
        }
    }

    fn block_text(&mut self, out: &mut String, scope: &Scope<'_>, k: usize, ind: &str) {
        let p = &scope.prefix;
        let every = self.knobs.preprocessor_every;
        if every > 0 && k % every == every - 1 {
            writeln!(
                out,
                "{ind}&SCOPED-DEFINE {p}-step-{k} {k}\n\
                 {ind}&IF DEFINED(TRACE-ON) &THEN\n\
                 {ind}    MESSAGE \"trace\" {{&{p}-step-{k}}}.\n\
                 {ind}&ELSE\n\
                 {ind}    {p}-total = {p}-total + {{&{p}-step-{k}}}.\n\
                 {ind}&ENDIF"
            )
            .unwrap();
            return;
        }
        let refs = self.knobs.ref_every;
        if refs > 0 && !scope.defines.is_empty() && k % refs == refs - 1 {
            let d = &scope.defines[k % scope.defines.len()];
            writeln!(out, "{ind}{p}-text = {{&{d}}}.").unwrap();
            return;
        }
        let mix = self.knobs.mix;
        let kind = mix.kind(self.rng.below(mix.total() as usize) as u32);
        let skew = self.knobs.reference_skew;
        match kind {
            Kind::DoLoop => writeln!(
                out,
                "{ind}DO {p}-i = 1 TO {n}:\n\
                 {ind}    ASSIGN {p}-total = {p}-total + {p}-i * {k}\n\
                 {ind}           {p}-text = {p}-text + STRING({p}-i).\n\
                 {ind}END.",
                n = 2 + k % 7
            ),
            Kind::Database if self.knobs.tables > 0 => {
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
            Kind::Find if self.knobs.tables > 0 => {
                let t = self.table();
                writeln!(
                    out,
                    "{ind}FIND FIRST {t} WHERE {t}.fld01 = {k} NO-LOCK NO-ERROR.\n\
                     {ind}IF AVAILABLE {t} THEN {p}-text = {t}.fld02."
                )
            }
            Kind::TempTable if !scope.temp_tables.is_empty() => {
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
            Kind::Procedure if scope.procedures => writeln!(
                out,
                "PROCEDURE {p}-proc-{k}:\n    \
                 DEFINE INPUT PARAMETER pi-value AS INTEGER NO-UNDO.\n    \
                 DEFINE OUTPUT PARAMETER po-text AS CHARACTER NO-UNDO.\n    \
                 po-text = STRING(pi-value * 2).\n    \
                 IF pi-value > 10 THEN po-text = po-text + \"+\".\n\
                 END PROCEDURE.\n\
                 RUN {p}-proc-{k} (INPUT {k}, OUTPUT {p}-text)."
            ),
            Kind::Function if !scope.functions.is_empty() => {
                let f = &scope.functions[self.rng.below(scope.functions.len())];
                writeln!(out, "{ind}{p}-text = {f}({p}-total).")
            }
            Kind::FunctionDef if scope.procedures => writeln!(
                out,
                "FUNCTION {p}-fn-{k} RETURNS CHARACTER (INPUT pd-value AS DECIMAL):\n    \
                 RETURN STRING(pd-value, \">>>,>>9.99\").\n\
                 END FUNCTION.\n\
                 {p}-text = {p}-fn-{k}({p}-total)."
            ),
            Kind::NewObject if !scope.classes.is_empty() => {
                let c = &scope.classes[self.rng.pick(scope.classes.len(), skew)];
                writeln!(
                    out,
                    "{ind}DEFINE VARIABLE {p}-obj-{k} AS {c} NO-UNDO.\n\
                     {ind}{p}-obj-{k} = NEW {c}().\n\
                     {ind}{p}-total = {p}-total + {p}-obj-{k}:Compute({k}).\n\
                     {ind}DELETE OBJECT {p}-obj-{k}."
                )
            }
            Kind::Run if !scope.programs.is_empty() => {
                let r = &scope.programs[self.rng.pick(scope.programs.len(), skew)];
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

    /// The temp-tables of a file that may have some (and a dataset), with
    /// names from `prefix`. `ordinal` counts the includes.
    fn file_tables(
        &mut self,
        out: &mut String,
        prefix: &str,
        ordinal: Option<usize>,
    ) -> Vec<String> {
        let share = self.knobs.temp_table_share;
        let count = if share < 1.0 && self.rng.unit() >= share {
            0
        } else {
            self.knobs.temp_tables.draw(&mut self.rng)
        };
        let names: Vec<String> = (0..count).map(|t| format!("tt-{prefix}-{t}")).collect();
        for name in &names {
            Self::temp_table(out, name, "", "");
        }
        let every = self.knobs.dataset_every;
        if every > 0 && ordinal.is_some_and(|o| o.is_multiple_of(every)) && names.len() >= 2 {
            Self::dataset(out, &format!("ds-{prefix}"), &names[0], &names[1]);
        }
        names
    }
}

/// A trailing comment of about `bytes` bytes, with its delimiters.
fn comment(bytes: usize, k: usize) -> String {
    const WORDS: [&str; 12] = [
        "validate", "the", "order", "before", "release", "and", "post", "totals", "to", "ledger",
        "check", "credit",
    ];
    let mut text = String::from(" /*");
    let mut i = k;
    while text.len() + 3 + WORDS[i % WORDS.len()].len() < bytes {
        text.push(' ');
        text.push_str(WORDS[i % WORDS.len()]);
        i += 1;
    }
    text.push_str(" */");
    text
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

/// Which pool includes each pool include and each hot include reads. Nodes are
/// the pool indexes, then the hot ones after them. The closure of a node holds
/// the node and everything below it, so a root can read two includes only when
/// their closures do not meet.
#[derive(Default)]
struct Nesting {
    pool_children: Vec<Vec<usize>>,
    hot_children: Vec<Vec<usize>>,
    closure: Vec<BTreeSet<usize>>,
    /// Pool includes that an include reads.
    is_child: Vec<bool>,
}

impl Nesting {
    /// Drawn from its own random source, so the nesting does not move the
    /// draws of the files.
    fn plan(knobs: &Knobs) -> Nesting {
        let pool = knobs.lib_includes;
        let mut rng = Rng(knobs.seed ^ 0x6E65_7374);
        let skew = knobs.pool_skew.max(1.0);
        let mut plan = Nesting {
            pool_children: vec![Vec::new(); pool],
            hot_children: vec![Vec::new(); knobs.hot.len()],
            closure: Vec::with_capacity(pool + knobs.hot.len()),
            is_child: vec![false; pool],
        };
        let mut depth = vec![0usize; pool];
        for l in 0..pool {
            let mut closure = BTreeSet::from([l]);
            if l > 0 && knobs.nest_depth > 0 {
                for _ in 0..knobs.pool_children.draw(&mut rng) {
                    let c = rng.pick(l, skew);
                    if depth[c] < knobs.nest_depth && plan.closure[c].is_disjoint(&closure) {
                        closure.extend(&plan.closure[c]);
                        plan.pool_children[l].push(c);
                        plan.is_child[c] = true;
                        depth[l] = depth[l].max(depth[c] + 1);
                    }
                }
            }
            plan.closure.push(closure);
        }
        let mut hot_used = BTreeSet::new();
        for h in 0..knobs.hot.len() {
            let mut closure = BTreeSet::from([pool + h]);
            if pool > 0 {
                for _ in 0..knobs.hot_children.draw(&mut rng) {
                    let c = rng.pick(pool, skew);
                    if plan.closure[c].is_disjoint(&closure)
                        && plan.closure[c].is_disjoint(&hot_used)
                    {
                        closure.extend(&plan.closure[c]);
                        plan.hot_children[h].push(c);
                        plan.is_child[c] = true;
                    }
                }
            }
            hot_used.extend(closure.iter().filter(|&&n| n < pool));
            plan.closure.push(closure);
        }
        plan
    }
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

    let launchers = (knobs.programs as f64 * knobs.launcher_share) as usize;
    let programs: Vec<String> = (0..knobs.programs).map(program_name).collect();
    let runnable = &programs[..programs.len() - launchers];
    let classes: Vec<String> = (0..knobs.classes).map(|n| class_name(knobs, n)).collect();

    // The files come in this order: two fixed files, the hot includes, the
    // pool, then the chains. The ids of the hot and pool includes are known
    // before the files are, because the nesting reaches ahead.
    let pool_size = knobs.lib_includes;
    let hot_id = |h: usize| 2 + h;
    let pool_id = |l: usize| 2 + knobs.hot.len() + l;
    let nesting = Nesting::plan(knobs);
    let lib_path = |l: usize| format!("inc/lib/l{l}.i");

    // Hot includes: shared definitions and a function each.
    let mut hot = Vec::new();
    for (h, spec) in knobs.hot.iter().enumerate() {
        let prefix = format!("h{h}");
        let mut out = format!("/* Hot include {h}. */\n&GLOBAL-DEFINE {prefix}-app \"bench\"\n");
        let mut includes = Vec::new();
        for &c in &nesting.hot_children[h] {
            writeln!(out, "{{{}}}", lib_path(c)).unwrap();
            includes.push(pool_id(c));
        }
        let temp_tables = w.file_tables(&mut out, &prefix, Some(h));
        writeln!(
            out,
            "FUNCTION {prefix}-fmt RETURNS CHARACTER (INPUT pd-value AS DECIMAL):\n    \
             RETURN STRING(pd-value, \">>>,>>9.99\").\n\
             END FUNCTION."
        )
        .unwrap();
        Writer::variables(&mut out, &prefix, "");
        for u in 0..knobs.include_findings {
            writeln!(
                out,
                "DEFINE VARIABLE {prefix}-unused-{u} AS INTEGER NO-UNDO."
            )
            .unwrap();
        }
        let scope = Scope {
            prefix: prefix.clone(),
            temp_tables,
            procedures: true,
            programs: &[],
            classes: &[],
            functions: Vec::new(),
            defines: Vec::new(),
        };
        w.blocks(&mut out, &scope, spec.lines, "");
        let id = add(
            &mut files,
            format!("inc/hot/{prefix}.i"),
            out,
            includes,
            false,
        );
        assert_eq!(id, hot_id(h));
        hot.push((id, format!("{prefix}-fmt"), format!("{prefix}-app")));
    }

    // Pool includes.
    let mut pool = Vec::new();
    for l in 0..pool_size {
        let prefix = format!("l{l}");
        let mut out = format!("/* Pool include {l}. */\n");
        let mut includes = Vec::new();
        for &c in &nesting.pool_children[l] {
            writeln!(out, "{{{}}}", lib_path(c)).unwrap();
            includes.push(pool_id(c));
        }
        let temp_tables = w.file_tables(&mut out, &prefix, Some(l));
        Writer::variables(&mut out, &prefix, "");
        let scope = Scope {
            prefix: prefix.clone(),
            temp_tables,
            procedures: true,
            programs: &[],
            classes: &[],
            functions: Vec::new(),
            defines: Vec::new(),
        };
        let lines = if nesting.is_child[l] {
            knobs.child_lines
        } else {
            knobs.lib_lines
        }
        .draw(&mut w.rng);
        w.blocks(&mut out, &scope, lines, "");
        let id = add(&mut files, lib_path(l), out, includes, false);
        assert_eq!(id, pool_id(l));
        pool.push(id);
    }

    // Chains, written from the deepest include up so each knows its child.
    let mut chain_heads = Vec::new();
    for c in 0..knobs.chains {
        let mut child: Option<usize> = None;
        for d in (0..knobs.chain_depth).rev() {
            let prefix = format!("c{c}d{d}");
            let mut out = format!("/* Chain {c}, depth {d}, argument \"{{1}}\". */\n");
            let temp_tables = w.file_tables(&mut out, &prefix, Some(c * knobs.chain_depth + d));
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
                programs: runnable,
                classes: &[],
                functions: Vec::new(),
                defines: Vec::new(),
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
    let stride = |count: usize| (knobs.programs / count.max(1)).max(1);
    // The producers and consumers that fit: one per two runnable programs
    // at most, each kind on its own parity.
    let slots = runnable.len() / 2;
    let producers = knobs.shared_producers.min(slots);
    let consumers = if producers > 0 {
        knobs.shared_consumers.min(slots)
    } else {
        0
    };
    let producer_gap = (slots / producers.max(1)).max(1);
    let consumer_gap = (slots / consumers.max(1)).max(1);
    for (n, name) in programs.iter().enumerate() {
        let launcher = n >= runnable.len();
        let mut out = if launcher {
            format!("/* Launcher {n}. */\n")
        } else {
            format!("/* Program {n}. */\nDEFINE INPUT PARAMETER pi-text AS CHARACTER NO-UNDO.\n")
        };
        let mut includes = Vec::new();
        let mut functions = Vec::new();
        let mut defines = Vec::new();
        // Nodes of the includes read so far, with everything below them.
        let mut seen: BTreeSet<usize> = BTreeSet::new();
        // The appetite of this program for hot includes, 1 on average.
        let appetite = if knobs.hot_spread > 0.0 {
            1.0 - knobs.hot_spread + 2.0 * knobs.hot_spread * w.rng.unit()
        } else {
            1.0
        };
        for (h, (id, function, define)) in hot.iter().enumerate() {
            let node = pool_size + h;
            if w.rng.unit() < knobs.hot[h].share * appetite
                && nesting.closure[node].is_disjoint(&seen)
            {
                writeln!(out, "{{{}}}", files[*id].path).unwrap();
                seen.extend(&nesting.closure[node]);
                includes.push(*id);
                functions.push(function.clone());
                defines.push(define.clone());
            }
        }
        if n < knobs.chain_programs && !chain_heads.is_empty() {
            let head = chain_heads[n % chain_heads.len()];
            writeln!(out, "{{{} \"level0\"}}", files[head].path).unwrap();
            includes.push(head);
        }
        // The fan-out programs are spread over the list, not the first ones.
        let fan_out = !launcher
            && knobs.fan_out_programs > 0
            && n % stride(knobs.fan_out_programs) == 1
            && n / stride(knobs.fan_out_programs) < knobs.fan_out_programs;
        let count = if launcher {
            knobs.launcher_includes.draw(&mut w.rng)
        } else if fan_out {
            knobs.fan_out
        } else {
            knobs.includes_per_program.draw(&mut w.rng)
        }
        .min(pool.len());
        if knobs.pool_skew > 0.0 {
            for _ in 0..count {
                let l = w.rng.pick(pool.len(), knobs.pool_skew);
                if nesting.closure[l].is_disjoint(&seen) {
                    writeln!(out, "{{{}}}", lib_path(l)).unwrap();
                    seen.extend(&nesting.closure[l]);
                    includes.push(pool[l]);
                }
            }
        } else {
            let start = w.rng.below(pool.len());
            for i in 0..count {
                let id = pool[(start + i) % pool.len()];
                writeln!(out, "{{{}}}", files[id].path).unwrap();
                includes.push(id);
            }
        }
        if launcher {
            add(&mut files, format!("src/{name}"), out, includes, true);
            continue;
        }
        let prefix = "r".to_string();
        Writer::variables(&mut out, &prefix, "");
        out.push_str("r-text = pi-text.\n");
        let temp_tables = {
            let mut tt = String::new();
            let names = w.file_tables(&mut tt, &prefix, None);
            out.push_str(&tt);
            names
        };
        // Producers sit on the even runnable programs and consumers on the
        // odd ones, spread over the list, so none shares a program and
        // every consumer set has a producer.
        let producer = n % 2 == 0 && n / 2 % producer_gap == 0 && n / 2 / producer_gap < producers;
        let consumer = n % 2 == 1
            && producers > 0
            && n / 2 % consumer_gap == 0
            && n / 2 / consumer_gap < consumers;
        if producer || consumer {
            let set = if producer {
                n / 2 / producer_gap
            } else {
                n / 2 / consumer_gap % producers
            };
            let access = if producer { "NEW SHARED" } else { "SHARED" };
            for v in 0..knobs.shared_vars {
                writeln!(
                    out,
                    "DEFINE {access} VARIABLE sh{set}-v{v} AS INTEGER NO-UNDO."
                )
                .unwrap();
            }
            Writer::temp_table(&mut out, &format!("tt-sh{set}"), &format!("{access} "), "");
            let used: Vec<String> = (0..knobs.shared_vars)
                .map(|v| format!("sh{set}-v{v}"))
                .collect();
            writeln!(out, "MESSAGE {}.", used.join(" ")).unwrap();
        }
        let scope = Scope {
            prefix,
            temp_tables,
            procedures: true,
            programs: runnable,
            classes: &classes,
            functions,
            defines,
        };
        let lines = knobs.root_lines.draw(&mut w.rng);
        w.blocks(&mut out, &scope, lines, "");
        add(&mut files, format!("src/{name}"), out, includes, true);
    }

    // Classes: runs of classes in a package inherit from the one before.
    for (n, name) in classes.iter().enumerate() {
        let per = knobs.classes_per_package.max(1);
        let base = n % per == 0 || n % 4 == 0;
        let directive =
            knobs.class_directive_share > 0.0 && w.rng.unit() < knobs.class_directive_share;
        let mut out = format!("/* Class {n}. */\n");
        if directive {
            writeln!(out, "&SCOPED-DEFINE cv{n} {n}").unwrap();
        }
        write!(out, "CLASS {name}").unwrap();
        if !base {
            write!(out, " INHERITS {}", classes[n - 1]).unwrap();
        }
        out.push_str(":\n");
        writeln!(
            out,
            "    DEFINE PUBLIC PROPERTY Label{n} AS CHARACTER NO-UNDO GET. SET."
        )
        .unwrap();
        if directive {
            writeln!(
                out,
                "    DEFINE PRIVATE VARIABLE mv-ver{n} AS INTEGER NO-UNDO INITIAL {{&cv{n}}}.\n    \
                 METHOD PUBLIC INTEGER Version{n}():\n        \
                 RETURN mv-ver{n}.\n    \
                 END METHOD."
            )
            .unwrap();
        }
        let tt = format!("tt-c{n}");
        Writer::temp_table(&mut out, &tt, "PRIVATE ", "    ");
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
        let lines = knobs.root_lines.draw(&mut w.rng);
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
                defines: Vec::new(),
            };
            w.blocks(&mut out, &scope, knobs.method_lines, "        ");
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

    /// The include files below each file, through any depth, and the nesting
    /// depth of each file. A file's includes are never itself.
    fn closures(&self) -> (Vec<BTreeSet<usize>>, Vec<usize>) {
        fn walk(w: &Workspace, memo: &mut Vec<Option<(BTreeSet<usize>, usize)>>, id: usize) {
            if memo[id].is_some() {
                return;
            }
            let mut closure = BTreeSet::new();
            let mut depth = 0;
            for &child in &w.files[id].includes {
                walk(w, memo, child);
                let (below, d) = memo[child].as_ref().expect("walked");
                closure.insert(child);
                closure.extend(below);
                depth = depth.max(d + 1);
            }
            memo[id] = Some((closure, depth));
        }
        let mut memo = vec![None; self.files.len()];
        for id in 0..self.files.len() {
            walk(self, &mut memo, id);
        }
        memo.into_iter().map(|m| m.expect("walked")).unzip()
    }

    /// The measured shape.
    pub fn stats(&self) -> Stats {
        let lines: Vec<usize> = self.files.iter().map(|f| f.text.lines().count()).collect();
        let (closures, depths) = self.closures();
        let is_include = |f: &GenFile| !f.root && f.path.ends_with(".i");
        let mut fan_in: HashMap<usize, usize> = HashMap::new();
        let mut direct: HashMap<usize, usize> = HashMap::new();
        let mut stats = Stats {
            files: self.files.len(),
            bytes: self.files.iter().map(|f| f.text.len()).sum(),
            lines: lines.iter().sum(),
            ..Stats::default()
        };
        let (mut root_lines, mut include_lines, mut source_lines) = (vec![], vec![], vec![]);
        let (mut direct_includes, mut closure_sizes, mut depth_list, mut expanded) =
            (vec![], vec![], vec![], vec![]);
        for (id, file) in self.files.iter().enumerate() {
            if file.root || is_include(file) {
                source_lines.push(lines[id]);
            }
            for &child in &file.includes {
                *direct.entry(child).or_default() += 1;
            }
            if !file.root {
                if is_include(file) {
                    stats.includes += 1;
                    include_lines.push(lines[id]);
                }
                continue;
            }
            stats.roots += 1;
            root_lines.push(lines[id]);
            stats.max_fan_out = stats.max_fan_out.max(file.includes.len());
            direct_includes.push(file.includes.len());
            closure_sizes.push(closures[id].len());
            depth_list.push(depths[id]);
            stats.max_depth = stats.max_depth.max(depths[id]);
            let total = lines[id] + closures[id].iter().map(|&c| lines[c]).sum::<usize>();
            expanded.push(total);
            stats.max_expanded_lines = stats.max_expanded_lines.max(total);
            if total > 30_000 {
                stats.roots_over_30000_lines += 1;
            }
            for &include in &closures[id] {
                *fan_in.entry(include).or_default() += 1;
            }
        }
        let mut counts: Vec<usize> = fan_in.into_values().collect();
        counts.sort_unstable_by(|a, b| b.cmp(a));
        stats.includes_over_1000_roots = counts.iter().filter(|&&c| c >= 1_000).count();
        counts.truncate(5);
        stats.top_fan_in = counts;

        let mut includer_counts = Vec::new();
        for (id, file) in self.files.iter().enumerate() {
            if is_include(file) {
                includer_counts.push(direct.get(&id).copied().unwrap_or(0));
            }
        }
        stats.never_included = includer_counts.iter().filter(|&&c| c == 0).count();
        let mut top = includer_counts.clone();
        top.sort_unstable_by(|a, b| b.cmp(a));
        top.truncate(20);
        stats.top_direct_fan_in = top;
        stats.direct_fan_in = Spread::of(includer_counts);
        stats.lines_per_file = Spread::of(source_lines);
        stats.root_lines = Spread::of(root_lines);
        stats.include_lines = Spread::of(include_lines);
        stats.direct_includes = Spread::of(direct_includes);
        stats.closure = Spread::of(closure_sizes);
        stats.depth = Spread::of(depth_list);
        stats.expanded_lines = Spread::of(expanded);
        let mut per_file = Vec::new();
        stats.counts = self.counts(&mut per_file);
        stats.temp_tables_per_file = Spread::of(per_file);
        stats
    }

    /// The statements and directives in the text of the source files. The
    /// temp-tables of each file that has any go to `per_file`.
    fn counts(&self, per_file: &mut Vec<usize>) -> Counts {
        let mut c = Counts::default();
        for file in &self.files {
            if !file.root && !file.path.ends_with(".i") {
                continue;
            }
            let mut tables = 0;
            let new_shared = c.new_shared;
            for line in file.text.lines() {
                let t = line.trim_start();
                let words = |prefix: &str| t.starts_with(prefix) as usize;
                c.scoped_define += words("&SCOPED-DEFINE");
                c.global_define += words("&GLOBAL-DEFINE");
                c.if_directive += words("&IF");
                c.name_refs += line.matches("{&").count();
                c.arg_refs += line.matches("{1}").count();
                if t.starts_with("DEFINE ") && t.contains(" TEMP-TABLE ") {
                    tables += 1;
                }
                c.datasets += words("DEFINE DATASET");
                c.classes += words("CLASS ");
                c.inherits += t.contains(" INHERITS ") as usize;
                c.procedures += words("PROCEDURE ");
                c.functions += words("FUNCTION ");
                c.methods += words("METHOD ");
                c.find += words("FIND ");
                c.for_each += words("FOR EACH ");
                c.new_object += line.matches("= NEW ").count();
                c.run += words("RUN ");
                c.new_shared += (t.starts_with("DEFINE NEW SHARED")) as usize;
                c.define_shared += words("DEFINE SHARED");
            }
            c.producers += (c.new_shared > new_shared) as usize;
            c.temp_tables += tables;
            if tables > 0 {
                c.files_with_temp_tables += 1;
                per_file.push(tables);
            }
        }
        c
    }
}
