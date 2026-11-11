import { mkdirSync, readFileSync, readdirSync, statSync, writeFileSync } from "node:fs";
import { createRequire } from "node:module";
import { dirname, extname, join, relative, resolve, sep } from "node:path";
import { fileURLToPath } from "node:url";
import { beforeAll, describe, expect, it } from "vitest";
import * as vsctm from "vscode-textmate";
import * as onig from "vscode-oniguruma";

// Fuzzes the TextMate grammar for slow lines and for a rule stack that grows without bound.
//
//   OXABL_FUZZ_SEED     seed of the random generators (default 1); a failure prints it
//   OXABL_FUZZ_SECONDS  time box of each random phase (default 2)
//   OXABL_FUZZ_SCALE    largest size of the pathological shapes (default 10000)
//   OXABL_FUZZ_OUT      directory that receives the worst inputs as files
//   OXABL_FUZZ_DEEP    1 runs the shapes that grow the rule stack at the full scale too; they
//                       cost quadratic time there (see the report of the run), so they fail
//   OXABL_FUZZ_LINE_CAP longest line fed to the grammar (default 20000, the editor's
//                       `editor.maxTokenizationLineLength`); 0 feeds every line whole
//
// Replay a failure with `OXABL_FUZZ_SEED=<seed> pnpm vitest run test/grammar/fuzz.test.ts`.

const here = dirname(fileURLToPath(import.meta.url));
const repoRoot = resolve(here, "../../../..");
const grammarPath = process.env.OXABL_GRAMMAR ?? resolve(here, "../../syntaxes/abl.tmLanguage.json");

const SEED = Number(process.env.OXABL_FUZZ_SEED ?? 1);
const SECONDS = Number(process.env.OXABL_FUZZ_SECONDS ?? 2);
const SCALE = Number(process.env.OXABL_FUZZ_SCALE ?? 10000);
const OUT = process.env.OXABL_FUZZ_OUT;
const DEEP = process.env.OXABL_FUZZ_DEEP === "1";
const LINE_CAP = Number(process.env.OXABL_FUZZ_LINE_CAP ?? 20000);

// The time budgets hold on a machine that runs the calibration in CALIBRATION_MS or less; a
// slower machine (a CI runner) gets budgets larger by the same factor. The calibration does not
// use the grammar, so a slow grammar cannot raise its own budget. A grammar that is slow on
// every line is the corpus test's to catch: its budget is absolute.
const CALIBRATION_MS = 2.5;
let speed = 1;

// A line of n characters may take this long. The corpus test holds 50 ms for real lines.
const lineBudgetMs = (chars: number) => (50 + chars * 0.02) * speed;
const fileBudgetMs = () => 2000 * speed;
// Deepest rule stack any input may leave behind. Real files stay far below it.
const DEPTH_BUDGET = 2000;

const EXTENSIONS = new Set([".p", ".w", ".cls", ".i", ".v", ".abl"]);
const SKIP_DIRS = new Set(["target", "node_modules"]);

function corpusFiles(): string[] {
  const found: string[] = [];
  const walk = (dir: string, inTests: boolean) => {
    for (const name of readdirSync(dir)) {
      if (SKIP_DIRS.has(name)) continue;
      const path = join(dir, name);
      if (statSync(path).isDirectory()) {
        walk(path, inTests || name === "tests");
      } else if (inTests && EXTENSIONS.has(extname(name).toLowerCase())) {
        found.push(path);
      }
    }
  };
  walk(join(repoRoot, "resources"), true);
  const crates = join(repoRoot, "crates");
  for (const name of readdirSync(crates)) {
    const path = join(crates, name);
    if (statSync(path).isDirectory()) walk(path, false);
  }
  return found.sort();
}

// The best of 20 rounds of a fixed scan: the engine's speed on this machine, not its load.
function calibrationMs(): number {
  const text = 'ASSIGN cName = "abc":U + STRING(iCount, ">>9") cNote = SUBSTITUTE("&1", x). '.repeat(250);
  const scanner = new onig.OnigScanner(["[A-Za-z_][A-Za-z0-9_-]*", '"(?:[^"~]|~.)*"', "\\s+", "\\d+(?:\\.\\d+)?", "."]);
  const str = new onig.OnigString(text);
  let best = Infinity;
  for (let round = 0; round < 20; round++) {
    const start = performance.now();
    for (let at = 0; at < text.length; ) {
      const match = scanner.findNextMatchSync(str, at);
      at = match ? Math.max(match.captureIndices[0].end, at + 1) : text.length;
    }
    best = Math.min(best, performance.now() - start);
  }
  str.dispose();
  scanner.dispose();
  return best;
}

// Small, fast, seedable generator.
function rng(seed: number) {
  let a = seed >>> 0;
  const next = () => {
    a = (a + 0x6d2b79f5) >>> 0;
    let t = a;
    t = Math.imul(t ^ (t >>> 15), t | 1);
    t ^= t + Math.imul(t ^ (t >>> 7), t | 61);
    return ((t ^ (t >>> 14)) >>> 0) / 4294967296;
  };
  return {
    next,
    int: (n: number) => Math.floor(next() * n),
    pick: <T>(items: readonly T[]): T => items[Math.floor(next() * items.length)],
  };
}
type Rng = ReturnType<typeof rng>;

let grammar: vsctm.IGrammar;
beforeAll(async () => {
  const wasm = readFileSync(createRequire(import.meta.url).resolve("vscode-oniguruma/release/onig.wasm"));
  await onig.loadWASM(wasm.buffer.slice(wasm.byteOffset, wasm.byteOffset + wasm.byteLength));
  const registry = new vsctm.Registry({
    onigLib: Promise.resolve({
      createOnigScanner: (sources: string[]) => new onig.OnigScanner(sources),
      createOnigString: (s: string) => new onig.OnigString(s),
    }),
    loadGrammar: async (scope) =>
      scope === "source.abl" ? vsctm.parseRawGrammar(readFileSync(grammarPath, "utf8"), grammarPath) : null,
  });
  const loaded = await registry.loadGrammar("source.abl");
  if (!loaded) throw new Error(`cannot load ${grammarPath}`);
  grammar = loaded;
  const calibration = calibrationMs();
  speed = Math.max(1, calibration / CALIBRATION_MS);
  console.log(`calibration ${calibration.toFixed(2)} ms: budgets x${speed.toFixed(2)}`);
  // The engine compiles its scanners on first use; time the steady state.
  let warm = vsctm.INITIAL;
  for (const line of ["/* c */ DEFINE VARIABLE a AS INTEGER NO-UNDO.", "&SCOPED-DEFINE x 1", 'x = "s":U + {&x}.']) {
    warm = grammar.tokenizeLine(line, warm).ruleStack;
  }
});

interface Run {
  totalMs: number;
  worstLineMs: number;
  worstLine: number;
  worstLineChars: number;
  maxDepth: number;
  lines: number;
}

function tokenize(text: string): Run {
  const lines = text.split(/\r?\n/);
  let stack = vsctm.INITIAL;
  const run: Run = { totalMs: 0, worstLineMs: 0, worstLine: 0, worstLineChars: 0, maxDepth: 0, lines: lines.length };
  for (let n = 0; n < lines.length; n++) {
    const line = LINE_CAP > 0 ? lines[n].slice(0, LINE_CAP) : lines[n];
    const start = performance.now();
    stack = grammar.tokenizeLine(line, stack).ruleStack;
    const ms = performance.now() - start;
    run.totalMs += ms;
    run.maxDepth = Math.max(run.maxDepth, stack.depth);
    if (ms > run.worstLineMs) {
      run.worstLineMs = ms;
      run.worstLine = n + 1;
      run.worstLineChars = line.length;
    }
  }
  return run;
}

interface Finding {
  name: string;
  run: Run;
  input: string;
}
const findings: Finding[] = [];
let slowest: Finding | undefined;
let deepest: Finding | undefined;
let inputsRun = 0;

// Runs one input, records it, and returns the failures it causes. A slow run is repeated
// twice: a scheduling pause or a garbage collection is not a slow grammar, so the fastest
// of the three counts.
function check(name: string, input: string, runs?: Run[]): string[] {
  let run = tokenize(input);
  for (let retry = 0; retry < 2 && run.worstLineMs > lineBudgetMs(run.worstLineChars); retry++) {
    const again = tokenize(input);
    if (again.worstLineMs < run.worstLineMs) run = again;
  }
  inputsRun++;
  runs?.push(run);
  const finding = { name, run, input };
  if (!slowest || run.worstLineMs > slowest.run.worstLineMs) slowest = finding;
  if (!deepest || run.maxDepth > deepest.run.maxDepth) deepest = finding;
  const failures: string[] = [];
  const budget = lineBudgetMs(run.worstLineChars);
  if (run.worstLineMs > budget) {
    failures.push(
      `${name}: line ${run.worstLine} (${run.worstLineChars} chars) took ${run.worstLineMs.toFixed(1)} ms, budget ${budget.toFixed(0)} ms`,
    );
  }
  if (run.totalMs > fileBudgetMs()) {
    failures.push(`${name}: input took ${run.totalMs.toFixed(0)} ms, budget ${fileBudgetMs().toFixed(0)} ms`);
  }
  if (run.maxDepth > DEPTH_BUDGET) failures.push(`${name}: rule stack reached depth ${run.maxDepth}, budget ${DEPTH_BUDGET}`);
  if (failures.length) findings.push(finding);
  return failures;
}

function save(label: string, f: Finding | undefined) {
  if (!OUT || !f) return;
  mkdirSync(OUT, { recursive: true });
  writeFileSync(join(OUT, `${label}.txt`), f.input);
}

function summary(label: string) {
  const s = slowest;
  const d = deepest;
  console.log(
    `${label}: seed ${SEED}, ${inputsRun} inputs; ` +
      `slowest line ${s?.run.worstLineMs.toFixed(2)} ms (${s?.run.worstLineChars} chars) in ${s?.name}; ` +
      `deepest stack ${d?.run.maxDepth} in ${d?.name}`,
  );
  save(`${label}-slowest`, s);
  save(`${label}-deepest`, d);
  findings.forEach((f, i) => save(`${label}-finding-${i}`, f));
  slowest = deepest = undefined;
  inputsRun = 0;
  findings.length = 0;
}

// Pathological shapes. Each takes a size n and returns a whole input.
const rep = (s: string, n: number) => s.repeat(n);
const shapes: Record<string, (n: number) => string> = {
  "open comments, one line": (n) => rep("/*", n),
  "open comments, one per line": (n) => rep("/*\n", n),
  "nested comments with text": (n) => rep("/* a ", n) + rep(" */", n),
  "closed comments, one line": (n) => rep("/* a */ ", n),
  "unbalanced double quote": (n) => `x = "${rep("a", n)}`,
  "unbalanced single quote": (n) => `x = '${rep("a", n)}`,
  "unbalanced quotes, one per line": (n) => rep('"a\n', n),
  "quotes alternating": (n) => rep(`"'`, n),
  "long run of tildes": (n) => rep("~", n),
  "tildes before quote": (n) => `x = "${rep("~", n)}"`,
  "tilde quote pairs": (n) => `x = "${rep('~"', n)}`,
  "open include refs": (n) => rep("{", n),
  "open preprocessor refs": (n) => rep("{&", n),
  "preprocessor refs, closed": (n) => rep("{&a}", n),
  "nested include refs": (n) => rep("{a ", n) + rep("}", n),
  "nested preprocessor refs": (n) => rep("{&a ", n) + rep("}", n),
  "nested refs, one per line": (n) => rep("{&a\n", n),
  "&IF run": (n) => rep("&IF ", n),
  "&IF nest, one per line": (n) => rep("&IF a &THEN\n", n),
  "&IF nest with ELSE": (n) => rep("&IF a &THEN\n&ELSE\n", n) + rep("&ENDIF\n", n),
  "&SCOPED-DEFINE run": (n) => rep("&SCOPED-DEFINE a ", n),
  "hyphen run": (n) => rep("-", n),
  "hyphenated name": (n) => rep("a-", n) + "b",
  "keyword hyphen chain": (n) => rep("no-undo-", n),
  "colon run": (n) => rep(":", n),
  "colon names": (n) => rep("a:", n),
  "period run": (n) => rep(".", n),
  "period names": (n) => rep("a.", n),
  "slash star run": (n) => rep("/", n) + rep("*", n),
  "star slash run": (n) => rep("*/", n),
  "block comment lines": (n) => "/*\n" + rep("a\n", n),
  "dots and colons": (n) => rep(".:", n),
  "digits and dots": (n) => rep("1.", n),
  "long numeric": (n) => rep("1", n),
  "long identifier": (n) => rep("a", n),
  "long spaces": (n) => `x${rep(" ", n)}y`,
  "long whitespace tail": (n) => `x = 1.${rep(" ", n)}`,
  "long tabs": (n) => rep("\t", n),
  "long line of operators": (n) => rep("+ - * / < > = <> >= <= ", n),
  "parentheses": (n) => rep("(", n) + rep(")", n),
  "parentheses, one per line": (n) => rep("(\n", n),
  "brackets": (n) => rep("[", n),
  "block nesting": (n) => rep("DO:\n", n) + rep("END.\n", n),
  "block nesting, one line": (n) => rep("DO: ", n) + rep("END. ", n),
  "block nesting, unclosed": (n) => rep("IF a THEN DO:\n", n),
  "procedure nesting": (n) => rep("PROCEDURE p:\n", n),
  "function nesting": (n) => rep("FUNCTION f RETURNS INTEGER ():\n", n),
  "method nesting": (n) => rep("METHOD PUBLIC VOID m():\n", n),
  "class nesting": (n) => rep("CLASS c:\n", n),
  "define without end": (n) => rep("DEFINE VARIABLE a AS INTEGER ", n),
  "define chain": (n) => "DEFINE " + rep("VARIABLE ", n),
  "for each nest": (n) => rep("FOR EACH a:\n", n),
  "string with colon suffix": (n) => rep('"a":U ', n),
  "unicode": (n) => rep("é€𝄞", n),
  "nul and control": (n) => rep("\u0000\u0001\u0007", n),
  "carriage returns": (n) => rep("a\r", n),
};

// Shapes whose rule stack grows with n. A real file nests a few levels; the default run
// stops them at DEEP_SIZE.
const DEEP_SIZE = 1000;
const deepShapes = new Set([
  "open comments, one line",
  "open comments, one per line",
  "nested comments with text",
  "open include refs",
  "nested include refs",
  "star slash run",
]);

// Sizes in steps of ten up to the scale, to expose super-linear growth.
function sizes(name: string): number[] {
  const top = deepShapes.has(name) && !DEEP ? Math.min(SCALE, DEEP_SIZE) : SCALE;
  const out: number[] = [];
  for (let n = 100; n < top; n *= 10) out.push(n);
  out.push(top);
  return out;
}

describe("pathological shapes", () => {
  it("stay inside the line, file and depth budgets", () => {
    const failures: string[] = [];
    const growth: string[] = [];
    for (const [name, make] of Object.entries(shapes)) {
      const runs: Run[] = [];
      for (const n of sizes(name)) failures.push(...check(`${name} x${n}`, make(n), runs));
      // Ten times the input may cost at most about thirty times the time.
      const superLinear = runs.some((run, i) => i > 0 && run.totalMs > 30 * Math.max(runs[i - 1].totalMs, 1));
      growth.push(
        `${name}: depth ${runs[runs.length - 1].maxDepth}; ${runs.map((run) => run.totalMs.toFixed(1)).join(" / ")} ms for ${sizes(name).join(" / ")}${superLinear ? "  <-- super-linear" : ""}`,
      );
    }
    console.log(`shapes (total ms by size):\n  ${growth.join("\n  ")}`);
    summary("shapes");
    expect(failures, failures.join("\n")).toEqual([]);
  }, 600_000);
});

const WORDS = [
  "DEFINE", "VARIABLE", "AS", "INTEGER", "CHARACTER", "NO-UNDO", "FOR", "EACH", "FIND", "FIRST", "WHERE", "NO-LOCK",
  "IF", "THEN", "ELSE", "DO", ":", "END", ".", "PROCEDURE", "FUNCTION", "RETURNS", "METHOD", "CLASS", "INTERFACE",
  "USING", "RETURN", "MESSAGE", "VIEW-AS", "ALERT-BOX", "TEMP-TABLE", "FIELD", "INDEX", "&IF", "&THEN", "&ELSE",
  "&ENDIF", "&SCOPED-DEFINE", "&GLOBAL-DEFINE", "{&x}", "{inc.i", "}", "/*", "*/", "//", "~", "~\"", "\"", "'", '":U',
  "(", ")", "[", "]", ",", "=", "<>", "+", "-", "*", "/", "@", "#", "$", "%", "\\", "|", "?", "!", "0x1F", "1.5e3",
  "a.b", "a:b", "a-b", "NEW", "VOID", "PUBLIC", "STATIC", "DATASET", "BUFFER", "HANDLE", "LIKE", "EXTENT", "INITIAL",
  "ON", "ERROR", "UNDO", "RETRY", "CATCH", "FINALLY", "THROW", "REPEAT", "WHILE", "CASE", "WHEN", "OTHERWISE",
  "{1}", "{*}", "{&*}", "&ANALYZE-SUSPEND", "&ANALYZE-RESUME", "&UNDEFINE", "&MESSAGE",
];

function corpusTokens(files: string[]): string[] {
  const tokens = new Set<string>(WORDS);
  for (const file of files) {
    for (const t of readFileSync(file, "latin1").match(/[A-Za-z0-9_&{}-]+|\S/g) ?? []) {
      if (t.length <= 40) tokens.add(t);
    }
  }
  return [...tokens];
}

const SEPARATORS = [" ", " ", " ", "\n", "\n", "", "\t", ". ", ".\n", ": ", ":\n"];

function randomTokens(r: Rng, tokens: string[]): string {
  const count = 1 + r.int(400);
  let out = "";
  for (let i = 0; i < count; i++) out += r.pick(tokens) + r.pick(SEPARATORS);
  return out;
}

const NOISE = ["/*", "*/", "~", '"', "'", "{", "}", "{&", "&IF", "-", ":", ".", "\n", " ", "\t", "(", ")", "\u0000", "é"];

function mutate(r: Rng, text: string): string {
  const rounds = 1 + r.int(20);
  let t = text;
  for (let i = 0; i < rounds && t.length > 0; i++) {
    const at = r.int(t.length);
    switch (r.int(7)) {
      case 0:
        t = t.slice(0, at) + r.pick(NOISE) + t.slice(at);
        break;
      case 1:
        t = t.slice(0, at) + t.slice(at + 1 + r.int(20));
        break;
      case 2: {
        const len = 1 + r.int(200);
        t = t.slice(0, at) + t.slice(at, at + len).repeat(2 + r.int(10)) + t.slice(at + len);
        break;
      }
      case 3:
        t = t.slice(0, at) + r.pick(NOISE).repeat(1 + r.int(20)) + t.slice(at);
        break;
      case 4: {
        const other = r.int(t.length);
        const len = 1 + r.int(300);
        t = t.slice(0, at) + t.slice(other, other + len) + t.slice(at);
        break;
      }
      case 5:
        // Join lines, so a long line carries the constructs of many.
        t = t.replace(/\n/g, r.pick([" ", "", "\t"]));
        break;
      default:
        t = t.slice(0, at) + r.pick(NOISE) + t.slice(at + 1);
    }
  }
  return t;
}

function timeBox(seconds: number, body: () => void) {
  const end = performance.now() + seconds * 1000;
  while (performance.now() < end) body();
}

describe("random inputs", () => {
  const files = corpusFiles();
  const tokens = corpusTokens(files);
  const texts = files.map((f) => readFileSync(f, "latin1")).filter((t) => t.length > 0);

  it("random corpus tokens stay inside the budgets", () => {
    const r = rng(SEED);
    const failures: string[] = [];
    let i = 0;
    timeBox(SECONDS, () => failures.push(...check(`tokens #${i++} (seed ${SEED})`, randomTokens(r, tokens))));
    summary("tokens");
    expect(failures.slice(0, 20), failures.slice(0, 20).join("\n")).toEqual([]);
  }, 3_600_000);

  it("mutated corpus files stay inside the budgets", () => {
    const r = rng(SEED + 1);
    const failures: string[] = [];
    let i = 0;
    timeBox(SECONDS, () => {
      const base = r.pick(texts);
      failures.push(...check(`mutation #${i++} (seed ${SEED + 1})`, mutate(r, base)));
    });
    summary("mutations");
    expect(failures.slice(0, 20), failures.slice(0, 20).join("\n")).toEqual([]);
  }, 3_600_000);

  it("a stray opener in a real file stays inside the budgets", () => {
    // What a developer types before they close it: one unbalanced token, the rest of the file below it.
    const openers = ["/*", "*/", "{", "{&", '"', "'", "~", "(", "&IF a &THEN", "DO:", "// ", "/* /*", "{&a"];
    const r = rng(SEED + 2);
    const failures: string[] = [];
    let i = 0;
    timeBox(SECONDS, () => {
      const lines = r.pick(texts).split(/\r?\n/);
      const at = r.int(lines.length);
      const opener = r.pick(openers);
      lines[at] = lines[at].slice(0, r.int(lines[at].length + 1)) + opener + lines[at].slice(r.int(lines[at].length + 1));
      failures.push(...check(`stray ${JSON.stringify(opener)} #${i++} (seed ${SEED + 2})`, lines.join("\n")));
    });
    summary("stray");
    expect(failures.slice(0, 20), failures.slice(0, 20).join("\n")).toEqual([]);
  }, 3_600_000);

  it("keeps the corpus inside its own budgets after the fuzz", () => {
    // The scanner caches of the engine must not slow a normal file down after hostile input.
    const name = (f: string) => relative(repoRoot, f).split(sep).join("/");
    const failures: string[] = [];
    for (const f of files.slice(0, 40)) failures.push(...check(name(f), readFileSync(f, "latin1")));
    expect(failures, failures.join("\n")).toEqual([]);
  });
});
