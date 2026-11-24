import { readFileSync, readdirSync, statSync } from "node:fs";
import { createRequire } from "node:module";
import { dirname, extname, join, relative, resolve, sep } from "node:path";
import { fileURLToPath } from "node:url";
import { beforeAll, describe, expect, it } from "vitest";
import * as vsctm from "vscode-textmate";
import * as onig from "vscode-oniguruma";

const here = dirname(fileURLToPath(import.meta.url));
const repoRoot = resolve(here, "../../../..");
const grammarPath = process.env.OXABL_GRAMMAR ?? resolve(here, "../../syntaxes/abl.tmLanguage.json");

const LINE_BUDGET_MS = 50;
const FILE_BUDGET_MS = 2000;
const EXTENSIONS = new Set([".p", ".w", ".cls", ".i", ".v", ".abl"]);
const SKIP_DIRS = new Set(["target", "node_modules"]);

// Every ABL file under resources/ and under a tests/ directory of a crate.
function corpusFiles(): string[] {
  const found: string[] = [];
  const walk = (dir: string, inTests: boolean) => {
    for (const name of readdirSync(dir)) {
      if (SKIP_DIRS.has(name)) continue;
      const path = join(dir, name);
      const stat = statSync(path);
      if (stat.isDirectory()) {
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

type Rule = Record<string, unknown>;

// The path and source of every regex of the grammar.
function grammarRegexes(node: unknown, path: string, out: { path: string; source: string }[]) {
  if (Array.isArray(node)) {
    node.forEach((child, i) => grammarRegexes(child, `${path}[${i}]`, out));
  } else if (node && typeof node === "object") {
    for (const [key, value] of Object.entries(node as Rule)) {
      if ((key === "match" || key === "begin" || key === "end" || key === "while") && typeof value === "string") {
        out.push({ path: `${path}.${key}`, source: value });
      } else {
        grammarRegexes(value, `${path}.${key}`, out);
      }
    }
  }
}

const grammarJson = readFileSync(grammarPath, "utf8");
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
      scope === "source.abl" ? vsctm.parseRawGrammar(grammarJson, grammarPath) : null,
  });
  const loaded = await registry.loadGrammar("source.abl");
  if (!loaded) throw new Error(`cannot load ${grammarPath}`);
  grammar = loaded;
});

describe("grammar regexes", () => {
  it("every match, begin, end and while regex compiles", () => {
    const regexes: { path: string; source: string }[] = [];
    grammarRegexes(JSON.parse(grammarJson), "$", regexes);
    expect(regexes.length, "the grammar has no regexes").toBeGreaterThan(0);
    const failures: string[] = [];
    for (const { path, source } of regexes) {
      try {
        new onig.OnigScanner([source]).dispose?.();
      } catch (e) {
        failures.push(`${path}: ${(e as Error).message}\n    ${source}`);
      }
    }
    expect(failures, failures.join("\n")).toEqual([]);
  });
});

describe("corpus", () => {
  const files = corpusFiles();

  it("is not empty", () => {
    expect(files.length, `no ABL files found under ${repoRoot}`).toBeGreaterThan(0);
  });

  it("tokenizes every file within the time budget", () => {
    // The engine compiles its scanners on first use; time the steady state, not that start-up.
    let warm = vsctm.INITIAL;
    for (const line of ["/* c */ DEFINE VARIABLE a AS INTEGER NO-UNDO.", "&SCOPED-DEFINE x 1", 'x = "s":U + {&x}.']) {
      warm = grammar.tokenizeLine(line, warm).ruleStack;
    }
    const failures: string[] = [];
    let slowest = { ms: 0, where: "" };
    for (const file of files) {
      const name = relative(repoRoot, file).split(sep).join("/");
      const lines = readFileSync(file, "latin1").split(/\r?\n/);
      let stack = vsctm.INITIAL;
      const fileStart = performance.now();
      for (let n = 0; n < lines.length; n++) {
        const start = performance.now();
        try {
          stack = grammar.tokenizeLine(lines[n], stack).ruleStack;
        } catch (e) {
          failures.push(`${name}:${n + 1}: exception: ${(e as Error).message}`);
          break;
        }
        const ms = performance.now() - start;
        if (ms > slowest.ms) slowest = { ms, where: `${name}:${n + 1} (${lines[n].length} chars)` };
        if (ms > LINE_BUDGET_MS) {
          failures.push(`${name}:${n + 1}: line took ${ms.toFixed(1)} ms, budget ${LINE_BUDGET_MS} ms`);
        }
      }
      const fileMs = performance.now() - fileStart;
      if (fileMs > FILE_BUDGET_MS) {
        failures.push(`${name}: file took ${fileMs.toFixed(0)} ms, budget ${FILE_BUDGET_MS} ms`);
      }
    }
    console.log(`corpus: ${files.length} files, slowest line ${slowest.ms.toFixed(2)} ms at ${slowest.where}`);
    expect(failures, failures.join("\n")).toEqual([]);
  }, 300_000);
});
