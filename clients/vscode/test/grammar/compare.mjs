// Tokenizes ABL files with two grammars, counts the lines whose tokens differ, and times both.
// Usage: node test/grammar/compare.mjs <old.json> <new.json> [file or directory ...] [--runs N] [--counts-only]
//
// With no file or directory, it reads every ABL file of the repository (resources/ and the
// tests/ directories of the crates). --counts-only prints no file name and no source text.
import { readFileSync, readdirSync, statSync } from "node:fs";
import { createRequire } from "node:module";
import { dirname, extname, join, resolve } from "node:path";
import { fileURLToPath } from "node:url";

const require = createRequire(import.meta.url);
const vsctm = require("vscode-textmate");
const onig = require("vscode-oniguruma");

const here = dirname(fileURLToPath(import.meta.url));
const repoRoot = resolve(here, "../../../..");
const EXTENSIONS = new Set([".p", ".w", ".cls", ".i", ".v", ".abl"]);
const SKIP_DIRS = new Set(["target", "node_modules", ".git"]);

const args = process.argv.slice(2);
let runs = 7;
let countsOnly = false;
const positional = [];
for (let i = 0; i < args.length; i++) {
  if (args[i] === "--runs") runs = Number(args[++i]);
  else if (args[i] === "--counts-only") countsOnly = true;
  else positional.push(args[i]);
}
const [oldPath, newPath, ...roots] = positional;
if (!oldPath || !newPath || !(runs > 0)) {
  console.error("usage: compare.mjs <old.json> <new.json> [file or directory ...] [--runs N] [--counts-only]");
  process.exit(2);
}

function walk(dir, all, out) {
  for (const name of readdirSync(dir)) {
    if (SKIP_DIRS.has(name)) continue;
    const path = join(dir, name);
    if (statSync(path).isDirectory()) walk(path, all || name === "tests", out);
    else if (all && EXTENSIONS.has(extname(name).toLowerCase())) out.push(path);
  }
}

function files() {
  const out = [];
  if (roots.length === 0) {
    walk(join(repoRoot, "resources"), true, out);
    walk(join(repoRoot, "crates"), false, out);
  } else {
    for (const root of roots) {
      if (statSync(root).isDirectory()) walk(root, true, out);
      else out.push(root);
    }
  }
  return out.sort();
}

const wasm = readFileSync(require.resolve("vscode-oniguruma/release/onig.wasm"));
await onig.loadWASM(wasm.buffer.slice(wasm.byteOffset, wasm.byteOffset + wasm.byteLength));

async function load(path) {
  const registry = new vsctm.Registry({
    onigLib: Promise.resolve({
      createOnigScanner: (s) => new onig.OnigScanner(s),
      createOnigString: (s) => new onig.OnigString(s),
    }),
    loadGrammar: async (scope) => (scope === "source.abl" ? vsctm.parseRawGrammar(readFileSync(path, "utf8"), path) : null),
  });
  const grammar = await registry.loadGrammar("source.abl");
  if (!grammar) throw new Error(`cannot load ${path}`);
  return grammar;
}

function tokens(grammar, lines) {
  let stack = vsctm.INITIAL;
  return lines.map((line) => {
    const r = grammar.tokenizeLine(line, stack);
    stack = r.ruleStack;
    return r.tokens.map((t) => `${t.startIndex}-${t.endIndex} ${t.scopes.join(" ")}`).join("\n");
  });
}

function time(grammar, texts) {
  const samples = [];
  for (let i = 0; i < runs; i++) {
    const start = performance.now();
    for (const lines of texts) tokens(grammar, lines);
    samples.push(performance.now() - start);
  }
  samples.sort((a, b) => a - b);
  return samples[Math.floor(samples.length / 2)];
}

const [oldGrammar, newGrammar] = [await load(oldPath), await load(newPath)];
const list = files();
const texts = list.map((f) => readFileSync(f, "latin1").split(/\r?\n/));
let lines = 0;
let differing = 0;
let differingFiles = 0;
for (let i = 0; i < list.length; i++) {
  const [a, b] = [tokens(oldGrammar, texts[i]), tokens(newGrammar, texts[i])];
  lines += a.length;
  let inFile = 0;
  for (let n = 0; n < a.length; n++) {
    if (a[n] === b[n]) continue;
    inFile++;
    if (countsOnly || inFile > 3) continue;
    const [x, y] = [a[n].split("\n"), b[n].split("\n")];
    const k = x.findIndex((token, j) => token !== y[j]);
    console.log(`${list[i]}:${n + 1}: tokens differ\n  ${texts[i][n]}\n  old: ${x[k]}\n  new: ${y[k]}`);
  }
  differing += inFile;
  if (inFile) differingFiles++;
}
// Alternate the order, so neither grammar always runs on a warmer machine.
let [oldMs, newMs] = [time(oldGrammar, texts), time(newGrammar, texts)];
oldMs = (oldMs + time(oldGrammar, texts)) / 2;
newMs = (newMs + time(newGrammar, texts)) / 2;
console.log(`${list.length} files, ${lines} lines: ${differing} lines differ in ${differingFiles} files`);
console.log(`median of ${runs} runs: old ${oldMs.toFixed(0)} ms, new ${newMs.toFixed(0)} ms (${(oldMs / newMs).toFixed(2)}x)`);
process.exit(differing === 0 ? 0 : 1);
