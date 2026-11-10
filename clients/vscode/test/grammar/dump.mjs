// Prints the scopes of each token of the ABL text on stdin (or of a file).
// Usage: node test/grammar/dump.mjs [file] [grammar.json]
import { readFileSync } from "node:fs";
import { createRequire } from "node:module";
import { dirname, resolve } from "node:path";
import { fileURLToPath } from "node:url";

const require = createRequire(import.meta.url);
const vsctm = require("vscode-textmate");
const onig = require("vscode-oniguruma");

const here = dirname(fileURLToPath(import.meta.url));
const grammarPath = process.argv[3] ?? resolve(here, "../../syntaxes/abl.tmLanguage.json");
const wasm = readFileSync(require.resolve("vscode-oniguruma/release/onig.wasm"));
await onig.loadWASM(wasm.buffer.slice(wasm.byteOffset, wasm.byteOffset + wasm.byteLength));
const registry = new vsctm.Registry({
  onigLib: Promise.resolve({
    createOnigScanner: (s) => new onig.OnigScanner(s),
    createOnigString: (s) => new onig.OnigString(s),
  }),
  loadGrammar: async (scope) =>
    scope === "source.abl" ? vsctm.parseRawGrammar(readFileSync(grammarPath, "utf8"), grammarPath) : null,
});
const grammar = await registry.loadGrammar("source.abl");
const src = readFileSync(process.argv[2] ?? 0, "utf8");
let stack = vsctm.INITIAL;
for (const line of src.split("\n")) {
  const r = grammar.tokenizeLine(line, stack);
  console.log("> " + line);
  for (const t of r.tokens) {
    console.log(`   ${JSON.stringify(line.slice(t.startIndex, t.endIndex))} ${t.scopes.slice(1).join(" ")}`);
  }
  stack = r.ruleStack;
}
