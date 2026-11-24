import { readFileSync } from "node:fs";
import { createRequire } from "node:module";
import { dirname, resolve } from "node:path";
import { fileURLToPath } from "node:url";
import { beforeAll, describe, expect, it } from "vitest";
import * as vsctm from "vscode-textmate";
import * as onig from "vscode-oniguruma";

// Nested comments and include references keep their scopes up to the nesting limit, and the
// rule stack stops growing past it.

const here = dirname(fileURLToPath(import.meta.url));
const grammarPath = process.env.OXABL_GRAMMAR ?? resolve(here, "../../syntaxes/abl.tmLanguage.json");

// `x-nesting-limit` of `comment-block` and `include` in resources/abl.tmLanguage.template.json.
const LIMIT = 50;

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
});

interface Tokenized {
  // The scopes of the first token that is `text` with blanks around it, on any line.
  scopes: (text: string) => string[];
  maxDepth: number;
  endDepth: number;
}

function tokenize(source: string): Tokenized {
  let stack = vsctm.INITIAL;
  let maxDepth = 0;
  const tokens: { text: string; scopes: string[] }[] = [];
  for (const line of source.split("\n")) {
    const r = grammar.tokenizeLine(line, stack);
    for (const t of r.tokens) tokens.push({ text: line.slice(t.startIndex, t.endIndex), scopes: t.scopes });
    stack = r.ruleStack;
    maxDepth = Math.max(maxDepth, stack.depth);
  }
  return {
    scopes: (text) => {
      const found = tokens.find((t) => t.text.trim() === text);
      if (!found) throw new Error(`no token ${JSON.stringify(text)} in ${JSON.stringify(source)}`);
      return found.scopes;
    },
    maxDepth,
    endDepth: stack.depth,
  };
}

const count = (scopes: string[], scope: string) => scopes.filter((s) => s === scope).length;
const COMMENT = "comment.block.abl";
const INCLUDE = "meta.preprocessor.include.abl";

describe("nested block comments", () => {
  for (const depth of [1, 2, 9, LIMIT - 1, LIMIT]) {
    for (const [layout, sep] of [
      ["one line", " "],
      ["one per line", "\n"],
    ]) {
      it(`keep ${depth} levels, ${layout}`, () => {
        const r = tokenize(`${"/*" + sep}`.repeat(depth) + `inside${sep}` + `${"*/" + sep}`.repeat(depth) + "after = 1.");
        expect(count(r.scopes("inside"), COMMENT)).toBe(depth);
        expect(r.scopes("after")).not.toContain(COMMENT);
        expect(r.scopes("after")).toContain("variable.other.abl");
        expect(r.endDepth).toBe(1);
      });
    }
  }

  it(`stop counting past ${LIMIT} levels`, () => {
    const r = tokenize("/*\n".repeat(LIMIT * 4) + "inside");
    expect(count(r.scopes("inside"), COMMENT)).toBe(LIMIT);
    expect(r.maxDepth).toBe(LIMIT + 1);
  });

  for (const n of [LIMIT + 1, LIMIT + 2, LIMIT * 2, LIMIT * 20]) {
    it(`stay a comment past ${LIMIT} adjacent openers on one line (${n})`, () => {
      const r = tokenize("/*".repeat(n) + " inside");
      expect(count(r.scopes("inside"), COMMENT)).toBe(LIMIT);
      expect(r.endDepth).toBe(LIMIT + 1);
    });
  }
});

describe("nested include references", () => {
  for (const depth of [1, 2, 9, LIMIT - 1, LIMIT]) {
    it(`keep ${depth} levels`, () => {
      const r = tokenize("{inc.i ".repeat(depth) + "arg" + "}".repeat(depth) + " after = 1.");
      expect(count(r.scopes("arg"), INCLUDE)).toBe(depth);
      expect(r.scopes("arg")).toContain("string.unquoted.argument.abl");
      expect(r.scopes("after")).not.toContain(INCLUDE);
      expect(r.endDepth).toBe(1);
    });
  }

  it(`stop counting past ${LIMIT} levels`, () => {
    const r = tokenize("{inc.i\n".repeat(LIMIT * 4) + "arg");
    expect(count(r.scopes("arg"), INCLUDE)).toBe(LIMIT);
    expect(r.maxDepth).toBe(LIMIT + 1);
  });
});

describe("a define directive in the value of another", () => {
  it("is a keyword and does not open a second define", () => {
    const r = tokenize("&SCOPED-DEFINE a &GLOBAL-DEFINE b ~\n".repeat(100) + "1\nx = 2.");
    expect(r.maxDepth).toBe(2);
    expect(r.endDepth).toBe(1);
    const nested = r.scopes("&GLOBAL-DEFINE");
    expect(nested).toContain("keyword.control.directive.define.abl");
    expect(count(nested, "meta.preprocessor.define.abl")).toBe(1);
    expect(r.scopes("x")).not.toContain("meta.preprocessor.define.abl");
  });
});
