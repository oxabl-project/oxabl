import * as fs from "node:fs";
import * as path from "node:path";
import { describe, expect, it } from "vitest";

import { keywordCaseFor, keywordCaseOf, memoizeReader, parseSnippets, renderBody } from "./snippets";

const shipped = fs.readFileSync(path.join(__dirname, "..", "snippets", "abl.json"), "utf8");

describe("the shipped snippets file", () => {
  it("holds the nine documented prefixes", () => {
    expect(parseSnippets(shipped).map((s) => s.prefix)).toEqual([
      "find",
      "foreach",
      "defvar",
      "deftt",
      "proc",
      "function",
      "method",
      "class",
      "trycatch",
    ]);
  });

  it("has balanced keyword markers and a description in each snippet", () => {
    for (const snippet of parseSnippets(shipped)) {
      expect(snippet.description, snippet.prefix).not.toBe("");
      expect(snippet.body.split("<<").length, snippet.prefix).toBe(snippet.body.split(">>").length);
      expect(renderBody(snippet.body, "upper"), snippet.prefix).not.toMatch(/<<|>>/);
    }
  });

  it("has no keyword left in upper case in the lower case text", () => {
    for (const snippet of parseSnippets(shipped)) {
      const text = renderBody(snippet.body, "lower").replace(/\$TM_\w+/g, "");
      expect(text, snippet.prefix).not.toMatch(/\b[A-Z][A-Z-]+\b/);
    }
  });
});

describe("parseSnippets", () => {
  it("joins the lines of a body", () => {
    expect(parseSnippets('{"a": {"description": "d", "body": ["x", "y"]}}')).toEqual([
      { prefix: "a", description: "d", body: "x\ny" },
    ]);
  });

  it.each(['[]', '{"a": {"body": "x"}}', '{"a": {"description": "d", "body": [1]}}', "{"])(
    "refuses %s",
    (text) => {
      expect(() => parseSnippets(text)).toThrow();
    },
  );
});

describe("renderBody", () => {
  const body = "<<FIND FIRST>> ${1:Customer} <<NO-LOCK>>.";

  it("writes the markers in upper case and leaves the rest", () => {
    expect(renderBody(body, "upper")).toBe("FIND FIRST ${1:Customer} NO-LOCK.");
  });

  it("writes the markers in lower case and leaves the rest", () => {
    expect(renderBody(body, "lower")).toBe("find first ${1:Customer} no-lock.");
  });
});

describe("keywordCaseOf", () => {
  it("is lower for Lowercase in [workspace.style]", () => {
    expect(keywordCaseOf('[workspace.style]\nkeyword_case = "Lowercase"\n')).toBe("lower");
    expect(keywordCaseOf('[ workspace . style ]\nkeyword_case="Lowercase" # keep\n')).toBe("lower");
  });

  it("is lower for the dotted and inline forms", () => {
    expect(keywordCaseOf('[workspace]\nstyle.keyword_case = "Lowercase"\n')).toBe("lower");
    expect(keywordCaseOf('[workspace]\nstyle = { indent_size = 2, keyword_case = "Lowercase" }\n')).toBe("lower");
    expect(keywordCaseOf('workspace.style.keyword_case = "Lowercase"\n')).toBe("lower");
  });

  it("is upper for Uppercase, Preserve and no setting", () => {
    expect(keywordCaseOf('[workspace.style]\nkeyword_case = "Uppercase"\n')).toBe("upper");
    expect(keywordCaseOf('[workspace.style]\nkeyword_case = "Preserve"\n')).toBe("upper");
    expect(keywordCaseOf("[workspace.style]\nindent_size = 2\n")).toBe("upper");
    expect(keywordCaseOf("")).toBe("upper");
  });

  it("ignores the same key in another table and in a comment", () => {
    expect(keywordCaseOf('[other]\nkeyword_case = "Lowercase"\n')).toBe("upper");
    expect(keywordCaseOf('[[workspace.style]]\nkeyword_case = "Lowercase"\n')).toBe("upper");
    expect(keywordCaseOf('[workspace.style]\n# keyword_case = "Lowercase"\n')).toBe("upper");
  });
});

describe("keywordCaseOf against the table shared with the server", () => {
  const table = JSON.parse(
    fs.readFileSync(path.join(__dirname, "..", "test", "keyword-case.json"), "utf8"),
  ) as { cases: { name: string; toml: string[]; expected: string; reader?: string }[] };

  it("has cases", () => {
    expect(table.cases.length).toBeGreaterThanOrEqual(10);
  });

  it.each(table.cases)("$name", ({ toml, expected, reader }) => {
    expect(keywordCaseOf(toml.join("\n"))).toBe(reader ?? expected);
    expect(keywordCaseOf(toml.join("\r\n"))).toBe(reader ?? expected);
  });
});

describe("keywordCaseFor", () => {
  const files: Record<string, string> = {
    "/p/oxabl.toml": '[workspace.style]\nkeyword_case = "Lowercase"\n',
    "/q/oxabl.toml": "",
  };
  const read = async (file: string) => files[file];

  it("takes the nearest oxabl.toml above the directory", async () => {
    expect(await keywordCaseFor("/p/src/deep", read)).toBe("lower");
    expect(await keywordCaseFor("/q/src", read)).toBe("upper");
  });

  it("is upper when no oxabl.toml is above", async () => {
    expect(await keywordCaseFor("/r/src", read)).toBe("upper");
  });

  it("counts a file that cannot be read as absent", async () => {
    const failing = async (file: string) => {
      if (file === "/p/src/oxabl.toml") {
        throw new Error("EACCES");
      }
      return read(file);
    };
    expect(await keywordCaseFor("/p/src", failing)).toBe("lower");
  });
});

describe("keywordCaseFor with a cancelled request", () => {
  it("stops before the next read and gives no answer", async () => {
    const reads: string[] = [];
    let cancelled = false;
    const read = async (file: string) => {
      reads.push(file);
      cancelled = true;
      return undefined;
    };
    expect(await keywordCaseFor("/a/b/c", read, () => cancelled)).toBeUndefined();
    expect(reads).toEqual(["/a/b/c/oxabl.toml"]);
  });
});

describe("memoizeReader", () => {
  it("reads each file once, present or absent, until it is cleared", async () => {
    const reads: string[] = [];
    const cached = memoizeReader(async (file) => {
      reads.push(file);
      return file === "/p/oxabl.toml" ? "x" : undefined;
    });
    expect(await cached.read("/p/oxabl.toml")).toBe("x");
    expect(await cached.read("/p/oxabl.toml")).toBe("x");
    expect(await cached.read("/q/oxabl.toml")).toBeUndefined();
    expect(await cached.read("/q/oxabl.toml")).toBeUndefined();
    expect(reads).toEqual(["/p/oxabl.toml", "/q/oxabl.toml"]);
    cached.clear();
    await cached.read("/p/oxabl.toml");
    expect(reads).toHaveLength(3);
  });

  it("does not keep a read that failed", async () => {
    let calls = 0;
    const cached = memoizeReader(async () => {
      calls += 1;
      if (calls === 1) {
        throw new Error("EACCES");
      }
      return "x";
    });
    await expect(cached.read("/p/oxabl.toml")).rejects.toThrow("EACCES");
    await Promise.resolve();
    expect(await cached.read("/p/oxabl.toml")).toBe("x");
  });
});
