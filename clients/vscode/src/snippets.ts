// The ABL snippets of the extension. Like `config.ts`, this module has no
// `vscode` import, so vitest can test it without an extension host.

import * as path from "node:path";

export type KeywordCase = "upper" | "lower";

export interface Snippet {
  /** What the person types to get the snippet. */
  prefix: string;
  description: string;
  /** VS Code snippet syntax. Keywords are in `<<...>>` markers. */
  body: string;
}

/**
 * The snippets of `snippets/abl.json`: an object that maps each prefix to its
 * `description` and its `body` (one string, or an array of lines). Throws on
 * any other shape, so a damaged file gives no snippets instead of broken ones.
 */
export function parseSnippets(text: string): Snippet[] {
  const doc: unknown = JSON.parse(text);
  if (typeof doc !== "object" || doc === null || Array.isArray(doc)) {
    throw new Error("the snippets file is not an object");
  }
  return Object.entries(doc).map(([prefix, entry]) => {
    const { description, body } = (entry ?? {}) as { description?: unknown; body?: unknown };
    const lines = Array.isArray(body) ? body : [body];
    if (typeof description !== "string" || !lines.every((line) => typeof line === "string")) {
      throw new Error(`snippet ${prefix}: needs a description and a body of text`);
    }
    return { prefix, description, body: lines.join("\n") };
  });
}

/** The body with each `<<KEYWORDS>>` marker written in `keywordCase`. */
export function renderBody(body: string, keywordCase: KeywordCase): string {
  return body.replace(/<<(.*?)>>/g, (_, words: string) =>
    keywordCase === "lower" ? words.toLowerCase() : words.toUpperCase(),
  );
}

/**
 * The keyword case of the `oxabl.toml` text: `lower` when `[workspace.style]`
 * sets `keyword_case = "Lowercase"`. `Uppercase`, `Preserve` and no setting
 * give `upper`: a project that keeps the case of each author has none to follow.
 */
export function keywordCaseOf(toml: string): KeywordCase {
  let table = "";
  for (const raw of toml.split(/\r?\n/)) {
    const line = raw.replace(/\s+#.*$/, "").trim();
    if (line.startsWith("[[")) {
      table = "[[";
      continue;
    }
    const header = /^\[\s*([^\[\]]+?)\s*\]$/.exec(line);
    if (header) {
      table = header[1].replace(/\s*\.\s*/g, ".");
      continue;
    }
    const assignment = /^([A-Za-z0-9_.\s-]+?)\s*=\s*(.*)$/.exec(line);
    if (!assignment) {
      continue;
    }
    const key = [table, assignment[1].replace(/\s*\.\s*/g, ".")].filter(Boolean).join(".");
    const value = assignment[2];
    const setting =
      key === "workspace.style.keyword_case"
        ? /^["']([A-Za-z]+)["']/.exec(value)?.[1]
        : key === "workspace.style"
          ? /[{,]\s*keyword_case\s*=\s*["']([A-Za-z]+)["']/.exec(value)?.[1]
          : undefined;
    if (setting !== undefined) {
      return setting === "Lowercase" ? "lower" : "upper";
    }
  }
  return "upper";
}

/**
 * The keyword case for a file in `dir`, from the nearest `oxabl.toml` in `dir`
 * or above it. `read` returns the text of a file, or `undefined` when there is
 * none. A file that cannot be read counts as absent.
 */
export async function keywordCaseFor(
  dir: string,
  read: (file: string) => Promise<string | undefined>,
): Promise<KeywordCase> {
  for (let current = dir; ; current = path.dirname(current)) {
    const text = await read(path.join(current, "oxabl.toml")).catch(() => undefined);
    if (text !== undefined) {
      return keywordCaseOf(text);
    }
    if (path.dirname(current) === current) {
      return "upper";
    }
  }
}
