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

const KEY_PART = String.raw`(?:"[^"]*"|'[^']*'|[A-Za-z0-9_-]+)`;
const DOTTED_KEY = String.raw`${KEY_PART}(?:\s*\.\s*${KEY_PART})*`;
const HEADER = new RegExp(String.raw`^\[\s*(${DOTTED_KEY})\s*\]$`);
const ASSIGNMENT = new RegExp(String.raw`^(${DOTTED_KEY})\s*=\s*(.*)$`);
const STRING = String.raw`(?:"([A-Za-z]+)"|'([A-Za-z]+)')`;
const VALUE = new RegExp(`^${STRING}`);
const INLINE_SETTING = new RegExp(String.raw`[{,]\s*(?:keyword_case|"keyword_case"|'keyword_case')\s*=\s*${STRING}`);
const INLINE_STYLE = /\bstyle\s*=\s*(\{.*)$/;

/** The parts of a dotted key, without their quotes. */
function keyParts(key: string): string[] {
  return [...key.matchAll(new RegExp(KEY_PART, "g"))].map(([part]) =>
    part.startsWith('"') || part.startsWith("'") ? part.slice(1, -1) : part,
  );
}

function count(text: string, part: string): number {
  return text.split(part).length - 1;
}

/** The text of the string that `match` found, if any. */
function word(match: RegExpExecArray | null | undefined): string | undefined {
  return match?.[1] ?? match?.[2];
}

/**
 * The keyword case of the `oxabl.toml` text: `lower` when `[workspace.style]`
 * sets `keyword_case = "Lowercase"`. `Uppercase`, `Preserve` and no setting
 * give `upper`: a project that keeps the case of each author has none to follow.
 *
 * This is a line reader, not a TOML parser. It reads the setting as a plain,
 * dotted or inline key, with quoted key parts, and skips multi-line strings.
 * It cannot tell that the server rejects the file for another reason, such as
 * an unknown key: then the server uses its defaults, and this gives the
 * setting of the file. `test/keyword-case.json` lists the texts that this
 * reader and the server's reader are both checked against.
 */
export function keywordCaseOf(toml: string): KeywordCase {
  let table: string[] = [];
  let openString: string | undefined;
  for (const raw of toml.split(/\r?\n/)) {
    if (openString !== undefined) {
      if (count(raw, openString) % 2 === 1) {
        openString = undefined;
      }
      continue;
    }
    const line = raw.replace(/\s+#.*$/, "").trim();
    if (line.startsWith("#")) {
      continue;
    }
    if (line.startsWith("[[")) {
      table = ["[["];
      continue;
    }
    const header = HEADER.exec(line);
    if (header) {
      table = keyParts(header[1]);
      continue;
    }
    const assignment = ASSIGNMENT.exec(line);
    if (!assignment) {
      continue;
    }
    const key = [...table, ...keyParts(assignment[1])].join(".");
    const value = assignment[2];
    for (const delimiter of ['"""', "'''"]) {
      if (count(value, delimiter) % 2 === 1) {
        openString = delimiter;
      }
    }
    let setting: string | undefined;
    if (key === "workspace.style.keyword_case") {
      setting = word(VALUE.exec(value));
    } else if (key === "workspace.style") {
      setting = word(INLINE_SETTING.exec(value));
    } else if (key === "workspace") {
      const style = INLINE_STYLE.exec(value);
      setting = style ? word(INLINE_SETTING.exec(style[1])) : undefined;
    }
    if (setting !== undefined) {
      return setting === "Lowercase" ? "lower" : "upper";
    }
  }
  return "upper";
}

/**
 * Wrap `read` so each file is read once. Present and absent files are both
 * kept; a read that fails is not. `clear` drops everything, for when an
 * `oxabl.toml` is created, changed or deleted.
 */
export function memoizeReader(read: (file: string) => Promise<string | undefined>): {
  read: (file: string) => Promise<string | undefined>;
  clear: () => void;
} {
  const cache = new Map<string, Promise<string | undefined>>();
  return {
    read(file) {
      let hit = cache.get(file);
      if (hit === undefined) {
        hit = read(file);
        cache.set(file, hit);
        const stored = hit;
        hit.catch(() => cache.get(file) === stored && cache.delete(file));
      }
      return hit;
    },
    clear: () => cache.clear(),
  };
}

/**
 * The keyword case for a file in `dir`, from the nearest `oxabl.toml` in `dir`
 * or above it. `read` returns the text of a file, or `undefined` when there is
 * none. A file that cannot be read counts as absent. When `cancelled` says so,
 * the walk stops before the next read and gives `undefined`.
 */
export async function keywordCaseFor(
  dir: string,
  read: (file: string) => Promise<string | undefined>,
  cancelled: () => boolean = () => false,
): Promise<KeywordCase | undefined> {
  for (let current = dir; ; current = path.dirname(current)) {
    if (cancelled()) {
      return undefined;
    }
    const text = await read(path.join(current, "oxabl.toml")).catch(() => undefined);
    if (text !== undefined) {
      return keywordCaseOf(text);
    }
    if (path.dirname(current) === current) {
      return "upper";
    }
  }
}
