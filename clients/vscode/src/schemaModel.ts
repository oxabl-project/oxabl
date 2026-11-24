// The answer of `oxabl/schema` and the filter of the schema view. No `vscode`
// import: the logic is tested on its own (schemaModel.test.ts).

import * as path from "node:path";

export interface Pos {
  line: number;
  character: number;
}
export interface Range {
  start: Pos;
  end: Pos;
}

export interface SchemaProblem {
  severity: string;
  code: string;
  message: string;
  range: Range | null;
}

/** An attribute that the `.df` does not set is absent. */
export interface SchemaField {
  name: string;
  dataType?: string | null;
  extent?: number | null;
  format?: string | null;
  label?: string | null;
  initial?: string | null;
  mandatory: boolean;
  description?: string | null;
  decimals?: number | null;
  caseSensitive: boolean;
  help?: string | null;
  order?: number | null;
  range: Range | null;
}

export interface SchemaIndex {
  name: string;
  unique: boolean;
  primary: boolean;
  word: boolean;
  inactive: boolean;
  description?: string | null;
  fields: { name: string; ascending: boolean; abbreviated: boolean }[];
  range: Range | null;
}

export interface SchemaTable {
  name: string;
  declared: boolean;
  label?: string | null;
  description?: string | null;
  dumpName?: string | null;
  area?: string | null;
  hidden: boolean;
  frozen: boolean;
  fields: SchemaField[];
  indexes: SchemaIndex[];
  range: Range | null;
}

export interface SchemaSequence {
  name: string;
  initial?: number | null;
  increment?: number | null;
  minValue?: number | null;
  maxValue?: number | null;
  cycleOnLimit: boolean;
  range: Range | null;
}

export interface SchemaDatabase {
  name: string;
  path: string;
  uri: string | null;
  complete: boolean;
  problems: SchemaProblem[];
  tables: SchemaTable[];
  sequences: SchemaSequence[];
}

/** The answer of `oxabl/schema`. */
export interface SchemaAnswer {
  config: string | null;
  reason: string | null;
  warnings: string[];
  databases: SchemaDatabase[];
  /** The directories that hold the configured `.df` files, and each configured schema directory. */
  directories: string[];
}

/** The answer of `oxabl/schemaTable`, or `null` for no such table. */
export interface TableAnswer {
  database: { name: string; path: string; uri: string | null; complete: boolean };
  table: SchemaTable;
}

const has = (text: string | null | undefined, query: string): boolean =>
  !!text && text.toLowerCase().includes(query);

/**
 * Whether the table itself matches: its name, label, description or dump
 * name. An ABL abbreviation of a table name is a prefix of it, so a substring
 * finds it too.
 */
export const tableMatches = (table: SchemaTable, query: string): boolean =>
  has(table.name, query) || has(table.label, query) || has(table.description, query) || has(table.dumpName, query);

export const fieldMatches = (field: SchemaField, query: string): boolean =>
  has(field.name, query) || has(field.label, query) || has(field.description, query);

export const indexMatches = (index: SchemaIndex, query: string): boolean =>
  has(index.name, query) || has(index.description, query);

/**
 * The databases with only what matches `query`, case-insensitive substring.
 * A table that matches keeps every field and index; otherwise it stays with
 * the fields and indexes that match. An empty query keeps everything.
 */
export function filterSchema(databases: SchemaDatabase[], query: string): SchemaDatabase[] {
  const q = query.trim().toLowerCase();
  if (!q) {
    return databases;
  }
  const out: SchemaDatabase[] = [];
  for (const database of databases) {
    const tables: SchemaTable[] = [];
    for (const table of database.tables) {
      if (tableMatches(table, q)) {
        tables.push(table);
        continue;
      }
      const fields = table.fields.filter((f) => fieldMatches(f, q));
      const indexes = table.indexes.filter((i) => indexMatches(i, q));
      if (fields.length || indexes.length) {
        tables.push({ ...table, fields, indexes });
      }
    }
    const sequences = database.sequences.filter((s) => has(s.name, q));
    if (tables.length || sequences.length) {
      out.push({ ...database, tables, sequences });
    }
  }
  return out;
}

/**
 * The message above the tree: why the view cannot show a schema, the warnings
 * of the configuration (a schema directory with no `.df`, a file named twice),
 * and what the filter keeps. `undefined` when there is nothing to say.
 */
export function viewMessage(
  answer: SchemaAnswer | undefined,
  shown: SchemaDatabase[],
  filter: string,
  failure?: string,
): string | undefined {
  if (failure) {
    return failure;
  }
  if (!answer) {
    return undefined;
  }
  if (answer.databases.length === 0) {
    return `No schema: ${answer.reason ?? "the server gave no reason."}`;
  }
  const parts: string[] = [];
  if (answer.warnings.length) {
    parts.push(`Warning: ${answer.warnings.join("; ")}`);
  }
  if (filter) {
    const tables = shown.reduce((n, d) => n + d.tables.length, 0);
    parts.push(shown.length === 0 ? `Nothing matches "${filter}".` : `Filter "${filter}": ${tables} table(s).`);
  }
  return parts.length ? parts.join(" ") : undefined;
}

/** JSON-RPC `MethodNotFound`: a server older than the request. */
const METHOD_NOT_FOUND = -32601;

/** What to show when a request fails with `code` and `message`. */
export function failureText(what: string, code: number | undefined, message: string): string {
  if (code === METHOD_NOT_FOUND) {
    return "This oxabl is too old for the schema view. Update oxabl (oxabl: Install or Update the CLI).";
  }
  return `${what} could not be read: ${message}`;
}

/**
 * The schema directories (`directories` of the answer) that no workspace
 * folder holds: a change there is not seen by a workspace file watcher, so
 * each needs its own. Compared with `path.relative`, which ignores the case
 * of a Windows drive letter.
 */
export function outsideDirectories(directories: string[], folders: string[]): string[] {
  const inside = (dir: string) =>
    folders.some((folder) => {
      const rel = path.relative(folder, dir);
      return rel === "" || (!rel.startsWith("..") && !path.isAbsolute(rel));
    });
  const dirs = new Set<string>();
  for (const directory of directories) {
    // `oxabl.toml` can name `../schema/db.df`: resolve the `..` first.
    const dir = path.resolve(directory);
    if (!inside(dir)) {
      dirs.add(dir);
    }
  }
  return [...dirs].sort();
}

/** A `.df` in any case of its extension, as the loader takes it. */
export const DF_GLOB = "*.{df,DF,Df,dF}";

/** One watcher: the files `glob` matches directly in `dir`. */
export interface Watch {
  dir: string;
  glob: string;
}

/**
 * What the view must watch beside the workspace folders: each schema
 * directory outside them, for `.df` files, and the `oxabl.toml` of the
 * answer when it is above them (the server finds it by walking up).
 */
export function outsideWatches(answer: SchemaAnswer, folders: string[]): Watch[] {
  const watches: Watch[] = outsideDirectories(answer.directories ?? [], folders).map((dir) => ({ dir, glob: DF_GLOB }));
  if (answer.config) {
    for (const dir of outsideDirectories([path.dirname(answer.config)], folders)) {
      watches.push({ dir, glob: path.basename(answer.config) });
    }
  }
  return watches;
}

/** `integer`, `character[3]`, or `?` for a field declared twice with conflicting types. */
export const typeText = (field: SchemaField): string =>
  `${field.dataType ?? "?"}${field.extent ? `[${field.extent}]` : ""}`;

/** `unique primary` and so on, then the fields: `unique primary (CustNum)`. */
export function indexText(index: SchemaIndex): string {
  const flags = [
    index.primary && "primary",
    index.unique && "unique",
    index.word && "word",
    index.inactive && "inactive",
  ].filter(Boolean);
  const fields = index.fields.map((f) => `${f.name}${f.ascending ? "" : " desc"}`).join(", ");
  return [...flags, `(${fields})`].join(" ");
}

/** The text for "Copy table.field". */
export const qualifiedName = (table: SchemaTable, field: SchemaField): string => `${table.name}.${field.name}`;

const escapeHtml = (text: string): string =>
  text.replace(/[&<>"']/g, (c) => ({ "&": "&amp;", "<": "&lt;", ">": "&gt;", '"': "&quot;", "'": "&#39;" })[c] ?? c);

const cell = (text: string | number | null | undefined): string => `<td>${text === null || text === undefined ? "" : escapeHtml(String(text))}</td>`;

/**
 * The detail page of one table: its fields as a table, then its indexes. No
 * script: `open(range)` gives the `command:` link that opens a node in the `.df`.
 */
export function tableHtml(answer: TableAnswer, open: (range: Range | null) => string, csp: string): string {
  const { database, table } = answer;
  const link = (name: string, range: Range | null) =>
    range ? `<a href="${escapeHtml(open(range))}" title="Open in .df">${escapeHtml(name)}</a>` : escapeHtml(name);
  const fields = table.fields
    .map(
      (f) =>
        `<tr><td>${link(f.name, f.range)}</td>${cell(typeText(f))}${cell(f.format)}${cell(f.label)}${cell(f.initial)}${cell(
          f.mandatory ? "yes" : "",
        )}${cell(f.decimals)}${cell(f.description)}</tr>`,
    )
    .join("\n");
  const indexes = table.indexes
    .map(
      (i) =>
        `<tr><td>${link(i.name, i.range)}</td>${cell(i.primary ? "yes" : "")}${cell(i.unique ? "yes" : "")}${cell(
          i.fields.map((f) => `${f.name}${f.ascending ? "" : " (desc)"}`).join(", "),
        )}${cell(i.description)}</tr>`,
    )
    .join("\n");
  const facts = [
    ["Database", `${database.name} (${database.path})`],
    ["Label", table.label],
    ["Description", table.description],
    ["Dump name", table.dumpName],
    ["Area", table.area],
    ["Hidden", table.hidden ? "yes" : null],
    ["Frozen", table.frozen ? "yes" : null],
  ]
    .filter(([, value]) => value)
    .map(([key, value]) => `<tr><th>${escapeHtml(key as string)}</th>${cell(value)}</tr>`)
    .join("\n");
  const notes = [
    table.declared ? "" : `<p class="note">${escapeHtml(database.name)} adds fields or indexes to this table but does not declare it.</p>`,
    database.complete ? "" : `<p class="note">The schema file has problems, so this table can miss fields or indexes.</p>`,
  ].join("");
  return `<!DOCTYPE html>
<html lang="en">
<head>
<meta charset="UTF-8">
<meta http-equiv="Content-Security-Policy" content="${escapeHtml(csp)}">
<title>${escapeHtml(table.name)}</title>
<style>
body { font-family: var(--vscode-font-family); font-size: var(--vscode-font-size); color: var(--vscode-foreground); }
table { border-collapse: collapse; margin-bottom: 1.5em; }
th, td { text-align: left; padding: 2px 10px 2px 0; vertical-align: top; }
thead th { border-bottom: 1px solid var(--vscode-panel-border); }
.note { color: var(--vscode-editorWarning-foreground); }
</style>
</head>
<body>
<h1>${link(table.name, table.range)}</h1>
${notes}
<table class="facts">${facts}</table>
<h2>Fields (${table.fields.length})</h2>
<table class="fields">
<thead><tr><th>Name</th><th>Type</th><th>Format</th><th>Label</th><th>Initial</th><th>Mandatory</th><th>Decimals</th><th>Description</th></tr></thead>
<tbody>
${fields}
</tbody>
</table>
<h2>Indexes (${table.indexes.length})</h2>
<table class="indexes">
<thead><tr><th>Name</th><th>Primary</th><th>Unique</th><th>Fields</th><th>Description</th></tr></thead>
<tbody>
${indexes}
</tbody>
</table>
</body>
</html>
`;
}
