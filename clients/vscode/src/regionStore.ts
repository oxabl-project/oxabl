// The inactive regions of each document, with the version they were computed
// for. Free of any `vscode` import so it runs under vitest.

export interface Pos {
  line: number;
  character: number;
}
export interface Region {
  start: Pos;
  end: Pos;
}
/** One edit: the replaced range, in the text before it, and the new text. */
export interface Edit {
  range: Region;
  text: string;
}

const before = (a: Pos, b: Pos) => a.line < b.line || (a.line === b.line && a.character < b.character);

/**
 * `regions` after `edit`. A region the edit touches is dropped, as its text is
 * no longer the text the server decided on. A region after the edit moves with
 * the text.
 */
export function shiftRegions(regions: Region[], edit: Edit): Region[] {
  const lines = edit.text.split("\n");
  const end: Pos = {
    line: edit.range.start.line + lines.length - 1,
    character: (lines.length === 1 ? edit.range.start.character : 0) + lines[lines.length - 1].length,
  };
  const lineDelta = end.line - edit.range.end.line;
  const move = (p: Pos): Pos =>
    p.line === edit.range.end.line
      ? { line: end.line, character: end.character + (p.character - edit.range.end.character) }
      : { line: p.line + lineDelta, character: p.character };
  const out: Region[] = [];
  for (const region of regions) {
    if (before(region.end, edit.range.start)) {
      out.push(region);
    } else if (before(edit.range.end, region.start)) {
      out.push({ start: move(region.start), end: move(region.end) });
    }
    // Otherwise the edit touches the region: drop it.
  }
  return out;
}

/**
 * Characters that can change what the preprocessor decides: directives and
 * references (`&`, `{`, `}`), comments (`/`, `*`), strings (`"`, `'`) and the
 * escape `~`.
 */
const DECIDING = /[&{}\/*"'~]/;

/**
 * Whether `edit` cannot change a branch decision, so the regions of the text
 * before it still hold for the text after it. Only an insertion qualifies:
 * removed text is not known. Neither the inserted text nor the lines that hold
 * it after the edit (`lineAt`, in the new text) may hold a character that
 * changes a decision.
 */
export function keepsDecisions(edit: Edit, lineAt: (line: number) => string): boolean {
  const { start, end } = edit.range;
  if (start.line !== end.line || start.character !== end.character || DECIDING.test(edit.text)) {
    return false;
  }
  const added = edit.text.split("\n").length - 1;
  for (let line = start.line; line <= start.line + added; line++) {
    if (DECIDING.test(lineAt(line))) {
      return false;
    }
  }
  return true;
}

/**
 * Keeps the last regions of each document with their document version. A
 * lookup for another version gives nothing: regions of older text sit at the
 * wrong lines.
 */
export class RegionStore {
  private readonly entries = new Map<string, { version: number; regions: Region[] }>();

  set(uri: string, version: number, regions: Region[]): void {
    this.entries.set(uri, { version, regions });
  }

  /** The regions of `uri` for `version`, or none. */
  get(uri: string, version: number): Region[] {
    const entry = this.entries.get(uri);
    return entry && entry.version === version ? entry.regions : [];
  }

  /**
   * Carry the regions of `uri` from `from` to `to` over `edits`, so the dimming
   * stays while the server analyses the new text. They are dropped unless
   * there is one edit, it [keeps the decisions](keepsDecisions) (`lineAt` reads
   * the new text), and they are of version `from`.
   */
  edit(uri: string, from: number, to: number, edits: readonly Edit[], lineAt: (line: number) => string): void {
    const entry = this.entries.get(uri);
    if (!entry || entry.version !== from || edits.length !== 1 || !keepsDecisions(edits[0], lineAt)) {
      this.entries.delete(uri);
      return;
    }
    this.entries.set(uri, { version: to, regions: shiftRegions(entry.regions, edits[0]) });
  }

  delete(uri: string): void {
    this.entries.delete(uri);
  }

  clear(): void {
    this.entries.clear();
  }
}
