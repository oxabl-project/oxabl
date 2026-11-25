// The command that a reference-count lens runs. Free of any `vscode` import, so the reading of its
// arguments is a pure function testable under vitest.

/** The id of the command; the server names it in each resolved lens. */
export const SHOW_REFERENCES = "oxabl.showReferences";

export interface LensPosition {
  line: number;
  character: number;
}

/** A place a lens counts, as the server sends it (an LSP `Location`). */
export interface LensPlace {
  uri: string;
  start: LensPosition;
  end: LensPosition;
}

const isPosition = (value: unknown): value is LensPosition => {
  const p = value as LensPosition | undefined;
  return !!p && Number.isInteger(p.line) && Number.isInteger(p.character);
};

/**
 * The places of a lens command argument. The list is what the lens counts, so a malformed entry is
 * an error, not a place to leave out: a shorter list would not match the number on the lens.
 */
export function readPlaces(raw: unknown): LensPlace[] {
  if (!Array.isArray(raw)) {
    throw new Error("the lens carries no list of places");
  }
  return raw.map((entry) => {
    const { uri, range } = (entry ?? {}) as { uri?: unknown; range?: { start?: unknown; end?: unknown } };
    if (typeof uri !== "string" || !isPosition(range?.start) || !isPosition(range?.end)) {
      throw new Error("the lens carries a malformed place");
    }
    return { uri, start: range.start, end: range.end };
  });
}
