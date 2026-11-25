// The command that a reference-count lens runs. Free of any `vscode` import, so the filter is a
// pure function testable under vitest.

/** The id of the command; the server names it in each resolved lens. */
export const SHOW_REFERENCES = "oxabl.showReferences";

/** The part of a `vscode.Location` the filter reads. */
export interface Place<P> {
  uri: { toString(): string };
  range: { contains(position: P): boolean };
}

/**
 * The places of `found` that are not the declaration at `position` in `uri`. A lens counts the
 * uses of a symbol, and the reference provider also lists its declaration.
 */
export function withoutDeclaration<P, L extends Place<P>>(found: L[], uri: string, position: P): L[] {
  return found.filter((place) => !(place.uri.toString() === uri && place.range.contains(position)));
}
