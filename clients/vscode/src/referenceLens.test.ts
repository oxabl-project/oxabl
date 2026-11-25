import { describe, expect, it } from "vitest";

import { SHOW_REFERENCES, withoutDeclaration } from "./referenceLens";

interface Pos {
  line: number;
  character: number;
}

/** A place in `uri` that spans `from` to `to` on one line. */
function place(uri: string, line: number, from: number, to: number) {
  return {
    uri: { toString: () => uri },
    range: {
      contains: (p: Pos) => p.line === line && p.character >= from && p.character <= to,
    },
  };
}

describe("withoutDeclaration", () => {
  const at = { line: 3, character: 10 };

  it("drops the place that holds the declaration and keeps every use", () => {
    const declaration = place("file:///a.p", 3, 10, 16);
    const use = place("file:///a.p", 8, 4, 10);
    const other = place("file:///b.p", 3, 10, 16);
    expect(withoutDeclaration([declaration, use, other], "file:///a.p", at)).toEqual([use, other]);
  });

  it("keeps all places when none holds the position", () => {
    const uses = [place("file:///a.p", 1, 0, 4), place("file:///b.p", 2, 0, 4)];
    expect(withoutDeclaration(uses, "file:///a.p", at)).toEqual(uses);
  });

  it("names the command that the server puts in a lens", () => {
    expect(SHOW_REFERENCES).toBe("oxabl.showReferences");
  });
});
