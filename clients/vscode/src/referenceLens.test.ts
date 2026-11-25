import { describe, expect, it } from "vitest";

import { SHOW_REFERENCES, readPlaces } from "./referenceLens";

const place = (uri: string, line: number, from: number, to: number) => ({
  uri,
  range: { start: { line, character: from }, end: { line, character: to } },
});

describe("readPlaces", () => {
  it("reads every place of the list, in order", () => {
    expect(readPlaces([place("file:///a.p", 3, 10, 16), place("file:///b.p", 0, 0, 4)])).toEqual([
      { uri: "file:///a.p", start: { line: 3, character: 10 }, end: { line: 3, character: 16 } },
      { uri: "file:///b.p", start: { line: 0, character: 0 }, end: { line: 0, character: 4 } },
    ]);
  });

  it("reads an empty list", () => {
    expect(readPlaces([])).toEqual([]);
  });

  it("refuses a missing list and a malformed place, never a shorter list", () => {
    expect(() => readPlaces(undefined)).toThrow(/no list/);
    expect(() => readPlaces([place("file:///a.p", 1, 0, 4), { uri: "file:///b.p" }])).toThrow(/malformed/);
    expect(() => readPlaces([null])).toThrow(/malformed/);
    expect(() => readPlaces([{ ...place("file:///a.p", 1, 0, 4), uri: 7 }])).toThrow(/malformed/);
  });

  it("names the command that the server puts in a lens", () => {
    expect(SHOW_REFERENCES).toBe("oxabl.showReferences");
  });
});
