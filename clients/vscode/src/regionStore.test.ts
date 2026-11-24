import { describe, expect, it } from "vitest";

import { type Region, RegionStore, keepsDecisions, shiftRegions } from "./regionStore";

/** The new text, as `lineAt` reads it. */
const lines = (...text: string[]) => (line: number) => text[line] ?? "";
const plain = lines("MESSAGE 1.", "x", "y", "x.", "z", "w", "v");

const r = (sl: number, sc: number, el: number, ec: number): Region => ({
  start: { line: sl, character: sc },
  end: { line: el, character: ec },
});

describe("RegionStore", () => {
  it("gives the regions of the version they were computed for", () => {
    const store = new RegionStore();
    store.set("file:///a.p", 3, [r(1, 0, 2, 0)]);
    expect(store.get("file:///a.p", 3)).toEqual([r(1, 0, 2, 0)]);
  });

  it("gives nothing for text that changed since", () => {
    const store = new RegionStore();
    store.set("file:///a.p", 3, [r(1, 0, 2, 0)]);
    expect(store.get("file:///a.p", 4)).toEqual([]);
  });

  it("carries the regions over one edit to the next version", () => {
    const store = new RegionStore();
    store.set("file:///a.p", 3, [r(1, 0, 2, 0), r(5, 0, 6, 0)]);
    // A new line typed at the start of line 3.
    store.edit("file:///a.p", 3, 4, [{ range: r(3, 0, 3, 0), text: "x.\n" }], plain);
    expect(store.get("file:///a.p", 4)).toEqual([r(1, 0, 2, 0), r(6, 0, 7, 0)]);
  });

  it("drops the regions of another version or of several edits", () => {
    const store = new RegionStore();
    store.set("file:///a.p", 3, [r(1, 0, 2, 0)]);
    store.edit("file:///a.p", 2, 3, [{ range: r(0, 0, 0, 0), text: "x" }], plain);
    expect(store.get("file:///a.p", 3)).toEqual([]);
    store.set("file:///a.p", 3, [r(1, 0, 2, 0)]);
    const two = [
      { range: r(0, 0, 0, 0), text: "x" },
      { range: r(4, 0, 4, 0), text: "y" },
    ];
    store.edit("file:///a.p", 3, 4, two, plain);
    expect(store.get("file:///a.p", 4)).toEqual([]);
  });

  it("drops the regions on an edit of a directive line above them", () => {
    const store = new RegionStore();
    store.set("file:///a.p", 3, [r(3, 0, 4, 0)]);
    // `&SCOPED-DEFINE mode fast` becomes `fastx`: the branch below may compile.
    const after = lines("&SCOPED-DEFINE mode fastx", "&IF \"{&mode}\" = \"fast\" &THEN", "&ELSE", "MESSAGE 1.");
    store.edit("file:///a.p", 3, 4, [{ range: r(0, 24, 0, 24), text: "x" }], after);
    expect(store.get("file:///a.p", 4)).toEqual([]);
  });

  it("drops the regions on a removal, which may have held a directive", () => {
    const store = new RegionStore();
    store.set("file:///a.p", 3, [r(5, 0, 6, 0)]);
    store.edit("file:///a.p", 3, 4, [{ range: r(1, 0, 2, 0), text: "" }], plain);
    expect(store.get("file:///a.p", 4)).toEqual([]);
  });

  it("forgets a document and every document", () => {
    const store = new RegionStore();
    store.set("file:///a.p", 1, [r(1, 0, 2, 0)]);
    store.set("file:///b.p", 1, [r(1, 0, 2, 0)]);
    store.delete("file:///a.p");
    expect(store.get("file:///a.p", 1)).toEqual([]);
    store.clear();
    expect(store.get("file:///b.p", 1)).toEqual([]);
  });
});

describe("shiftRegions", () => {
  it("drops a region the edit touches", () => {
    expect(shiftRegions([r(1, 0, 3, 0)], { range: r(2, 4, 2, 4), text: "z" })).toEqual([]);
  });

  it("keeps a region before the edit and moves one after it", () => {
    const regions = [r(0, 0, 1, 0), r(4, 2, 4, 8)];
    // Line 2 removed, and text joined onto line 3's start.
    expect(shiftRegions(regions, { range: r(2, 0, 3, 0), text: "" })).toEqual([r(0, 0, 1, 0), r(3, 2, 3, 8)]);
  });

  it("moves a region on the last line of the edit by the change in columns", () => {
    expect(shiftRegions([r(2, 6, 2, 9)], { range: r(2, 1, 2, 3), text: "abcd" })).toEqual([r(2, 8, 2, 11)]);
  });
});

describe("keepsDecisions", () => {
  it("keeps plain text typed on a plain line", () => {
    expect(keepsDecisions({ range: r(1, 1, 1, 1), text: "abc" }, plain)).toBe(true);
  });

  it("refuses text that can open a comment, a string or a reference", () => {
    for (const text of ["/", "*", '"', "'", "~", "{", "}", "&"]) {
      expect(keepsDecisions({ range: r(1, 1, 1, 1), text }, plain), text).toBe(false);
    }
  });

  it("refuses an insertion on a line that holds one", () => {
    // Typing between `/` and `*` breaks a comment opener.
    expect(keepsDecisions({ range: r(0, 1, 0, 1), text: "x" }, lines("/x* note */"))).toBe(false);
  });
});
