import { describe, expect, it } from "vitest";

import { type Region, RegionStore, shiftRegions } from "./regionStore";

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
    store.edit("file:///a.p", 3, 4, [{ range: r(3, 0, 3, 0), text: "x.\n" }]);
    expect(store.get("file:///a.p", 4)).toEqual([r(1, 0, 2, 0), r(6, 0, 7, 0)]);
  });

  it("drops the regions of another version or of several edits", () => {
    const store = new RegionStore();
    store.set("file:///a.p", 3, [r(1, 0, 2, 0)]);
    store.edit("file:///a.p", 2, 3, [{ range: r(0, 0, 0, 0), text: "x" }]);
    expect(store.get("file:///a.p", 3)).toEqual([]);
    store.set("file:///a.p", 3, [r(1, 0, 2, 0)]);
    const two = [
      { range: r(0, 0, 0, 0), text: "x" },
      { range: r(4, 0, 4, 0), text: "y" },
    ];
    store.edit("file:///a.p", 3, 4, two);
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
