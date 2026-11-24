import { describe, expect, it } from "vitest";

import { RegionStore } from "./regionStore";

describe("RegionStore", () => {
  it("gives the regions of the version they were computed for", () => {
    const store = new RegionStore<string>();
    store.set("file:///a.p", 3, ["r1"]);
    expect(store.get("file:///a.p", 3)).toEqual(["r1"]);
  });

  it("gives nothing for text that changed since", () => {
    const store = new RegionStore<string>();
    store.set("file:///a.p", 3, ["r1"]);
    expect(store.get("file:///a.p", 4)).toEqual([]);
  });

  it("forgets a document and every document", () => {
    const store = new RegionStore<string>();
    store.set("file:///a.p", 1, ["r1"]);
    store.set("file:///b.p", 1, ["r2"]);
    store.delete("file:///a.p");
    expect(store.get("file:///a.p", 1)).toEqual([]);
    store.clear();
    expect(store.get("file:///b.p", 1)).toEqual([]);
  });
});
