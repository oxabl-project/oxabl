import * as path from "node:path";
import { describe, expect, it } from "vitest";

import { modeOf, ownerOf, owningFolders } from "./folders";

const folder = (name: string, ...parts: string[]) => ({ name, fsPath: path.join(path.sep, ...parts) });

const code = folder("code", "x", "code");
const code2 = folder("code2", "x", "code2");
const inner = folder("inner", "x", "code", "libs", "inner");

describe("modeOf", () => {
  it("serves a window of zero or one folder with one client", () => {
    expect(modeOf(0)).toBe("single");
    expect(modeOf(1)).toBe("single");
  });

  it("gives each folder of a window of two or more its own client", () => {
    expect(modeOf(2)).toBe("multi");
    expect(modeOf(5)).toBe("multi");
  });
});

describe("owningFolders", () => {
  it("keeps every folder of a window with unrelated folders", () => {
    expect(owningFolders([code, code2])).toEqual([code, code2]);
  });

  it("takes the folder nested in another folder out, in either order", () => {
    expect(owningFolders([code, inner])).toEqual([code]);
    expect(owningFolders([inner, code])).toEqual([code]);
  });

  it("does not take a sibling with a common prefix for a nested folder", () => {
    expect(owningFolders([code, code2])).toHaveLength(2);
    expect(owningFolders([code2, code])).toHaveLength(2);
  });

  it("keeps the first of two folders with the same path", () => {
    const again = { ...code, name: "again" };
    expect(owningFolders([code, again])).toEqual([code]);
    expect(owningFolders([again, code])).toEqual([again]);
  });

  it("handles one folder and none", () => {
    expect(owningFolders([code])).toEqual([code]);
    expect(owningFolders([])).toEqual([]);
  });
});

describe("ownerOf", () => {
  const owning = owningFolders([code, code2, inner]);

  it("gives the outermost folder for a file in a nested folder", () => {
    expect(ownerOf(path.join(inner.fsPath, "a.p"), owning)).toBe(code);
  });

  it("does not give /x/code the files of /x/code2", () => {
    expect(ownerOf(path.join(code2.fsPath, "a.p"), owning)).toBe(code2);
  });

  it("gives nothing for a file outside every folder", () => {
    expect(ownerOf(path.join(path.sep, "x", "other", "a.p"), owning)).toBeUndefined();
    expect(ownerOf(path.join(path.sep, "x", "codeish.p"), owning)).toBeUndefined();
  });

  it("gives nothing when there is no folder", () => {
    expect(ownerOf(path.join(code.fsPath, "a.p"), [])).toBeUndefined();
  });
});
