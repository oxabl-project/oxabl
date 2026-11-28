import * as path from "node:path";
import { describe, expect, it } from "vitest";

import { isInside } from "./roots";

const at = (...parts: string[]) => path.join(path.sep, ...parts);

describe("isInside", () => {
  it("holds the directory itself and what is below it", () => {
    expect(isInside(at("w", "alpha"), at("w", "alpha"))).toBe(true);
    expect(isInside(at("w", "alpha", "src", "a.p"), at("w", "alpha"))).toBe(true);
  });

  it("holds a child whose name starts with two dots", () => {
    expect(isInside(at("w", "alpha", "..cache", "a.p"), at("w", "alpha"))).toBe(true);
    expect(isInside(at("w", "alpha", "..."), at("w", "alpha"))).toBe(true);
  });

  it("does not hold the parent, a sibling or a sibling with a common prefix", () => {
    expect(isInside(at("w"), at("w", "alpha"))).toBe(false);
    expect(isInside(at("w", "beta", "a.p"), at("w", "alpha"))).toBe(false);
    expect(isInside(at("w", "alpha2", "a.p"), at("w", "alpha"))).toBe(false);
  });
});
