import { describe, expect, it } from "vitest";

import { Memory, Once } from "./once";

/** A memento that keeps its values, as the workspace state does between two windows. */
function memory(values = new Map<string, unknown>()): Memory {
  return {
    get: <T>(key: string) => values.get(key) as T | undefined,
    update: (key, value) => Promise.resolve(void values.set(key, value)),
  };
}

describe("Once", () => {
  it("shows a reason once per root, also in a later window", async () => {
    const values = new Map<string, unknown>();
    const first = new Once(memory(values), "unkeyed");
    expect(first.shown("/a", "needs [dlc]")).toBe(false);
    await first.mark("/a", "needs [dlc]");
    expect(first.shown("/a", "needs [dlc]")).toBe(true);

    const later = new Once(memory(values), "unkeyed");
    expect(later.shown("/a", "needs [dlc]")).toBe(true);
  });

  it("shows a new reason, another root and another kind of notice again", async () => {
    const values = new Map<string, unknown>();
    const once = new Once(memory(values), "unkeyed");
    await once.mark("/a", "needs [dlc]");
    expect(once.shown("/a", "class-with-two-propaths")).toBe(false);
    expect(once.shown("/b", "needs [dlc]")).toBe(false);
    expect(new Once(memory(values), "finished").shown("/a", "needs [dlc]")).toBe(false);
  });
});
