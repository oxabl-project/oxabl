import { describe, expect, it } from "vitest";

import { isSuperMethodAnswer, planSuperMethod } from "./superMethodModel";

const place = (uri: string) => ({ uri, range: {} });

describe("planSuperMethod", () => {
  it("opens the one method that was found", () => {
    expect(planSuperMethod({ locations: [place("file:///a.cls")], gaps: [] })).toEqual({ action: "open" });
  });

  it("lists several methods", () => {
    expect(planSuperMethod({ locations: [place("file:///a.cls"), place("file:///b.cls")], gaps: [] })).toEqual({ action: "peek" });
  });

  it("says that a method overrides nothing only when the answer has no gap", () => {
    expect(planSuperMethod({ locations: [], gaps: [] })).toEqual({
      action: "none",
      message: { level: "info", text: "oxabl: this method does not override or implement another." },
    });
  });

  it("never turns an empty answer with a gap into 'overrides nothing'", () => {
    const plan = planSuperMethod({ locations: [], gaps: ["`app.Heir` names `app.Gone`, which oxabl cannot resolve"] });
    expect(plan.action).toBe("none");
    expect(plan.message?.level).toBe("warning");
    expect(plan.message?.text).toContain("`app.Gone`");
    expect(plan.message?.text).not.toContain("does not override");
  });

  it("warns when a list with a method can be short", () => {
    const plan = planSuperMethod({ locations: [place("file:///a.cls")], gaps: ["one"] });
    expect(plan.action).toBe("open");
    expect(plan.message).toEqual({ level: "warning", text: "oxabl: the super method found. The list can be short: one." });
  });
});

describe("isSuperMethodAnswer", () => {
  it("accepts the answer of the server", () => {
    expect(isSuperMethodAnswer({ locations: [], gaps: [] })).toBe(true);
  });

  it("refuses what is not an answer", () => {
    expect(isSuperMethodAnswer(null)).toBe(false);
    expect(isSuperMethodAnswer([])).toBe(false);
    expect(isSuperMethodAnswer({ locations: [] })).toBe(false);
    expect(isSuperMethodAnswer({ locations: [], gaps: [1] })).toBe(false);
  });
});
