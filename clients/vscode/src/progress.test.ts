import { describe, expect, it } from "vitest";

import {
  BuildProgress,
  MessageStore,
  finishedText,
  formatDuration,
  renderStatus,
  shouldWarnUnkeyed,
  unkeyedFromReport,
  unkeyedText,
} from "./progress";

const apply = (p: BuildProgress, ...events: object[]) => events.flatMap((e) => p.apply(e));

describe("BuildProgress", () => {
  it("counts a unit once, also when the retry pass compiles it again", () => {
    const p = new BuildProgress();
    apply(
      p,
      { event: "build-started", rebuild: 3, units: 5 },
      { event: "unit-fetched", path: "a.p", cache: "local" },
      { event: "unit-started", path: "b.p" },
      { event: "unit-finished", path: "b.p", status: "fail" },
      { event: "unit-started", path: "b.p" },
      { event: "unit-finished", path: "b.p", status: "ok" },
    );
    expect(p.done).toBe(2);
    expect(p.failedNow).toBe(0);
    expect(p.cachedPercent).toBe(50);
  });

  it("shows no percent before the first unit", () => {
    const p = new BuildProgress();
    apply(p, { event: "build-started", rebuild: 3, units: 5 });
    expect(p.cachedPercent).toBeUndefined();
    expect(renderStatus([{ phase: "building", progress: p }])?.text).toBe("oxabl 0/3");
  });

  it("shows progress and the cache hit rate", () => {
    const p = new BuildProgress();
    apply(p, { event: "build-started", rebuild: 4, units: 4 });
    for (const path of ["a.p", "b.p", "c.p"]) apply(p, { event: "unit-fetched", path, cache: "remote" });
    apply(p, { event: "unit-finished", path: "d.p", status: "ok" });
    expect(renderStatus([{ phase: "building", progress: p }])?.text).toBe("oxabl 4/4 · 75% cached");
    expect(p.fetchedFrom("remote")).toBe(3);
  });

  it("returns the messages of a unit at its unit-finished and clears at unit-fetched", () => {
    const p = new BuildProgress();
    const effects = apply(
      p,
      { event: "unit-started", path: "a.p" },
      { event: "message", path: "a.p", file: "inc/x.i", row: 3, column: 0, number: 12, text: "bad", type: "error" },
      { event: "unit-finished", path: "a.p", status: "fail" },
      { event: "unit-fetched", path: "b.p", cache: "local" },
    );
    expect(effects).toEqual([
      {
        kind: "messages",
        unit: "a.p",
        messages: [{ type: "error", file: "inc/x.i", row: 3, column: 0, number: 12, text: "bad" }],
      },
      { kind: "clear", unit: "b.p" },
    ]);
  });
});

describe("MessageStore", () => {
  const msg = (file: string, text: string) => ({ type: "error" as const, file, row: 1, column: 1, number: 0, text });

  it("replaces a unit's set and reports every file it touched", () => {
    const store = new MessageStore();
    expect(store.set("a.p", [msg("a.p", "one"), msg("x.i", "two")]).sort()).toEqual(["a.p", "x.i"]);
    expect(store.set("a.p", [msg("a.p", "three")]).sort()).toEqual(["a.p", "x.i"]);
    expect(store.forFile("x.i")).toEqual([]);
    expect(store.forFile("a.p").map((m) => m.text)).toEqual(["three"]);
  });

  it("keeps the messages of two units about one include", () => {
    const store = new MessageStore();
    store.set("a.p", [msg("x.i", "from a")]);
    store.set("b.p", [msg("x.i", "from b")]);
    store.clear("a.p");
    expect(store.forFile("x.i").map((m) => m.text)).toEqual(["from b"]);
  });
});

describe("renderStatus", () => {
  it("shows a failure until a success replaces it, and hides when idle", () => {
    expect(renderStatus([{ phase: "idle", last: { failed: 2 } }])).toMatchObject({
      text: "$(error) oxabl: 2 failed",
      action: "problems",
    });
    expect(renderStatus([{ phase: "idle", last: "ok" }])?.text).toBe("$(check) oxabl");
    expect(renderStatus([{ phase: "idle", last: "none" }])).toBeUndefined();
  });

  it("puts the first line of stderr in the tooltip of a warning", () => {
    expect(renderStatus([{ phase: "idle", last: { warning: "no worker command" } }])).toMatchObject({
      text: "$(warning) oxabl",
      tooltip: "no worker command",
      action: "log",
    });
  });

  it("says when another process took over", () => {
    expect(renderStatus([{ phase: "elsewhere" }])?.text).toContain("build running elsewhere");
  });
});

describe("finishedText", () => {
  it("names the units, the cache share and the time", () => {
    const p = new BuildProgress();
    apply(p, { event: "build-started", rebuild: 10, units: 12 });
    for (let i = 0; i < 8; i++) apply(p, { event: "unit-fetched", path: `${i}.p`, cache: "local" });
    apply(p, { event: "summary", compiled: 2, failed: 0, fetched: 8, exit: 0 });
    expect(finishedText(p, 72_000)).toBe(
      "oxabl build finished in 1m 12s: 10 of 12 units rebuilt, 80% from cache (8 local, 0 remote), 2 compiled",
    );
  });

  it("says so when nothing needed a build", () => {
    const p = new BuildProgress();
    apply(p, { event: "build-started", rebuild: 0, units: 12 }, { event: "summary", compiled: 0, failed: 0, fetched: 0, exit: 0 });
    expect(finishedText(p, 2_000)).toBe("oxabl build finished: 12 units up to date in 2s");
  });

  it("formats durations", () => {
    expect(formatDuration(59_400)).toBe("59s");
    expect(formatDuration(61_000)).toBe("1m 1s");
  });
});

describe("unkeyedFromReport", () => {
  const dlc = "the PROPATH entry $DLC/tty needs [workspace.sources].dlc";
  it("counts units whose plan reason or compile note says they have no key, once each", () => {
    const report = {
      units: [
        { path: "a.p", reason: `unverified: ${dlc}` },
        { path: "b.p", reason: "unkeyable: class-with-two-propaths" },
        { path: "c.p", reason: "source-changed" },
      ],
      notes: [
        `a.p: no key from what the compiler read: ${dlc}; the unit compiles at each build`,
        `d.p: no key from what the compiler read: ${dlc}; the unit compiles at each build`,
        "local: 3 sets offered, 3 stored",
      ],
    };
    expect(unkeyedFromReport(JSON.stringify(report))).toEqual({ count: 3, reason: dlc, reasonCount: 2 });
  });

  it("is undefined when every unit has a key or the report is not JSON", () => {
    expect(unkeyedFromReport(JSON.stringify({ units: [{ path: "a.p", reason: "new" }], notes: [] }))).toBeUndefined();
    expect(unkeyedFromReport("not json")).toBeUndefined();
  });

  it("tells how to finish the setup when the reason is the DLC copy", () => {
    const text = unkeyedText({ count: 2143, reason: dlc, reasonCount: 2143 });
    expect(text).toContain("2,143 units have no compile key");
    expect(text).toContain(dlc);
    expect(text).toContain("OXABL_DLC_SOURCES");
  });
});

describe("shouldWarnUnkeyed", () => {
  it("warns for a missing setting, or a tenth of the workspace, not for a few encrypted units", () => {
    const missing = { count: 3, reason: "the PROPATH entry $DLC/tty needs [workspace.sources].dlc", reasonCount: 3 };
    const encrypted = { count: 3, reason: "ms/encryptor.p looks encrypted, and the compiler does not report the includes of encrypted source", reasonCount: 1 };
    expect(shouldWarnUnkeyed(missing, 10_300)).toBe(true);
    expect(shouldWarnUnkeyed(encrypted, 10_300)).toBe(false);
    expect(shouldWarnUnkeyed({ ...encrypted, count: 1_030 }, 10_300)).toBe(true);
  });
});

describe("the building tooltip", () => {
  it("splits this build from the workspace", () => {
    const p = new BuildProgress();
    apply(p, { event: "build-started", rebuild: 2578, units: 10300 }, { event: "unit-fetched", path: "a.p", cache: "remote" });
    const tooltip = renderStatus([{ phase: "building", progress: p, unkeyed: 3 }])?.tooltip ?? "";
    expect(tooltip).toContain("This build: 2,578 units to rebuild, 1 done");
    expect(tooltip).toContain("from the remote cache: 1");
    expect(tooltip).toContain("Workspace: 10,300 units, 7,722 up to date (75%), not rebuilt");
    expect(tooltip).toContain("3 units have no compile key");
  });
});
