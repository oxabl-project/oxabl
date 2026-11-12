import * as path from "node:path";
import { describe, expect, it } from "vitest";

import { parsePrintConfig, parseVersionOutput } from "./cli";
import { isBuildInput, isIgnored, ownerRoot } from "./roots";

const config = (settings: Record<string, unknown>) =>
  JSON.stringify({
    settings: Object.fromEntries(Object.entries(settings).map(([k, value]) => [k, { value, source: "default" }])),
  });

describe("parsePrintConfig", () => {
  const full = {
    "build.output": "build/r",
    "build.state": ".oxabl/build",
    "build.store": "",
    "build.outputs.xref": "build/xref",
    "build.workers.command": ["docker", "exec", "{program}"],
  };

  it("lists the directories the build writes, as absolute paths", () => {
    expect(parsePrintConfig(config(full), "/w/p")?.ignore).toEqual([
      "/w/p/build/r",
      "/w/p/.oxabl/build",
      "/w/p/build/xref",
    ]);
  });

  it("is not a build root without an output or a worker command", () => {
    expect(parsePrintConfig(config({ ...full, "build.output": "" }), "/w/p")).toBeUndefined();
    expect(parsePrintConfig(config({ ...full, "build.workers.command": [] }), "/w/p")).toBeUndefined();
    expect(parsePrintConfig("not json", "/w/p")).toBeUndefined();
  });
});

describe("parseVersionOutput", () => {
  it("reads oxabl X.Y.Z and nothing else", () => {
    expect(parseVersionOutput("oxabl 5.2.0\n")).toBe("5.2.0");
    expect(parseVersionOutput("oxabl-fake 5.2.0")).toBeUndefined();
  });
});

describe("roots", () => {
  it("maps a path to the root with the longest prefix", () => {
    expect(ownerRoot("/w/a/b/x.p", ["/w/a", "/w/a/b", "/w/c"])).toBe("/w/a/b");
    expect(ownerRoot("/w/ab/x.p", ["/w/a"])).toBeUndefined();
  });

  it("ignores .git and the output and state directories, not .oxabl/cache.toml", () => {
    const ignore = ["/w/p/build/r", "/w/p/.oxabl/build"];
    expect(isIgnored("/w/p/.git/x.p", ignore)).toBe(true);
    expect(isIgnored("/w/p/build/r/a.p", ignore)).toBe(true);
    expect(isIgnored("/w/p/.oxabl/build/state.db", ignore)).toBe(true);
    expect(isIgnored("/w/p/.oxabl/cache.toml", ignore)).toBe(false);
    expect(isIgnored("/w/p/src/a.p", ignore)).toBe(false);
  });

  it("takes sources, schemas and the two configuration files as build inputs", () => {
    for (const f of ["a.p", "a.W", "a.cls", "a.i", "a.v", "s.df", "oxabl.toml", `${path.sep}w${path.sep}.oxabl${path.sep}cache.toml`]) {
      expect(isBuildInput(f)).toBe(true);
    }
    expect(isBuildInput("README.md")).toBe(false);
    expect(isBuildInput("a.r")).toBe(false);
  });
});
