import * as path from "node:path";
import { describe, expect, it } from "vitest";

import { parsePrintConfig, parseUpdateCheck, parseVersionOutput } from "./cli";
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

  it("builds a root of a binary with build.rcode in directory mode as before", () => {
    expect(parsePrintConfig(config({ ...full, "build.rcode": "directory" }), "/w/p")?.ignore).toEqual([
      "/w/p/build/r",
      "/w/p/.oxabl/build",
      "/w/p/build/xref",
    ]);
    expect(parsePrintConfig(config({ ...full, "build.rcode": "directory", "build.output": "" }), "/w/p")).toBeUndefined();
  });

  it("builds a root with r-code beside the source and ignores no source directory", () => {
    const beside = { ...full, "build.output": "", "build.rcode": "beside-source" };
    const ignore = parsePrintConfig(config(beside), "/w/p")?.ignore;
    expect(ignore).toEqual(["/w/p/.oxabl/build", "/w/p/build/xref"]);
    for (const source of ["/w/p/a.p", "/w/p/src/a.p", "/w/p/src/pkg/Foo.cls", "/w/p/build/r/a.p"]) {
      expect(isIgnored(source, ignore!)).toBe(false);
    }
    // An output left in the configuration is not where the r-code goes, so it is no reason to ignore a directory.
    expect(parsePrintConfig(config({ ...beside, "build.output": "src" }), "/w/p")?.ignore).toEqual([
      "/w/p/.oxabl/build",
      "/w/p/build/xref",
    ]);
  });

  it("does not build a root whose r-code place it does not know", () => {
    expect(parsePrintConfig(config({ ...full, "build.rcode": "elsewhere" }), "/w/p")).toBeUndefined();
    expect(parsePrintConfig(config({ ...full, "build.output": "", "build.rcode": "beside-source", "build.workers.command": [] }), "/w/p")).toBeUndefined();
  });
});

describe("parseVersionOutput", () => {
  it("reads oxabl X.Y.Z and nothing else", () => {
    expect(parseVersionOutput("oxabl 5.2.0\n")).toBe("5.2.0");
    expect(parseVersionOutput("oxabl-fake 5.2.0")).toBeUndefined();
  });
});

describe("parseUpdateCheck", () => {
  it("reads the answer of oxabl self-update --check --json", () => {
    expect(parseUpdateCheck('{"installed":"5.3.0","latest":"5.3.1","update":true,"managed":true}\n')).toEqual({
      installed: "5.3.0",
      latest: "5.3.1",
      update: true,
      managed: true,
    });
  });

  it("gives nothing for any other answer, so nothing is installed on a guess", () => {
    expect(parseUpdateCheck("oxabl is up to date (5.3.1)")).toBeUndefined();
    expect(parseUpdateCheck('{"installed":"5.3.0","latest":"5.3.1","update":"yes","managed":true}')).toBeUndefined();
    expect(parseUpdateCheck('{"installed":"5.3.0","update":true,"managed":true}')).toBeUndefined();
    expect(parseUpdateCheck("null")).toBeUndefined();
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
    for (const rcode of ["a.r", "A.R", `src${path.sep}pkg${path.sep}Foo.r`]) {
      expect(isBuildInput(rcode)).toBe(false);
    }
  });
});
