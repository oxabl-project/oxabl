import * as fs from "node:fs";
import * as path from "node:path";
import { describe, expect, it } from "vitest";

import { resolveServerBinary, serverCandidateNames, splitPath } from "./server";

describe("resolveServerBinary", () => {
  it("returns the configured oxabl.server.path when set (highest precedence)", () => {
    const result = resolveServerBinary({
      configuredPath: "/opt/oxabl/bin/oxabl",
      // A PATH hit exists too, but the setting must win.
      pathDirs: ["/usr/bin"],
      fileExists: () => true,
      platform: "linux",
    });
    expect(result).toEqual({
      kind: "found",
      command: "/opt/oxabl/bin/oxabl",
      source: "setting",
    });
  });

  it("ignores a blank/whitespace setting and falls back to PATH", () => {
    const result = resolveServerBinary({
      configuredPath: "   ",
      pathDirs: ["/nope", "/usr/local/bin"],
      fileExists: (p) => p === path.join("/usr/local/bin", "oxabl"),
      platform: "linux",
    });
    expect(result).toEqual({
      kind: "found",
      command: path.join("/usr/local/bin", "oxabl"),
      source: "path",
    });
  });

  it("falls back to `oxabl` on PATH when the setting is unset", () => {
    const result = resolveServerBinary({
      configuredPath: undefined,
      pathDirs: ["/a", "/b"],
      fileExists: (p) => p === path.join("/b", "oxabl"),
      platform: "linux",
    });
    expect(result).toEqual({ kind: "found", command: path.join("/b", "oxabl"), source: "path" });
  });

  it("reports not-found when neither the setting nor PATH resolves (drives R8)", () => {
    const result = resolveServerBinary({
      configuredPath: "",
      pathDirs: ["/a", "/b"],
      fileExists: () => false,
      platform: "linux",
    });
    expect(result).toEqual({ kind: "not-found" });
  });

  it("looks for a .exe on Windows", () => {
    const result = resolveServerBinary({
      pathDirs: ["C:\\tools"],
      fileExists: (p) => p === path.join("C:\\tools", "oxabl.exe"),
      platform: "win32",
    });
    expect(result.kind).toBe("found");
    if (result.kind === "found") {
      expect(result.command).toContain("oxabl.exe");
      expect(result.source).toBe("path");
    }
  });
});

describe("serverCandidateNames", () => {
  it("is just oxabl on unix", () => {
    expect(serverCandidateNames("linux")).toEqual(["oxabl"]);
  });
  it("includes windows executable extensions on win32", () => {
    expect(serverCandidateNames("win32")).toContain("oxabl.exe");
  });
});

describe("splitPath", () => {
  it("splits on : for unix and drops empties", () => {
    expect(splitPath("/a::/b", "linux")).toEqual(["/a", "/b"]);
  });
  it("splits on ; for windows", () => {
    expect(splitPath("C:\\a;C:\\b", "win32")).toEqual(["C:\\a", "C:\\b"]);
  });
  it("returns [] for an undefined PATH", () => {
    expect(splitPath(undefined, "linux")).toEqual([]);
  });
});

describe("language contribution manifest", () => {
  it("registers the abl language for all five ABL extensions", () => {
    const manifest = JSON.parse(
      fs.readFileSync(path.join(__dirname, "..", "package.json"), "utf8"),
    );
    const abl = manifest.contributes.languages.find((l: { id: string }) => l.id === "abl");
    expect(abl).toBeDefined();
    expect(new Set(abl.extensions)).toEqual(new Set([".p", ".w", ".cls", ".i", ".v"]));
  });
});
