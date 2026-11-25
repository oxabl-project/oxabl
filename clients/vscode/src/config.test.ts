import * as fs from "node:fs";
import * as path from "node:path";
import { describe, expect, it } from "vitest";

import {
  INLAY_HINT_KINDS,
  RESTART_CONFIG_KEYS,
  inlayHintSettings,
  semanticTokenSettings,
  shouldRestartForConfigChange,
} from "./config";

describe("shouldRestartForConfigChange", () => {
  it("restarts when oxabl.enable changes", () => {
    expect(shouldRestartForConfigChange((s) => s === "oxabl.enable")).toBe(true);
  });

  it("restarts when oxabl.server.path changes", () => {
    expect(shouldRestartForConfigChange((s) => s === "oxabl.server.path")).toBe(true);
  });

  it("restarts when oxabl.trace.server changes", () => {
    expect(shouldRestartForConfigChange((s) => s === "oxabl.trace.server")).toBe(true);
  });

  it("does not restart for an unrelated setting change", () => {
    expect(shouldRestartForConfigChange((s) => s === "editor.fontSize")).toBe(false);
  });

  it("does not restart when nothing was affected", () => {
    expect(shouldRestartForConfigChange(() => false)).toBe(false);
  });

  it("matches VS Code's prefix semantics (a parent section affects its keys)", () => {
    // affectsConfiguration("oxabl") returns true for any oxabl.* change.
    expect(shouldRestartForConfigChange((s) => s === "oxabl")).toBe(false);
    // But the real event returns true for exact keys we ask about.
    const affected = new Set(["oxabl.server.path"]);
    expect(shouldRestartForConfigChange((s) => affected.has(s))).toBe(true);
  });
});

describe("restart-server command manifest", () => {
  const manifest = JSON.parse(
    fs.readFileSync(path.join(__dirname, "..", "package.json"), "utf8"),
  );

  it("contributes the oxabl.restartServer command with its palette title", () => {
    const commands: { command: string; title: string }[] = manifest.contributes.commands ?? [];
    const restart = commands.find((c) => c.command === "oxabl.restartServer");
    expect(restart).toBeDefined();
    expect(restart?.title).toBe("oxabl: Restart Server");
  });

  it("activates on the restart command so it works without an open ABL file", () => {
    expect(manifest.activationEvents).toContain("onCommand:oxabl.restartServer");
  });

  it("covers every restart-triggering key with a real setting definition", () => {
    const props = manifest.contributes.configuration.properties as Record<string, unknown>;
    for (const key of RESTART_CONFIG_KEYS) {
      expect(props[key]).toBeDefined();
    }
  });
});

describe("inlayHintSettings", () => {
  it("turns every kind on when nothing is set", () => {
    expect(inlayHintSettings(() => undefined)).toEqual({
      locks: true,
      index: true,
      scope: true,
      transactions: true,
      parameters: true,
    });
  });

  it("turns one kind off on its own", () => {
    expect(inlayHintSettings((kind) => (kind === "index" ? false : undefined))).toEqual({
      locks: true,
      index: false,
      scope: true,
      transactions: true,
      parameters: true,
    });
  });

  it("turns the parameter-name hints off on their own", () => {
    expect(inlayHintSettings((kind) => (kind === "parameters" ? false : undefined))).toEqual({
      locks: true,
      index: true,
      scope: true,
      transactions: true,
      parameters: false,
    });
  });

  it("does not restart the server", () => {
    expect(shouldRestartForConfigChange((s) => s.startsWith("oxabl.inlayHints"))).toBe(false);
  });

  it("has a boolean setting for each kind, on by default", () => {
    const manifest = JSON.parse(fs.readFileSync(path.join(__dirname, "..", "package.json"), "utf8"));
    const props = manifest.contributes.configuration.properties as Record<
      string,
      { type: string; default: unknown }
    >;
    for (const kind of INLAY_HINT_KINDS) {
      expect(props[`oxabl.inlayHints.${kind}`]).toMatchObject({ type: "boolean", default: true });
    }
  });
});

describe("semanticTokenSettings", () => {
  it("is on when the setting is not set", () => {
    expect(semanticTokenSettings(undefined)).toEqual({ enable: true });
  });

  it("follows the setting", () => {
    expect(semanticTokenSettings(false)).toEqual({ enable: false });
    expect(semanticTokenSettings(true)).toEqual({ enable: true });
  });

  it("does not restart the server", () => {
    expect(shouldRestartForConfigChange((s) => s.startsWith("oxabl.semanticTokens"))).toBe(false);
  });

  it("has a boolean setting, on by default, and types for the names the server gives", () => {
    const manifest = JSON.parse(fs.readFileSync(path.join(__dirname, "..", "package.json"), "utf8"));
    expect(manifest.contributes.configuration.properties["oxabl.semanticTokens.enable"]).toMatchObject({
      type: "boolean",
      default: true,
    });
    const types = manifest.contributes.semanticTokenTypes as { id: string; superType: string }[];
    expect(Object.fromEntries(types.map((t) => [t.id, t.superType]))).toEqual({
      table: "type",
      tempTable: "type",
      buffer: "variable",
      dataset: "type",
      databaseField: "property",
      tempTableField: "property",
    });
  });
});
