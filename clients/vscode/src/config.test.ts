import * as fs from "node:fs";
import * as path from "node:path";
import { describe, expect, it } from "vitest";

import { RESTART_CONFIG_KEYS, shouldRestartForConfigChange } from "./config";

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
