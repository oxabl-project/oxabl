// Configuration helpers for the oxabl client.
//
// Like `server.ts`, this module is deliberately free of any `vscode` import so
// the logic is a pure function testable under vitest without an extension host.

/** The `oxabl.*` settings that only take effect on a (re)start of the server. */
export const RESTART_CONFIG_KEYS = [
  "oxabl.enable",
  "oxabl.server.path",
  "oxabl.trace.server",
] as const;

/**
 * Whether a configuration change touched a setting that only applies on
 * restart. The caller passes VS Code's `event.affectsConfiguration`.
 */
export function shouldRestartForConfigChange(
  affectsConfiguration: (section: string) => boolean,
): boolean {
  return RESTART_CONFIG_KEYS.some((key) => affectsConfiguration(key));
}
