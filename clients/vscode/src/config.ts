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

/** The inlay hint kinds, each turned off on its own by `oxabl.inlayHints.<kind>`. */
export const INLAY_HINT_KINDS = ["locks", "index", "scope", "transactions"] as const;

export type InlayHintKind = (typeof INLAY_HINT_KINDS)[number];

/**
 * The `inlayHints` object the server reads from its initialization options and
 * from `workspace/didChangeConfiguration`. `get` reads one
 * `oxabl.inlayHints.<kind>` setting; a kind that is not set is on.
 */
export function inlayHintSettings(
  get: (kind: InlayHintKind) => boolean | undefined,
): Record<InlayHintKind, boolean> {
  const settings = {} as Record<InlayHintKind, boolean>;
  for (const kind of INLAY_HINT_KINDS) {
    settings[kind] = get(kind) ?? true;
  }
  return settings;
}

/**
 * The `semanticTokens` object the server reads from its initialization options
 * and from `workspace/didChangeConfiguration`. `enable` is the value of
 * `oxabl.semanticTokens.enable`; a setting that is not set is on.
 */
export function semanticTokenSettings(enable: boolean | undefined): { enable: boolean } {
  return { enable: enable ?? true };
}
