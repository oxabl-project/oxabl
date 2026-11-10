// Self-update of the extension from GitHub Releases. It is not on the
// Marketplace, so VS Code never updates it. The network and verification
// logic is in `update.ts`; this module holds the settings, the prompts and the
// install.

import * as fs from "node:fs";
import * as vscode from "vscode";

import {
  DEFAULT_API_BASE,
  UpdateError,
  downloadVerified,
  findUpdate,
  type AvailableUpdate,
  type UpdateEnv,
} from "./update";

const LAST_CHECK_KEY = "oxabl.updates.lastCheck";
const SKIPPED_KEY = "oxabl.updates.skipped";
const CHECK_INTERVAL_MS = 24 * 60 * 60 * 1000;
const RELEASE_URL = "https://github.com/oxabl-project/oxabl/releases/tag/oxabl_vscode-v";

type Mode = "auto" | "notify" | "off";

/** Whether the person asked for the check (a manual one reports every outcome). */
type Trigger = "automatic" | "manual";

let busy = false;

/** Register the check command and start the automatic check without awaiting it. */
export function registerUpdates(context: vscode.ExtensionContext, log: vscode.LogOutputChannel): void {
  context.subscriptions.push(
    vscode.commands.registerCommand("oxabl.checkForUpdates", () =>
      checkForUpdates(context, log, "manual"),
    ),
  );
  if (context.extensionMode === vscode.ExtensionMode.Production && currentMode() !== "off") {
    void checkForUpdates(context, log, "automatic");
  }
}

function currentMode(): Mode {
  const configured = vscode.workspace.getConfiguration("oxabl").get<string>("updates", "auto");
  const mode: Mode = configured === "notify" || configured === "off" ? configured : "auto";
  // Without VS Code's own extension auto-update, install nothing unasked.
  const autoUpdate = vscode.workspace.getConfiguration("extensions").get<unknown>("autoUpdate");
  return mode === "auto" && autoUpdate === false ? "notify" : mode;
}

function updateEnv(context: vscode.ExtensionContext): UpdateEnv {
  const override = process.env.OXABL_UPDATE_API_URL;
  return {
    apiBase: (override || DEFAULT_API_BASE).replace(/\/+$/, ""),
    allowHttp: Boolean(override),
    installedVersion: context.extension.packageJSON.version as string,
  };
}

async function checkForUpdates(
  context: vscode.ExtensionContext,
  log: vscode.LogOutputChannel,
  trigger: Trigger,
): Promise<void> {
  const manual = trigger === "manual";
  if (busy) {
    log.info("update check skipped: another one is running");
    if (manual) {
      void vscode.window.showInformationMessage("oxabl: an update check is already running.");
    }
    return;
  }

  if (!manual) {
    const last = context.globalState.get<number>(LAST_CHECK_KEY);
    const now = Date.now();
    // A stamp in the future (clock change) does not count as a recent check.
    if (typeof last === "number" && last <= now && now - last < CHECK_INTERVAL_MS) {
      return;
    }
  }

  busy = true;
  try {
    // Written before the network call: a failing check is not retried at once.
    await context.globalState.update(LAST_CHECK_KEY, Date.now());
    const env = updateEnv(context);
    const installed = env.installedVersion;
    const mode = currentMode();
    log.info(`update check (${trigger}, mode ${mode}), installed v${installed}`);

    const update = await findUpdate(installed, env, (m) => log.info(m));
    if (!update) {
      log.info("decision: up to date");
      if (manual) {
        void vscode.window.showInformationMessage(`oxabl extension is up to date (v${installed}).`);
      }
      return;
    }

    if (!manual && context.globalState.get<string>(SKIPPED_KEY) === update.version) {
      log.info(`decision: v${update.version} was skipped`);
      return;
    }

    // A manual check in `off` mode offers the update instead of installing it.
    if (mode === "auto") {
      log.info(`decision: install v${update.version}`);
      await installUpdate(context, log, env, update);
    } else {
      log.info(`decision: offer v${update.version}`);
      offerUpdate(context, log, env, update);
    }
  } catch (err) {
    const reason = failureReason(err);
    log.error(`update check failed: ${reason}`);
    if (manual) {
      void vscode.window.showErrorMessage(`oxabl: could not check for updates: ${reason}`);
    }
  } finally {
    busy = false;
  }
}

function failureReason(err: unknown): string {
  return err instanceof UpdateError || err instanceof Error ? err.message : String(err);
}

/** Show the `notify` prompt without holding the check open while it waits. */
function offerUpdate(
  context: vscode.ExtensionContext,
  log: vscode.LogOutputChannel,
  env: UpdateEnv,
  update: AvailableUpdate,
): void {
  void (async () => {
    const choice = await vscode.window.showInformationMessage(
      `oxabl extension v${update.version} is available (installed: v${env.installedVersion}).`,
      "Update",
      "Release Notes",
      "Skip This Version",
    );
    if (choice === "Update") {
      if (busy) {
        void vscode.window.showInformationMessage("oxabl: an update check is already running.");
        return;
      }
      busy = true;
      try {
        await installUpdate(context, log, env, update);
      } catch (err) {
        const reason = failureReason(err);
        log.error(`update failed: ${reason}`);
        void vscode.window.showErrorMessage(`oxabl: could not check for updates: ${reason}`);
      } finally {
        busy = false;
      }
    } else if (choice === "Release Notes") {
      openReleaseNotes(update.version);
    } else if (choice === "Skip This Version") {
      log.info(`skipping v${update.version}`);
      await context.globalState.update(SKIPPED_KEY, update.version);
    }
  })();
}

/** Download, verify and install. Throws on any failure; the VSIX is always removed. */
async function installUpdate(
  context: vscode.ExtensionContext,
  log: vscode.LogOutputChannel,
  env: UpdateEnv,
  update: AvailableUpdate,
): Promise<void> {
  const dir = vscode.Uri.joinPath(context.globalStorageUri, "updates").fsPath;
  const file = await downloadVerified(update, env, dir);
  log.info(`verified ${update.vsixName}`);
  try {
    await vscode.commands.executeCommand(
      "workbench.extensions.installExtension",
      vscode.Uri.file(file),
    );
  } finally {
    await fs.promises.rm(file, { force: true });
  }
  log.info(`installed v${update.version}`);

  void vscode.window
    .showInformationMessage(
      `oxabl extension updated to v${update.version}. Reload the window to use it.`,
      "Reload Window",
      "Release Notes",
    )
    .then((choice) => {
      if (choice === "Reload Window") {
        void vscode.commands.executeCommand("workbench.action.reloadWindow");
      } else if (choice === "Release Notes") {
        openReleaseNotes(update.version);
      }
    });
}

function openReleaseNotes(version: string): void {
  void vscode.env.openExternal(vscode.Uri.parse(`${RELEASE_URL}${version}`));
}
