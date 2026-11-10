// Self-update of the extension from GitHub Releases. It is not on the
// Marketplace, so VS Code never updates it. The network and verification
// logic is in `update.ts`; this module holds the settings, the prompts and the
// install.

import * as fs from "node:fs";
import * as path from "node:path";
import * as vscode from "vscode";

import {
  DEFAULT_API_BASE,
  NotReadyError,
  UpdateError,
  downloadVerified,
  findUpdate,
  highestInstalledVersion,
  isLoopbackHost,
  isPendingReloadSpent,
  type AvailableUpdate,
  type UpdateEnv,
} from "./update";

const LAST_CHECK_KEY = "oxabl.updates.lastCheck";
const SKIPPED_KEY = "oxabl.updates.skipped";
const PENDING_KEY = "oxabl.updates.pendingReload";
const STALE_DOWNLOAD_MS = 60 * 60 * 1000;
const CHECK_INTERVAL_MS = 24 * 60 * 60 * 1000;
const TIMER_INTERVAL_MS = 6 * 60 * 60 * 1000;
const RELEASE_URL = "https://github.com/oxabl-project/oxabl/releases/tag/oxabl_vscode-v";

type Mode = "auto" | "notify" | "off";

/** Whether the person asked for the check (a manual one reports every outcome). */
type Trigger = "automatic" | "manual";

let busy = false;

/**
 * Register the check command and start the automatic check without awaiting
 * it. A timer repeats the check; the 24-hour stamp keeps it to one a day.
 */
export function registerUpdates(context: vscode.ExtensionContext, log: vscode.LogOutputChannel): void {
  context.subscriptions.push(
    vscode.commands.registerCommand("oxabl.checkForUpdates", () =>
      checkForUpdates(context, log, "manual"),
    ),
  );
  void checkForUpdates(context, log, "automatic");
  const timer = setInterval(() => void checkForUpdates(context, log, "automatic"), TIMER_INTERVAL_MS);
  context.subscriptions.push({ dispose: () => clearInterval(timer) });
}

function currentMode(): Mode {
  const configured = vscode.workspace.getConfiguration("oxabl").get<string>("updates", "auto");
  const mode: Mode = configured === "notify" || configured === "off" ? configured : "auto";
  // Without VS Code's own extension auto-update, install nothing unasked.
  const autoUpdate = vscode.workspace.getConfiguration("extensions").get<unknown>("autoUpdate");
  return mode === "auto" && autoUpdate === false ? "notify" : mode;
}

/** The test hook `OXABL_UPDATE_API_URL`, honoured only for a loopback host. */
function apiOverride(log: vscode.LogOutputChannel): string | undefined {
  const override = process.env.OXABL_UPDATE_API_URL;
  if (!override) {
    return undefined;
  }
  try {
    if (isLoopbackHost(new URL(override).hostname)) {
      return override;
    }
  } catch {
    // Reported below with the other rejected values.
  }
  log.warn("ignoring OXABL_UPDATE_API_URL: the host is not a loopback address");
  return undefined;
}

function updateEnv(context: vscode.ExtensionContext, log: vscode.LogOutputChannel): UpdateEnv {
  const override = apiOverride(log);
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
  // The mode and the extension mode are read at each automatic check.
  if (!manual && (context.extensionMode !== vscode.ExtensionMode.Production || currentMode() === "off")) {
    return;
  }
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
    const env = updateEnv(context, log);
    const installed = env.installedVersion;
    const mode = currentMode();
    log.info(`update check (${trigger}, mode ${mode}), installed v${installed}`);

    // The record is per profile and names the host that installed; the folder confirms the install is still there.
    const dir = path.dirname(context.extensionUri.fsPath);
    const record = context.globalState.get<unknown>(PENDING_KEY);
    if (record !== undefined && isPendingReloadSpent(record, installed)) {
      await context.globalState.update(PENDING_KEY, undefined);
    }
    const onDisk = await highestInstalledVersion(dir);
    const check = await findUpdate(installed, env, (m) => log.info(m), { record, dir, onDisk });
    if (check.kind === "current") {
      log.info("decision: up to date");
      if (manual) {
        void vscode.window.showInformationMessage(`oxabl extension is up to date (v${installed}).`);
      }
      return;
    }
    if (check.kind === "pending-reload") {
      log.info(`decision: v${check.version} waits for a reload`);
      if (manual) {
        offerReload(`oxabl extension v${check.version} is installed. Reload the window to use it.`);
      }
      return;
    }
    const update = check.update;

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
    const message = `update check failed: ${reason}`;
    if (!manual && err instanceof NotReadyError) {
      log.warn(message);
    } else {
      log.error(message);
    }
    if (manual) {
      void vscode.window.showErrorMessage(`oxabl: could not check for updates: ${reason}`);
    }
  } finally {
    busy = false;
  }
}

function offerReload(message: string, ...extra: string[]): Promise<string | undefined> {
  return Promise.resolve(vscode.window.showInformationMessage(message, "Reload Window", ...extra)).then(
    (choice) => {
      if (choice === "Reload Window") {
        void vscode.commands.executeCommand("workbench.action.reloadWindow");
      }
      return choice;
    },
  );
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

/** Download, verify and install. Throws on any failure; the download directory is always removed. */
async function installUpdate(
  context: vscode.ExtensionContext,
  log: vscode.LogOutputChannel,
  env: UpdateEnv,
  update: AvailableUpdate,
): Promise<void> {
  // A fresh directory per attempt: another window may install at the same time.
  const root = vscode.Uri.joinPath(context.globalStorageUri, "updates").fsPath;
  await fs.promises.mkdir(root, { recursive: true });
  await removeStaleDownloads(root, log);
  const dir = await fs.promises.mkdtemp(path.join(root, "dl-"));
  try {
    const file = await downloadVerified(update, env, dir);
    log.info(`verified ${update.vsixName}`);
    await vscode.commands.executeCommand(
      "workbench.extensions.installExtension",
      vscode.Uri.file(file),
    );
  } finally {
    await fs.promises.rm(dir, { recursive: true, force: true });
  }
  log.info(`installed v${update.version}`);
  await context.globalState.update(PENDING_KEY, {
    version: update.version,
    dir: path.dirname(context.extensionUri.fsPath),
  });

  void offerReload(
    `oxabl extension updated to v${update.version}. Reload the window to use it.`,
    "Release Notes",
  ).then((choice) => {
    if (choice === "Release Notes") {
      openReleaseNotes(update.version);
    }
  });
}

/** Remove `dl-*` folders that a killed window left behind. Best effort. */
async function removeStaleDownloads(root: string, log: vscode.LogOutputChannel): Promise<void> {
  try {
    for (const entry of await fs.promises.readdir(root, { withFileTypes: true })) {
      if (!entry.isDirectory() || !entry.name.startsWith("dl-")) {
        continue;
      }
      const dir = path.join(root, entry.name);
      try {
        if (Date.now() - (await fs.promises.stat(dir)).mtimeMs > STALE_DOWNLOAD_MS) {
          await fs.promises.rm(dir, { recursive: true, force: true });
        }
      } catch (err) {
        log.warn(`could not remove ${dir}: ${failureReason(err)}`);
      }
    }
  } catch (err) {
    log.warn(`could not list ${root}: ${failureReason(err)}`);
  }
}

function openReleaseNotes(version: string): void {
  void vscode.env.openExternal(vscode.Uri.parse(`${RELEASE_URL}${version}`));
}
