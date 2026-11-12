// The `oxabl` binary: where it is, and getting it onto the machine. The
// download and the checks are in `install.ts`; this module holds the settings,
// the prompts and the progress.

import * as path from "node:path";
import * as vscode from "vscode";

import { runCli, supportsSelfUpdate } from "./cli";
import { WSL_DOCS_URL, classifyHost, hostMessage, type HostClass } from "./host";
import { installCli, findCliRelease, installRecordPath, isManaged } from "./install";
import { apiBaseFrom } from "./net";
import { defaultInstallPath, resolveServerBinary, splitPath, type DiscoveryResult } from "./server";

const DONT_ASK_KEY = "oxabl.install.dontAsk";
const REOPEN_IN_WSL = "remote-wsl.reopenInWSL";
const SELF_UPDATE_TIMEOUT_MS = 180_000;

/** The binary by the setting, then the install path, then `PATH`. */
export function resolveBinary(): DiscoveryResult {
  return resolveServerBinary({
    configuredPath: vscode.workspace.getConfiguration("oxabl").get<string>("server.path"),
    installPath: defaultInstallPath(),
    pathDirs: splitPath(process.env.PATH),
  });
}

function currentHost(): HostClass {
  return classifyHost({ platform: process.platform, arch: process.arch, remoteName: vscode.env.remoteName });
}

/** Whether a folder of the workspace holds an `oxabl.toml`. */
async function hasProject(): Promise<boolean> {
  return (await vscode.workspace.findFiles("**/oxabl.toml", "**/{node_modules,.git}/**", 1)).length > 0;
}

export class CliManager {
  /**
   * `onChanged` runs after the binary was installed or updated on request, so the
   * language server and the build pick it up.
   */
  constructor(
    private readonly context: vscode.ExtensionContext,
    private readonly log: vscode.LogOutputChannel,
    private readonly onChanged: () => Promise<void>,
  ) {}

  /** The one prompt at open: no binary and a project that needs it. Silent otherwise. */
  async offerInstall(): Promise<void> {
    if (resolveBinary().kind === "found" || this.context.globalState.get<boolean>(DONT_ASK_KEY)) {
      return;
    }
    if (!(await hasProject())) {
      return;
    }
    const host = currentHost();
    if (host.kind !== "supported") {
      await this.offerOtherHost(host);
      return;
    }
    const choice = await vscode.window.showInformationMessage(
      "The oxabl CLI is not installed. It builds and checks this project.",
      "Install",
      "Not Now",
      "Don't Ask Again",
    );
    if (choice === "Install") {
      await this.install();
    } else if (choice === "Don't Ask Again") {
      await this.context.globalState.update(DONT_ASK_KEY, true);
    }
  }

  private async offerOtherHost(host: Exclude<HostClass, { kind: "supported" }>): Promise<void> {
    const wsl = host.kind === "windows" && (await vscode.commands.getCommands(true)).includes(REOPEN_IN_WSL);
    const action = host.kind === "windows" ? (wsl ? "Reopen Folder in WSL" : "Open Docs") : undefined;
    const buttons = [...(action ? [action] : []), "Don't Ask Again"];
    const choice = await vscode.window.showInformationMessage(hostMessage(host), ...buttons);
    if (choice === "Reopen Folder in WSL") {
      await vscode.commands.executeCommand(REOPEN_IN_WSL);
    } else if (choice === "Open Docs") {
      void vscode.env.openExternal(vscode.Uri.parse(WSL_DOCS_URL));
    } else if (choice === "Don't Ask Again") {
      await this.context.globalState.update(DONT_ASK_KEY, true);
    }
  }

  /** The command `oxabl: Install or Update the CLI`. */
  async installOrUpdate(): Promise<void> {
    const found = resolveBinary();
    if (found.kind === "found") {
      if (await isManaged(found.command, installRecordPath())) {
        await this.updateManaged(found.command, true);
      } else {
        void vscode.window.showInformationMessage(
          `oxabl: ${found.command} was not installed by the oxabl installer, so the extension does not update it. ` +
            "Update it the way you installed it (for example `cargo install oxabl`).",
        );
      }
      return;
    }
    const host = currentHost();
    if (host.kind !== "supported") {
      await this.offerOtherHost(host);
      return;
    }
    await this.install();
  }

  /**
   * Bring a managed binary up to date: `oxabl self-update` when it has the
   * command, else a fresh install over it. True when that succeeded.
   */
  async updateManaged(binary: string, announce: boolean): Promise<boolean> {
    if (await supportsSelfUpdate(binary)) {
      return this.selfUpdate(binary, announce);
    }
    this.log.info(`${binary} has no self-update; installing the newest release over it`);
    return this.install(binary, announce);
  }

  private async install(target = defaultInstallPath(), announce = true): Promise<boolean> {
    const env = {
      ...apiBaseFrom(process.env.OXABL_UPDATE_API_URL, (m) => this.log.warn(m)),
      installedVersion: this.context.extension.packageJSON.version as string,
    };
    try {
      const release = await vscode.window.withProgress(
        { location: vscode.ProgressLocation.Notification, title: "Installing the oxabl CLI" },
        async () => {
          const found = await findCliRelease(env, (m) => this.log.info(m));
          await installCli(found, env, { path: target, recordPath: installRecordPath() });
          return found;
        },
      );
      this.log.info(`installed oxabl ${release.version} to ${target}`);
      if (announce) {
        void vscode.window.showInformationMessage(`oxabl ${release.version} is installed in ${path.dirname(target)}.`);
        await this.onChanged();
      }
      return true;
    } catch (err) {
      const reason = err instanceof Error ? err.message : String(err);
      this.log.error(`install failed: ${reason}`);
      void vscode.window.showErrorMessage(`oxabl: could not install the CLI: ${reason}`);
      return false;
    }
  }

  /**
   * Run `oxabl self-update` on a managed binary. `announce` shows the result and
   * runs `onChanged`; without it the caller continues on its own. True when the
   * command succeeded.
   */
  async selfUpdate(binary: string, announce: boolean): Promise<boolean> {
    return vscode.window.withProgress(
      { location: vscode.ProgressLocation.Notification, title: "Updating the oxabl CLI" },
      async () => {
        try {
          const out = await runCli(binary, ["self-update"], SELF_UPDATE_TIMEOUT_MS);
          const line = out.stdout.trim().split("\n").pop() ?? "oxabl self-update finished";
          this.log.info(`self-update: ${line}`);
          if (announce) {
            void vscode.window.showInformationMessage(line);
            await this.onChanged();
          }
          return true;
        } catch (err) {
          const reason = err instanceof Error ? err.message : String(err);
          this.log.error(`self-update failed: ${reason}`);
          void vscode.window.showErrorMessage(`oxabl: could not update the CLI: ${reason}`);
          return false;
        }
      },
    );
  }
}
