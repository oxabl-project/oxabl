import * as vscode from "vscode";
import {
  LanguageClient,
  type LanguageClientOptions,
  type ServerOptions,
} from "vscode-languageclient/node";

import { CliManager, resolveBinary } from "./binary";
import { BuildController } from "./builds";
import { binaryVersion, supportsYield } from "./cli";
import { shouldRestartForConfigChange } from "./config";
import { installRecordPath, isManaged } from "./install";
import { registerUpdates } from "./updates";

let client: LanguageClient | undefined;
/** The version of the binary the running client started with. */
let clientVersion: string | undefined;
let context: vscode.ExtensionContext;
let log: vscode.LogOutputChannel;
let builds: BuildController;
let cli: CliManager;
/** `path@version` of binaries known to support `--yield`. */
const capable = new Set<string>();
/** `path@version` of binaries that were too old, for which the message was shown. */
const tooOld = new Set<string>();

export async function activate(extensionContext: vscode.ExtensionContext): Promise<void> {
  context = extensionContext;
  log = vscode.window.createOutputChannel("oxabl", { log: true });
  cli = new CliManager(context, log, async () => {
    if (vscode.workspace.getConfiguration("oxabl").get<boolean>("enable", true)) {
      await restartServer();
    }
    await builds.reconfigure();
  });
  builds = new BuildController({
    log,
    prepare: prepareBinary,
    binary: () => {
      const found = resolveBinary();
      return found.kind === "found" ? found.command : undefined;
    },
  });
  context.subscriptions.push(
    log,
    vscode.commands.registerCommand("oxabl.restartServer", restartServer),
    vscode.commands.registerCommand("oxabl.installCli", () => cli.installOrUpdate()),
    vscode.commands.registerCommand("oxabl.build", () => builds.buildNow()),
    vscode.commands.registerCommand("oxabl.cancelBuild", () => builds.cancelBuilds()),
    vscode.commands.registerCommand("oxabl.showBuildLog", () => builds.showLog()),
    vscode.workspace.onDidChangeConfiguration((event) => {
      // A repath/toggle of the relevant settings applies without a window
      // reload (#105) — restart the client to pick it up.
      if (shouldRestartForConfigChange((section) => event.affectsConfiguration(section))) {
        void restartServer();
      }
      if (event.affectsConfiguration("oxabl.build.enable")) {
        applyBuildEnabled();
      } else if (event.affectsConfiguration("oxabl.build.args") || event.affectsConfiguration("oxabl.server.path")) {
        void builds.reconfigure();
      }
    }),
    vscode.workspace.onDidGrantWorkspaceTrust(() => applyBuildEnabled()),
  );

  // Not awaited: an update check never delays the language client.
  registerUpdates(context, log);
  applyBuildEnabled();

  await startClient();
  void cli.offerInstall();
}

/**
 * Start the build loop when it is enabled and the workspace is trusted: a build
 * runs the worker command from `oxabl.toml`. Stop it otherwise.
 */
function applyBuildEnabled(): void {
  const enabled = vscode.workspace.getConfiguration("oxabl.build").get<boolean>("enable", true);
  if (enabled && vscode.workspace.isTrusted) {
    void builds.start();
  } else {
    builds.stop();
  }
}

/**
 * The binary to build with, or `undefined` when there is none or it cannot
 * build. A binary that predates `--yield` is updated when it is managed. A
 * binary that differs from the one the language server started with restarts
 * the server.
 */
async function prepareBinary(): Promise<string | undefined> {
  const found = resolveBinary();
  if (found.kind !== "found") {
    return undefined;
  }
  const command = found.command;
  let version = await binaryVersion(command);
  if (!capable.has(`${command}@${version}`)) {
    if (!(await supportsYield(command))) {
      if (!(await isManaged(command, installRecordPath())) || !(await cli.updateManaged(command, false))) {
        const key = `${command}@${version}`;
        if (!tooOld.has(key)) {
          tooOld.add(key);
          void vscode.window.showWarningMessage(
            `oxabl: ${command} is too old to build here (it has no \`oxabl build --yield\`). Update oxabl.`,
          );
        }
        return undefined;
      }
      version = await binaryVersion(command);
      if (!(await supportsYield(command))) {
        return undefined;
      }
    }
    capable.add(`${command}@${version}`);
  }
  if (client && clientVersion !== undefined && version !== clientVersion) {
    log.info(`oxabl changed from ${clientVersion} to ${version}; restarting the language server`);
    await restartServer();
  }
  return command;
}

/** Start the client if enabled; a no-op (silent) when `oxabl.enable` is false. */
async function startClient(): Promise<void> {
  const config = vscode.workspace.getConfiguration("oxabl");
  if (!config.get<boolean>("enable", true)) {
    return;
  }

  const discovery = resolveBinary();

  // Highlighting needs no server, so a missing binary is not an error: no message, a log line.
  if (discovery.kind === "not-found") {
    log.info(
      "oxabl executable not found on PATH or in oxabl.server.path; running without the " +
        "language server (syntax highlighting still works)",
    );
    return;
  }

  // No `transport` field: stdio is the default for an Executable. Setting
  // `TransportKind.stdio` explicitly makes the client append `--stdio` to argv,
  // which `oxabl lsp` (no such flag) rejects. `oxabl lsp` already speaks LSP
  // over stdin/stdout.
  const serverOptions: ServerOptions = {
    command: discovery.command,
    args: ["lsp"],
  };

  const clientOptions: LanguageClientOptions = {
    documentSelector: [{ scheme: "file", language: "abl" }],
    outputChannel: log,
    synchronize: {
      // The server watches these itself, but forwarding the events lets live
      // reload work even when the client owns the file watcher (#90 / U2).
      fileEvents: [
        vscode.workspace.createFileSystemWatcher("**/oxabl.toml"),
        vscode.workspace.createFileSystemWatcher("**/*.df"),
        vscode.workspace.createFileSystemWatcher("**/*.i"),
      ],
    },
  };

  clientVersion = await binaryVersion(discovery.command);
  client = new LanguageClient("oxabl", "oxabl language server", serverOptions, clientOptions);

  try {
    await client.start();
  } catch (err) {
    // R8: a spawn/handshake failure surfaces as a clear message, not a crash.
    const detail = err instanceof Error ? err.message : String(err);
    void vscode.window.showErrorMessage(
      `oxabl: failed to start the language server (\`${discovery.command} lsp\`): ${detail}`,
    );
    client = undefined;
  }
}

/** Stop the running client, if any. Safe to call when nothing is running. */
async function stopClient(): Promise<void> {
  if (!client) {
    return;
  }
  const running = client;
  client = undefined;
  clientVersion = undefined;
  try {
    await running.stop();
  } catch {
    // Already stopped/never fully started — nothing to clean up.
  }
}

/**
 * Command `oxabl.restartServer`: stop the current client (if any) and start a
 * fresh one, re-running binary discovery so a newly-installed or repathed
 * `oxabl` is picked up without a window reload (#105). Clears the language
 * client's crash-cap state as a side effect. No-ops gracefully when the server
 * was never running or `oxabl.enable` is false.
 */
async function restartServer(): Promise<void> {
  await stopClient();

  const config = vscode.workspace.getConfiguration("oxabl");
  if (!config.get<boolean>("enable", true)) {
    void vscode.window.showInformationMessage(
      "oxabl is disabled (`oxabl.enable` is false); the language server was not started.",
    );
    return;
  }

  await startClient();
}

export async function deactivate(): Promise<void> {
  await Promise.all([builds?.dispose(), stopClient()]);
}
