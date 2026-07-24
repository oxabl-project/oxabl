import * as vscode from "vscode";
import {
  LanguageClient,
  type LanguageClientOptions,
  type ServerOptions,
} from "vscode-languageclient/node";

import { shouldRestartForConfigChange } from "./config";
import { resolveServerBinary, splitPath } from "./server";

let client: LanguageClient | undefined;

const OUTPUT_CHANNEL = "oxabl";

export async function activate(context: vscode.ExtensionContext): Promise<void> {
  context.subscriptions.push(
    vscode.commands.registerCommand("oxabl.restartServer", restartServer),
    vscode.workspace.onDidChangeConfiguration((event) => {
      // A repath/toggle of the relevant settings applies without a window
      // reload (#105) — restart the client to pick it up.
      if (shouldRestartForConfigChange((section) => event.affectsConfiguration(section))) {
        void restartServer();
      }
    }),
  );

  await startClient();
}

/** Start the client if enabled; a no-op (silent) when `oxabl.enable` is false. */
async function startClient(): Promise<void> {
  const config = vscode.workspace.getConfiguration("oxabl");
  if (!config.get<boolean>("enable", true)) {
    return;
  }

  const discovery = resolveServerBinary({
    configuredPath: config.get<string>("server.path"),
    pathDirs: splitPath(process.env.PATH),
  });

  if (discovery.kind === "not-found") {
    // R8: no crash loop — one clear, actionable message and stop.
    void vscode.window
      .showErrorMessage(
        "oxabl: could not find the `oxabl` executable on your PATH. " +
          "Install it, or set `oxabl.server.path` to its location.",
        "Open Settings",
      )
      .then((choice) => {
        if (choice === "Open Settings") {
          void vscode.commands.executeCommand(
            "workbench.action.openSettings",
            "oxabl.server.path",
          );
        }
      });
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
    outputChannelName: OUTPUT_CHANNEL,
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
  await stopClient();
}
