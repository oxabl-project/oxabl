import * as vscode from "vscode";
import {
  LanguageClient,
  type LanguageClientOptions,
  type ServerOptions,
  TransportKind,
} from "vscode-languageclient/node";

import { resolveServerBinary, splitPath } from "./server";

let client: LanguageClient | undefined;

const OUTPUT_CHANNEL = "oxabl";

export async function activate(): Promise<void> {
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

  const serverOptions: ServerOptions = {
    command: discovery.command,
    args: ["lsp"],
    transport: TransportKind.stdio,
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

export async function deactivate(): Promise<void> {
  if (client) {
    await client.stop();
    client = undefined;
  }
}
