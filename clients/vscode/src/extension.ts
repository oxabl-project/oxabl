import * as vscode from "vscode";
import { CliManager, resolveBinary } from "./binary";
import { BuildController } from "./builds";
import { binaryVersion, supportsYield } from "./cli";
import { Clients, DEACTIVATE_WAIT_MS, readInlayHints, readReferenceLenses, readSemanticTokens } from "./clients";
import { shouldRestartForConfigChange } from "./config";
import { ExpandedUnits } from "./expandedUnit";
import { InactiveRegions } from "./inactiveRegions";
import { IncluderContexts } from "./includerContext";
import { installRecordPath, isManaged } from "./install";
import { SHOW_REFERENCES, readPlaces } from "./referenceLens";
import { SchemaBrowser } from "./schemaView";
import { registerSnippets } from "./snippetCompletion";
import { installSkill } from "./skill";
import { goToSuperMethod } from "./superMethod";
import { registerUpdates } from "./updates";

let clients: Clients;
let context: vscode.ExtensionContext;
let log: vscode.LogOutputChannel;
let builds: BuildController;
let cli: CliManager;
let includerContexts: IncluderContexts;
let expandedUnits: ExpandedUnits;
let inactiveRegions: InactiveRegions;
let schema: SchemaBrowser;
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
    storageDir: context.globalStorageUri.fsPath,
    memory: context.workspaceState,
  });
  clients = new Clients(log, {
    attach: (client) => inactiveRegions.attach(client),
    started: (folder) => {
      includerContexts.show();
      schema.serverChanged(folder);
    },
    stopped: (serves) => {
      includerContexts.reset(serves);
      inactiveRegions.reset(serves);
    },
    layoutChanged: () => schema.layoutChanged(),
    loadedFolders: () => schema.loadedFolders(),
  });
  expandedUnits = new ExpandedUnits((uri) => clients.forUri(uri));
  inactiveRegions = new InactiveRegions();
  includerContexts = new IncluderContexts(
    (uri) => clients.forUri(uri),
    (uri) => expandedUnits.refreshSource(uri),
  );
  schema = new SchemaBrowser(clients);
  context.subscriptions.push(
    log,
    clients,
    expandedUnits,
    inactiveRegions,
    schema,
    includerContexts,
    vscode.commands.registerCommand("oxabl.showExpandedUnit", () => expandedUnits.show()),
    vscode.commands.registerCommand("oxabl.selectIncluderContext", () => includerContexts.select()),
    vscode.commands.registerCommand("oxabl.goToSuperMethod", () => goToSuperMethod((uri) => clients.forUri(uri))),
    vscode.commands.registerCommand(
      SHOW_REFERENCES,
      (uri: string, position: { line: number; character: number }, places: unknown) =>
        showReferences(uri, position, places),
    ),
    vscode.commands.registerCommand("oxabl.restartServer", restartServer),
    vscode.commands.registerCommand("oxabl.installCli", () => cli.installOrUpdate()),
    vscode.commands.registerCommand("oxabl.installSkill", installSkillCommand),
    vscode.commands.registerCommand("oxabl.build", () => builds.buildNow()),
    vscode.commands.registerCommand("oxabl.cancelBuild", () => builds.cancelBuilds()),
    vscode.commands.registerCommand("oxabl.showBuildLog", () => builds.showLog()),
    vscode.workspace.onDidChangeConfiguration((event) => {
      // A repath/toggle of the relevant settings applies without a window
      // reload (#105) — restart the client to pick it up.
      if (shouldRestartForConfigChange((section) => event.affectsConfiguration(section))) {
        void restartServer();
      }
      if (event.affectsConfiguration("oxabl.inlayHints")) {
        notifyClients({ inlayHints: readInlayHints() });
      }
      if (event.affectsConfiguration("oxabl.semanticTokens")) {
        notifyClients({ semanticTokens: readSemanticTokens() });
      }
      if (event.affectsConfiguration("oxabl.referenceLenses")) {
        notifyClients({ referenceLenses: readReferenceLenses() });
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
  registerUpdates(context, log, cli);
  applyBuildEnabled();
  const snippets = await registerSnippets(
    context.extensionPath,
    log,
    // A client that stopped (crash limit, restart) keeps its initialize
    // result: only a running server completes.
    (document) => {
      const client = clients.forUri(document.uri);
      return client?.isRunning() === true && client.initializeResult?.capabilities.completionProvider !== undefined;
    },
  );
  if (snippets) {
    context.subscriptions.push(snippets);
  }

  await clients.startAll();
  void cli.offerInstall();
}

/**
 * Command `oxabl.installSkill`: `oxabl skill install` with the binary the
 * extension resolved, and its outcome as a message.
 */
async function installSkillCommand(): Promise<void> {
  const found = resolveBinary();
  if (found.kind !== "found") {
    void vscode.window.showErrorMessage(
      'oxabl: the CLI is not installed. Run "oxabl: Install or Update the CLI" first.',
    );
    return;
  }
  const result = await installSkill(found.command);
  if (result.ok) {
    log.info(`agent skill:\n${result.lines.join("\n")}`);
    void vscode.window.showInformationMessage(`oxabl agent skill:\n${result.lines.join("\n")}`);
  } else {
    log.error(`agent skill: ${result.message}`);
    void vscode.window.showErrorMessage(`oxabl: could not install the agent skill: ${result.message}`);
  }
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
 * binary that differs from the one a language client started with restarts
 * the clients.
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
  const running = clients.differing(version);
  if (running !== undefined) {
    log.info(`oxabl changed from ${running} to ${version}; restarting the language server`);
    await restartServer();
  }
  return command;
}

/** Show the places that a lens counts, in the references view anchored at `position` in `uri`. */
async function showReferences(
  uri: string,
  position: { line: number; character: number },
  places: unknown,
): Promise<void> {
  const shown = readPlaces(places).map(
    (place) =>
      new vscode.Location(
        vscode.Uri.parse(place.uri),
        new vscode.Range(
          new vscode.Position(place.start.line, place.start.character),
          new vscode.Position(place.end.line, place.end.character),
        ),
      ),
  );
  await vscode.commands.executeCommand(
    "editor.action.showReferences",
    vscode.Uri.parse(uri),
    new vscode.Position(position.line, position.character),
    shown,
  );
}

/** Send `settings` to every language client as a configuration change. */
function notifyClients(settings: Record<string, unknown>): void {
  for (const client of clients.all()) {
    void client.sendNotification("workspace/didChangeConfiguration", { settings: { oxabl: settings } });
  }
}

/**
 * Command `oxabl.restartServer`: stop the clients and start fresh ones,
 * re-running binary discovery so a newly-installed or repathed `oxabl` is
 * picked up without a window reload (#105). Clears the language client's
 * crash-cap state as a side effect. No-ops gracefully when the server was
 * never running or `oxabl.enable` is false.
 */
function restartServer(): Promise<void> {
  return clients.restart();
}

export async function deactivate(): Promise<void> {
  await Promise.all([builds?.dispose(), clients?.stopAll(DEACTIVATE_WAIT_MS)]);
}
