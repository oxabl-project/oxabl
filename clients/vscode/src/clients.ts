// The language clients of a window. A window with zero or one workspace folder
// has one client for all ABL files. A window with two or more folders has one
// client for each folder that owns files, started when a file of that folder
// opens (`folders.ts` holds the rules; docs/design/vscode-extension.md the design).

import * as vscode from "vscode";
import {
  LanguageClient,
  type LanguageClientOptions,
  type ServerOptions,
} from "vscode-languageclient/node";

import { resolveBinary } from "./binary";
import { binaryVersion } from "./cli";
import {
  inlayHintSettings,
  outlineSettings,
  referenceLensSettings,
  semanticTokenSettings,
} from "./config";
import { type Mode, modeOf, ownerOf, owningFolders } from "./folders";
import { isInside } from "./roots";

/** A workspace folder, with the fields the rules and the logs read. */
export interface FolderInfo {
  /** The folder URI as text: the key of its client. */
  key: string;
  name: string;
  fsPath: string;
  folder: vscode.WorkspaceFolder;
}

const folderInfo = (folder: vscode.WorkspaceFolder): FolderInfo => ({
  key: folder.uri.toString(),
  name: folder.name,
  fsPath: folder.uri.fsPath,
  folder,
});

/** What the clients tell the rest of the extension. */
export interface ClientHooks {
  /** Called for each new client before it starts. */
  attach(client: LanguageClient): void;
  /** A start attempt ended; the client is running, or the attempt failed. */
  started(folder: FolderInfo | undefined): void;
  /** A client is stopping. `serves` tells which documents it served. */
  stopped(serves: (uri: vscode.Uri) => boolean): void;
  /** The folders or the mode changed, and the clients follow them. */
  layoutChanged(): void;
  /** The keys of the folders whose schema node shows an answer; a restart starts them again. */
  loadedFolders(): string[];
}

interface Entry {
  folder: FolderInfo | undefined;
  client: LanguageClient;
  /** The version of the binary the client started with. */
  version: string | undefined;
  serves: (uri: vscode.Uri) => boolean;
  /** The file watchers and the trace channel of the client; the client does not own them. */
  watchers: vscode.Disposable[];
  /** The promise of `client.start()`: it ends after the client registered its features. */
  start: Promise<void>;
}

/**
 * The longest wait for a client that is still starting before it is stopped.
 * The server gives up on its daemon after 60 s.
 */
const START_WAIT_MS = 70_000;

/** The wait of `deactivate`: VS Code does not wait long for an extension to end. */
export const DEACTIVATE_WAIT_MS = 3_000;

/** The inlay hint kinds the `oxabl.inlayHints.*` settings turn on, for the server. */
export function readInlayHints(): Record<string, boolean> {
  const config = vscode.workspace.getConfiguration("oxabl.inlayHints");
  return inlayHintSettings((kind) => config.get<boolean>(kind));
}

/** The `oxabl.semanticTokens.enable` setting, for the server. */
export function readSemanticTokens(): { enable: boolean } {
  const config = vscode.workspace.getConfiguration("oxabl.semanticTokens");
  return semanticTokenSettings(config.get<boolean>("enable"));
}

/** The `oxabl.referenceLenses.enable` setting, for the server. */
export function readReferenceLenses(): { enable: boolean } {
  const config = vscode.workspace.getConfiguration("oxabl.referenceLenses");
  return referenceLensSettings(config.get<boolean>("enable"));
}

/** The `oxabl.outline.includes` setting, for the server. */
function readOutline(): { includes: boolean } {
  const config = vscode.workspace.getConfiguration("oxabl.outline");
  return outlineSettings(config.get<boolean>("includes"));
}

const enabled = () => vscode.workspace.getConfiguration("oxabl").get<boolean>("enable", true);

export class Clients implements vscode.Disposable {
  private readonly entries = new Map<string, Entry>();
  private readonly starting = new Map<string, Promise<void>>();
  /** Folders whose client failed to start; a restart or an explicit start tries again. */
  private readonly failed = new Set<string>();
  private readonly outside = new Set<string>();
  private currentMode: Mode = "single";
  /** Counts stops of every client, so a start that a stop overtook ends quietly. */
  private epoch = 0;
  private queue: Promise<void> = Promise.resolve();
  private notFoundLogged = false;
  private readonly subscriptions: vscode.Disposable[];

  constructor(
    private readonly log: vscode.LogOutputChannel,
    private readonly hooks: ClientHooks,
  ) {
    // The schema view reads the mode from the first moment, and a node it opens before `startAll`
    // runs must be able to start its folder.
    this.currentMode = this.mode;
    this.subscriptions = [
      vscode.workspace.onDidOpenTextDocument((document) => {
        if (this.currentMode === "multi" && enabled()) {
          void this.serve(document);
        }
      }),
      vscode.workspace.onDidChangeWorkspaceFolders(() => void this.foldersChanged()),
    ];
  }

  /** The mode the window's folders call for now; the clients follow it as soon as they can. */
  get mode(): Mode {
    return modeOf(this.folders().length);
  }

  /** The folders that own files now. */
  owning(): FolderInfo[] {
    return owningFolders(this.folders());
  }

  /** Every client there is, running or not. */
  all(): LanguageClient[] {
    return [...this.entries.values()].map((entry) => entry.client);
  }

  /** The client that serves `uri`: the one client, or the one of the folder that owns it. */
  forUri(uri: vscode.Uri | undefined): LanguageClient | undefined {
    if (this.currentMode === "single") {
      return this.entries.get("")?.client;
    }
    if (!uri || uri.scheme !== "file") {
      return undefined;
    }
    const owner = ownerOf(uri.fsPath, this.owning());
    return owner ? this.entries.get(owner.key)?.client : undefined;
  }

  forFolder(folder: FolderInfo): LanguageClient | undefined {
    return this.entries.get(folder.key)?.client;
  }

  /** The version of a client that started with another version than `version`, if any. */
  differing(version: string | undefined): string | undefined {
    for (const entry of this.entries.values()) {
      if (entry.version !== undefined && entry.version !== version) {
        return entry.version;
      }
    }
    return undefined;
  }

  /**
   * Start the client of an owning folder on request, as the schema view does
   * when the person opens the folder's node. Gives the client, or `undefined`
   * when none runs.
   */
  async startFolder(folder: FolderInfo): Promise<LanguageClient | undefined> {
    if (this.currentMode !== "multi" || !enabled()) {
      return undefined;
    }
    await this.startClient(folder, true);
    return this.forFolder(folder);
  }

  /** Start what the layout needs: the one client, or a client for each folder with an open ABL file. */
  async startAll(): Promise<void> {
    this.currentMode = modeOf(this.folders().length);
    if (!enabled()) {
      return;
    }
    if (this.currentMode === "single") {
      await this.startClient(undefined, false);
      return;
    }
    await Promise.all(vscode.workspace.textDocuments.map((document) => this.serve(document)));
  }

  /**
   * Stop every client and start again, running binary discovery once more so
   * that a new or repathed `oxabl` is used without a window reload. One
   * restart at a time: a second request waits for the first.
   */
  restart(): Promise<void> {
    return this.serial(async () => {
      await this.stopAll();
      if (!enabled()) {
        void vscode.window.showInformationMessage(
          "oxabl is disabled (`oxabl.enable` is false); the language server was not started.",
        );
        return;
      }
      await this.startAll();
      // A folder that the schema view opened has no open file, so `startAll` skipped it. Not
      // explicit: a folder that `startAll` tried and that failed is not tried a second time.
      const loaded = new Set(this.hooks.loadedFolders());
      await Promise.all(
        this.owning()
          .filter((folder) => loaded.has(folder.key))
          .map((folder) => this.startClient(folder, false)),
      );
    });
  }

  /**
   * Stop every client. Safe to call when nothing runs. A client that is still
   * starting is waited for at most `startWaitMs`.
   */
  async stopAll(startWaitMs = START_WAIT_MS): Promise<void> {
    this.epoch++;
    const stopping = [...this.entries.values()];
    this.entries.clear();
    this.starting.clear();
    this.failed.clear();
    this.notFoundLogged = false;
    await Promise.all(stopping.map((entry) => this.stopEntry(entry, startWaitMs)));
  }

  dispose(): void {
    for (const subscription of this.subscriptions) {
      subscription.dispose();
    }
  }

  private folders(): FolderInfo[] {
    return (vscode.workspace.workspaceFolders ?? []).map(folderInfo);
  }

  private serial(run: () => Promise<void>): Promise<void> {
    const result = this.queue.then(run, run);
    this.queue = result.catch(() => undefined);
    return result;
  }

  /** Start the client of the folder that owns `document`, or log that none does. */
  private async serve(document: vscode.TextDocument): Promise<void> {
    if (document.languageId !== "abl" || document.uri.scheme !== "file") {
      return;
    }
    const file = document.uri.fsPath;
    const owner = ownerOf(file, this.owning());
    if (!owner) {
      if (!this.outside.has(file)) {
        this.outside.add(file);
        this.log.info(`${file} is outside every workspace folder; no language server serves it`);
      }
      return;
    }
    await this.startClient(owner, false);
  }

  private async foldersChanged(): Promise<void> {
    await this.serial(async () => {
      const mode = modeOf(this.folders().length);
      const owning = new Set(this.owning().map((folder) => folder.key));
      const left = [...this.entries.values()].filter((entry) => entry.folder && !owning.has(entry.folder.key));
      if (mode !== this.currentMode) {
        // Set first: a file that opens while the clients stop must not start a client of the old mode.
        this.currentMode = mode;
        await this.stopAll();
        this.logStopped(left);
        this.hooks.layoutChanged();
        await this.startAll();
      } else if (mode === "multi") {
        for (const entry of left) {
          this.entries.delete(entry.folder!.key);
          // A start under way ends with no entry; a folder added back must start a client of its own.
          this.starting.delete(entry.folder!.key);
        }
        // A folder that left and comes back must not stay marked as failed.
        for (const key of [...this.failed]) {
          if (!owning.has(key)) {
            this.failed.delete(key);
          }
        }
        await Promise.all(left.map((entry) => this.stopEntry(entry)));
        this.logStopped(left);
        // The schema view follows the folders before the starts: a start waits for its daemon.
        this.hooks.layoutChanged();
        // A folder that owns files now, or a file that has an owner now.
        await this.startAll();
      } else {
        this.hooks.layoutChanged();
      }
    });
  }

  private logStopped(entries: Entry[]): void {
    for (const entry of entries) {
      this.log.info(`stopped the language server of folder "${entry.folder!.name}"`);
    }
  }

  private startClient(folder: FolderInfo | undefined, explicit: boolean): Promise<void> {
    const key = folder?.key ?? "";
    if (explicit) {
      this.failed.delete(key);
    }
    const pending = this.starting.get(key);
    if (pending) {
      return pending;
    }
    if (this.entries.has(key) || this.failed.has(key)) {
      return Promise.resolve();
    }
    const run = this.doStart(folder, key).finally(() => {
      if (this.starting.get(key) === run) {
        this.starting.delete(key);
      }
    });
    this.starting.set(key, run);
    return run;
  }

  private async doStart(folder: FolderInfo | undefined, key: string): Promise<void> {
    const epoch = this.epoch;
    const discovery = resolveBinary();

    // Highlighting needs no server, so a missing binary is not an error: no message, a log line.
    if (discovery.kind === "not-found") {
      if (!this.notFoundLogged) {
        this.notFoundLogged = true;
        this.log.info(
          "oxabl executable not found on PATH or in oxabl.server.path; running without the " +
            "language server (syntax highlighting still works)",
        );
      }
      return;
    }

    const version = await binaryVersion(discovery.command);
    // A mode switch may be under way: its stop bumped the epoch before this start began, and the
    // mode that the folders call for now decides which kind of client may start.
    const mode = this.mode;
    if (
      epoch !== this.epoch ||
      (folder ? mode !== "multi" || !this.owning().some((owner) => owner.key === folder.key) : mode !== "single")
    ) {
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

    // The server watches these itself, but forwarding the events lets live
    // reload work even when the client owns the file watcher. A folder client
    // watches only its folder.
    const watch = (glob: string) =>
      vscode.workspace.createFileSystemWatcher(folder ? new vscode.RelativePattern(folder.folder, glob) : glob);
    const fileEvents = [watch("**/oxabl.toml"), watch("**/*.df"), watch("**/*.i")];
    const watchers: vscode.Disposable[] = [...fileEvents];
    // The client writes trace lines with no client name. With several folders each client gets a
    // channel of its own, made only while `oxabl.trace.server` is on (a change restarts the clients).
    let traceOutputChannel: vscode.OutputChannel | undefined;
    if (folder && vscode.workspace.getConfiguration("oxabl").get<string>("trace.server", "off") !== "off") {
      traceOutputChannel = vscode.window.createOutputChannel(`oxabl (${folder.name}) trace`);
      watchers.push(traceOutputChannel);
    }

    const clientOptions: LanguageClientOptions = {
      documentSelector: folder
        ? [
            {
              scheme: "file",
              language: "abl",
              // The client hands the pattern to `languages.match`, which takes a `RelativePattern`;
              // the protocol type only lists the string form.
              pattern: new vscode.RelativePattern(folder.folder, "**/*") as unknown as string,
            },
          ]
        : [{ scheme: "file", language: "abl" }],
      // With a folder, the client sends only that folder as the root, so the
      // server reads that folder's `oxabl.toml`.
      workspaceFolder: folder?.folder,
      outputChannel: this.log,
      traceOutputChannel,
      // Sends a progress token with `initialize`; the server reports a slow daemon start on it.
      progressOnInitialization: true,
      initializationOptions: {
        inlayHints: readInlayHints(),
        semanticTokens: readSemanticTokens(),
        referenceLenses: readReferenceLenses(),
        outline: readOutline(),
      },
      synchronize: { fileEvents },
    };

    if (folder) {
      this.log.info(`starting the language server of folder "${folder.name}" (${folder.fsPath})`);
    }
    // Every client has the id `oxabl`: the id names the setting `<id>.trace.server`, which each client
    // reads itself. Clients share no command or collection through the id.
    const name = folder ? `oxabl (${folder.name})` : "oxabl language server";
    const client = new LanguageClient("oxabl", name, serverOptions, clientOptions);
    const entry: Entry = {
      folder,
      client,
      version,
      serves: folder
        ? (uri) => uri.scheme === "file" && isInside(uri.fsPath, folder.fsPath)
        : () => true,
      watchers,
      start: Promise.resolve(),
    };
    this.entries.set(key, entry);
    this.hooks.attach(client);

    try {
      entry.start = client.start();
      await entry.start;
    } catch (err) {
      // A spawn/handshake failure surfaces as a clear message, not a crash.
      const detail = err instanceof Error ? err.message : String(err);
      void vscode.window.showErrorMessage(
        `oxabl: failed to start the language server (\`${discovery.command} lsp\`): ${detail}`,
      );
      if (this.entries.get(key) === entry) {
        this.entries.delete(key);
        this.failed.add(key);
      }
      for (const watcher of watchers) {
        watcher.dispose();
      }
      if (epoch === this.epoch) {
        this.hooks.started(folder);
      }
      return;
    }
    // A stop or a folder change that came during the start did not reach the process: stop it now.
    if (epoch !== this.epoch || this.entries.get(key) !== entry) {
      await this.halt(entry);
      return;
    }
    this.hooks.started(folder);
  }

  private async stopEntry(entry: Entry, startWaitMs = START_WAIT_MS): Promise<void> {
    this.hooks.stopped(entry.serves);
    await this.halt(entry, startWaitMs);
  }

  /**
   * Stop the client and dispose its watchers. A client that is still starting
   * cannot stop, so wait (at most `startWaitMs`) for its start to end. The wait
   * is on the promise of `client.start()`, in every state, not on the `Running` state: the
   * client sets that state before it registers its features, and a stop then
   * would leave them registered. A start that outlasts the wait stops itself in
   * `doStart` when it ends.
   */
  private async halt(entry: Entry, startWaitMs = START_WAIT_MS): Promise<void> {
    const { client } = entry;
    // The client is `Running` before its start has registered the features, and the library starts
    // it again after a crash, which makes a new start promise. `start()` of a client that needs a
    // stop gives its current start and starts nothing; a start that ended makes the race return at once.
    const ignore = () => undefined;
    const current = client.needsStop() ? client.start().catch(ignore) : Promise.resolve();
    let timer: NodeJS.Timeout | undefined;
    const limit = new Promise<void>((resolve) => {
      timer = setTimeout(resolve, startWaitMs);
    });
    await Promise.race([Promise.all([entry.start.catch(ignore), current]), limit]);
    clearTimeout(timer);
    try {
      await client.stop();
    } catch {
      // Already stopped/never fully started — nothing to clean up.
    }
    for (const watcher of entry.watchers) {
      watcher.dispose();
    }
  }
}
