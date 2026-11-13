// The build loop: `oxabl build` for each root that has one, at open, on save
// and on changes on disk. The scheduling is in `scheduler.ts`, the event
// bookkeeping and the status text in `progress.ts`; this module holds the
// processes, the watchers and the UI.

import * as cp from "node:child_process";
import * as fs from "node:fs";
import * as path from "node:path";
import * as readline from "node:readline";
import * as vscode from "vscode";

import { readRootConfig, type RootConfig } from "./cli";
import {
  BuildProgress,
  MessageStore,
  finishedText,
  renderStatus,
  type BuildMessage,
  type RootStatus,
} from "./progress";
import { isBuildInput, isConfigFile, isIgnored, ownerRoot } from "./roots";
import { Scheduler } from "./scheduler";

const KILL_AFTER_MS = 5_000;
const OK_VISIBLE_MS = 3_000;
const STATUS_THROTTLE_MS = 100;
// VS Code hands file events to an extension in batches about half a second
// apart. A burst of writes on disk (an agent, a checkout) would otherwise
// outlast a debounce of that length between two batches.
const WATCHER_SLACK_MS = 600;

/** What the controller needs from the extension. */
export interface BuildDeps {
  log: vscode.LogOutputChannel;
  /** The binary to build with, checked and up to date; `undefined` when there is none or it is too old. */
  prepare(): Promise<string | undefined>;
  /** The binary to read the configuration with, without the checks of `prepare`. */
  binary(): string | undefined;
}

class Root {
  config: RootConfig | undefined;
  status: RootStatus = { phase: "idle", last: "none" };
  readonly messages = new MessageStore();
  readonly scheduler: Scheduler;
  child: cp.ChildProcess | undefined;
  /** Set when the extension asked the running build to stop. */
  cancelRequested = false;
  constructor(
    readonly dir: string,
    hooks: ConstructorParameters<typeof Scheduler>[0],
  ) {
    this.scheduler = new Scheduler(hooks);
  }
}

export class BuildController implements vscode.Disposable {
  private readonly roots = new Map<string, Root>();
  private readonly output = vscode.window.createOutputChannel("oxabl build");
  private readonly diagnostics = vscode.languages.createDiagnosticCollection("oxabl build");
  private readonly item = vscode.window.createStatusBarItem("oxabl.build", vscode.StatusBarAlignment.Left, 0);
  // VS Code restarts an icon's animation each time an item's text changes, so
  // the spinner has an item of its own whose text never changes.
  private readonly spinner = vscode.window.createStatusBarItem("oxabl.build.spinner", vscode.StatusBarAlignment.Left, 0.001);
  private readonly subscriptions: vscode.Disposable[] = [];
  private readonly shownErrors = new Set<string>();
  private started = false;
  private discovering: Promise<void> = Promise.resolve();
  /** Modification time and size of each input file at its last event. */
  private readonly stamps = new Map<string, string>();
  private firstReported = false;
  private statusTimer: NodeJS.Timeout | undefined;

  constructor(private readonly deps: BuildDeps) {
    this.item.name = "oxabl build";
    this.spinner.name = "oxabl build progress";
    this.spinner.text = "$(sync~spin)";
    this.spinner.tooltip = "oxabl build is running";
  }

  private enabled(): boolean {
    return vscode.workspace.getConfiguration("oxabl.build").get<boolean>("enable", true);
  }

  private extraArgs(): string[] {
    return vscode.workspace.getConfiguration("oxabl.build").get<string[]>("args", []);
  }

  /** Begin watching and build every root once. A second call does nothing. */
  async start(): Promise<void> {
    if (this.started) {
      return;
    }
    this.started = true;
    for (const glob of ["**/*.{p,w,cls,i,v,df}", "**/oxabl.toml", "**/.oxabl/cache.toml"]) {
      const w = vscode.workspace.createFileSystemWatcher(glob);
      this.subscriptions.push(
        w,
        w.onDidCreate((uri) => void this.onFile(uri, "disk")),
        w.onDidChange((uri) => void this.onFile(uri, "disk")),
        w.onDidDelete((uri) => void this.onFile(uri, "disk", true)),
      );
    }
    this.subscriptions.push(
      vscode.workspace.onDidSaveTextDocument((doc) => {
        if (doc.uri.scheme === "file") {
          void this.onFile(doc.uri, "save");
        }
      }),
    );
    await this.discover();
    for (const root of this.roots.values()) {
      root.scheduler.requestNow();
    }
  }

  /** Stop watching and stop every build; keeps the diagnostics. */
  stop(): void {
    if (!this.started) {
      return;
    }
    this.started = false;
    this.subscriptions.splice(0).forEach((d) => d.dispose());
    for (const root of this.roots.values()) {
      root.scheduler.cancelAll();
      root.status = { phase: "idle", last: "none" };
    }
    this.roots.clear();
    this.refreshStatus();
  }

  /** The binary or the configuration may have changed: read the roots again and build them. */
  async reconfigure(): Promise<void> {
    if (!this.started) {
      return;
    }
    await this.discover();
    for (const root of this.roots.values()) {
      root.scheduler.requestNow();
    }
  }

  /** The command `oxabl: Build`: build every root now, also when `oxabl.build.enable` is off. */
  async buildNow(): Promise<void> {
    await this.discover();
    if (this.roots.size === 0) {
      void vscode.window.showInformationMessage(
        "oxabl: no project to build. A build needs an oxabl.toml with [build].output and [build.workers].command.",
      );
      return;
    }
    for (const root of this.roots.values()) {
      root.scheduler.requestNow();
    }
  }

  /** The command `oxabl: Cancel Build`. */
  cancelBuilds(): void {
    for (const root of this.roots.values()) {
      root.scheduler.cancelAll();
    }
  }

  showLog(): void {
    this.output.show(true);
  }

  /** Candidate roots: the workspace folders and every other folder with an `oxabl.toml`. */
  private async candidates(): Promise<string[]> {
    const dirs = new Set<string>();
    for (const uri of await vscode.workspace.findFiles("**/oxabl.toml", "**/{node_modules,.git}/**")) {
      dirs.add(path.dirname(uri.fsPath));
    }
    return [...dirs];
  }

  /** Read the configuration of every candidate; keep the roots that build. One run at a time. */
  private discover(): Promise<void> {
    this.discovering = this.discovering.then(() => this.readRoots());
    return this.discovering;
  }

  private async readRoots(): Promise<void> {
    const binary = this.deps.binary();
    const dirs = binary ? await this.candidates() : [];
    const found = await Promise.all(
      dirs.map(async (dir) => [dir, binary ? await readRootConfig(binary, dir, this.extraArgs()) : undefined] as const),
    );
    for (const [dir, config] of found) {
      const existing = this.roots.get(dir);
      if (config && existing) {
        existing.config = config;
      } else if (config) {
        const root: Root = new Root(dir, {
          start: () => void this.run(root),
          cancel: () => this.cancel(root),
          debounceMs: () => vscode.workspace.getConfiguration("oxabl.build").get<number>("debounce", 500),
        });
        root.config = config;
        this.roots.set(dir, root);
        this.deps.log.info(`build root ${dir}`);
      } else if (existing) {
        existing.scheduler.cancelAll();
        this.roots.delete(dir);
        this.deps.log.info(`${dir} does not build any more`);
      } else {
        this.deps.log.info(`${dir} has an oxabl.toml but no build setup; not building it`);
      }
    }
    for (const dir of [...this.roots.keys()]) {
      if (!dirs.includes(dir)) {
        this.roots.get(dir)!.scheduler.cancelAll();
        this.roots.delete(dir);
      }
    }
  }

  /**
   * A file changed. A save and the watcher event it causes arrive apart; the
   * second one finds the same modification time and is dropped, so a build never
   * cancels itself.
   */
  private async onFile(uri: vscode.Uri, origin: "save" | "disk", deleted = false): Promise<void> {
    const file = uri.fsPath;
    if (!isBuildInput(file) || !this.enabled()) {
      return;
    }
    const dir = isConfigFile(file) ? undefined : ownerRoot(file, this.roots.keys());
    const root = dir && this.roots.get(dir);
    if (!isConfigFile(file) && (!root || isIgnored(file, root.config?.ignore ?? []))) {
      return;
    }
    const stat = deleted ? undefined : await fs.promises.stat(file).catch(() => undefined);
    if (stat) {
      const stamp = `${stat.mtimeMs}:${stat.size}`;
      if (this.stamps.get(file) === stamp) {
        return;
      }
      this.stamps.set(file, stamp);
    } else {
      this.stamps.delete(file);
    }
    if (!root) {
      void this.discover().then(() => this.roots.get(path.dirname(file))?.scheduler.request());
      return;
    }
    if (!stat) {
      this.applyMessages(root, path.relative(root.dir, file), []);
    }
    root.scheduler.request(origin === "disk" ? WATCHER_SLACK_MS : 0);
  }

  private cancel(root: Root): void {
    root.cancelRequested = true;
    root.child?.kill("SIGINT");
  }

  private async run(root: Root): Promise<void> {
    root.cancelRequested = false;
    root.status = { phase: "preparing" };
    this.refreshStatus();
    const binary = await this.deps.prepare();
    if (!binary) {
      root.status = { phase: "idle", last: { warning: "The oxabl CLI is missing or too old to build" } };
      this.refreshStatus();
      root.scheduler.exited();
      return;
    }
    if (root.cancelRequested) {
      root.scheduler.exited();
      return;
    }

    const args = ["build", "--root", root.dir, "--events", "--yield", "--wait-lock", "3600", ...this.extraArgs()];
    this.deps.log.info(`build: ${binary} ${args.join(" ")}`);
    this.output.appendLine(`$ oxabl ${args.join(" ")}`);
    const progress = new BuildProgress();
    const started = Date.now();
    let firstError: string | undefined;
    let spawnError: string | undefined;
    const child = cp.spawn(binary, args, { cwd: root.dir });
    root.child = child;
    root.status = { phase: "preparing" };

    readline.createInterface({ input: child.stdout }).on("line", (line) => {
      let event: unknown;
      try {
        event = JSON.parse(line);
      } catch {
        this.deps.log.warn(`build: not an event: ${line.slice(0, 200)}`);
        return;
      }
      for (const effect of progress.apply(event)) {
        this.applyMessages(root, effect.unit, effect.kind === "messages" ? effect.messages : []);
      }
      if (progress.total !== undefined) {
        root.status = { phase: "building", progress };
      }
      this.refreshStatus();
    });
    child.stderr.on("data", (chunk: Buffer) => {
      const text = chunk.toString("utf8");
      this.output.append(text);
      firstError ??= text.split("\n").find((l) => l.trim())?.trim();
    });
    child.on("error", (err) => {
      spawnError = err.message;
    });
    child.on("close", (code) => {
      root.child = undefined;
      const exit = code ?? progress.summary?.exit ?? -1;
      this.deps.log.info(`build: ${root.dir} exited ${exit}${root.cancelRequested ? " (cancelled)" : ""}`);
      this.finish(root, progress, exit, Date.now() - started, spawnError ?? firstError);
    });
  }

  private finish(root: Root, progress: BuildProgress, exit: number, elapsedMs: number, reason: string | undefined): void {
    if (exit === 130) {
      if (root.cancelRequested) {
        root.status = { phase: "idle", last: "none" };
        root.scheduler.exited();
      } else {
        root.status = { phase: "elsewhere" };
        root.scheduler.preempted();
      }
    } else if (exit === 0) {
      root.status = { phase: "idle", last: "ok" };
      setTimeout(() => {
        if (root.status.phase === "idle" && root.status.last === "ok") {
          root.status = { phase: "idle", last: "none" };
          this.refreshStatus();
        }
      }, OK_VISIBLE_MS);
      this.reportFirst(progress, exit, elapsedMs);
      root.scheduler.exited();
    } else if (exit === 1) {
      root.status = { phase: "idle", last: { failed: progress.summary?.failed ?? progress.failedNow } };
      this.reportFirst(progress, exit, elapsedMs);
      root.scheduler.exited();
    } else {
      root.status = { phase: "idle", last: { warning: reason ?? `oxabl build exited with code ${exit}` } };
      if (exit === 2 && reason && !this.shownErrors.has(reason)) {
        this.shownErrors.add(reason);
        void vscode.window
          .showErrorMessage(`oxabl build: ${reason}`, "Show Build Log")
          .then((choice) => choice && this.showLog());
      }
      root.scheduler.exited();
    }
    this.refreshStatus();
  }

  /** The notification of the first build that ends in this window. Later builds show the status bar only. */
  private reportFirst(progress: BuildProgress, exit: number, elapsedMs: number): void {
    if (this.firstReported) {
      return;
    }
    this.firstReported = true;
    if (exit === 0) {
      void vscode.window.showInformationMessage(finishedText(progress, elapsedMs));
    } else {
      const n = progress.summary?.failed ?? progress.failedNow;
      void vscode.window
        .showErrorMessage(`oxabl build failed: ${n} ${n === 1 ? "unit" : "units"} failed`, "Show Problems")
        .then((choice) => choice && vscode.commands.executeCommand("workbench.actions.view.problems"));
    }
  }

  /** Replace the messages of one unit and refresh the diagnostics of the files it touched. */
  private applyMessages(root: Root, unit: string, messages: BuildMessage[]): void {
    for (const file of root.messages.set(unit, messages)) {
      const uri = vscode.Uri.file(path.join(root.dir, file));
      this.diagnostics.set(
        uri,
        root.messages.forFile(file).map((m) => {
          const line = Math.max(m.row, 1) - 1;
          const col = Math.max(m.column, 1) - 1;
          const d = new vscode.Diagnostic(
            new vscode.Range(line, col, line, col),
            m.text,
            m.type === "warning" ? vscode.DiagnosticSeverity.Warning : vscode.DiagnosticSeverity.Error,
          );
          d.source = "oxabl build";
          if (m.number) {
            d.code = m.number;
          }
          return d;
        }),
      );
    }
  }

  private refreshStatus(): void {
    if (this.statusTimer) {
      return;
    }
    this.statusTimer = setTimeout(() => {
      this.statusTimer = undefined;
      const view = renderStatus([...this.roots.values()].map((r) => r.status));
      if (!view) {
        this.item.hide();
        this.spinner.hide();
        return;
      }
      if (view.busy) {
        this.spinner.show();
      } else {
        this.spinner.hide();
      }
      this.item.text = view.text;
      this.item.tooltip = view.tooltip;
      this.item.command =
        view.action === "problems"
          ? "workbench.actions.view.problems"
          : view.action === "log"
            ? "oxabl.showBuildLog"
            : undefined;
      this.item.show();
    }, STATUS_THROTTLE_MS);
  }

  /** Stop every build: `SIGINT`, then `SIGKILL` after 5 s. Resolves when the processes are gone. */
  async dispose(): Promise<void> {
    this.subscriptions.splice(0).forEach((d) => d.dispose());
    clearTimeout(this.statusTimer);
    const running = [...this.roots.values()].flatMap((r) => (r.child ? [r.child] : []));
    for (const root of this.roots.values()) {
      root.scheduler.cancelAll();
    }
    await Promise.all(
      running.map(
        (child) =>
          new Promise<void>((resolve) => {
            if (child.exitCode !== null || child.signalCode !== null) {
              resolve();
              return;
            }
            const timer = setTimeout(() => child.kill("SIGKILL"), KILL_AFTER_MS);
            child.once("close", () => {
              clearTimeout(timer);
              resolve();
            });
          }),
      ),
    );
    this.item.dispose();
    this.spinner.dispose();
    this.diagnostics.dispose();
    this.output.dispose();
  }
}
