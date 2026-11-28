import * as vscode from "vscode";
import type { LanguageClient } from "vscode-languageclient/node";

import type { Clients, FolderInfo } from "./clients";
import {
  type Range,
  type SchemaAnswer,
  type SchemaDatabase,
  type SchemaField,
  type SchemaIndex,
  type SchemaProblem,
  type SchemaSequence,
  type SchemaTable,
  type TableAnswer,
  failureText,
  filterSchema,
  indexText,
  DF_GLOB,
  outsideWatches,
  qualifiedName,
  tableHtml,
  typeText,
  viewMessage,
} from "./schemaModel";

export const VIEW = "oxabl.schema";

/** A filter that keeps at most this many tables shows their fields at once. */
const EXPAND_AT_MOST = 50;

/** Milliseconds of no typing before the filter applies. */
const FILTER_DELAY = 200;

/** The JSON-RPC error code of a failed request, if it has one. */
const errorCode = (err: unknown): number | undefined =>
  typeof err === "object" && err !== null && typeof (err as { code?: unknown }).code === "number" ? (err as { code: number }).code : undefined;

/**
 * A node. `scope` names the language client that answered: `""` in a window
 * with one client, the folder key when each folder has its own.
 */
type Node =
  | { kind: "folder"; folder: FolderInfo }
  | { kind: "notice"; scope: string; text: string }
  | { kind: "database"; scope: string; db: SchemaDatabase }
  | { kind: "problem"; scope: string; db: SchemaDatabase; problem: SchemaProblem; at: number }
  | { kind: "table"; scope: string; db: SchemaDatabase; table: SchemaTable }
  | { kind: "field"; scope: string; db: SchemaDatabase; table: SchemaTable; field: SchemaField }
  | { kind: "index"; scope: string; db: SchemaDatabase; table: SchemaTable; index: SchemaIndex }
  | { kind: "sequences"; scope: string; db: SchemaDatabase }
  | { kind: "sequence"; scope: string; db: SchemaDatabase; sequence: SchemaSequence };

/** What one language client answered, and how it shows. */
interface Scope {
  answer?: SchemaAnswer;
  failure?: string;
  shown: SchemaDatabase[];
  /** Whether a filtered tree opens its tables: only when there are few. */
  expandTables: boolean;
  /** Counts requests, so an answer to an older one never replaces a newer one. */
  requested: number;
  /** Whether an answer arrived once. */
  loaded: boolean;
  pending?: Promise<void>;
}

/** Where "Open in .df" goes: a `.df` URI and the range of the name. */
interface Place {
  uri: string;
  range: Range | null;
}

const markdown = (rows: [string, string | number | null | undefined][]): vscode.MarkdownString => {
  const text = new vscode.MarkdownString();
  for (const [key, value] of rows) {
    if (value !== null && value !== undefined && value !== "") {
      text.appendMarkdown(`**${key}:** `);
      text.appendText(String(value));
      text.appendMarkdown("  \n");
    }
  }
  return text;
};

/**
 * The schema view: the configured `.df` files as databases, tables, fields,
 * indexes and sequences, from `oxabl/schema`. It asks the server when it first
 * shows, when a `.df` or `oxabl.toml` changes, when the server restarts, and
 * on Refresh. A table's detail page asks `oxabl/schemaTable`. In a window with
 * one folder or none, the tree lists the databases of the one client. In a
 * window with several folders, it has one node for each owning folder, and
 * opening a node starts the client of that folder and lists its databases.
 */
export class SchemaBrowser implements vscode.TreeDataProvider<Node>, vscode.Disposable {
  private readonly changed = new vscode.EventEmitter<Node | undefined>();
  readonly onDidChangeTreeData = this.changed.event;
  private readonly view: vscode.TreeView<Node>;
  private readonly subscriptions: vscode.Disposable[];
  private readonly panels = new Map<
    string,
    { panel: vscode.WebviewPanel; scope: string; db: string; table: string; requested: number }
  >();
  /** Watchers outside the workspace folders, by directory and glob. */
  private readonly outside = new Map<string, vscode.FileSystemWatcher>();
  private readonly scopes = new Map<string, Scope>();
  private filter = "";
  private timer: NodeJS.Timeout | undefined;
  private filterTimer: NodeJS.Timeout | undefined;

  constructor(private readonly clients: Clients) {
    this.view = vscode.window.createTreeView(VIEW, { treeDataProvider: this, showCollapseAll: true });
    const df = vscode.workspace.createFileSystemWatcher(`**/${DF_GLOB}`);
    const config = vscode.workspace.createFileSystemWatcher("**/oxabl.toml");
    const later = () => this.refreshSoon();
    this.subscriptions = [
      this.changed,
      this.view,
      df,
      config,
      df.onDidChange(later),
      df.onDidCreate(later),
      df.onDidDelete(later),
      config.onDidChange(later),
      config.onDidCreate(later),
      config.onDidDelete(later),
      this.view.onDidCollapseElement((event) => {
        const scope = event.element.kind === "folder" ? this.scopes.get(event.element.folder.key) : undefined;
        if (scope && !scope.answer) {
          scope.failure = undefined;
        }
      }),
      this.view.onDidChangeVisibility((event) => {
        if (event.visible && this.single && !this.scopes.get("")?.loaded) {
          void this.refresh();
        }
      }),
      vscode.commands.registerCommand("oxabl.schema.refresh", () => this.refresh()),
      vscode.commands.registerCommand("oxabl.schema.filter", () => this.askFilter()),
      vscode.commands.registerCommand("oxabl.schema.clearFilter", () => this.setFilter("")),
      vscode.commands.registerCommand("oxabl.schema.open", (target?: Node | Place) => this.open(target)),
      vscode.commands.registerCommand("oxabl.schema.copyName", (node?: Node) => this.copy(node, false)),
      vscode.commands.registerCommand("oxabl.schema.copyQualifiedName", (node?: Node) => this.copy(node, true)),
      vscode.commands.registerCommand("oxabl.schema.showTable", (node?: Node) => this.showTable(node)),
    ];
    void vscode.commands.executeCommand("setContext", "oxabl.schema.available", true);
    if (this.view.visible && this.single) {
      void this.refresh();
    }
  }

  /** Whether the window has one client for all files (see `Clients.mode`). */
  private get single(): boolean {
    return this.clients.mode === "single";
  }

  /** A client started or failed to start (`folder` is `undefined` for the one client): ask again when its answer was shown. */
  serverChanged(folder: FolderInfo | undefined): void {
    const scope = this.scopes.get(folder?.key ?? "");
    if (folder ? scope : scope?.loaded || this.view.visible) {
      void this.refreshScope(folder?.key ?? "", false);
    }
  }

  /** The keys of the folders whose node shows an answer. */
  loadedFolders(): string[] {
    return [...this.scopes].filter(([key, scope]) => key !== "" && scope.loaded).map(([key]) => key);
  }

  /** The folders or the mode changed: drop what no longer has a client, and list again. */
  layoutChanged(): void {
    const keys = this.single ? [""] : this.clients.owning().map((folder) => folder.key);
    for (const key of [...this.scopes.keys()]) {
      if (!keys.includes(key)) {
        this.scopes.delete(key);
      }
    }
    for (const [key, entry] of [...this.panels]) {
      if (!keys.includes(entry.scope)) {
        entry.panel.dispose();
        this.panels.delete(key);
      }
    }
    this.watchOutside();
    this.updateMessage();
    this.changed.fire(undefined);
    if (this.single && this.view.visible) {
      void this.refresh();
    }
  }

  private refreshSoon(): void {
    if (this.single ? !this.scopes.get("")?.loaded : this.scopes.size === 0) {
      return;
    }
    clearTimeout(this.timer);
    this.timer = setTimeout(() => void this.refresh(false), 300);
  }

  /**
   * Ask again. With several folders, only the folders that were opened are
   * asked; `start` lets the command start a client that is not running.
   */
  async refresh(start = true): Promise<void> {
    if (this.single) {
      await this.refreshScope("", false);
    } else {
      await Promise.all([...this.scopes.keys()].map((key) => this.refreshScope(key, start)));
    }
  }

  private scope(key: string): Scope {
    let scope = this.scopes.get(key);
    if (!scope) {
      scope = { shown: [], expandTables: false, requested: 0, loaded: false };
      this.scopes.set(key, scope);
    }
    return scope;
  }

  /** The client that answers for `key`, started first when `start` is set and the folder has none. */
  private async clientOf(key: string, start: boolean): Promise<LanguageClient | undefined> {
    if (key === "") {
      return this.clients.forUri(undefined);
    }
    const folder = this.clients.owning().find((owner) => owner.key === key);
    if (!folder) {
      return undefined;
    }
    return start ? this.clients.startFolder(folder) : this.clients.forFolder(folder);
  }

  private refreshScope(key: string, start: boolean): Promise<void> {
    const scope = this.scope(key);
    const run = this.ask(key, scope, start).finally(() => {
      if (scope.pending === run) {
        scope.pending = undefined;
      }
    });
    scope.pending = run;
    return run;
  }

  private async ask(key: string, scope: Scope, start: boolean): Promise<void> {
    const requested = ++scope.requested;
    const client = await this.clientOf(key, start);
    // A start tells `serverChanged`, which asks again while this ask still waits for its start: the
    // newer ask answers, and this one must neither send a second request (the server drops the
    // older of two) nor overwrite the scope.
    if (requested !== scope.requested || this.scopes.get(key) !== scope) {
      return;
    }
    if (!client) {
      this.install(scope, undefined, "The oxabl language server is not running.");
      return;
    }
    let answer: SchemaAnswer;
    try {
      answer = await client.sendRequest<SchemaAnswer>("oxabl/schema", {});
    } catch (err) {
      if (requested === scope.requested) {
        this.install(scope, undefined, failureText("The schema", errorCode(err), err instanceof Error ? err.message : String(err)));
      }
      return;
    }
    if (requested !== scope.requested) {
      return;
    }
    scope.loaded = true;
    this.install(scope, answer);
    this.watchOutside();
    for (const [panelKey, panel] of this.panels) {
      if (panel.scope === key) {
        void this.renderPanel(panelKey);
      }
    }
  }

  private install(scope: Scope, answer: SchemaAnswer | undefined, failure?: string): void {
    scope.answer = answer;
    scope.failure = failure;
    this.filterScope(scope);
    this.updateMessage();
    this.changed.fire(undefined);
  }

  private filterScope(scope: Scope): void {
    scope.shown = filterSchema(scope.answer?.databases ?? [], this.filter);
    scope.expandTables = this.filter !== "" && scope.shown.reduce((n, d) => n + d.tables.length, 0) <= EXPAND_AT_MOST;
  }

  /** The view message of a window with one client. A folder shows its own as a row. */
  private updateMessage(): void {
    const scope = this.scopes.get("");
    this.view.message = this.single && scope ? this.message(scope) : undefined;
  }

  private message(scope: Scope): string | undefined {
    return viewMessage(scope.answer, scope.shown, this.filter, scope.failure);
  }

  /**
   * A workspace watcher sees only the workspace folders, so each schema
   * directory outside them (also one that holds no `.df` yet), and an
   * `oxabl.toml` above them, gets a watcher.
   */
  private watchOutside(): void {
    const folders = (vscode.workspace.workspaceFolders ?? []).map((f) => f.uri.fsPath);
    const wanted = new Map<string, { dir: string; glob: string }>();
    for (const scope of this.scopes.values()) {
      for (const w of scope.answer ? outsideWatches(scope.answer, folders) : []) {
        wanted.set(`${w.dir}\u0000${w.glob}`, w);
      }
    }
    for (const [key, watcher] of this.outside) {
      if (!wanted.has(key)) {
        watcher.dispose();
        this.outside.delete(key);
      }
    }
    for (const [key, { dir, glob }] of wanted) {
      if (!this.outside.has(key)) {
        const watcher = vscode.workspace.createFileSystemWatcher(new vscode.RelativePattern(vscode.Uri.file(dir), glob));
        const later = () => this.refreshSoon();
        watcher.onDidChange(later);
        watcher.onDidCreate(later);
        watcher.onDidDelete(later);
        this.outside.set(key, watcher);
      }
    }
  }

  private async askFilter(): Promise<void> {
    const input = vscode.window.createInputBox();
    input.title = "Filter the ABL schema";
    input.placeholder = "Part of a table or field name, label, description or dump name";
    input.value = this.filter;
    const before = this.filter;
    let accepted = false;
    // A large schema takes a moment to filter, so wait for a pause in typing.
    input.onDidChangeValue((value) => {
      clearTimeout(this.filterTimer);
      this.filterTimer = setTimeout(() => this.setFilter(value), FILTER_DELAY);
    });
    input.onDidAccept(() => {
      accepted = true;
      clearTimeout(this.filterTimer);
      this.setFilter(input.value);
      input.hide();
    });
    input.onDidHide(() => {
      clearTimeout(this.filterTimer);
      if (!accepted) {
        this.setFilter(before);
      }
      input.dispose();
    });
    input.show();
  }

  private setFilter(value: string): void {
    this.filter = value.trim();
    void vscode.commands.executeCommand("setContext", "oxabl.schema.filtered", this.filter !== "");
    for (const scope of this.scopes.values()) {
      this.filterScope(scope);
    }
    this.updateMessage();
    this.changed.fire(undefined);
  }

  getChildren(node?: Node): Node[] | Promise<Node[]> {
    if (!node) {
      return this.single
        ? this.databases("")
        : this.clients.owning().map((folder): Node => ({ kind: "folder", folder }));
    }
    switch (node.kind) {
      case "folder":
        return this.folderChildren(node.folder);
      case "database": {
        const { scope, db } = node;
        const children: Node[] = db.problems.map((problem, at) => ({ kind: "problem", scope, db, problem, at }));
        children.push(...db.tables.map((table): Node => ({ kind: "table", scope, db, table })));
        if (db.sequences.length) {
          children.push({ kind: "sequences", scope, db });
        }
        return children;
      }
      case "table":
        return [
          ...node.table.fields.map((field): Node => ({ kind: "field", scope: node.scope, db: node.db, table: node.table, field })),
          ...node.table.indexes.map((index): Node => ({ kind: "index", scope: node.scope, db: node.db, table: node.table, index })),
        ];
      case "sequences":
        return node.db.sequences.map((sequence) => ({ kind: "sequence", scope: node.scope, db: node.db, sequence }));
      default:
        return [];
    }
  }

  private databases(key: string): Node[] {
    return (this.scopes.get(key)?.shown ?? []).map((db): Node => ({ kind: "database", scope: key, db }));
  }

  /**
   * The rows of a folder. Opening the folder is the explicit action that
   * starts its client, so a folder whose files are never opened costs no
   * daemon until then.
   */
  private async folderChildren(folder: FolderInfo): Promise<Node[]> {
    const scope = this.scope(folder.key);
    // A start makes the client tell `serverChanged`, which asks once more: wait for the last request.
    const settled = async () => {
      while (scope.pending) {
        await scope.pending;
      }
    };
    await settled();
    // A failure is shown, not asked again: an answer that fails fires a tree change, which lists
    // this node again, so asking here would loop. Refresh, or collapse and expand, asks again.
    if (!scope.answer && !scope.failure) {
      await this.refreshScope(folder.key, true);
      await settled();
    }
    const message = this.message(scope);
    const notice: Node[] = message ? [{ kind: "notice", scope: folder.key, text: message }] : [];
    return [...notice, ...this.databases(folder.key)];
  }

  getTreeItem(node: Node): vscode.TreeItem {
    if (node.kind === "folder") {
      const item = new vscode.TreeItem(node.folder.name, vscode.TreeItemCollapsibleState.Collapsed);
      item.id = node.folder.key;
      item.tooltip = node.folder.fsPath;
      item.iconPath = new vscode.ThemeIcon("root-folder");
      item.contextValue = "folder";
      return item;
    }
    if (node.kind === "notice") {
      const item = new vscode.TreeItem(node.text, vscode.TreeItemCollapsibleState.None);
      item.id = [node.scope, "notice", node.text].join("\u0000");
      item.iconPath = new vscode.ThemeIcon("info");
      return item;
    }
    const expanded = this.scopes.get(node.scope)?.expandTables
      ? vscode.TreeItemCollapsibleState.Expanded
      : vscode.TreeItemCollapsibleState.Collapsed;
    // The filter is part of the id, so a filtered tree opens expanded.
    const id = (...parts: string[]) => [this.filter, node.scope, node.db.path, ...parts].join("\u0000");
    switch (node.kind) {
      case "database": {
        const { db } = node;
        // A database starts open: its rows are its tables, which start closed.
        const item = new vscode.TreeItem(db.name, vscode.TreeItemCollapsibleState.Expanded);
        item.id = id();
        item.description = `${db.tables.length} table(s)${db.complete ? "" : ", incomplete"}`;
        item.tooltip = markdown([
          ["File", db.path],
          ["Tables", db.tables.length],
          ["Sequences", db.sequences.length],
          ["Problems", db.problems.length || null],
        ]);
        item.iconPath = new vscode.ThemeIcon(db.complete ? "database" : "warning");
        item.contextValue = "database";
        item.resourceUri = db.uri ? vscode.Uri.parse(db.uri) : undefined;
        return item;
      }
      case "problem": {
        const item = new vscode.TreeItem(node.problem.message, vscode.TreeItemCollapsibleState.None);
        item.id = id("problem", String(node.at));
        item.description = node.problem.range ? `line ${node.problem.range.start.line + 1}` : node.problem.severity;
        item.iconPath = new vscode.ThemeIcon(node.problem.severity === "error" ? "error" : "warning");
        item.contextValue = "problem";
        item.command = this.openCommand(node);
        return item;
      }
      case "table": {
        const { table } = node;
        const item = new vscode.TreeItem(table.name, expanded);
        item.id = id("table", table.name);
        item.description = table.declared ? (table.label ?? "") : "fields added here";
        item.tooltip = markdown([
          ["Table", table.name],
          ["Label", table.label],
          ["Description", table.description],
          ["Dump name", table.dumpName],
          ["Area", table.area],
          ["Fields", table.fields.length],
          ["Indexes", table.indexes.length],
          ["Declared", table.declared ? null : `not in ${node.db.name}; this file adds to it`],
        ]);
        item.iconPath = new vscode.ThemeIcon("table");
        item.contextValue = "table";
        return item;
      }
      case "field": {
        const { field } = node;
        const item = new vscode.TreeItem(field.name, vscode.TreeItemCollapsibleState.None);
        item.id = id("table", node.table.name, "field", field.name);
        item.description = [typeText(field), field.mandatory ? "mandatory" : null, field.label].filter(Boolean).join("  ");
        item.tooltip = markdown([
          ["Field", qualifiedName(node.table, field)],
          ["Type", typeText(field)],
          ["Format", field.format],
          ["Label", field.label],
          ["Initial", field.initial],
          ["Mandatory", field.mandatory ? "yes" : null],
          ["Decimals", field.decimals],
          ["Description", field.description],
          ["Help", field.help],
        ]);
        item.iconPath = new vscode.ThemeIcon("symbol-field");
        item.contextValue = "field";
        item.command = this.openCommand(node);
        return item;
      }
      case "index": {
        const { index } = node;
        const item = new vscode.TreeItem(index.name, vscode.TreeItemCollapsibleState.None);
        item.id = id("table", node.table.name, "index", index.name);
        item.description = `index ${indexText(index)}`;
        item.tooltip = markdown([
          ["Index", index.name],
          ["Fields", index.fields.map((f) => f.name).join(", ")],
          ["Primary", index.primary ? "yes" : null],
          ["Unique", index.unique ? "yes" : null],
          ["Word", index.word ? "yes" : null],
          ["Inactive", index.inactive ? "yes" : null],
          ["Description", index.description],
        ]);
        item.iconPath = new vscode.ThemeIcon("list-ordered");
        item.contextValue = "index";
        item.command = this.openCommand(node);
        return item;
      }
      case "sequences": {
        const item = new vscode.TreeItem("Sequences", expanded);
        item.id = id("sequences");
        item.description = String(node.db.sequences.length);
        item.iconPath = new vscode.ThemeIcon("symbol-number");
        return item;
      }
      case "sequence": {
        const { sequence } = node;
        const item = new vscode.TreeItem(sequence.name, vscode.TreeItemCollapsibleState.None);
        item.id = id("sequence", sequence.name);
        item.description = `initial ${sequence.initial ?? "?"}, increment ${sequence.increment ?? "?"}`;
        item.tooltip = markdown([
          ["Sequence", sequence.name],
          ["Initial", sequence.initial],
          ["Increment", sequence.increment],
          ["Minimum", sequence.minValue ?? "no limit"],
          ["Maximum", sequence.maxValue ?? "no limit"],
          ["Cycles", sequence.cycleOnLimit ? "yes" : "no"],
        ]);
        item.iconPath = new vscode.ThemeIcon("symbol-number");
        item.contextValue = "sequence";
        item.command = this.openCommand(node);
        return item;
      }
    }
  }

  private openCommand(node: Node): vscode.Command {
    return { command: "oxabl.schema.open", title: "Open in .df", arguments: [node] };
  }

  /** The `.df` place of a node: the line of its `ADD`, at its name. */
  private place(target: Node | Place | undefined): Place | undefined {
    if (!target) {
      const selected = this.view.selection[0];
      return selected ? this.place(selected) : undefined;
    }
    if (!("kind" in target)) {
      return target;
    }
    if (target.kind === "folder" || target.kind === "notice") {
      return undefined;
    }
    const uri = target.db.uri;
    if (!uri) {
      return undefined;
    }
    switch (target.kind) {
      case "database":
        return { uri, range: null };
      case "problem":
        return { uri, range: target.problem.range };
      case "table":
        return { uri, range: target.table.range };
      case "field":
        return { uri, range: target.field.range };
      case "index":
        return { uri, range: target.index.range };
      case "sequence":
        return { uri, range: target.sequence.range };
      default:
        return { uri, range: null };
    }
  }

  private async open(target?: Node | Place): Promise<void> {
    const place = this.place(target);
    if (!place) {
      return;
    }
    const start = place.range ? new vscode.Position(place.range.start.line, place.range.start.character) : new vscode.Position(0, 0);
    await vscode.window.showTextDocument(vscode.Uri.parse(place.uri), {
      selection: new vscode.Range(start, start),
      preview: true,
    });
  }

  private async copy(node: Node | undefined, qualified: boolean): Promise<void> {
    node ??= this.view.selection[0];
    if (!node) {
      return;
    }
    let text: string | undefined;
    switch (node.kind) {
      case "database":
        text = node.db.name;
        break;
      case "table":
        text = node.table.name;
        break;
      case "field":
        text = qualified ? qualifiedName(node.table, node.field) : node.field.name;
        break;
      case "index":
        text = node.index.name;
        break;
      case "sequence":
        text = node.sequence.name;
        break;
    }
    if (text !== undefined) {
      await vscode.env.clipboard.writeText(text);
    }
  }

  private async showTable(node?: Node): Promise<void> {
    node ??= this.view.selection[0];
    if (!node || (node.kind !== "table" && node.kind !== "field" && node.kind !== "index") || !node.db.uri) {
      return;
    }
    const key = `${node.scope}\u0000${node.db.uri}\u0000${node.table.name.toLowerCase()}`;
    const open = this.panels.get(key);
    if (open) {
      open.panel.reveal();
      return;
    }
    const panel = vscode.window.createWebviewPanel(
      "oxabl.schemaTable",
      `${node.table.name} (${node.db.name})`,
      { viewColumn: vscode.ViewColumn.Active, preserveFocus: false },
      { enableScripts: false, enableCommandUris: ["oxabl.schema.open"], localResourceRoots: [] },
    );
    this.panels.set(key, { panel, scope: node.scope, db: node.db.uri, table: node.table.name, requested: 0 });
    panel.onDidDispose(() => this.panels.delete(key));
    await this.renderPanel(key);
  }

  private async renderPanel(key: string): Promise<void> {
    const entry = this.panels.get(key);
    if (!entry) {
      return;
    }
    const client = await this.clientOf(entry.scope, false);
    const csp = `default-src 'none'; style-src ${entry.panel.webview.cspSource} 'unsafe-inline';`;
    const requested = ++entry.requested;
    // A newer request, or a closed page, makes this answer stale.
    const stale = () => this.panels.get(key) !== entry || entry.requested !== requested;
    let answer: TableAnswer | null;
    try {
      if (!client) {
        throw new Error("the oxabl language server is not running");
      }
      answer = await client.sendRequest<TableAnswer | null>("oxabl/schemaTable", { database: entry.db, table: entry.table });
    } catch (err) {
      if (!stale()) {
        entry.panel.webview.html = notice(failureText("The table", errorCode(err), err instanceof Error ? err.message : String(err)), csp);
      }
      return;
    }
    if (stale()) {
      return;
    }
    if (!answer) {
      entry.panel.webview.html = notice(`${entry.table} is no longer in ${vscode.Uri.parse(entry.db).fsPath}.`, csp);
      return;
    }
    const uri = answer.database.uri ?? entry.db;
    entry.panel.webview.html = tableHtml(
      answer,
      (range) => `command:oxabl.schema.open?${encodeURIComponent(JSON.stringify([{ uri, range }]))}`,
      csp,
    );
  }

  dispose(): void {
    clearTimeout(this.timer);
    clearTimeout(this.filterTimer);
    for (const watcher of this.outside.values()) {
      watcher.dispose();
    }
    for (const { panel } of this.panels.values()) {
      panel.dispose();
    }
    for (const subscription of this.subscriptions) {
      subscription.dispose();
    }
  }
}

const notice = (text: string, csp: string) =>
  `<!DOCTYPE html><html lang="en"><head><meta charset="UTF-8"><meta http-equiv="Content-Security-Policy" content="${csp}"></head><body><p>${text
    .replace(/&/g, "&amp;")
    .replace(/</g, "&lt;")}</p></body></html>`;
