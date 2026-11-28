import * as path from "node:path";
import * as vscode from "vscode";
import type { LanguageClient } from "vscode-languageclient/node";

/** One root file that expands an include, as `oxabl/includerContexts` lists it. */
interface ContextRow {
  root: string;
  uri: string | null;
  direct: boolean;
}

interface ContextList {
  selected: string | null;
  contexts: ContextRow[];
  complete: boolean;
  reason: string | null;
}

const isInclude = (document: vscode.TextDocument | undefined): document is vscode.TextDocument =>
  !!document && document.languageId === "abl" && document.uri.scheme === "file" && /\.i$/i.test(document.uri.fsPath);

/**
 * The compilation context of an open include: a status bar item that shows it,
 * and the command `oxabl.selectIncluderContext` that chooses it. The server
 * keeps the selection; this keeps a copy to show. A new server starts with
 * every include in fragment mode, so `reset` clears the copy.
 */
export class IncluderContexts implements vscode.Disposable {
  private readonly item = vscode.window.createStatusBarItem("oxabl.includerContext", vscode.StatusBarAlignment.Right, 100);
  private readonly selected = new Map<string, string>();
  private readonly subscriptions: vscode.Disposable[];

  constructor(
    private readonly client: (uri: vscode.Uri | undefined) => LanguageClient | undefined,
    private readonly selectedChanged: (uri: vscode.Uri) => void = () => {},
  ) {
    this.item.name = "oxabl include context";
    this.item.command = "oxabl.selectIncluderContext";
    this.subscriptions = [
      this.item,
      vscode.window.onDidChangeActiveTextEditor(() => this.show()),
      vscode.workspace.onDidCloseTextDocument((document) => {
        this.selected.delete(document.uri.toString());
        this.show();
      }),
    ];
  }

  /**
   * Forget the selections of the documents that `serves` accepts (every
   * document without it): the server that held them stopped.
   */
  reset(serves?: (uri: vscode.Uri) => boolean): void {
    for (const uri of [...this.selected.keys()]) {
      if (!serves || serves(vscode.Uri.parse(uri))) {
        this.selected.delete(uri);
      }
    }
    this.show();
  }

  show(): void {
    const document = vscode.window.activeTextEditor?.document;
    if (!this.client(document?.uri) || !isInclude(document)) {
      this.item.hide();
      return;
    }
    const root = this.selected.get(document.uri.toString());
    if (root) {
      this.item.text = `$(references) oxabl: in ${path.basename(root)}`;
      this.item.tooltip = `The diagnostics of this include come from ${root}, with the include expanded in it. Click to choose another root or fragment mode.`;
    } else {
      this.item.text = "$(references) oxabl: fragment";
      this.item.tooltip =
        "This include is analysed alone, without a root that includes it, so some findings are left out. Click to choose a root file.";
    }
    this.item.show();
  }

  async select(): Promise<void> {
    const document = vscode.window.activeTextEditor?.document;
    const client = this.client(document?.uri);
    if (!client) {
      void vscode.window.showInformationMessage("oxabl: the language server is not running.");
      return;
    }
    if (!isInclude(document)) {
      void vscode.window.showInformationMessage("oxabl: open an include file (.i) to choose its compilation context.");
      return;
    }
    const textDocument = { uri: document.uri.toString() };
    let list: ContextList;
    try {
      list = await vscode.window.withProgress(
        { location: vscode.ProgressLocation.Window, title: "oxabl: finding the roots that include this file" },
        () => client.sendRequest<ContextList>("oxabl/includerContexts", { textDocument }),
      );
    } catch (err) {
      void vscode.window.showErrorMessage(`oxabl: cannot list the roots of this include: ${message(err)}`);
      return;
    }

    type Pick = vscode.QuickPickItem & { root: string | null };
    const mark = (root: string | null) => (list.selected === root ? "$(check) " : "");
    const folder = vscode.workspace.getWorkspaceFolder(document.uri)?.uri.fsPath;
    const items: Pick[] = [
      { label: `${mark(null)}Fragment mode`, description: "analyse the include alone", root: null },
      ...list.contexts.map((row) => ({
        label: `${mark(row.root)}${path.basename(row.root)}`,
        description: folder ? path.relative(folder, row.root) : row.root,
        detail: row.direct ? "includes this file" : "includes this file through another include",
        root: row.root,
      })),
    ];
    const picked = await vscode.window.showQuickPick(items, {
      title: "oxabl: Select Include Context",
      placeHolder: list.reason ?? "Select the root file to analyse this include in",
      matchOnDescription: true,
    });
    if (!picked) {
      return;
    }
    try {
      const answer = await client.sendRequest<{ selected: string | null }>("oxabl/selectIncluderContext", {
        textDocument,
        root: picked.root,
      });
      if (answer.selected) {
        this.selected.set(textDocument.uri, answer.selected);
      } else {
        this.selected.delete(textDocument.uri);
      }
      this.selectedChanged(document.uri);
    } catch (err) {
      void vscode.window.showErrorMessage(`oxabl: cannot select ${picked.root ?? "fragment mode"}: ${message(err)}`);
    }
    this.show();
  }

  dispose(): void {
    for (const subscription of this.subscriptions) {
      subscription.dispose();
    }
  }
}

const message = (err: unknown) => (err instanceof Error ? err.message : String(err));
