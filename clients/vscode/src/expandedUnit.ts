import * as path from "node:path";
import * as vscode from "vscode";
import type { LanguageClient } from "vscode-languageclient/node";

export const SCHEME = "oxabl-expanded";

interface Pos {
  line: number;
  character: number;
}
interface Loc {
  uri: string;
  range: { start: Pos; end: Pos };
}

/** The answer of `oxabl/expandedUnit`. */
interface ExpandedAnswer {
  root: string;
  mode: "root" | "fragment";
  status: "complete" | "reduced" | "failed";
  problems: { message: string; location: Loc | null }[];
  text: string;
  regions: { startLine: number; endLine: number; path: string; uri: string; depth: number; chain: Loc[] }[];
  lines: ([number, number] | null)[];
  files: string[];
  diagnostics: { range: { start: Pos; end: Pos }; severity: number; code: string; message: string; source: Loc }[];
  dependencies: string[];
}

interface View {
  /** The document the view expands. */
  source: vscode.Uri;
  answer: ExpandedAnswer;
  /** Counts requests, so an answer to an older one never replaces a newer one. */
  requested: number;
  dependencies: Set<string>;
  timer?: NodeJS.Timeout;
}

const range = (r: { start: Pos; end: Pos }) => new vscode.Range(r.start.line, r.start.character, r.end.line, r.end.character);
const location = (l: Loc) => new vscode.Location(vscode.Uri.parse(l.uri), range(l.range));
const severities = [
  vscode.DiagnosticSeverity.Error,
  vscode.DiagnosticSeverity.Error,
  vscode.DiagnosticSeverity.Warning,
  vscode.DiagnosticSeverity.Information,
  vscode.DiagnosticSeverity.Hint,
];

/**
 * The read-only "Expanded Compilation Unit" view: the source oxabl parsed for a
 * root file, or for an include in its selected context, with each include
 * folded, linked to its source and its chain, and the findings of the same
 * analysis. Edits to the root or to an include it reads, and a change of
 * `oxabl.toml`, ask the server again.
 */
export class ExpandedUnits implements vscode.Disposable, vscode.TextDocumentContentProvider {
  private readonly changed = new vscode.EventEmitter<vscode.Uri>();
  readonly onDidChange = this.changed.event;
  private readonly views = new Map<string, View>();
  private readonly diagnostics = vscode.languages.createDiagnosticCollection("oxabl-expanded");
  private readonly subscriptions: vscode.Disposable[];

  constructor(private readonly client: () => LanguageClient | undefined) {
    const selector = { scheme: SCHEME };
    const config = vscode.workspace.createFileSystemWatcher("**/oxabl.toml");
    this.subscriptions = [
      this.changed,
      this.diagnostics,
      config,
      vscode.workspace.registerTextDocumentContentProvider(SCHEME, this),
      vscode.languages.registerFoldingRangeProvider(selector, {
        provideFoldingRanges: (document) =>
          (this.views.get(document.uri.toString())?.answer.regions ?? []).map(
            (r) => new vscode.FoldingRange(r.startLine, r.endLine, vscode.FoldingRangeKind.Region),
          ),
      }),
      vscode.languages.registerDocumentLinkProvider(selector, {
        provideDocumentLinks: (document) => this.links(document),
      }),
      vscode.languages.registerDefinitionProvider(selector, {
        provideDefinition: (document, position) => this.sourceOf(document.uri, position.line),
      }),
      vscode.workspace.onDidChangeTextDocument((event) => this.touched(event.document.uri)),
      vscode.workspace.onDidSaveTextDocument((document) => this.touched(document.uri)),
      config.onDidChange(() => this.refreshAll()),
      config.onDidCreate(() => this.refreshAll()),
      config.onDidDelete(() => this.refreshAll()),
      vscode.workspace.onDidCloseTextDocument((document) => {
        if (document.uri.scheme === SCHEME) {
          clearTimeout(this.views.get(document.uri.toString())?.timer);
          this.views.delete(document.uri.toString());
          this.diagnostics.delete(document.uri);
        }
      }),
    ];
  }

  provideTextDocumentContent(uri: vscode.Uri): string {
    return this.views.get(uri.toString())?.answer.text ?? "/* oxabl: this expansion is no longer available; run oxabl: Show Expanded Compilation Unit again. */\n";
  }

  /** The view URI of a source document: `reads (expanded).p` for root `reads.p`. */
  private viewUri(source: vscode.Uri, root: string): vscode.Uri {
    const ext = path.extname(root);
    return vscode.Uri.from({
      scheme: SCHEME,
      path: `/${path.basename(root, ext)} (expanded)${ext}`,
      query: source.toString(),
    });
  }

  /** Command `oxabl.showExpandedUnit`. */
  async show(): Promise<void> {
    const document = vscode.window.activeTextEditor?.document;
    if (!this.client()) {
      void vscode.window.showInformationMessage("oxabl: the language server is not running.");
      return;
    }
    if (!document || document.languageId !== "abl" || document.uri.scheme !== "file") {
      void vscode.window.showInformationMessage("oxabl: open an ABL file to show its expanded compilation unit.");
      return;
    }
    const answer = await this.ask(document.uri);
    if (!answer) {
      return;
    }
    const uri = this.viewUri(document.uri, answer.root);
    for (const [key, view] of this.views) {
      if (view.source.toString() === document.uri.toString() && key !== uri.toString()) {
        this.views.delete(key);
      }
    }
    this.install(uri, document.uri, answer, 0);
    const shown = await vscode.window.showTextDocument(await vscode.workspace.openTextDocument(uri), {
      viewColumn: vscode.ViewColumn.Beside,
      preview: false,
    });
    await vscode.languages.setTextDocumentLanguage(shown.document, "abl");
    // Every include region starts folded; each unfolds on its own. The editor
    // asks for the folding ranges after it opens, so wait for them.
    await new Promise((resolve) => setTimeout(resolve, 300));
    await vscode.commands.executeCommand("editor.foldAll");
    if (answer.status !== "complete") {
      void vscode.window.showWarningMessage(
        answer.status === "failed"
          ? `oxabl: no expansion for ${path.basename(answer.root)}: ${answer.problems.map((p) => p.message).join("; ")}`
          : `oxabl: the expansion of ${path.basename(answer.root)} is reduced (${answer.problems.length} problem(s)); see the top of the view.`,
      );
    }
  }

  /** Ask again for every view that expands `source`, after its context changed. */
  refreshSource(source: vscode.Uri): void {
    for (const [key, view] of this.views) {
      if (view.source.toString() === source.toString()) {
        void this.refresh(key);
      }
    }
  }

  private async ask(source: vscode.Uri): Promise<ExpandedAnswer | undefined> {
    const client = this.client();
    if (!client) {
      return undefined;
    }
    try {
      return await client.sendRequest<ExpandedAnswer>("oxabl/expandedUnit", { textDocument: { uri: source.toString() } });
    } catch (err) {
      void vscode.window.showErrorMessage(`oxabl: cannot expand ${path.basename(source.fsPath)}: ${err instanceof Error ? err.message : String(err)}`);
      return undefined;
    }
  }

  private install(uri: vscode.Uri, source: vscode.Uri, answer: ExpandedAnswer, requested: number): void {
    const key = uri.toString();
    const previous = this.views.get(key);
    this.views.set(key, {
      source,
      answer,
      requested,
      dependencies: new Set([source.toString(), ...answer.dependencies]),
      timer: previous?.timer,
    });
    this.diagnostics.set(
      uri,
      answer.diagnostics.map((d) => {
        const diagnostic = new vscode.Diagnostic(range(d.range), d.message, severities[d.severity] ?? vscode.DiagnosticSeverity.Information);
        diagnostic.code = d.code;
        diagnostic.source = "oxabl";
        diagnostic.relatedInformation = [new vscode.DiagnosticRelatedInformation(location(d.source), `in ${path.basename(vscode.Uri.parse(d.source.uri).fsPath)}`)];
        return diagnostic;
      }),
    );
    this.changed.fire(uri);
  }

  private touched(uri: vscode.Uri): void {
    if (uri.scheme === SCHEME) {
      return;
    }
    const changed = uri.toString();
    for (const [key, view] of this.views) {
      if (view.dependencies.has(changed)) {
        clearTimeout(view.timer);
        view.timer = setTimeout(() => void this.refresh(key), 400);
      }
    }
  }

  private refreshAll(): void {
    for (const key of this.views.keys()) {
      void this.refresh(key);
    }
  }

  private async refresh(key: string): Promise<void> {
    const view = this.views.get(key);
    if (!view) {
      return;
    }
    const requested = ++view.requested;
    const answer = await this.ask(view.source);
    const current = this.views.get(key);
    // A newer request, or a closed view, makes this answer stale.
    if (!answer || !current || current.requested !== requested) {
      return;
    }
    this.install(vscode.Uri.parse(key), view.source, answer, requested);
  }

  private links(document: vscode.TextDocument): vscode.DocumentLink[] {
    const answer = this.views.get(document.uri.toString())?.answer;
    if (!answer) {
      return [];
    }
    const links: vscode.DocumentLink[] = [];
    for (const region of answer.regions) {
      const text = document.lineAt(region.startLine).text;
      const at = text.indexOf(region.path);
      if (at >= 0) {
        const link = new vscode.DocumentLink(new vscode.Range(region.startLine, at, region.startLine, at + region.path.length), vscode.Uri.parse(region.uri));
        link.tooltip = "Open the include file";
        links.push(link);
      }
      // `name:line` for each step of the chain, after "from".
      const from = text.indexOf("(from ");
      let cursor = from + "(from ".length;
      for (const step of region.chain) {
        const label = `${path.basename(vscode.Uri.parse(step.uri).fsPath)}:${step.range.start.line + 1}`;
        const found = from >= 0 ? text.indexOf(label, cursor) : -1;
        if (found < 0) {
          continue;
        }
        const target = vscode.Uri.parse(step.uri).with({ fragment: `L${step.range.start.line + 1},${step.range.start.character + 1}` });
        const link = new vscode.DocumentLink(new vscode.Range(region.startLine, found, region.startLine, found + label.length), target);
        link.tooltip = "Go to the include reference";
        links.push(link);
        cursor = found + label.length;
      }
    }
    return links;
  }

  private sourceOf(uri: vscode.Uri, line: number): vscode.Location | undefined {
    const answer = this.views.get(uri.toString())?.answer;
    const entry = answer?.lines[line];
    if (!answer || !entry) {
      return undefined;
    }
    return new vscode.Location(vscode.Uri.parse(answer.files[entry[0]]), new vscode.Position(entry[1], 0));
  }

  dispose(): void {
    for (const view of this.views.values()) {
      clearTimeout(view.timer);
    }
    for (const subscription of this.subscriptions) {
      subscription.dispose();
    }
  }
}
