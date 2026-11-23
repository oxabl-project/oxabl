import * as vscode from "vscode";
import type { LanguageClient, StaticFeature } from "vscode-languageclient/node";

/** The notification the server pushes, in the shape clangd uses. */
export const METHOD = "textDocument/inactiveRegions";

interface Pos {
  line: number;
  character: number;
}
interface InactiveRegionsParams {
  textDocument: { uri: string; version?: number | null };
  regions: { start: Pos; end: Pos }[];
}

/** How much of its colour inactive text keeps, as the C/C++ extension does. */
const OPACITY = "0.55";

const enabled = () => vscode.workspace.getConfiguration("oxabl").get<boolean>("inactiveRegions.enable", true);

/**
 * Dims the `&IF` branches the preprocessor does not compile, with an editor
 * decoration over the TextMate colours. The server sends the regions with each
 * diagnostics publish, for one document version; regions for a version the
 * editor no longer shows are dropped. `oxabl.inactiveRegions.enable` turns the
 * dimming off without a restart.
 */
export class InactiveRegions implements vscode.Disposable {
  private readonly decoration = vscode.window.createTextEditorDecorationType({
    opacity: OPACITY,
    rangeBehavior: vscode.DecorationRangeBehavior.ClosedClosed,
  });
  /** The last regions of each document, by URI. */
  private readonly regions = new Map<string, vscode.Range[]>();
  private readonly subscriptions: vscode.Disposable[];

  constructor() {
    this.subscriptions = [
      this.decoration,
      vscode.window.onDidChangeVisibleTextEditors(() => this.paintAll()),
      vscode.workspace.onDidCloseTextDocument((document) => this.regions.delete(document.uri.toString())),
      vscode.workspace.onDidChangeConfiguration((event) => {
        if (event.affectsConfiguration("oxabl.inactiveRegions")) {
          this.paintAll();
        }
      }),
    ];
  }

  /** Ask `client` for the regions and listen for them. Call before it starts. */
  attach(client: LanguageClient): void {
    const feature: StaticFeature = {
      fillClientCapabilities(capabilities) {
        const textDocument = (capabilities.textDocument ??= {}) as Record<string, unknown>;
        textDocument.inactiveRegionsCapabilities = { inactiveRegions: true };
      },
      initialize() {},
      getState: () => ({ kind: "static" }),
      clear() {},
    };
    client.registerFeature(feature);
    client.onNotification(METHOD, (params: InactiveRegionsParams) => this.receive(params));
  }

  /** Forget every region: the server that sent them stopped. */
  reset(): void {
    this.regions.clear();
    this.paintAll();
  }

  private receive(params: InactiveRegionsParams): void {
    const uri = params.textDocument.uri;
    const document = vscode.workspace.textDocuments.find((d) => d.uri.toString() === uri);
    if (!document) {
      return;
    }
    const version = params.textDocument.version;
    if (version !== undefined && version !== null && version !== document.version) {
      // Computed for text the editor no longer shows; the next publish follows.
      return;
    }
    this.regions.set(
      uri,
      params.regions.map((r) => new vscode.Range(r.start.line, r.start.character, r.end.line, r.end.character)),
    );
    for (const editor of vscode.window.visibleTextEditors) {
      if (editor.document.uri.toString() === uri) {
        this.paint(editor);
      }
    }
  }

  private paintAll(): void {
    for (const editor of vscode.window.visibleTextEditors) {
      this.paint(editor);
    }
  }

  private paint(editor: vscode.TextEditor): void {
    const ranges = enabled() ? (this.regions.get(editor.document.uri.toString()) ?? []) : [];
    editor.setDecorations(this.decoration, ranges);
  }

  dispose(): void {
    for (const subscription of this.subscriptions) {
      subscription.dispose();
    }
  }
}
