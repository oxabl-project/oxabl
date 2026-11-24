import * as fs from "node:fs/promises";
import * as path from "node:path";
import * as vscode from "vscode";

import { keywordCaseFor, parseSnippets, renderBody, type Snippet } from "./snippets";

/** The file that a path names, or `undefined` when it is not there. */
async function readIfPresent(file: string): Promise<string | undefined> {
  try {
    return await fs.readFile(file, "utf8");
  } catch (err) {
    if ((err as NodeJS.ErrnoException).code === "ENOENT" || (err as NodeJS.ErrnoException).code === "ENOTDIR") {
      return undefined;
    }
    throw err;
  }
}

/**
 * Offer the snippets of `snippets/abl.json` as completion items in ABL files.
 * They need no language server. The keywords follow `keyword_case` of the
 * nearest `oxabl.toml`, and are upper case where it names none.
 */
export async function registerSnippets(
  extensionPath: string,
  log: vscode.LogOutputChannel,
): Promise<vscode.Disposable | undefined> {
  let snippets: Snippet[];
  try {
    snippets = parseSnippets(await fs.readFile(path.join(extensionPath, "snippets", "abl.json"), "utf8"));
  } catch (err) {
    log.error(`snippets: not loaded: ${err instanceof Error ? err.message : String(err)}`);
    return undefined;
  }
  return vscode.languages.registerCompletionItemProvider("abl", {
    async provideCompletionItems(document) {
      const keywordCase =
        document.uri.scheme === "file"
          ? await keywordCaseFor(path.dirname(document.uri.fsPath), readIfPresent).catch(() => "upper" as const)
          : "upper";
      return snippets.map((snippet) => {
        const item = new vscode.CompletionItem(snippet.prefix, vscode.CompletionItemKind.Snippet);
        const body = renderBody(snippet.body, keywordCase);
        item.insertText = new vscode.SnippetString(body);
        item.detail = snippet.description;
        item.documentation = new vscode.MarkdownString().appendCodeblock(body, "abl");
        return item;
      });
    },
  });
}
