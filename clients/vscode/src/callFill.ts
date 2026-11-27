import * as vscode from "vscode";
import type { LanguageClient } from "vscode-languageclient/node";

import { readCallFill, stillCurrent } from "./callFillModel";

/** The command the server names on a call item whose arguments it gives only after the insert. */
export const FILL_CALL = "oxabl.fillCall";

/**
 * Command `oxabl.fillCall`: VS Code runs it after it inserted the name of a
 * call item (a program after `RUN`). Ask the server for the arguments of that
 * call (`oxabl/callArguments`) and insert them as a snippet at the cursor. An
 * answer for a text the person changed in the meantime is dropped, and so is a
 * failed request: the name stays as it is.
 */
export async function fillCall(client: () => LanguageClient | undefined): Promise<void> {
  const running = client();
  const editor = vscode.window.activeTextEditor;
  if (!running || !editor || editor.document.languageId !== "abl") {
    return;
  }
  const document = editor.document;
  const position = editor.selection.active;
  const asked = { version: document.version, line: position.line, character: position.character };
  let answer: unknown;
  try {
    answer = await running.sendRequest("oxabl/callArguments", {
      textDocument: { uri: running.code2ProtocolConverter.asUri(document.uri), version: document.version },
      position: running.code2ProtocolConverter.asPosition(position),
    });
  } catch {
    return;
  }
  const fill = readCallFill(answer);
  const now = editor.selection.active;
  if (
    !fill ||
    vscode.window.activeTextEditor !== editor ||
    !stillCurrent(asked, { version: document.version, line: now.line, character: now.character })
  ) {
    return;
  }
  if (await editor.insertSnippet(new vscode.SnippetString(fill.snippet), position)) {
    if (fill.parameterHints) {
      await vscode.commands.executeCommand("editor.action.triggerParameterHints");
    }
  }
}
