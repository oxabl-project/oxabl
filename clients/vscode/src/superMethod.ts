import * as vscode from "vscode";
import type { LanguageClient, Location } from "vscode-languageclient/node";

import { isSuperMethodAnswer, planSuperMethod } from "./superMethodModel";

/**
 * Command `oxabl.goToSuperMethod`: ask the server for the method that the
 * method at the cursor overrides or implements (`oxabl/superMethod`), and go to
 * it. Several methods are listed in a peek view. A reason the answer can be
 * short is always shown.
 */
export async function goToSuperMethod(client: (uri: vscode.Uri | undefined) => LanguageClient | undefined): Promise<void> {
  const editor = vscode.window.activeTextEditor;
  const running = client(editor?.document.uri);
  if (!running) {
    void vscode.window.showInformationMessage("oxabl: the language server is not running.");
    return;
  }
  if (!editor || editor.document.languageId !== "abl") {
    void vscode.window.showInformationMessage("oxabl: open an ABL file and put the cursor on a method.");
    return;
  }
  const params = running.code2ProtocolConverter.asTextDocumentPositionParams(editor.document, editor.selection.active);
  let answer: unknown;
  try {
    answer = await vscode.window.withProgress(
      { location: vscode.ProgressLocation.Window, title: "oxabl: finding the super method" },
      () => running.sendRequest("oxabl/superMethod", params),
    );
  } catch (err) {
    void vscode.window.showErrorMessage(`oxabl: cannot find the super method: ${err instanceof Error ? err.message : String(err)}`);
    return;
  }
  if (!isSuperMethodAnswer(answer)) {
    void vscode.window.showErrorMessage("oxabl: the language server gave no answer. Update oxabl and restart the server.");
    return;
  }
  const plan = planSuperMethod(answer);
  const locations = answer.locations.map((location) => running.protocol2CodeConverter.asLocation(location as Location));
  if (plan.action === "open") {
    await vscode.window.showTextDocument(locations[0].uri, { selection: locations[0].range });
  } else if (plan.action === "peek") {
    await vscode.commands.executeCommand("editor.action.peekLocations", editor.document.uri, editor.selection.active, locations, "peek");
  }
  if (plan.message) {
    const show = plan.message.level === "warning" ? vscode.window.showWarningMessage : vscode.window.showInformationMessage;
    void show(plan.message.text);
  }
}
