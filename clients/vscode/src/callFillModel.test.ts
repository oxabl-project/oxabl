import * as fs from "node:fs";
import * as path from "node:path";
import { describe, expect, it } from "vitest";

import { CALL_ARGUMENTS, completionSettings, readCallFill, stillCurrent } from "./callFillModel";

describe("completionSettings", () => {
  it("fills the arguments when the setting is not set or not known", () => {
    expect(completionSettings(undefined)).toEqual({ callArguments: "fill", fillCommand: true });
    expect(completionSettings("everything")).toEqual({ callArguments: "fill", fillCommand: true });
  });

  it("follows the setting", () => {
    expect(completionSettings("parentheses").callArguments).toBe("parentheses");
    expect(completionSettings("none").callArguments).toBe("none");
  });

  it("has the values of the manifest, fill by default", () => {
    const manifest = JSON.parse(fs.readFileSync(path.join(__dirname, "..", "package.json"), "utf8"));
    const setting = manifest.contributes.configuration.properties["oxabl.completion.callArguments"];
    expect(setting).toMatchObject({ type: "string", default: "fill" });
    expect(setting.enum).toEqual([...CALL_ARGUMENTS]);
    expect(setting.enumDescriptions).toHaveLength(CALL_ARGUMENTS.length);
  });
});

describe("readCallFill", () => {
  it("reads a snippet and whether to show signature help", () => {
    expect(readCallFill({ snippet: " (${1:pcIn}).$0", parameterHints: true })).toEqual({
      snippet: " (${1:pcIn}).$0",
      parameterHints: true,
    });
    expect(readCallFill({ snippet: ".$0" })).toEqual({ snippet: ".$0", parameterHints: false });
  });

  it("gives nothing for null, an empty snippet or another shape", () => {
    expect(readCallFill(null)).toBeUndefined();
    expect(readCallFill({ snippet: "" })).toBeUndefined();
    expect(readCallFill({ text: "x" })).toBeUndefined();
    expect(readCallFill("x")).toBeUndefined();
  });
});

describe("stillCurrent", () => {
  const asked = { version: 3, line: 1, character: 10 };

  it("keeps an answer for the text and the place that were asked", () => {
    expect(stillCurrent(asked, { ...asked })).toBe(true);
  });

  it("drops an answer after an edit or a move of the cursor", () => {
    expect(stillCurrent(asked, { ...asked, version: 4 })).toBe(false);
    expect(stillCurrent(asked, { ...asked, character: 11 })).toBe(false);
    expect(stillCurrent(asked, { ...asked, line: 2 })).toBe(false);
  });
});
