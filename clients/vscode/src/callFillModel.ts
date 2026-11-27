/** The values of `oxabl.completion.callArguments`. */
export const CALL_ARGUMENTS = ["fill", "parentheses", "none"] as const;

export type CallArguments = (typeof CALL_ARGUMENTS)[number];

/**
 * The `completion` object the server reads from its initialization options
 * and from `workspace/didChangeConfiguration`. `callArguments` is the value of
 * `oxabl.completion.callArguments`; a value that is not set or not known is
 * `fill`. `fillCommand` says that the extension has the command
 * `oxabl.fillCall`, so the server may name it on an item.
 */
export function completionSettings(callArguments: unknown): {
  callArguments: CallArguments;
  fillCommand: true;
} {
  const known = CALL_ARGUMENTS.find((value) => value === callArguments);
  return { callArguments: known ?? "fill", fillCommand: true };
}

/** The answer of `oxabl/callArguments`: the snippet to insert at the cursor. */
export interface CallFillAnswer {
  snippet: string;
  /** Show signature help after the insert. */
  parameterHints: boolean;
}

/** The answer, or `undefined` for `null` and for a shape this extension does not know. */
export function readCallFill(answer: unknown): CallFillAnswer | undefined {
  if (typeof answer !== "object" || answer === null) {
    return undefined;
  }
  const { snippet, parameterHints } = answer as Record<string, unknown>;
  if (typeof snippet !== "string" || snippet.length === 0) {
    return undefined;
  }
  return { snippet, parameterHints: parameterHints === true };
}

/**
 * Whether the fill still belongs where it was asked: the document has the
 * version and the cursor the place that the request named. Any key the person
 * pressed while the server worked makes the answer stale.
 */
export function stillCurrent(
  asked: { version: number; line: number; character: number },
  now: { version: number; line: number; character: number },
): boolean {
  return asked.version === now.version && asked.line === now.line && asked.character === now.character;
}
