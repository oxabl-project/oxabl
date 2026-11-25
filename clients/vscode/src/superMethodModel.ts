/** The answer of `oxabl/superMethod`: the locations, and why the list can be short. */
export interface SuperMethodAnswer {
  locations: { uri: string; range: unknown }[];
  gaps: string[];
  /** Set when the cursor is on no method that the server reads: the list says nothing about overrides. */
  notAMethod?: string;
}

/** What the command does with an answer. */
export interface SuperMethodPlan {
  /** `open` goes to the one method, `peek` lists several, `none` shows only the message. */
  action: "open" | "peek" | "none";
  /** A message to show with the action, and how loudly. */
  message?: { level: "info" | "warning"; text: string };
}

/**
 * The reasons an answer can be short are never dropped: an empty answer that
 * has one says that oxabl does not know, not that nothing is overridden.
 */
export function planSuperMethod(answer: SuperMethodAnswer): SuperMethodPlan {
  if (answer.notAMethod !== undefined) {
    return { action: "none", message: { level: "info", text: `${answer.notAMethod}.` } };
  }
  const count = answer.locations.length;
  const gaps = answer.gaps.length > 0 ? ` The list can be short: ${answer.gaps.join("; ")}.` : "";
  if (count === 0) {
    return {
      action: "none",
      message:
        answer.gaps.length > 0
          ? { level: "warning", text: `oxabl: no super method found.${gaps}` }
          : { level: "info", text: "oxabl: this method does not override or implement another." },
    };
  }
  const action = count === 1 ? "open" : "peek";
  return answer.gaps.length > 0
    ? { action, message: { level: "warning", text: `oxabl: the super method${count === 1 ? "" : "s"} found.${gaps}` } }
    : { action };
}

/** Whether a value is an answer of `oxabl/superMethod`. A server without the request answers an error instead. */
export function isSuperMethodAnswer(value: unknown): value is SuperMethodAnswer {
  if (typeof value !== "object" || value === null) {
    return false;
  }
  const answer = value as Partial<SuperMethodAnswer>;
  return (
    Array.isArray(answer.locations) &&
    Array.isArray(answer.gaps) &&
    answer.gaps.every((gap) => typeof gap === "string") &&
    (answer.notAMethod === undefined || typeof answer.notAMethod === "string")
  );
}
