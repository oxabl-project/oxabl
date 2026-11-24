import path from "node:path";

const EXT_STATE_KEY = "oxabl-project.oxabl";

async function openDb(profile) {
  let DatabaseSync;
  try {
    ({ DatabaseSync } = await import("node:sqlite"));
  } catch (err) {
    throw new Error(
      `node:sqlite is not available in Node ${process.versions.node} (needs 22.13 or newer): ${err.message}`,
    );
  }
  return new DatabaseSync(path.join(profile.userData, "User", "globalStorage", "state.vscdb"));
}

/**
 * Reads the extension's `globalState` from VS Code's state database and, when
 * `mutate` is given, writes back what it returns. VS Code must be closed.
 */
export async function extensionState(profile, mutate) {
  const db = await openDb(profile);
  try {
    const row = db.prepare("SELECT value FROM ItemTable WHERE key = ?").get(EXT_STATE_KEY);
    if (!row) throw new Error(`state.vscdb has no ${EXT_STATE_KEY} entry`);
    const state = JSON.parse(row.value);
    if (mutate) {
      const next = mutate(structuredClone(state));
      db.prepare("UPDATE ItemTable SET value = ? WHERE key = ?").run(JSON.stringify(next), EXT_STATE_KEY);
    }
    return state;
  } finally {
    db.close();
  }
}
