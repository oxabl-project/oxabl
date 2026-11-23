import fs from "node:fs";
import path from "node:path";
import { check, waitFor } from "./report.mjs";

const alive = (pid) => {
  try {
    process.kill(pid, 0);
    return true;
  } catch {
    return false;
  }
};

/** The daemon outlives the editor: stop the ones registered under `runtimeDir` and check that they exit. */
export async function stopDaemons(name, runtimeDir) {
  const dir = path.join(runtimeDir, "oxabl", "daemon");
  const pids = fs.existsSync(dir)
    ? fs
        .readdirSync(dir)
        .filter((x) => x.endsWith(".json"))
        .map((f) => JSON.parse(fs.readFileSync(path.join(dir, f), "utf8")).pid)
    : [];
  check(`${name}: the daemon registered itself`, pids.length > 0, `no registration under ${dir}`);
  for (const pid of pids.filter(alive)) process.kill(pid, "SIGTERM");
  await waitFor("the daemon to exit", async () => !pids.some(alive), { timeout: 10_000, interval: 200 }).catch(() => {});
  check(`${name}: the daemon exits on SIGTERM`, !pids.some(alive), `still running: ${pids.filter(alive).join(", ")}`);
}
