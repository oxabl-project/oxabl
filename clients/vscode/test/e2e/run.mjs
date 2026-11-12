// End-to-end test of the packaged extension in a real VS Code.
//   xvfb-run -a pnpm run test:e2e
// OXABL_E2E_SCENARIOS=highlight,markdown runs a subset (name prefixes);
// OXABL_E2E_VSIX=<file> tests that VSIX instead of packaging the tree.
import { SHOTS_DIR, WORK_DIR } from "./lib/paths.mjs";
import fs from "node:fs";
import { check, failures, total } from "./lib/report.mjs";
import { manifestVersion, newerVsix, packagedVsix } from "./lib/vsix.mjs";
import { VSCODE_VERSION, downloadVSCode, leakedProcesses, takeProfiles } from "./lib/vscode.mjs";
import {
  buildBinaryChanged,
  buildBurstDisk,
  buildBurstSave,
  buildDiagnostic,
  buildExternalPreemption,
  buildInstall,
  buildInstallBadChecksum,
  buildOpen,
  buildTooOld,
  buildTooOldManaged,
  buildSaveKeepsItsBuild,
  buildUntrusted,
} from "./scenarios/build.mjs";
import { highlightCls, highlightP, markdown } from "./scenarios/highlight.mjs";
import { serverCrashLoop, serverSoak } from "./scenarios/server.mjs";
import {
  updateAuto,
  updateAutoUpdateOff,
  updateChecksum,
  updateInstalledReload,
  updateManualNoTags,
  updateNotify,
  updateSkip,
  updateNothingNewer,
  updateServerError,
  updateThrottle,
  updateTwoProfiles,
} from "./scenarios/update.mjs";

const SCENARIOS = [
  ["highlight-p", highlightP],
  ["highlight-cls", highlightCls],
  ["markdown", markdown],
  ["update-auto", updateAuto],
  ["update-checksum", updateChecksum],
  ["update-notify", updateNotify],
  ["update-skip", updateSkip],
  ["update-none", updateNothingNewer],
  ["update-error", updateServerError],
  ["update-autoupdate-off", updateAutoUpdateOff],
  ["update-throttle", updateThrottle],
  ["update-installed-reload", updateInstalledReload],
  ["update-manual-no-tags", updateManualNoTags],
  ["update-two-profiles", updateTwoProfiles],
  ["build-open", buildOpen],
  ["build-save-keeps-its-build", buildSaveKeepsItsBuild],
  ["build-burst-save", buildBurstSave],
  ["build-burst-disk", buildBurstDisk],
  ["build-diagnostic", buildDiagnostic],
  ["build-untrusted", buildUntrusted],
  ["build-external-preemption", buildExternalPreemption],
  ["build-too-old", buildTooOld],
  ["build-too-old-managed", buildTooOldManaged],
  ["build-binary-changed", buildBinaryChanged],
  ["build-install", buildInstall],
  ["build-install-bad-checksum", buildInstallBadChecksum],
];

// Measurements, not checks of the product's output: they run only when named in
// OXABL_E2E_SCENARIOS (see scenarios/server.mjs for their settings).
const OPT_IN = [
  ["server-crash-loop", serverCrashLoop],
  ["server-soak", serverSoak],
];

if (!process.env.DISPLAY) {
  console.error("test:e2e needs a display. Run it as: xvfb-run -a pnpm run test:e2e");
  process.exit(2);
}

const wanted = (process.env.OXABL_E2E_SCENARIOS ?? "").split(",").map((s) => s.trim()).filter(Boolean);
const selected = [...SCENARIOS, ...(wanted.length ? OPT_IN : [])].filter(
  ([n]) => wanted.length === 0 || wanted.some((w) => n.startsWith(w)),
);
if (selected.length === 0) {
  console.error(`no scenario matches OXABL_E2E_SCENARIOS=${process.env.OXABL_E2E_SCENARIOS}`);
  process.exit(2);
}

const started = Date.now();
fs.rmSync(SHOTS_DIR, { recursive: true, force: true });
fs.rmSync(`${WORK_DIR}/profiles`, { recursive: true, force: true });

console.log(`VS Code ${VSCODE_VERSION}`);
const exe = await downloadVSCode();
const vsix = packagedVsix();
const installed = manifestVersion(vsix);
console.log(`VSIX under test: ${vsix} (v${installed})`);
const ctx = { exe, vsix, installed };
if (selected.some(([n]) => n.startsWith("update"))) ctx.newerVsix = newerVsix("99.0.0");

for (const [name, fn] of selected) {
  const t = Date.now();
  console.log(`\n== ${name}`);
  try {
    await fn(ctx);
  } catch (err) {
    check(`${name}: scenario ran to the end`, false, err instanceof Error ? err.message : String(err));
  }
  const leaked = await leakedProcesses(takeProfiles());
  check(
    `${name}: no process of the scenario is left running`,
    leaked.length === 0,
    `killed ${leaked.map((l) => `${l.pid} ${l.cmd.slice(0, 120)}`).join("; ")}`,
  );
  console.log(`   (${((Date.now() - t) / 1000).toFixed(1)}s)`);
}

const failed = failures();
console.log(`\n${total() - failed.length}/${total()} checks passed in ${((Date.now() - started) / 1000).toFixed(0)}s`);
console.log(`screenshots: ${SHOTS_DIR}`);
if (failed.length) {
  console.log(`failed:\n${failed.map((f) => `  - ${f.name}`).join("\n")}`);
}
process.exit(failed.length ? 1 : 0);
