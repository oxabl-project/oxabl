import cp from "node:child_process";
import fs from "node:fs";
import path from "node:path";
import {
  downloadAndUnzipVSCode,
  resolveCliArgsFromVSCodeExecutablePath,
} from "@vscode/test-electron";
import { _electron } from "playwright-core";
import { VSCODE_CACHE, WORK_DIR, FIXTURES_DIR } from "./paths.mjs";

// The one place the VS Code under test is pinned.
export const VSCODE_VERSION = "1.145.2";

export const THEME = "Default Dark Modern";

export function downloadVSCode() {
  return downloadAndUnzipVSCode({ version: VSCODE_VERSION, cachePath: VSCODE_CACHE });
}

// PATH without any directory that holds an `oxabl` executable, so the run
// behaves like a machine with no language server.
function pathWithoutOxabl() {
  return (process.env.PATH ?? "")
    .split(path.delimiter)
    .filter((dir) => {
      if (!dir) return false;
      try {
        fs.accessSync(path.join(dir, "oxabl"), fs.constants.X_OK);
        return false;
      } catch {
        return true;
      }
    })
    .join(path.delimiter);
}

// VSCODE_CLI=1 stops VS Code from loading the login shell's environment,
// which would put `oxabl` back on PATH.
function cleanEnv(extra = {}) {
  const env = { ...process.env, PATH: pathWithoutOxabl(), VSCODE_CLI: "1" };
  delete env.ELECTRON_RUN_AS_NODE;
  delete env.OXABL_UPDATE_API_URL;
  return { ...env, ...extra };
}

/** Profile directories made since the last call; the runner checks them for leaked processes. */
const madeProfiles = [];
export const takeProfiles = () => madeProfiles.splice(0);

/** Kills (by PID) and returns the processes whose command line holds one of `dirs`. */
export async function leakedProcesses(dirs) {
  const find = () => {
    const found = [];
    for (const pid of fs.readdirSync("/proc").filter((d) => /^\d+$/.test(d))) {
      if (Number(pid) === process.pid) continue;
      let cmd;
      try {
        cmd = fs.readFileSync(`/proc/${pid}/cmdline`, "utf8").replaceAll("\0", " ");
      } catch {
        continue;
      }
      if (dirs.some((d) => cmd.includes(d))) found.push({ pid: Number(pid), cmd: cmd.trim() });
    }
    return found;
  };
  // Processes need a moment to exit after the window closed.
  for (let i = 0; i < 20 && find().length; i++) await new Promise((r) => setTimeout(r, 250));
  const left = find();
  for (const { pid } of left) {
    try {
      process.kill(pid, "SIGKILL");
    } catch {}
  }
  return left;
}

/** A fresh user data dir, extensions dir and workspace for one scenario. */
export function makeProfile(name, { settings = {}, files = [] } = {}) {
  const root = path.join(WORK_DIR, "profiles", name);
  fs.rmSync(root, { recursive: true, force: true });
  madeProfiles.push(root);
  const profile = {
    root,
    userData: path.join(root, "user-data"),
    extensions: path.join(root, "extensions"),
    workspace: path.join(root, "workspace"),
  };
  fs.mkdirSync(path.join(profile.userData, "User"), { recursive: true });
  fs.mkdirSync(profile.extensions, { recursive: true });
  fs.mkdirSync(profile.workspace, { recursive: true });
  fs.writeFileSync(
    path.join(profile.userData, "User", "settings.json"),
    JSON.stringify(
      {
        "workbench.colorTheme": THEME,
        "chat.disableAIFeatures": true,
        "workbench.startupEditor": "none",
        "security.workspace.trust.enabled": false,
        "update.mode": "none",
        "telemetry.telemetryLevel": "off",
        "editor.minimap.enabled": false,
        "git.enabled": false,
        ...settings,
      },
      null,
      2,
    ),
  );
  for (const f of files) fs.copyFileSync(path.join(FIXTURES_DIR, f), path.join(profile.workspace, f));
  return profile;
}

function runCli(exe, profile, args) {
  const [cli, ...base] = resolveCliArgsFromVSCodeExecutablePath(exe);
  const res = cp.spawnSync(
    cli,
    [...base, "--extensions-dir", profile.extensions, "--user-data-dir", profile.userData, ...args],
    { encoding: "utf8", env: cleanEnv(), timeout: 120_000 },
  );
  if (res.status !== 0) {
    throw new Error(`code ${args.join(" ")} exited ${res.status}: ${res.stderr || res.stdout}`);
  }
  return res.stdout;
}

export function installVsix(exe, profile, vsix) {
  return runCli(exe, profile, ["--install-extension", vsix]);
}

/** `publisher.name@version` lines of the profile's extensions dir. */
export function listExtensions(exe, profile) {
  return runCli(exe, profile, ["--list-extensions", "--show-versions"])
    .split("\n")
    .map((l) => l.trim())
    .filter(Boolean);
}

/** Starts VS Code on the profile and returns { app, page, close }. */
export async function launch(exe, profile, { open = [], env = {} } = {}) {
  const app = await _electron.launch({
    executablePath: exe,
    args: [
      "--user-data-dir", profile.userData,
      "--extensions-dir", profile.extensions,
      "--disable-workspace-trust",
      "--skip-welcome",
      "--skip-release-notes",
      "--disable-gpu",
      "--no-sandbox",
      "--password-store=basic",
      profile.workspace,
      ...open.map((f) => path.join(profile.workspace, f)),
    ],
    env: cleanEnv(env),
    timeout: 90_000,
  });
  const proc = app.process();
  const close = async () => {
    const closed = app.close().catch(() => {});
    const timer = new Promise((r) => setTimeout(r, 20_000, "timeout"));
    if ((await Promise.race([closed, timer])) === "timeout") proc.kill("SIGKILL");
    // Child processes of a killed VS Code may outlive it; make sure it is gone.
    if (proc.exitCode === null) {
      await new Promise((r) => {
        proc.once("exit", r);
        setTimeout(r, 5_000);
      });
    }
  };
  const page = await app.firstWindow();
  await page.setViewportSize({ width: 1280, height: 900 }).catch(() => {});
  await page.waitForSelector(".monaco-workbench", { timeout: 60_000 });
  return { app, page, close };
}
