import cp from "node:child_process";
import crypto from "node:crypto";
import fs from "node:fs";
import path from "node:path";
import { EXT_DIR, WORK_DIR } from "./paths.mjs";

export function manifestVersion(vsix) {
  const json = cp.execFileSync("unzip", ["-p", vsix, "extension/package.json"], { encoding: "utf8" });
  return JSON.parse(json).version;
}

/**
 * The VSIX under test: OXABL_E2E_VSIX when set, otherwise the package the
 * release pipeline builds (scripts/package-vsix.sh) from the current tree.
 */
export function packagedVsix() {
  if (process.env.OXABL_E2E_VSIX) return path.resolve(process.env.OXABL_E2E_VSIX);
  const out = path.join(WORK_DIR, "vsix");
  fs.rmSync(out, { recursive: true, force: true });
  cp.execFileSync("bash", [path.join(EXT_DIR, "scripts/package-vsix.sh"), out], {
    cwd: EXT_DIR,
    stdio: ["ignore", "inherit", "inherit"],
  });
  const vsix = fs.readdirSync(out).find((f) => f.endsWith(".vsix"));
  return path.join(out, vsix);
}

/** The same tree packaged as `version`, without touching package.json or git. */
export function newerVsix(version) {
  const file = path.join(WORK_DIR, "vsix-newer", `oxabl-vscode-${version}.vsix`);
  fs.mkdirSync(path.dirname(file), { recursive: true });
  cp.execFileSync(
    "pnpm",
    ["exec", "vsce", "package", version, "--no-update-package-json", "--no-git-tag-version", "--no-dependencies", "--out", file],
    { cwd: EXT_DIR, stdio: ["ignore", "ignore", "inherit"] },
  );
  return file;
}

export const sha256 = (file) => crypto.createHash("sha256").update(fs.readFileSync(file)).digest("hex");
