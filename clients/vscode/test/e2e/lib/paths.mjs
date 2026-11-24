import path from "node:path";
import { fileURLToPath } from "node:url";

export const E2E_DIR = path.dirname(path.dirname(fileURLToPath(import.meta.url)));
export const EXT_DIR = path.resolve(E2E_DIR, "../..");
export const WORK_DIR = path.join(E2E_DIR, ".work");
export const SHOTS_DIR = path.join(E2E_DIR, "screenshots");
export const FIXTURES_DIR = path.join(E2E_DIR, "fixtures");
export const VSCODE_CACHE = path.join(EXT_DIR, ".vscode-test");
