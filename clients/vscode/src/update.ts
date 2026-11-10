// Discovery, download and verification of extension updates from GitHub
// Releases.
//
// Like `server.ts`, this module is free of any `vscode` import so the whole
// flow runs under vitest against a local HTTP server. `updates.ts` drives it
// from the extension. Every failure throws an `UpdateError` with a short
// reason; nothing is installed on a failure (fail closed).

import * as crypto from "node:crypto";
import * as fs from "node:fs";
import * as http from "node:http";
import * as https from "node:https";
import * as path from "node:path";

export const DEFAULT_API_BASE = "https://api.github.com";
const REPO_PATH = "/repos/oxabl-project/oxabl";
const TAG_PREFIX = "oxabl_vscode-v";
const MAX_REDIRECTS = 5;
const TIMEOUT_MS = 30_000;
const MAX_JSON_BYTES = 5 * 1024 * 1024;
const MAX_CHECKSUM_BYTES = 4096;
const MAX_VSIX_BYTES = 200 * 1024 * 1024;
const MAX_PAGES = 10;

export class UpdateError extends Error {}

/** Where and how to talk to the release API. */
export interface UpdateEnv {
  /** API base URL without a trailing slash. */
  apiBase: string;
  /** Allow `http:` URLs. Set only when the base URL is a test hook. */
  allowHttp: boolean;
  /** Installed extension version, for the `User-Agent` header. */
  installedVersion: string;
  timeoutMs?: number;
}

/** A newer release whose assets are complete. */
export interface AvailableUpdate {
  version: string;
  vsixName: string;
  vsixUrl: string;
  sha256Url: string;
}

export type Version = [number, number, number];

/** Parse `X.Y.Z` (numbers only); anything else, a pre-release included, is `undefined`. */
export function parseVersion(text: string): Version | undefined {
  const m = /^(\d+)\.(\d+)\.(\d+)$/.exec(text);
  return m ? [Number(m[1]), Number(m[2]), Number(m[3])] : undefined;
}

export function compareVersions(a: Version, b: Version): number {
  for (let i = 0; i < 3; i++) {
    if (a[i] !== b[i]) {
      return a[i] < b[i] ? -1 : 1;
    }
  }
  return 0;
}

/** The leading `X.Y.Z` of an installed version, which may carry a suffix. */
function installedVersion(text: string): Version | undefined {
  const m = /^(\d+\.\d+\.\d+)/.exec(text);
  return m ? parseVersion(m[1]) : undefined;
}

/** The highest `oxabl_vscode-vX.Y.Z` release version among Git refs, if any. */
export function newestVersion(refs: unknown): string | undefined {
  if (!Array.isArray(refs)) {
    throw new UpdateError("unexpected tag list from the release API");
  }
  let best: Version | undefined;
  for (const entry of refs) {
    const ref = (entry as { ref?: unknown } | null)?.ref;
    if (typeof ref !== "string" || !ref.startsWith(`refs/tags/${TAG_PREFIX}`)) {
      continue;
    }
    const v = parseVersion(ref.slice(`refs/tags/${TAG_PREFIX}`.length));
    if (v && (!best || compareVersions(v, best) > 0)) {
      best = v;
    }
  }
  return best?.join(".");
}

/** Parse `sha256sum` output: a 64-hex digest, then optionally the file name. */
export function parseChecksum(text: string, vsixName: string): string {
  const m = /^([0-9a-f]{64})(?:\s+\*?(\S.*?))?$/i.exec(text.trim());
  if (!m) {
    throw new UpdateError("malformed checksum file");
  }
  if (m[2] !== undefined && m[2] !== vsixName) {
    throw new UpdateError("checksum file is for a different file");
  }
  return m[1].toLowerCase();
}

interface Response {
  body: Buffer;
  link: string | undefined;
}

function request(
  url: string,
  env: UpdateEnv,
  accept: string | undefined,
  maxBytes: number,
  redirects = MAX_REDIRECTS,
): Promise<Response> {
  return new Promise((resolve, reject) => {
    let parsed: URL;
    try {
      parsed = new URL(url);
    } catch {
      reject(new UpdateError(`invalid URL ${url}`));
      return;
    }
    const secure = parsed.protocol === "https:";
    if (!secure && !(parsed.protocol === "http:" && env.allowHttp)) {
      reject(new UpdateError(`refusing ${parsed.protocol} URL from ${parsed.hostname}`));
      return;
    }

    const headers: Record<string, string> = {
      "User-Agent": `oxabl-vscode/${env.installedVersion}`,
    };
    if (accept) {
      headers.Accept = accept;
    }

    const timeoutMs = env.timeoutMs ?? TIMEOUT_MS;
    let timer: NodeJS.Timeout | undefined;
    const req = (secure ? https : http).get(parsed, { headers }, (res) => {
      const status = res.statusCode ?? 0;

      if ([301, 302, 303, 307, 308].includes(status) && res.headers.location) {
        res.resume();
        clearTimeout(timer);
        if (redirects <= 0) {
          reject(new UpdateError(`too many redirects from ${parsed.hostname}`));
          return;
        }
        const next = new URL(res.headers.location, parsed).toString();
        request(next, env, accept, maxBytes, redirects - 1).then(resolve, reject);
        return;
      }
      if (status !== 200) {
        res.resume();
        clearTimeout(timer);
        reject(new UpdateError(`HTTP ${status} from ${parsed.hostname}`));
        return;
      }

      const chunks: Buffer[] = [];
      let size = 0;
      res.on("data", (chunk: Buffer) => {
        size += chunk.length;
        if (size > maxBytes) {
          req.destroy(new UpdateError(`response from ${parsed.hostname} is too large`));
          return;
        }
        chunks.push(chunk);
      });
      res.on("end", () => {
        clearTimeout(timer);
        resolve({ body: Buffer.concat(chunks), link: [res.headers.link ?? []].flat().join(", ") || undefined });
      });
      res.on("error", (err) => {
        clearTimeout(timer);
        reject(err);
      });
    });
    timer = setTimeout(() => {
      req.destroy(new UpdateError(`timeout talking to ${parsed.hostname}`));
    }, timeoutMs);
    req.on("error", (err) => {
      clearTimeout(timer);
      reject(
        err instanceof UpdateError
          ? err
          : new UpdateError(`${(err as NodeJS.ErrnoException).code ?? err.message} from ${parsed.hostname}`),
      );
    });
  });
}

const JSON_ACCEPT = "application/vnd.github+json";

async function getJson(url: string, env: UpdateEnv): Promise<{ json: unknown; next?: string }> {
  const res = await request(url, env, JSON_ACCEPT, MAX_JSON_BYTES);
  let json: unknown;
  try {
    json = JSON.parse(res.body.toString("utf8"));
  } catch {
    throw new UpdateError(`invalid JSON from ${new URL(url).hostname}`);
  }
  const next = res.link ? /<([^>]+)>\s*;\s*rel="next"/.exec(res.link)?.[1] : undefined;
  return { json, next };
}

/**
 * Look for a newer release than `installed`. Returns `undefined` when there is
 * none, or when the newest one has no complete assets yet.
 */
export async function findUpdate(
  installed: string,
  env: UpdateEnv,
  log: (message: string) => void,
): Promise<AvailableUpdate | undefined> {
  const current = installedVersion(installed);
  if (!current) {
    throw new UpdateError(`cannot parse the installed version ${installed}`);
  }

  let refs: unknown[] = [];
  let url: string | undefined = `${env.apiBase}${REPO_PATH}/git/matching-refs/tags/${TAG_PREFIX}`;
  for (let page = 0; url && page < MAX_PAGES; page++) {
    const { json, next } = await getJson(url, env);
    if (!Array.isArray(json)) {
      throw new UpdateError("unexpected tag list from the release API");
    }
    refs = refs.concat(json);
    url = next;
  }

  const newest = newestVersion(refs);
  if (!newest) {
    log("no oxabl_vscode release tags found");
    return undefined;
  }
  log(`installed v${installed}, newest tag v${newest}`);
  if (compareVersions(parseVersion(newest)!, current) <= 0) {
    return undefined;
  }

  const tag = `${TAG_PREFIX}${newest}`;
  const { json: release } = await getJson(`${env.apiBase}${REPO_PATH}/releases/tags/${tag}`, env);
  const r = release as { draft?: unknown; prerelease?: unknown; assets?: unknown } | null;
  if (!r || typeof r !== "object" || !Array.isArray(r.assets)) {
    throw new UpdateError("unexpected release JSON");
  }
  if (r.draft !== false || r.prerelease !== false) {
    log(`release ${tag} is a draft or a pre-release`);
    return undefined;
  }

  const vsixName = `oxabl-vscode-${newest}.vsix`;
  const urlOf = (name: string): string | undefined => {
    const asset = (r.assets as { name?: unknown; browser_download_url?: unknown }[]).find(
      (a) => a?.name === name,
    );
    return typeof asset?.browser_download_url === "string" ? asset.browser_download_url : undefined;
  };
  const vsixUrl = urlOf(vsixName);
  const sha256Url = urlOf(`${vsixName}.sha256`);
  if (!vsixUrl || !sha256Url) {
    log(`release ${tag} has no complete assets yet`);
    return undefined;
  }
  return { version: newest, vsixName, vsixUrl, sha256Url };
}

/**
 * Download the VSIX and its checksum, verify the digest, and write the VSIX
 * under `dir`. Returns the path. Nothing is written when verification fails.
 */
export async function downloadVerified(
  update: AvailableUpdate,
  env: UpdateEnv,
  dir: string,
): Promise<string> {
  const sumFile = await request(update.sha256Url, env, undefined, MAX_CHECKSUM_BYTES);
  const expected = parseChecksum(sumFile.body.toString("utf8"), update.vsixName);
  const vsix = await request(update.vsixUrl, env, undefined, MAX_VSIX_BYTES);
  const actual = crypto.createHash("sha256").update(vsix.body).digest("hex");
  if (actual !== expected) {
    throw new UpdateError("checksum mismatch");
  }
  await fs.promises.mkdir(dir, { recursive: true });
  const file = path.join(dir, update.vsixName);
  await fs.promises.writeFile(file, vsix.body);
  return file;
}
