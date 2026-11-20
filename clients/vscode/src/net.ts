// HTTP access to GitHub for the extension update and the CLI install.
//
// `http` and `https` carry no `node:` prefix: VS Code's proxy support
// intercepts the plain names in every version.
//
// Like `server.ts`, this module is free of any `vscode` import so the flows
// that use it run under vitest against a local HTTP server. Every failure
// throws an `UpdateError` with a short reason.

import * as http from "http";
import * as https from "https";

export const DEFAULT_API_BASE = "https://api.github.com";
export const REPO_PATH = "/repos/oxabl-project/oxabl";
const MAX_REDIRECTS = 5;
const TIMEOUT_MS = 30_000;
const MAX_JSON_BYTES = 5 * 1024 * 1024;
const MAX_PAGES = 10;

export class UpdateError extends Error {}

/** Where and how to talk to the release API. */
export interface UpdateEnv {
  /** API base URL without a trailing slash. */
  apiBase: string;
  /** Allow `http:` URLs, and then only for a loopback host. Set only for the test hook. */
  allowHttp: boolean;
  /** Installed extension version, for the `User-Agent` header. */
  installedVersion: string;
  timeoutMs?: number;
}

/** Hosts that may use `http:` and may carry the API test hook. */
export function isLoopbackHost(hostname: string): boolean {
  return hostname === "127.0.0.1" || hostname === "localhost" || hostname === "[::1]";
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

/**
 * The leading `X.Y.Z` of an installed version, which may carry a suffix: a
 * build such as `0.3.0-dev` counts as `0.3.0`.
 */
export function installedVersion(text: string): Version | undefined {
  const m = /^(\d+\.\d+\.\d+)/.exec(text);
  return m ? parseVersion(m[1]) : undefined;
}

export interface Response {
  body: Buffer;
  link: string | undefined;
}

export function request(
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
    const loopbackHttp = parsed.protocol === "http:" && env.allowHttp && isLoopbackHost(parsed.hostname);
    if (!secure && !loopbackHttp) {
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
        // The timeout is for a stalled transfer, not for a slow one.
        timer?.refresh();
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

export const JSON_ACCEPT = "application/vnd.github+json";

export async function getJson(url: string, env: UpdateEnv): Promise<{ json: unknown; next?: string }> {
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


/** Every Git ref under `refs/tags/<prefix>`, all pages. A list cut at the page limit throws. */
export async function listTags(prefix: string, env: UpdateEnv): Promise<unknown[]> {
  let refs: unknown[] = [];
  let url: string | undefined = `${env.apiBase}${REPO_PATH}/git/matching-refs/tags/${prefix}`;
  for (let page = 0; url && page < MAX_PAGES; page++) {
    const { json, next } = await getJson(url, env);
    if (!Array.isArray(json)) {
      throw new UpdateError("unexpected tag list from the release API");
    }
    refs = refs.concat(json);
    url = next;
  }
  if (url) {
    throw new UpdateError("tag list truncated");
  }
  return refs;
}

/**
 * The API base from the test hook `OXABL_UPDATE_API_URL`, honoured only for a
 * loopback host. `warn` gets the reason for a rejected value.
 */
export function apiBaseFrom(
  override: string | undefined,
  warn: (message: string) => void,
): { apiBase: string; allowHttp: boolean } {
  if (override) {
    try {
      if (isLoopbackHost(new URL(override).hostname)) {
        return { apiBase: override.replace(/\/+$/, ""), allowHttp: true };
      }
    } catch {
      // Reported below with the other rejected values.
    }
    warn("ignoring OXABL_UPDATE_API_URL: the host is not a loopback address");
  }
  return { apiBase: DEFAULT_API_BASE, allowHttp: false };
}
