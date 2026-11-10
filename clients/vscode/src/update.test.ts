import * as crypto from "node:crypto";
import * as fs from "node:fs";
import * as http from "node:http";
import * as os from "node:os";
import * as path from "node:path";
import { afterEach, beforeEach, describe, expect, it } from "vitest";

import {
  UpdateError,
  downloadVerified,
  findUpdate,
  newestVersion,
  parseChecksum,
  type UpdateEnv,
} from "./update";

const VSIX = Buffer.from("not really a vsix");
const DIGEST = crypto.createHash("sha256").update(VSIX).digest("hex");

interface Fake {
  tags: string[];
  release: Record<string, unknown> | undefined;
  sha256: string;
  status: Record<string, number>;
}

let server: http.Server;
let base: string;
let fake: Fake;
let tmp: string;

function env(): UpdateEnv {
  return { apiBase: base, allowHttp: true, installedVersion: "0.1.0", timeoutMs: 2000 };
}

function release(version: string, extra: Record<string, unknown> = {}) {
  const name = `oxabl-vscode-${version}.vsix`;
  return {
    draft: false,
    prerelease: false,
    assets: [
      { name, browser_download_url: `${base}/dl/${name}` },
      { name: `${name}.sha256`, browser_download_url: `${base}/redirect/${name}.sha256` },
    ],
    ...extra,
  };
}

beforeEach(async () => {
  fake = { tags: [], release: undefined, sha256: "", status: {} };
  server = http.createServer((req, res) => {
    const url = req.url ?? "";
    const forced = fake.status[url];
    if (forced) {
      res.writeHead(forced).end();
    } else if (url.includes("/git/matching-refs/tags/oxabl_vscode-v")) {
      res.end(JSON.stringify(fake.tags.map((t) => ({ ref: `refs/tags/${t}` }))));
    } else if (url.includes("/releases/tags/")) {
      if (fake.release) {
        res.end(JSON.stringify(fake.release));
      } else {
        res.writeHead(404).end("{}");
      }
    } else if (url.startsWith("/redirect/")) {
      res.writeHead(302, { Location: url.replace("/redirect/", "/dl/") }).end();
    } else if (url.endsWith(".sha256")) {
      res.end(fake.sha256);
    } else if (url.endsWith(".vsix")) {
      res.end(VSIX);
    } else {
      res.writeHead(404).end();
    }
  });
  await new Promise<void>((resolve) => server.listen(0, "127.0.0.1", resolve));
  base = `http://127.0.0.1:${(server.address() as { port: number }).port}`;
  tmp = fs.mkdtempSync(path.join(os.tmpdir(), "oxabl-update-"));
});

afterEach(async () => {
  await new Promise((resolve) => server.close(resolve));
  fs.rmSync(tmp, { recursive: true, force: true });
});

describe("newestVersion", () => {
  it("takes the highest numeric version and ignores other tags", () => {
    const refs = [
      "oxabl_vscode-v0.9.0",
      "oxabl_vscode-v0.10.0",
      "oxabl_vscode-v1.0.0-rc.1",
      "oxabl_vscode-v0.10.0-beta",
      "oxabl_vscode-v9.9",
    ].map((t) => ({ ref: `refs/tags/${t}` }));
    expect(newestVersion(refs)).toBe("0.10.0");
  });

  it("rejects a non-array answer", () => {
    expect(() => newestVersion({ message: "x" })).toThrow(UpdateError);
  });
});

describe("parseChecksum", () => {
  it("accepts sha256sum output and a bare digest", () => {
    expect(parseChecksum(`${DIGEST.toUpperCase()}  a.vsix\n`, "a.vsix")).toBe(DIGEST);
    expect(parseChecksum(`${DIGEST} *a.vsix`, "a.vsix")).toBe(DIGEST);
    expect(parseChecksum(DIGEST, "a.vsix")).toBe(DIGEST);
  });

  it("rejects another file name and junk", () => {
    expect(() => parseChecksum(`${DIGEST}  b.vsix`, "a.vsix")).toThrow("different file");
    expect(() => parseChecksum("deadbeef  a.vsix", "a.vsix")).toThrow("malformed");
  });
});

describe("findUpdate", () => {
  const log = () => {};

  it("finds a newer release with both assets", async () => {
    fake.tags = ["oxabl_vscode-v0.1.0", "oxabl_vscode-v0.2.0"];
    fake.release = release("0.2.0");
    const update = await findUpdate("0.1.0", env(), log);
    expect(update?.version).toBe("0.2.0");
    expect(update?.vsixName).toBe("oxabl-vscode-0.2.0.vsix");
  });

  it("reports nothing when the installed version is the newest", async () => {
    fake.tags = ["oxabl_vscode-v0.1.0"];
    expect(await findUpdate("0.1.0", env(), log)).toBeUndefined();
  });

  it("reports nothing for a draft, a pre-release or a missing asset", async () => {
    fake.tags = ["oxabl_vscode-v0.2.0"];
    fake.release = release("0.2.0", { draft: true });
    expect(await findUpdate("0.1.0", env(), log)).toBeUndefined();
    fake.release = release("0.2.0", { prerelease: true });
    expect(await findUpdate("0.1.0", env(), log)).toBeUndefined();
    fake.release = { ...release("0.2.0"), assets: [] };
    expect(await findUpdate("0.1.0", env(), log)).toBeUndefined();
  });

  it("fails on an HTTP error with the status and host", async () => {
    fake.status["/repos/oxabl-project/oxabl/git/matching-refs/tags/oxabl_vscode-v"] = 500;
    await expect(findUpdate("0.1.0", env(), log)).rejects.toThrow("HTTP 500 from 127.0.0.1");
  });

  it("refuses http when the test hook is not set", async () => {
    await expect(
      findUpdate("0.1.0", { ...env(), allowHttp: false }, log),
    ).rejects.toThrow("refusing http:");
  });
});

describe("downloadVerified", () => {
  async function update() {
    fake.tags = ["oxabl_vscode-v0.2.0"];
    fake.release = release("0.2.0");
    const found = await findUpdate("0.1.0", env(), () => {});
    expect(found).toBeDefined();
    return found!;
  }

  it("writes the VSIX when the digest matches, following a redirect", async () => {
    const found = await update();
    fake.sha256 = `${DIGEST}  ${found.vsixName}\n`;
    const file = await downloadVerified(found, env(), path.join(tmp, "updates"));
    expect(fs.readFileSync(file)).toEqual(VSIX);
  });

  it("writes nothing on a digest mismatch", async () => {
    const found = await update();
    fake.sha256 = `${"0".repeat(64)}  ${found.vsixName}\n`;
    await expect(downloadVerified(found, env(), path.join(tmp, "updates"))).rejects.toThrow(
      "checksum mismatch",
    );
    expect(fs.existsSync(path.join(tmp, "updates"))).toBe(false);
  });
});
