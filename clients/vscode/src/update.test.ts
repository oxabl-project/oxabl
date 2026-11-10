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
  highestInstalledVersion,
  isPendingReloadSpent,
  newestVersion,
  parseChecksum,
  parseVersion,
  pendingReloadVersion,
  type AvailableUpdate,
  type PendingState,
  type UpdateEnv,
} from "./update";

const VSIX = Buffer.from("not really a vsix");
const DIGEST = crypto.createHash("sha256").update(VSIX).digest("hex");

interface Fake {
  tags: string[];
  release: Record<string, unknown> | undefined;
  sha256: string;
  status: Record<string, number>;
  /** Answer every tag listing with a `next` link. */
  endless: boolean;
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
  fake = { tags: [], release: undefined, sha256: "", status: {}, endless: false };
  server = http.createServer((req, res) => {
    const url = req.url ?? "";
    const forced = fake.status[url];
    if (forced) {
      res.writeHead(forced).end();
    } else if (url.includes("/git/matching-refs/tags/oxabl_vscode-v")) {
      if (fake.endless) {
        res.setHeader("Link", `<${base}${url}>; rel="next"`);
      }
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
  const requests: string[] = [];

  beforeEach(() => {
    requests.length = 0;
    server.on("request", (req) => requests.push(req.url ?? ""));
  });

  const DIR = "/ext";
  const pendingOf = (version: string, onDisk: string | undefined = version): PendingState => ({
    record: { version, dir: DIR },
    dir: DIR,
    onDisk,
  });

  async function updateOf(installed: string, pending?: PendingState) {
    const check = await findUpdate(installed, env(), log, pending);
    return check.kind === "available" ? check.update : undefined;
  }

  it("finds a newer release with both assets", async () => {
    fake.tags = ["oxabl_vscode-v0.1.0", "oxabl_vscode-v0.2.0"];
    fake.release = release("0.2.0");
    const update = await updateOf("0.1.0");
    expect(update?.version).toBe("0.2.0");
    expect(update?.vsixName).toBe("oxabl-vscode-0.2.0.vsix");
  });

  it("reports current when the installed version is the newest", async () => {
    fake.tags = ["oxabl_vscode-v0.1.0"];
    expect(await findUpdate("0.1.0", env(), log)).toEqual({ kind: "current" });
  });

  it("never offers a downgrade", async () => {
    fake.tags = ["oxabl_vscode-v0.1.0", "oxabl_vscode-v0.2.0"];
    fake.release = release("0.2.0");
    expect(await findUpdate("0.3.0", env(), log)).toEqual({ kind: "current" });
    expect(requests.filter((u) => u.includes("/releases/tags/"))).toEqual([]);
  });

  it("counts a build with a suffix as its X.Y.Z", async () => {
    fake.tags = ["oxabl_vscode-v0.3.0"];
    fake.release = release("0.3.0");
    expect(await findUpdate("0.3.0-dev", env(), log)).toEqual({ kind: "current" });
    expect(requests.filter((u) => u.includes("/releases/tags/"))).toEqual([]);
  });

  it("reports pending-reload for a version on disk that is at least the newest tag", async () => {
    fake.tags = ["oxabl_vscode-v0.2.0"];
    expect(await findUpdate("0.1.0", env(), log, pendingOf("0.2.0"))).toEqual({
      kind: "pending-reload",
      version: "0.2.0",
    });
    expect(requests.filter((u) => u.includes("/releases/tags/"))).toEqual([]);
  });

  it("reports pending-reload with the installed version when it is above the newest tag", async () => {
    fake.tags = ["oxabl_vscode-v0.2.0"];
    expect(await findUpdate("0.1.0", env(), log, pendingOf("0.3.0"))).toEqual({
      kind: "pending-reload",
      version: "0.3.0",
    });
  });

  it("offers a version newer than the one that waits", async () => {
    fake.tags = ["oxabl_vscode-v0.2.0", "oxabl_vscode-v0.3.0"];
    fake.release = release("0.3.0");
    expect((await updateOf("0.1.0", pendingOf("0.2.0")))?.version).toBe("0.3.0");
  });

  it("offers the update when the record is for another folder", async () => {
    fake.tags = ["oxabl_vscode-v0.2.0"];
    fake.release = release("0.2.0");
    const pending = { ...pendingOf("0.2.0"), dir: "/other" };
    expect((await updateOf("0.1.0", pending))?.version).toBe("0.2.0");
  });

  it("fails when no extension tag exists", async () => {
    fake.tags = ["oxabl-v0.9.0"];
    await expect(findUpdate("0.1.0", env(), log)).rejects.toThrow("no extension releases found");
  });

  it("fails for a draft, a pre-release or a missing asset", async () => {
    const incomplete = "release v0.2.0 is not complete yet";
    fake.tags = ["oxabl_vscode-v0.2.0"];
    fake.release = release("0.2.0", { draft: true });
    await expect(findUpdate("0.1.0", env(), log)).rejects.toThrow(incomplete);
    fake.release = release("0.2.0", { prerelease: true });
    await expect(findUpdate("0.1.0", env(), log)).rejects.toThrow(incomplete);
    fake.release = { ...release("0.2.0"), assets: [] };
    await expect(findUpdate("0.1.0", env(), log)).rejects.toThrow(incomplete);
  });

  it("fails when the tag list is still paged after the page limit", async () => {
    fake.endless = true;
    await expect(findUpdate("0.1.0", env(), log)).rejects.toThrow("tag list truncated");
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

  it("refuses http for a host that is not loopback, even with the hook set", async () => {
    await expect(
      findUpdate("0.1.0", { ...env(), apiBase: "http://example.test" }, log),
    ).rejects.toThrow("refusing http: URL from example.test");
  });
});

describe("highestInstalledVersion", () => {
  const dirOf = (version: string) => `oxabl-project.oxabl-${version}`;

  function install(...versions: string[]) {
    for (const v of versions) {
      fs.mkdirSync(path.join(tmp, dirOf(v)));
    }
  }

  it("finds nothing in an empty or a missing folder", async () => {
    expect(await highestInstalledVersion(tmp)).toBeUndefined();
    expect(await highestInstalledVersion(path.join(tmp, "missing"))).toBeUndefined();
  });

  it("takes the highest version and ignores other folders", async () => {
    install("0.2.0", "0.10.0", "0.9.0");
    fs.mkdirSync(path.join(tmp, "someone.else-9.9.9"));
    fs.mkdirSync(path.join(tmp, "oxabl-project.oxabl-1.0.0-beta"));
    expect(await highestInstalledVersion(tmp)).toBe("0.10.0");
  });

  it("skips a version marked obsolete", async () => {
    install("0.2.0", "0.3.0");
    fs.writeFileSync(path.join(tmp, ".obsolete"), JSON.stringify({ [dirOf("0.3.0")]: true }));
    expect(await highestInstalledVersion(tmp)).toBe("0.2.0");
  });

  it("finds nothing when the obsolete file does not parse or is not an object", async () => {
    install("0.3.0");
    for (const text of ["{ not json", "", "null", "[]", "3"]) {
      fs.writeFileSync(path.join(tmp, ".obsolete"), text);
      expect(await highestInstalledVersion(tmp), text).toBeUndefined();
    }
  });

  it("finds nothing when the obsolete file cannot be read", async () => {
    install("0.3.0");
    fs.mkdirSync(path.join(tmp, ".obsolete"));
    expect(await highestInstalledVersion(tmp)).toBeUndefined();
  });

  it("drives the verdict: installed newer or equal is pending, obsolete or absent is available", async () => {
    const log = () => {};
    fake.tags = ["oxabl_vscode-v0.2.0"];
    fake.release = release("0.2.0");
    const verdict = async () => {
      const onDisk = await highestInstalledVersion(tmp);
      return (await findUpdate("0.1.0", env(), log, { record: { version: "0.2.0", dir: tmp }, dir: tmp, onDisk }))
        .kind;
    };

    expect(await verdict()).toBe("available");
    install("0.2.0");
    expect(await verdict()).toBe("pending-reload");
    install("0.3.0");
    expect(await verdict()).toBe("pending-reload");
    fs.writeFileSync(
      path.join(tmp, ".obsolete"),
      JSON.stringify({ [dirOf("0.2.0")]: true, [dirOf("0.3.0")]: true }),
    );
    expect(await verdict()).toBe("available");
  });

  it("is available, not pending, when the obsolete file is broken", async () => {
    fake.tags = ["oxabl_vscode-v0.2.0"];
    fake.release = release("0.2.0");
    install("0.2.0");
    fs.writeFileSync(path.join(tmp, ".obsolete"), "{ not json");
    const onDisk = await highestInstalledVersion(tmp);
    const check = await findUpdate("0.1.0", env(), () => {}, { record: { version: "0.2.0", dir: tmp }, dir: tmp, onDisk });
    expect(check.kind).toBe("available");
  });
});

describe("pendingReloadVersion", () => {
  const v = (text: string) => parseVersion(text)!;
  const state = (over: Partial<PendingState> = {}): PendingState => ({
    record: { version: "0.3.0", dir: "/ext" },
    dir: "/ext",
    onDisk: "0.3.0",
    ...over,
  });
  const decide = (s: PendingState | undefined, running = "0.2.0", newest = "0.3.0") =>
    pendingReloadVersion(s, v(running), v(newest));

  it("waits when the record, the folder and the versions agree", () => {
    expect(decide(state())).toBe("0.3.0");
    expect(decide(state({ onDisk: "0.4.0" }))).toBe("0.3.0");
    expect(decide(state(), "0.2.0", "0.2.5")).toBe("0.3.0");
  });

  it("does not wait without a record", () => {
    expect(decide(state({ record: undefined }))).toBeUndefined();
    expect(decide(state({ record: null }))).toBeUndefined();
    expect(decide(state({ record: { version: "0.3.0" } }))).toBeUndefined();
    expect(decide(state({ record: { version: "x", dir: "/ext" } }))).toBeUndefined();
    expect(decide(undefined)).toBeUndefined();
  });

  it("does not wait when the record is for another folder", () => {
    expect(decide(state({ record: { version: "0.3.0", dir: "/remote" } }))).toBeUndefined();
  });

  it("does not wait when the folder is missing or obsolete", () => {
    expect(decide(state({ onDisk: undefined }))).toBeUndefined();
    expect(decide(state({ onDisk: "0.2.0" }))).toBeUndefined();
  });

  it("does not wait when the record is below the newest tag or not above the running version", () => {
    expect(decide(state(), "0.2.0", "0.4.0")).toBeUndefined();
    expect(decide(state(), "0.3.0", "0.3.0")).toBeUndefined();
    expect(decide(state(), "0.4.0", "0.3.0")).toBeUndefined();
  });
});

describe("isPendingReloadSpent", () => {
  it("is spent once the running version is not older than the record", () => {
    const record = { version: "0.3.0", dir: "/ext" };
    expect(isPendingReloadSpent(record, "0.2.0")).toBe(false);
    expect(isPendingReloadSpent(record, "0.3.0")).toBe(true);
    expect(isPendingReloadSpent(record, "0.3.0-dev")).toBe(true);
    expect(isPendingReloadSpent(record, "0.4.0")).toBe(true);
    expect(isPendingReloadSpent({ version: "x" }, "0.2.0")).toBe(true);
  });
});

describe("downloadVerified", () => {
  async function update() {
    fake.tags = ["oxabl_vscode-v0.2.0"];
    fake.release = release("0.2.0");
    const check = await findUpdate("0.1.0", env(), () => {});
    expect(check.kind).toBe("available");
    return (check as { update: AvailableUpdate }).update;
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
