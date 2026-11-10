import crypto from "node:crypto";
import fs from "node:fs";
import http from "node:http";

const REPO = "/repos/oxabl-project/oxabl";
const REFS = `${REPO}/git/matching-refs/tags/oxabl_vscode-v`;
const TAG = (v) => `oxabl_vscode-v${v}`;

/**
 * A local imitation of the GitHub API the extension reads.
 *
 * opts.installed   version of the installed extension (listed as a tag)
 * opts.newest      version the release endpoint serves
 * opts.vsix        path of the VSIX that is served for `newest` (none: an empty file)
 * opts.digest      "good" | "bad": the digest in the .sha256 file
 * opts.refsStatus  when set, matching-refs answers with this HTTP status
 * opts.noTags      matching-refs answers with an empty list
 *
 * `requests` holds every request seen: { method, path, headers }.
 */
export async function startFakeGitHub(opts) {
  const requests = [];
  const vsixBytes = opts.vsix ? fs.readFileSync(opts.vsix) : Buffer.alloc(0);
  const goodDigest = crypto.createHash("sha256").update(vsixBytes).digest("hex");
  const digest = opts.digest === "bad" ? "0".repeat(64) : goodDigest;
  const vsixName = `oxabl-vscode-${opts.newest}.vsix`;
  let base = "";

  const json = (res, status, body) => {
    res.writeHead(status, { "content-type": "application/json" });
    res.end(JSON.stringify(body));
  };

  const server = http.createServer((req, res) => {
    const url = new URL(req.url, "http://127.0.0.1");
    requests.push({ method: req.method, path: url.pathname, headers: req.headers });

    if (url.pathname === REFS) {
      if (opts.refsStatus) return json(res, opts.refsStatus, { message: "boom" });
      if (opts.noTags) return json(res, 200, []);
      const tags = [...new Set(["0.0.1", opts.installed, opts.newest, "100.0.0-rc.1"])];
      const refs = tags.map((v) => ({ ref: `refs/tags/${TAG(v)}`, object: { type: "commit" } }));
      refs.push({ ref: "refs/tags/oxabl_vscode-vnext", object: { type: "commit" } });
      return json(res, 200, refs);
    }
    if (url.pathname === `${REPO}/releases/tags/${TAG(opts.newest)}`) {
      const asset = (name, urlPath) => ({ name, browser_download_url: `${base}${urlPath}` });
      return json(res, 200, {
        tag_name: TAG(opts.newest),
        draft: false,
        prerelease: false,
        assets: [
          asset(vsixName, `/redirect/${vsixName}`),
          asset(`${vsixName}.sha256`, `/download/${vsixName}.sha256`),
        ],
      });
    }
    if (url.pathname === `/redirect/${vsixName}`) {
      res.writeHead(302, { location: `${base}/download/${vsixName}` });
      return res.end();
    }
    if (url.pathname === `/download/${vsixName}`) {
      res.writeHead(200, { "content-type": "application/octet-stream", "content-length": vsixBytes.length });
      return res.end(vsixBytes);
    }
    if (url.pathname === `/download/${vsixName}.sha256`) {
      res.writeHead(200, { "content-type": "text/plain" });
      return res.end(`${digest}  ${vsixName}\n`);
    }
    json(res, 404, { message: "Not Found" });
  });

  await new Promise((r) => server.listen(0, "127.0.0.1", r));
  base = `http://127.0.0.1:${server.address().port}`;

  return {
    url: base,
    requests,
    served: (p) => requests.some((r) => r.path === p),
    count: (p) => requests.filter((r) => r.path === p).length,
    servedVsix: () => requests.some((r) => r.path === `/download/${vsixName}`),
    vsixDownloads: () => requests.filter((r) => r.path === `/download/${vsixName}`).length,
    vsixName,
    close: () => new Promise((r) => (server.closeAllConnections?.(), server.close(r))),
  };
}
