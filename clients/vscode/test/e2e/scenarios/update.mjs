import path from "node:path";
import { neverShows, screenshot, waitForText } from "../lib/editor.mjs";
import { startFakeGitHub } from "../lib/fake-github.mjs";
import { check, sleep, waitFor } from "../lib/report.mjs";
import { installVsix, launch, listExtensions, makeProfile } from "../lib/vscode.mjs";

const NEW = "99.0.0";
const ID = "oxabl-project.oxabl";
const API = (p) => `/repos/oxabl-project/oxabl/${p}`;
const REFS = API("git/matching-refs/tags/oxabl_vscode-v");
const RELEASE = (v) => API(`releases/tags/oxabl_vscode-v${v}`);
const NO_MESSAGE = ["oxabl extension", "could not check for updates"];

async function session(ctx, name, { mode, server: serverOpts }, body) {
  const profile = makeProfile(name, { settings: { "oxabl.updates": mode }, files: ["update.p"] });
  installVsix(ctx.exe, profile, ctx.vsix);
  check(
    `${name}: starts with ${ID}@${ctx.installed} installed`,
    listExtensions(ctx.exe, profile).includes(`${ID}@${ctx.installed}`),
  );
  const server = await startFakeGitHub({
    installed: ctx.installed,
    newest: NEW,
    vsix: ctx.newerVsix,
    ...serverOpts,
  });
  let vs;
  try {
    vs = await launch(ctx.exe, profile, { open: ["update.p"], env: { OXABL_UPDATE_API_URL: server.url } });
    await body({ profile, server, page: vs.page });
  } finally {
    if (vs) await vs.close();
    await server.close();
  }
  return { profile, server, versions: () => listExtensions(ctx.exe, profile) };
}

const waitForRequest = (what, server, pred, timeout = 60_000) =>
  waitFor(`${what} at the fake GitHub`, () => server.requests.some(pred), { timeout, interval: 200 });

export async function updateAuto(ctx) {
  const name = "update-auto";
  const { server, versions } = await session(ctx, name, { mode: "auto" }, async ({ page }) => {
    await waitForText(page, `oxabl extension updated to v${NEW}`);
    check(`${name}: the "updated to v${NEW}" message appears`, true);
    await screenshot(page, name);
  });
  const now = versions();
  check(`${name}: ${ID}@${NEW} is installed`, now.includes(`${ID}@${NEW}`), now.join(", "));
  const paths = server.requests.map((r) => r.path);
  check(`${name}: tags listed`, paths.includes(REFS), paths.join(" "));
  check(`${name}: release of v${NEW} read`, paths.includes(RELEASE(NEW)));
  check(`${name}: VSIX fetched through the redirect`, paths.includes(`/redirect/${server.vsixName}`) && server.servedVsix());
  check(`${name}: checksum file fetched`, paths.includes(`/download/${server.vsixName}.sha256`));
  const other = paths.filter((p) => p.includes("/releases/tags/") && p !== RELEASE(NEW));
  check(`${name}: no other release read`, other.length === 0, other.join(" "));
  const badUa = server.requests.filter((r) => !String(r.headers["user-agent"] ?? "").startsWith("oxabl-vscode/"));
  check(`${name}: every request has an oxabl-vscode/ User-Agent`, badUa.length === 0, badUa.map((r) => r.path).join(" "));
  const apiReqs = server.requests.filter((r) => r.path.startsWith("/repos/"));
  const noAccept = apiReqs.filter((r) => r.headers.accept !== "application/vnd.github+json");
  check(`${name}: API calls ask for application/vnd.github+json`, apiReqs.length > 0 && noAccept.length === 0, noAccept.map((r) => `${r.path}: ${r.headers.accept}`).join(" "));
}

export async function updateChecksum(ctx) {
  const name = "update-checksum";
  const { server, versions } = await session(ctx, name, { mode: "auto", server: { digest: "bad" } }, async ({ page, server }) => {
    await waitForRequest("the VSIX", server, (r) => r.path === `/download/${server.vsixName}`);
    await waitForRequest("the checksum", server, (r) => r.path === `/download/${server.vsixName}.sha256`);
    const hit = await neverShows(page, ["oxabl extension updated", ...NO_MESSAGE], 6_000);
    check(`${name}: no message is shown`, hit === null, `saw "${hit}"`);
    await screenshot(page, name);
  });
  const now = versions();
  check(`${name}: the installed version is unchanged`, now.includes(`${ID}@${ctx.installed}`) && !now.includes(`${ID}@${NEW}`), now.join(", "));
  check(`${name}: the VSIX was served`, server.servedVsix());
}

export async function updateNotify(ctx) {
  const name = "update-notify";
  const { server, versions } = await session(ctx, name, { mode: "notify" }, async ({ page }) => {
    await waitForText(page, `oxabl extension v${NEW} is available`);
    check(`${name}: the "v${NEW} is available" message appears`, true);
    const hit = await neverShows(page, ["oxabl extension updated"], 4_000);
    check(`${name}: nothing is installed while the message is open`, hit === null, `saw "${hit}"`);
    await screenshot(page, name);
  });
  const now = versions();
  check(`${name}: the installed version is unchanged`, now.includes(`${ID}@${ctx.installed}`) && !now.includes(`${ID}@${NEW}`), now.join(", "));
  check(`${name}: the VSIX was not downloaded`, !server.servedVsix());
}

export async function updateNothingNewer(ctx) {
  const name = "update-none";
  const { server } = await session(ctx, name, { mode: "auto", server: { newest: ctx.installed } }, async ({ page, server }) => {
    await waitForRequest("the tag listing", server, (r) => r.path === REFS);
    const hit = await neverShows(page, NO_MESSAGE, 6_000);
    check(`${name}: no message is shown`, hit === null, `saw "${hit}"`);
  });
  const releases = server.requests.filter((r) => r.path.includes("/releases/"));
  check(`${name}: no release is requested`, releases.length === 0, releases.map((r) => r.path).join(" "));
}

export async function updateServerError(ctx) {
  const name = "update-error";
  const { server } = await session(ctx, name, { mode: "auto", server: { refsStatus: 500 } }, async ({ page, server }) => {
    await waitForRequest("the tag listing", server, (r) => r.path === REFS);
    const hit = await neverShows(page, NO_MESSAGE, 6_000);
    check(`${name}: no error message on the automatic check`, hit === null, `saw "${hit}"`);
  });
  check(`${name}: nothing but the tag listing is requested`, server.requests.every((r) => r.path === REFS), server.requests.map((r) => r.path).join(" "));
}
