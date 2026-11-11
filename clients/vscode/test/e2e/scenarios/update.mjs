import path from "node:path";
import { clickButton, neverShows, runCommand, screenshot, waitForText } from "../lib/editor.mjs";
import { startFakeGitHub } from "../lib/fake-github.mjs";
import { check, sleep, waitFor } from "../lib/report.mjs";
import { extensionState } from "../lib/state.mjs";
import {
  NO_SERVER_LOG,
  addNamedProfile,
  installVsix,
  launch,
  listExtensions,
  logSessions,
  makeProfile,
  waitForLog,
} from "../lib/vscode.mjs";

const NEW = "99.0.0";
const ID = "oxabl-project.oxabl";
const API = (p) => `/repos/oxabl-project/oxabl/${p}`;
const REFS = API("git/matching-refs/tags/oxabl_vscode-v");
const RELEASE = (v) => API(`releases/tags/oxabl_vscode-v${v}`);
const NO_MESSAGE = ["oxabl extension", "could not check for updates"];

async function session(ctx, name, { mode, settings = {}, server: serverOpts }, body) {
  const profile = makeProfile(name, { settings: { "oxabl.updates": mode, ...settings }, files: ["update.p"] });
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

const CHECK_COMMAND = "oxabl: Check for Updates";

export async function updateAutoUpdateOff(ctx) {
  const name = "update-autoupdate-off";
  const { server, versions } = await session(
    ctx,
    name,
    { mode: "auto", settings: { "extensions.autoUpdate": false } },
    async ({ page }) => {
      await waitForText(page, `oxabl extension v${NEW} is available`);
      check(`${name}: the "v${NEW} is available" message appears`, true);
      const hit = await neverShows(page, ["oxabl extension updated"], 4_000);
      check(`${name}: nothing is installed while the message is open`, hit === null, `saw "${hit}"`);
      await screenshot(page, name);
    },
  );
  const now = versions();
  check(`${name}: the installed version is unchanged`, now.includes(`${ID}@${ctx.installed}`) && !now.includes(`${ID}@${NEW}`), now.join(", "));
  check(`${name}: the VSIX was not downloaded`, !server.servedVsix());
}

// A second launch of a profile within 24 hours makes no automatic check.
export async function updateThrottle(ctx) {
  const name = "update-throttle";
  const profile = makeProfile(name, { settings: { "oxabl.updates": "auto" }, files: ["update.p"] });
  installVsix(ctx.exe, profile, ctx.vsix);
  const server = await startFakeGitHub({ installed: ctx.installed, newest: ctx.installed });
  const env = { OXABL_UPDATE_API_URL: server.url };
  try {
    let vs = await launch(ctx.exe, profile, { open: ["update.p"], env });
    try {
      await waitForRequest("the tag listing", server, (r) => r.path === REFS);
      const hit = await neverShows(vs.page, NO_MESSAGE, 3_000);
      check(`${name}: no message is shown on the first launch`, hit === null, `saw "${hit}"`);
    } finally {
      await vs.close();
    }
    check(`${name}: the first launch listed the tags once`, server.count(REFS) === 1, `${server.count(REFS)} listings`);

    const before = logSessions(profile);
    vs = await launch(ctx.exe, profile, { open: ["update.p"], env });
    try {
      // The extension has started once it logs that there is no server; the check decision comes before that.
      await waitForLog(profile, NO_SERVER_LOG, before);
      await neverShows(vs.page, NO_MESSAGE, 6_000);
    } finally {
      await vs.close();
    }
    check(`${name}: the second launch made no tag listing`, server.count(REFS) === 1, `${server.count(REFS)} listings`);
    check(`${name}: nothing else was requested`, server.requests.every((r) => r.path === REFS), server.requests.map((r) => r.path).join(" "));
  } finally {
    await server.close();
  }
}

export async function updateInstalledReload(ctx) {
  const name = "update-installed-reload";
  const { server, versions } = await session(ctx, name, { mode: "auto" }, async ({ page, server }) => {
    await waitForText(page, `oxabl extension updated to v${NEW}`);
    check(`${name}: the "updated to v${NEW}" message appears`, true);
    await runCommand(page, CHECK_COMMAND);
    await waitForText(page, `oxabl extension v${NEW} is installed. Reload the window to use it.`);
    check(`${name}: the manual check says v${NEW} is installed and asks for a reload`, true);
    await screenshot(page, name);
  });
  const now = versions();
  check(`${name}: ${ID}@${NEW} is installed`, now.includes(`${ID}@${NEW}`), now.join(", "));
  check(`${name}: the VSIX was downloaded once`, server.vsixDownloads() === 1, `${server.vsixDownloads()} downloads`);
}

export async function updateManualNoTags(ctx) {
  const name = "update-manual-no-tags";
  const { server } = await session(ctx, name, { mode: "off", server: { noTags: true } }, async ({ page, server }) => {
    await runCommand(page, CHECK_COMMAND);
    await waitForText(page, "oxabl: could not check for updates: no extension releases found");
    check(`${name}: the manual check reports that no releases were found`, true);
    const hit = await neverShows(page, ["is up to date"], 3_000);
    check(`${name}: it does not say "up to date"`, hit === null, `saw "${hit}"`);
    await screenshot(page, name);
  });
  check(`${name}: the tag listing was requested`, server.served(REFS));
}

const SKIPPED_KEY = "oxabl.updates.skipped";
const LAST_CHECK_KEY = "oxabl.updates.lastCheck";
const AVAILABLE = `oxabl extension v${NEW} is available (installed: v`;

// "Skip This Version" stops the automatic check from offering that version again; a manual check still does.
async function skipScenario(ctx, name, choose) {
  const profile = makeProfile(name, { settings: { "oxabl.updates": "notify" }, files: ["update.p"] });
  installVsix(ctx.exe, profile, ctx.vsix);
  const server = await startFakeGitHub({ installed: ctx.installed, newest: NEW, vsix: ctx.newerVsix });
  const env = { OXABL_UPDATE_API_URL: server.url };
  try {
    let vs = await launch(ctx.exe, profile, { open: ["update.p"], env });
    try {
      await waitForText(vs.page, `${AVAILABLE}${ctx.installed}).`);
      check(`${name}: the prompt for v${NEW} appears with the three buttons`, true);
      for (const label of ["Update", "Release Notes", "Skip This Version"]) {
        const n = await vs.page.locator(".notification-toast .monaco-button", { hasText: label }).count();
        check(`${name}: the prompt has a "${label}" button`, n > 0);
      }
      await choose(vs.page);
    } finally {
      await vs.close();
    }
    check(`${name}: the first launch listed the tags once`, server.count(REFS) === 1, `${server.count(REFS)} listings`);

    const stored = await extensionState(profile, (state) => {
      delete state[LAST_CHECK_KEY];
      return state;
    });
    check(`${name}: the stored state holds ${SKIPPED_KEY} = ${NEW}`, stored[SKIPPED_KEY] === NEW, JSON.stringify(stored));
    check(`${name}: the stored state holds a ${LAST_CHECK_KEY} stamp before it is removed`, LAST_CHECK_KEY in stored, JSON.stringify(stored));
    const after = await extensionState(profile);
    check(`${name}: the stamp is gone and the rest of the state is kept`, !(LAST_CHECK_KEY in after) && after[SKIPPED_KEY] === stored[SKIPPED_KEY], JSON.stringify(after));

    vs = await launch(ctx.exe, profile, { open: ["update.p"], env });
    try {
      await waitForRequest("the second tag listing", server, () => server.count(REFS) >= 2);
      const hit = await neverShows(vs.page, [AVAILABLE], 10_000);
      check(`${name}: the automatic check does not offer the skipped v${NEW}`, hit === null, `saw "${hit}"`);
      await runCommand(vs.page, CHECK_COMMAND);
      await waitForText(vs.page, `${AVAILABLE}${ctx.installed}).`);
      check(`${name}: the manual check offers v${NEW} again`, true);
      await screenshot(vs.page, name);
    } finally {
      await vs.close();
    }
    check(`${name}: nothing was downloaded`, !server.servedVsix());
  } finally {
    await server.close();
  }
}

export const updateSkip = (ctx) =>
  skipScenario(ctx, "update-skip", (page) => clickButton(page, "Skip This Version"));

// VS Code profiles share one extensions folder but keep their own installed versions and `globalState`.
// An update in one profile must reach the others: the automatic check of a profile still on the old
// version installs the new one there and says so.
export async function updateTwoProfiles(ctx) {
  const name = "update-two-profiles";
  const other = "second";
  const profile = makeProfile(name, { settings: { "oxabl.updates": "auto" }, files: ["update.p"] });
  addNamedProfile(profile, other);
  for (const named of [undefined, other]) {
    installVsix(ctx.exe, profile, ctx.vsix, named);
    const label = named ?? "default";
    check(
      `${name}: the ${label} profile starts with ${ID}@${ctx.installed}`,
      listExtensions(ctx.exe, profile, named).includes(`${ID}@${ctx.installed}`),
      listExtensions(ctx.exe, profile, named).join(", "),
    );
  }
  const server = await startFakeGitHub({ installed: ctx.installed, newest: NEW, vsix: ctx.newerVsix });
  const env = { OXABL_UPDATE_API_URL: server.url };
  try {
    let vs = await launch(ctx.exe, profile, { open: ["update.p"], env });
    try {
      await waitForText(vs.page, `oxabl extension updated to v${NEW}`);
      check(`${name}: the default profile shows "updated to v${NEW}"`, true);
    } finally {
      await vs.close();
    }
    const first = listExtensions(ctx.exe, profile);
    const second = listExtensions(ctx.exe, profile, other);
    check(`${name}: the default profile lists ${ID}@${NEW}`, first.includes(`${ID}@${NEW}`), first.join(", "));
    check(`${name}: the ${other} profile still lists ${ID}@${ctx.installed}`, second.includes(`${ID}@${ctx.installed}`) && !second.includes(`${ID}@${NEW}`), second.join(", "));

    vs = await launch(ctx.exe, profile, { open: ["update.p"], env, named: other });
    try {
      await waitForText(vs.page, `oxabl extension updated to v${NEW}`);
      check(`${name}: the ${other} profile shows "updated to v${NEW}" and not silence`, true);
      await screenshot(vs.page, name);
    } finally {
      await vs.close();
    }
    const after = listExtensions(ctx.exe, profile, other);
    check(`${name}: the ${other} profile lists ${ID}@${NEW}`, after.includes(`${ID}@${NEW}`), after.join(", "));
  } finally {
    await server.close();
  }
}
