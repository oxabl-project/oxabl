import path from "node:path";
import fs from "node:fs";
import { SHOTS_DIR } from "./paths.mjs";
import { sleep, waitFor } from "./report.mjs";

const hex = (rgb) => {
  const m = /rgba?\((\d+),\s*(\d+),\s*(\d+)/.exec(rgb);
  return m ? `#${[m[1], m[2], m[3]].map((n) => Number(n).toString(16).padStart(2, "0")).join("")}` : rgb;
};

/** The rendered lines of the editor: [{ text, colors: [hex per character] }], top to bottom. */
export async function readLines(page) {
  const raw = await page.evaluate(() => {
    const lines = [...document.querySelectorAll(".monaco-editor .view-lines .view-line")];
    lines.sort((a, b) => parseFloat(a.style.top) - parseFloat(b.style.top));
    return lines.map((line) => {
      const runs = [];
      const walker = document.createTreeWalker(line, NodeFilter.SHOW_TEXT);
      for (let n = walker.nextNode(); n; n = walker.nextNode()) {
        runs.push({ text: n.textContent, color: getComputedStyle(n.parentElement).color });
      }
      return runs;
    });
  });
  return raw.map((runs) => {
    let text = "";
    const colors = [];
    for (const r of runs) {
      const t = r.text.replace(/\u00a0/g, " ");
      text += t;
      for (let i = 0; i < t.length; i++) colors.push(hex(r.color));
    }
    return { text, colors };
  });
}

/** Waits until the rendered lines stop changing, after the language is set. */
export async function settledLines(page, { stableFor = 3, interval = 400, timeout = 30_000 } = {}) {
  let last = "";
  let same = 0;
  return waitFor(
    "the editor tokens to settle",
    async () => {
      const lines = await readLines(page);
      const sig = JSON.stringify(lines);
      same = lines.length > 0 && sig === last ? same + 1 : 0;
      last = sig;
      return same >= stableFor ? lines : null;
    },
    { timeout, interval },
  );
}

/** The language shown in the status bar ("ABL", "Markdown", "Plain Text"). */
export async function statusLanguage(page) {
  return page.evaluate(() => {
    const el = document.querySelector('[id="status.editor.mode"]');
    return el ? el.textContent.trim() : null;
  });
}

export async function waitForLanguage(page, name, timeout = 60_000) {
  let seen = null;
  try {
    await waitFor(
      `the status bar language "${name}"`,
      async () => (seen = await statusLanguage(page)) === name,
      { timeout, interval: 300 },
    );
    return { ok: true, seen };
  } catch {
    return { ok: false, seen };
  }
}

export const bodyText = (page) => page.evaluate(() => document.body.textContent);

export async function waitForText(page, text, timeout = 60_000) {
  await waitFor(`the text "${text}" in the window`, async () => (await bodyText(page)).includes(text), {
    timeout,
    interval: 300,
  });
}

/** True when none of `texts` shows up in the window during `ms`. */
export async function neverShows(page, texts, ms) {
  const end = Date.now() + ms;
  while (Date.now() < end) {
    const body = await bodyText(page);
    const hit = texts.find((t) => body.includes(t));
    if (hit) return hit;
    await sleep(250);
  }
  return null;
}

export async function screenshot(page, name) {
  fs.mkdirSync(SHOTS_DIR, { recursive: true });
  const file = path.join(SHOTS_DIR, `${name}.png`);
  await page.screenshot({ path: file });
  return file;
}
