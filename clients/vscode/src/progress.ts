// What the extension shows of a running build: counts for the status bar, the
// compiler messages per unit, and the text of the status bar and of the
// notifications. The input is the event stream of `oxabl build --events`
// (docs/build-output.md).
//
// Like `server.ts`, this module is free of any `vscode` import.

export interface BuildMessage {
  type: "error" | "warning";
  file: string;
  row: number;
  column: number;
  number: number;
  text: string;
}

/** What one event changes outside the counts. */
export type Effect =
  | { kind: "messages"; unit: string; messages: BuildMessage[] }
  | { kind: "clear"; unit: string };

/** Counts of one build, from its events. A unit counts once, even when the retry pass compiles it again. */
export class BuildProgress {
  /** `rebuild` of `build-started`; undefined before it. */
  total: number | undefined;
  /** Units of the workspace, from `build-started`. */
  units = 0;
  readonly fetched = new Map<string, "local" | "remote">();
  private readonly finished = new Map<string, "ok" | "fail">();
  private readonly pending = new Map<string, BuildMessage[]>();
  /** The `summary` event, once it has come. */
  summary: { compiled: number; failed: number; fetched: number; exit: number } | undefined;

  /** Apply one event line (already parsed). Unknown events and shapes are ignored. */
  apply(event: unknown): Effect[] {
    const e = event as Record<string, unknown> | null;
    if (!e || typeof e !== "object") {
      return [];
    }
    const unit = typeof e.path === "string" ? e.path : undefined;
    switch (e.event) {
      case "build-started":
        this.total = typeof e.rebuild === "number" ? e.rebuild : 0;
        this.units = typeof e.units === "number" ? e.units : 0;
        return [];
      case "unit-started":
        if (unit) {
          this.pending.set(unit, []);
        }
        return [];
      case "message":
        if (unit) {
          const list = this.pending.get(unit) ?? [];
          list.push({
            type: e.type === "warning" ? "warning" : "error",
            file: typeof e.file === "string" ? e.file : unit,
            row: typeof e.row === "number" ? e.row : 0,
            column: typeof e.column === "number" ? e.column : 0,
            number: typeof e.number === "number" ? e.number : 0,
            text: typeof e.text === "string" ? e.text : "",
          });
          this.pending.set(unit, list);
        }
        return [];
      case "unit-finished":
        if (unit) {
          this.finished.set(unit, e.status === "ok" ? "ok" : "fail");
          const messages = this.pending.get(unit) ?? [];
          this.pending.delete(unit);
          return [{ kind: "messages", unit, messages }];
        }
        return [];
      case "unit-fetched":
        if (unit) {
          this.fetched.set(unit, e.cache === "remote" ? "remote" : "local");
          return [{ kind: "clear", unit }];
        }
        return [];
      case "summary":
        this.summary = {
          compiled: num(e.compiled),
          failed: num(e.failed),
          fetched: num(e.fetched),
          exit: num(e.exit),
        };
        return [];
      default:
        return [];
    }
  }

  get done(): number {
    return this.fetched.size + this.finished.size;
  }

  get failedNow(): number {
    return [...this.finished.values()].filter((s) => s === "fail").length;
  }

  get compiledNow(): number {
    return this.finished.size - this.failedNow;
  }

  /** Percent of the finished units that a cache gave; undefined before the first one. */
  /** Units that needed no work: built before in this workspace and unchanged. */
  get upToDate(): number {
    return this.total === undefined ? 0 : Math.max(0, this.units - this.total);
  }

  get cachedPercent(): number | undefined {
    return this.done === 0 ? undefined : Math.round((100 * this.fetched.size) / this.done);
  }

  fetchedFrom(cache: "local" | "remote"): number {
    return [...this.fetched.values()].filter((c) => c === cache).length;
  }
}

const num = (v: unknown): number => (typeof v === "number" ? v : 0);

/** Compiler messages per unit, and the files they are about. A message can be about an include file. */
export class MessageStore {
  private readonly byUnit = new Map<string, BuildMessage[]>();

  /** Replace the messages of `unit`; the files whose set changed. */
  set(unit: string, messages: BuildMessage[]): string[] {
    const files = new Set((this.byUnit.get(unit) ?? []).map((m) => m.file));
    messages.forEach((m) => files.add(m.file));
    if (messages.length === 0) {
      this.byUnit.delete(unit);
    } else {
      this.byUnit.set(unit, messages);
    }
    return [...files];
  }

  clear(unit: string): string[] {
    return this.set(unit, []);
  }

  forFile(file: string): BuildMessage[] {
    return [...this.byUnit.values()].flat().filter((m) => m.file === file);
  }
}

/** Where a root's build stands, for the status bar. */
export type RootStatus =
  | { phase: "preparing" }
  | { phase: "building"; progress: BuildProgress; unkeyed?: number }
  | { phase: "elsewhere" }
  | { phase: "idle"; last: "none" | "ok" | { failed: number } | { warning: string } };

export interface StatusView {
  /** The text, without the spinner: `busy` asks for it, in an item of its own. */
  text: string;
  busy: boolean;
  tooltip: string;
  /** `problems` opens the Problems view, `log` the build log, `none` nothing. */
  action: "problems" | "log" | "none";
}

/** A count with thousands separators. */
export function count(n: number): string {
  return n.toLocaleString("en-US");
}

/**
 * The two views of a build: the units it rebuilds, where the cache share comes
 * from, and the workspace, most of which is usually up to date.
 */
function buildingTooltip(p: BuildProgress, unkeyed: number | undefined): string {
  const lines = [
    `This build: ${count(p.total ?? 0)} units to rebuild, ${count(p.done)} done`,
    `  from the local cache: ${count(p.fetchedFrom("local"))}`,
    `  from the remote cache: ${count(p.fetchedFrom("remote"))}`,
    `  compiled: ${count(p.compiledNow)}, failed: ${count(p.failedNow)}`,
  ];
  if (p.units > 0) {
    const share = Math.round((100 * p.upToDate) / p.units);
    lines.push(`Workspace: ${count(p.units)} units, ${count(p.upToDate)} up to date (${share}%), not rebuilt`);
  }
  if (unkeyed) {
    lines.push(unkeyedTooltip(unkeyed));
  }
  return lines.join("\n");
}

export function buildingText(p: BuildProgress): string {
  const base = `oxabl ${count(p.done)}/${count(p.total ?? 0)}`;
  return p.cachedPercent === undefined ? base : `${base} · ${p.cachedPercent}% cached`;
}

/** The status bar item for the state of all roots, or `undefined` to hide it. The first root that builds wins. */
export function renderStatus(roots: RootStatus[]): StatusView | undefined {
  const building = roots.find((r) => r.phase === "building");
  if (building && building.phase === "building") {
    const p = building.progress;
    return {
      text: p.total === undefined ? "oxabl: preparing" : buildingText(p),
      busy: true,
      tooltip: buildingTooltip(p, building.unkeyed),
      action: "log",
    };
  }
  if (roots.some((r) => r.phase === "preparing")) {
    return { text: "oxabl: preparing", busy: true, tooltip: "oxabl build is starting", action: "log" };
  }
  if (roots.some((r) => r.phase === "elsewhere")) {
    return {
      text: "oxabl: build running elsewhere",
      busy: true,
      tooltip: "Another oxabl build took over the workspace; this one starts when it is done",
      action: "log",
    };
  }
  const idle = roots.flatMap((r) => (r.phase === "idle" ? [r.last] : []));
  const warning = idle.find((l): l is { warning: string } => typeof l === "object" && "warning" in l);
  if (warning) {
    return { text: "$(warning) oxabl", busy: false, tooltip: warning.warning, action: "log" };
  }
  const failed = idle.flatMap((l) => (typeof l === "object" && "failed" in l ? [l.failed] : []));
  if (failed.length > 0) {
    const n = failed.reduce((a, b) => a + b, 0);
    return { text: `$(error) oxabl: ${n} failed`, busy: false, tooltip: "Open the Problems view", action: "problems" };
  }
  if (idle.includes("ok")) {
    return { text: "$(check) oxabl", busy: false, tooltip: "The last oxabl build succeeded", action: "none" };
  }
  return undefined;
}

export function formatDuration(ms: number): string {
  const s = Math.max(0, Math.round(ms / 1000));
  if (s < 60) {
    return `${s}s`;
  }
  return `${Math.floor(s / 60)}m ${String(s % 60)}s`;
}

/** The notification for the first build of a window, from its `summary` counts. */
export function finishedText(p: BuildProgress, elapsedMs: number): string {
  const s = p.summary!;
  const total = s.fetched + s.compiled + s.failed;
  if (total === 0) {
    return `oxabl build finished: ${p.units} units up to date in ${formatDuration(elapsedMs)}`;
  }
  const local = p.fetchedFrom("local");
  const remote = p.fetchedFrom("remote");
  const pct = Math.round((100 * s.fetched) / total);
  const compiled = s.compiled + s.failed;
  const units = Math.max(p.units, total);
  return (
    `oxabl build finished in ${formatDuration(elapsedMs)}: ${count(total)} of ${count(units)} units rebuilt, ` +
    `${pct}% from cache (${count(local)} local, ${count(remote)} remote), ${count(compiled)} compiled`
  );
}

/** Units without a compile key, from the report of `oxabl build --report`. */
export interface Unkeyed {
  count: number;
  /** The most common reason, and how many units have it. */
  reason: string;
  reasonCount: number;
}

const NO_KEY_NOTE = /^(.+?): no key from what the compiler read: (.+?); the unit compiles at each build$/;

/**
 * The units of a report that have no compile key, so they compile at every
 * build: a plan reason `unkeyable` or `unverified`, or a note that the compile
 * gave no key. `undefined` when there is none or the report cannot be read.
 */
export function unkeyedFromReport(text: string): Unkeyed | undefined {
  let report: { units?: unknown; notes?: unknown };
  try {
    report = JSON.parse(text) as typeof report;
  } catch {
    return undefined;
  }
  const why = new Map<string, string>();
  for (const u of Array.isArray(report.units) ? report.units : []) {
    const { path, reason } = (u ?? {}) as { path?: unknown; reason?: unknown };
    const m = typeof reason === "string" ? /^(?:unkeyable|unverified): (.+)$/.exec(reason) : null;
    if (typeof path === "string" && m) {
      why.set(path, m[1]);
    }
  }
  for (const note of Array.isArray(report.notes) ? report.notes : []) {
    const m = typeof note === "string" ? NO_KEY_NOTE.exec(note) : null;
    if (m) {
      why.set(m[1], m[2]);
    }
  }
  if (why.size === 0) {
    return undefined;
  }
  const byReason = new Map<string, number>();
  for (const r of why.values()) {
    byReason.set(r, (byReason.get(r) ?? 0) + 1);
  }
  const [reason, reasonCount] = [...byReason].sort((a, b) => b[1] - a[1])[0];
  return { count: why.size, reason, reasonCount };
}

function unkeyedTooltip(n: number): string {
  return `${count(n)} ${n === 1 ? "unit has" : "units have"} no compile key and compile at every build.`;
}

/**
 * Whether units without a key deserve a warning: the reason names a setting
 * that is missing, or a tenth of the workspace or more has no key. A few units
 * of encrypted source only show in the tooltip and the build log.
 */
export function shouldWarnUnkeyed(u: Unkeyed, units: number): boolean {
  return /needs \[/.test(u.reason) || (units > 0 && u.count * 10 >= units);
}

/** The warning for units without a compile key. */
export function unkeyedText(u: Unkeyed): string {
  const units = u.count === 1 ? "1 unit has" : `${count(u.count)} units have`;
  const share = u.reasonCount === u.count ? "" : ` (${count(u.reasonCount)} of them)`;
  const dlc = u.reason.includes("[workspace.sources].dlc")
    ? " Set it in oxabl.toml, or set OXABL_DLC_SOURCES and start VS Code again from a session that has it."
    : "";
  return (
    `oxabl: ${units} no compile key, so ${u.count === 1 ? "it compiles" : "they compile"} at every build and no cache can hold ` +
    `${u.count === 1 ? "it" : "them"}. ${/needs \[/.test(u.reason) ? "The setup of the project is not complete" : "The most common reason"}: ` +
    `${u.reason}${share}.${dlc}`
  );
}
