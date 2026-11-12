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
  | { phase: "building"; progress: BuildProgress }
  | { phase: "elsewhere" }
  | { phase: "idle"; last: "none" | "ok" | { failed: number } | { warning: string } };

export interface StatusView {
  text: string;
  tooltip: string;
  /** `problems` opens the Problems view, `log` the build log, `none` nothing. */
  action: "problems" | "log" | "none";
}

export function buildingText(p: BuildProgress): string {
  const base = `$(sync~spin) oxabl ${p.done}/${p.total ?? 0}`;
  return p.cachedPercent === undefined ? base : `${base} · ${p.cachedPercent}% cached`;
}

/** The status bar item for the state of all roots, or `undefined` to hide it. The first root that builds wins. */
export function renderStatus(roots: RootStatus[]): StatusView | undefined {
  const building = roots.find((r) => r.phase === "building");
  if (building && building.phase === "building") {
    const p = building.progress;
    return {
      text: p.total === undefined ? "$(sync~spin) oxabl: preparing" : buildingText(p),
      tooltip:
        `${p.fetchedFrom("local")} fetched from the local cache, ${p.fetchedFrom("remote")} from the remote cache, ` +
        `${p.compiledNow} compiled, ${p.failedNow} failed`,
      action: "log",
    };
  }
  if (roots.some((r) => r.phase === "preparing")) {
    return { text: "$(sync~spin) oxabl: preparing", tooltip: "oxabl build is starting", action: "log" };
  }
  if (roots.some((r) => r.phase === "elsewhere")) {
    return {
      text: "$(sync~spin) oxabl: build running elsewhere",
      tooltip: "Another oxabl build took over the workspace; this one starts when it is done",
      action: "log",
    };
  }
  const idle = roots.flatMap((r) => (r.phase === "idle" ? [r.last] : []));
  const warning = idle.find((l): l is { warning: string } => typeof l === "object" && "warning" in l);
  if (warning) {
    return { text: "$(warning) oxabl", tooltip: warning.warning, action: "log" };
  }
  const failed = idle.flatMap((l) => (typeof l === "object" && "failed" in l ? [l.failed] : []));
  if (failed.length > 0) {
    const n = failed.reduce((a, b) => a + b, 0);
    return { text: `$(error) oxabl: ${n} failed`, tooltip: "Open the Problems view", action: "problems" };
  }
  if (idle.includes("ok")) {
    return { text: "$(check) oxabl", tooltip: "The last oxabl build succeeded", action: "none" };
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
  return (
    `oxabl build finished: ${total} units, ${pct}% from cache (${local} local, ${remote} remote), ` +
    `${compiled} compiled in ${formatDuration(elapsedMs)}`
  );
}
