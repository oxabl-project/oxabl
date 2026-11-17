// A notice that a project shows once, and not again at every start of VS Code:
// the memory is the workspace state, keyed by the root and the reason.
//
// Like `progress.ts`, this module is free of any `vscode` import.

/** The part of `vscode.Memento` that is used. */
export interface Memory {
  get<T>(key: string): T | undefined;
  update(key: string, value: unknown): PromiseLike<void>;
}

export class Once {
  constructor(
    private readonly memory: Memory,
    private readonly kind: string,
  ) {}

  private key(root: string, reason: string): string {
    return `oxabl.once.${this.kind}\n${root}\n${reason}`;
  }

  /** Whether a notice of `reason` was shown for `root`, in this window or an earlier one. */
  shown(root: string, reason: string): boolean {
    return this.memory.get<boolean>(this.key(root, reason)) === true;
  }

  /** Remember that the notice of `reason` was shown for `root`. */
  async mark(root: string, reason: string): Promise<void> {
    await this.memory.update(this.key(root, reason), true);
  }
}
