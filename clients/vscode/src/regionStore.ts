// The inactive regions of each document, with the version they were computed
// for. Free of any `vscode` import so it runs under vitest.

/**
 * Keeps the last regions of each document with their document version. A
 * lookup for another version gives nothing: regions of older text sit at the
 * wrong lines.
 */
export class RegionStore<R> {
  private readonly entries = new Map<string, { version: number; regions: R[] }>();

  set(uri: string, version: number, regions: R[]): void {
    this.entries.set(uri, { version, regions });
  }

  /** The regions of `uri` for `version`, or none. */
  get(uri: string, version: number): R[] {
    const entry = this.entries.get(uri);
    return entry && entry.version === version ? entry.regions : [];
  }

  delete(uri: string): void {
    this.entries.delete(uri);
  }

  clear(): void {
    this.entries.clear();
  }
}
