// Which hosts can run a released oxabl binary.
//
// Like `server.ts`, this module is free of any `vscode` import. The caller
// passes `process.platform`, `process.arch` and `vscode.env.remoteName`.

export type HostClass =
  /** Linux x86_64: a local window, WSL, SSH or a container. */
  | { kind: "supported" }
  /** A local Windows window: the binary runs in WSL. */
  | { kind: "windows" }
  /** Any other platform or architecture: no binary is released for it. */
  | { kind: "unsupported"; platform: string; arch: string };

export interface HostInfo {
  platform: NodeJS.Platform;
  arch: string;
  remoteName?: string;
}

export function classifyHost(host: HostInfo): HostClass {
  if (host.platform === "linux" && host.arch === "x64") {
    return { kind: "supported" };
  }
  if (host.platform === "win32" && !host.remoteName) {
    return { kind: "windows" };
  }
  return { kind: "unsupported", platform: host.platform, arch: host.arch };
}

export const WSL_DOCS_URL = "https://github.com/oxabl-project/oxabl/tree/master/clients/vscode#windows-and-wsl";

/** The message for a host that cannot install the binary. */
export function hostMessage(host: Exclude<HostClass, { kind: "supported" }>): string {
  if (host.kind === "windows") {
    return "The oxabl CLI runs on Linux. Reopen this folder in WSL to install and use it.";
  }
  return (
    `Released oxabl binaries exist only for Linux x86_64 (this is ${host.platform} ${host.arch}). ` +
    "Run `cargo install oxabl` to build the CLI."
  );
}
