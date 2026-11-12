import { describe, expect, it } from "vitest";

import { classifyHost, hostMessage } from "./host";

describe("classifyHost", () => {
  it("supports Linux x86_64 locally and in every remote kind", () => {
    for (const remoteName of [undefined, "wsl", "ssh-remote", "dev-container"]) {
      expect(classifyHost({ platform: "linux", arch: "x64", remoteName })).toEqual({ kind: "supported" });
    }
  });

  it("offers WSL in a local Windows window", () => {
    expect(classifyHost({ platform: "win32", arch: "x64" })).toEqual({ kind: "windows" });
  });

  it("does not offer WSL in a Windows remote window", () => {
    expect(classifyHost({ platform: "win32", arch: "x64", remoteName: "ssh-remote" }).kind).toBe("unsupported");
  });

  it("supports no other platform or architecture", () => {
    expect(classifyHost({ platform: "darwin", arch: "arm64" }).kind).toBe("unsupported");
    expect(classifyHost({ platform: "linux", arch: "arm64" }).kind).toBe("unsupported");
  });

  it("points other hosts to cargo install and never to a Windows binary", () => {
    const text = hostMessage({ kind: "unsupported", platform: "darwin", arch: "arm64" });
    expect(text).toContain("cargo install oxabl");
    expect(hostMessage({ kind: "windows" })).toContain("WSL");
  });
});
