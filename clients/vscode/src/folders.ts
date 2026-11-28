// Which workspace folder serves a file, when a window has several folders.
//
// Like `roots.ts`, this module is free of any `vscode` import.

import { isInside } from "./roots";

/** `single`: one client for the window. `multi`: one client for each owning folder. */
export type Mode = "single" | "multi";

/** A window with two or more folders gets a client for each folder. */
export function modeOf(folderCount: number): Mode {
  return folderCount >= 2 ? "multi" : "single";
}

/**
 * The folders that own files. A folder inside another folder of the window
 * owns nothing: the outermost folder owns its files. Of two folders with the
 * same path, the first owns.
 */
export function owningFolders<T extends { fsPath: string }>(folders: readonly T[]): T[] {
  return folders.filter(
    (folder, at) =>
      !folders.some(
        (other, otherAt) =>
          otherAt !== at &&
          isInside(folder.fsPath, other.fsPath) &&
          (!isInside(other.fsPath, folder.fsPath) || otherAt < at),
      ),
  );
}

/** The folder of `owning` (see {@link owningFolders}) that holds `file`, if any. */
export function ownerOf<T extends { fsPath: string }>(file: string, owning: readonly T[]): T | undefined {
  return owning.find((folder) => isInside(file, folder.fsPath));
}
