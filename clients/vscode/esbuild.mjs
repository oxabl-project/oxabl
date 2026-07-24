// Single-file bundle for the extension (KTD5). `@vscode/vsce` cannot walk
// pnpm's symlinked node_modules, so we bundle everything the extension needs
// into out/extension.js and package with `vsce package --no-dependencies`.
import { build, context } from "esbuild";

const watch = process.argv.includes("--watch");

/** @type {import('esbuild').BuildOptions} */
const options = {
  entryPoints: ["src/extension.ts"],
  bundle: true,
  outfile: "out/extension.js",
  format: "cjs",
  platform: "node",
  target: "node18",
  // `vscode` is provided by the extension host at runtime — never bundle it.
  external: ["vscode"],
  sourcemap: true,
  minify: !watch,
  logLevel: "info",
};

if (watch) {
  const ctx = await context(options);
  await ctx.watch();
  console.log("esbuild: watching…");
} else {
  await build(options);
}
