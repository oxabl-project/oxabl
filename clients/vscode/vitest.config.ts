import { defineConfig } from "vitest/config";

// The end-to-end suite (test/e2e) drives a real VS Code and runs through
// `pnpm run test:e2e`, not here.
export default defineConfig({
  test: {
    include: ["src/**/*.test.ts", "test/grammar/**/*.test.ts"],
  },
});
