---
title: "chore: Upgrade GitHub Actions to fix Node.js 20 deprecation warnings"
type: refactor
status: completed
date: 2026-04-04
---

# Upgrade GitHub Actions to fix Node.js 20 deprecation warnings

## Overview

All three workflow files (`ci.yml`, `release.yml`, `codspeed.yml`) emit Node.js 20 deprecation warnings. GitHub is removing Node.js 20 from Actions runners on **June 2, 2026** (warnings phase), with full removal in fall 2026. Two actions need version bumps; one has no fix yet upstream.

Additionally, `release.yml` uses `moonrepo/setup-rust@v0` (outdated) while `ci.yml` already uses `@v1`, creating an inconsistency.

## Problem Statement

9 annotations on every CI run:
1. **Node.js 20 deprecation** on `googleapis/release-please-action@v4`, `actions/checkout@v4`, `moonrepo/setup-rust@v0`/`@v1`
2. **Cache key not found / cache service 400 errors** from `moonrepo/setup-rust` (likely version-related)
3. **"Failed to save" service availability** — transient GitHub infrastructure issues (not actionable)

## Proposed Solution

### Phase 1: Immediate upgrades (fixes most warnings)

| Action | Current | Target | Node Runtime |
|--------|---------|--------|-------------|
| `actions/checkout` | `@v4` | `@v6` | Node 24 |
| `moonrepo/setup-rust` | `@v0` / `@v1` | `@v1` | Node 24 |

**Files to change:**

#### `.github/workflows/ci.yml`
- Update all 4 `actions/checkout@v4` → `@v6`
- `moonrepo/setup-rust@v1` is already correct but will get Node 24 via the latest v1 release

#### `.github/workflows/release.yml`
- Update all 3 `actions/checkout@v4` → `@v6`
- Update 2 `moonrepo/setup-rust@v0` → `@v1` (build + publish jobs)

#### `.github/workflows/codspeed.yml`
- Update `actions/checkout@v4` → `@v6`
- Update `moonrepo/setup-rust@v0` → `@v1`

### Phase 2: release-please (blocked upstream)

`googleapis/release-please-action@v4` still uses Node.js 20 with no released Node 24 version. PR [#1188](https://github.com/googleapis/release-please-action/pull/1188) is open but unmerged.

**Options:**
1. **Wait** — monitor the upstream PR; update when v5 ships (preferred)
2. **Workaround** — after June 2, set `ACTIONS_ALLOW_USE_UNSECURE_NODE_VERSION=true` as a temporary env var
3. **CLI migration** — run `release-please` as a CLI tool in a `run:` step instead of as a JS action, sidestepping the Node runtime entirely

**Recommendation:** Wait for upstream v5. The June 2 deadline gives ~2 months of buffer.

### Cache warnings

The "cache does not exist" and "cache service responded with 400" warnings are likely caused by the outdated `@v0` tag of `moonrepo/setup-rust`. Upgrading to `@v1` should resolve these. The "failed to save" errors are transient GitHub infrastructure issues — not actionable.

## Acceptance Criteria

- [x] `actions/checkout` updated to `@v6` in all 3 workflow files (8 occurrences total)
- [x] `moonrepo/setup-rust` updated to `@v1` in `release.yml` and `codspeed.yml` (3 occurrences)
- [ ] CI passes on PR branch with no Node.js 20 deprecation warnings (except release-please)
- [ ] Cache warnings eliminated after `moonrepo/setup-rust` upgrade
- [x] Add TODO comment above `release-please-action@v4` noting the upstream blocker

## Risk Analysis

- **`actions/checkout` v4→v6**: Low risk. Credential storage changed internally but standard checkout usage is unaffected. No input/output API changes.
- **`moonrepo/setup-rust` v0→v1**: Low risk. API is the same; `ci.yml` already uses v1 without issues.
- **release-please**: No action taken, no risk introduced.

## Sources

- [GitHub Node.js 20 deprecation timeline](https://github.blog/changelog/2025-09-19-deprecation-of-node-20-on-github-actions-runners/) — deadline June 2, 2026
- [actions/checkout releases](https://github.com/actions/checkout/releases) — v6.0.2 latest
- [moonrepo/setup-rust releases](https://github.com/moonrepo/setup-rust/releases) — v1.3.0 latest
- [release-please-action Node 24 PR #1188](https://github.com/googleapis/release-please-action/pull/1188)
- `CodSpeedHQ/action@v4` is a composite action — not affected by Node.js deprecation
