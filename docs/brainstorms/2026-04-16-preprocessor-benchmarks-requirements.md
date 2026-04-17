---
date: 2026-04-16
topic: preprocessor-benchmarks
---

# Preprocessor Benchmark Suite

## Problem Frame
With the parser+preprocessor now successfully processing the pcna-erp corpus, throughput drops from ~1k files/sec (parse-only) to ~70 files/sec (parse + preprocess with real includes). The existing CodSpeed benches cover only self-contained fixtures and give us zero signal on preprocessor performance or regressions. Before we start optimizing (include cache, hot-path tuning), we need a measurable baseline.

## Requirements
- R1. Add a `preprocessor_bench` target in `oxabl_preprocessor` that measures the preprocessor pass in isolation on synthetic fixtures.
- R2. Add a full-pipeline bench (tokenize → preprocess → parse) that exercises real `{include.i}` resolution against checked-in synthetic include files. This is the bench that should track the "70 files/sec" reality.
- R3. Add targeted micro-benchmarks for the preprocessor's hot paths: `&SCOPED-DEFINE` expansion, `{&var}` substitution, and `&IF` evaluation.
- R4. All fixtures are hand-crafted synthetic `.abl`/`.i` files checked into `resources/`, designed to exercise specific preprocessor features (deep include nesting, heavy `&IF`, scoped defines).
- R5. All new bench targets must be auto-discovered by the existing CodSpeed CI (`.github/workflows/codspeed.yml`) with no workflow changes.

## Success Criteria
- CodSpeed reports a baseline for preprocessor performance and full-pipeline-with-includes performance after the first merged run.
- Future PRs that touch the preprocessor or include resolution show a measurable delta in CodSpeed.

## Scope Boundaries
- No throughput target. We are establishing a baseline, not chasing a number.
- No include-file cache implementation. Cache work is a separate effort that this baseline will measure.
- No cold-vs-warm cache bench variants. Revisit once a cache exists.
- No real-corpus fixtures. The pcna-erp corpus stays out of the repo; synthetic fixtures only.
- No flamegraph-driven hotspot report as part of this work. Optimization is a follow-up.

## Key Decisions
- Synthetic fixtures only: keeps benches reproducible, vendored, and free of licensing/size concerns; easy to design around specific preproc features.
- Pipeline bench resolves includes from disk (against checked-in `.i` files) rather than mocking, so the baseline reflects real I/O the way the corpus run does.
- Three bench targets (isolation, pipeline, micros) instead of one: isolation catches preproc-only regressions, pipeline tracks the user-visible number, micros pinpoint hot-path regressions.

## Dependencies / Assumptions
- The existing CodSpeed workflow auto-discovers all `[[bench]]` targets, so no CI changes are needed.
- `oxabl_preprocessor` exposes a public API sufficient to drive both isolation and pipeline benches; verify during planning.

## Outstanding Questions

### Deferred to Planning
- [Affects R2][Technical] Where on disk should the pipeline bench's include files live, and how should the preprocessor's include search path be configured for the bench?
- [Affects R3][Technical] Do the chosen hot paths (`&SCOPED-DEFINE`, `{&var}`, `&IF`) have stable enough public entry points to micro-bench, or does the bench need a thin harness?
- [Affects R1, R2][Needs research] Should fixtures be sized to land in a CodSpeed-friendly per-iteration time band, and if so what range?

## Next Steps
→ `/ce:plan` for structured implementation planning
