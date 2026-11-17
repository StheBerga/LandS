# Branch: `refactor-lineplots`

**Remote snapshot SHA:** `67f77099078c817fc799139ca9aea8338928f4d0`  
**Snapshot date:** 2026-11-16  
**Comparison baseline:** `refactoring_CRAN` (`bd5342e2e1b696a45f00e229940e4fe44904701d`)

<img alt="status" src="https://img.shields.io/badge/status-under%20development-d29922">

## Purpose

Refactors `Lineplots()` for longitudinal summary visualization and aligns its statistical annotation workflow with `cont_var_test()` / `posthoc_df()`.

## Implemented in this branch

- Supports mean or median trajectories, optional LOESS smoothing, interquartile-range ribbons, observed points and individual subject trajectories.
- Supports overall or group-stratified plots, custom time breaks/labels, plot titles/legend panels and optional PowerPoint export.
- Supports overall and post-hoc statistical annotations using the shared `Test_results` structure.
- Adds the common plot-theme and grid/PPTX parameter helpers, and updates `master-lineplots.Rmd`.
- Merges the `refactor-posthoc` and `refactor-roadmap` statistical changes.

## Related GitHub issues

| Issue | Relationship to this branch | Status in supplied issue snapshot |
|---|---|---|
| [#39](https://github.com/StheBerga/LandS/issues/39) — Lineplots | The branch contains substantive changes to `Lineplots()`; the issue remains open in the supplied issue snapshot. | OPEN |
| [#19](https://github.com/StheBerga/LandS/issues/19), #20, #23 — testing and post-hoc workflow | Inherited from the roadmap/posthoc branches. | OPEN |

## Remaining work / review checklist

- [ ] Add tests for numeric time requirements, missing timepoints, uneven repeated measurements, group colours and ID trajectories.
- [ ] Test the interaction of ribbons, LOESS, annotations and custom x-axis breaks/labels.
- [ ] Verify consistency of output types for one variable versus multiple variables and `pptx = TRUE`.
- [ ] Bring the branch forward from merge base `0979769`; it is merged into `refactor-integrate`, but that integration branch itself is not yet merged into `refactoring_CRAN`.

## Relationship to `refactoring_CRAN`

17 commits ahead and 4 behind `refactoring_CRAN`; merge base is `0979769`. It is merged into `refactor-integrate`.

## Validation notes

- The analysis is based on the Git objects and branch snapshots contained in `LandS-all-branches.bundle`.
- Issue states above reflect the issue export supplied in this conversation; they were not refreshed from GitHub live.
- No `tests/` directory is present at this branch tip in the supplied bundle.
- R is not available in the inspection runtime, so `R CMD check`, `devtools::test()` and vignette builds were not executed.

## Notes

The branch shares substantial history with `refactor-boxplot`; do not treat their commit counts as independent feature counts.
