# Branch: `refactor-roadmap`

**Remote snapshot SHA:** `f1b653396d8d4932887a5073fbfe636c82a17219`  
**Snapshot date:** 2026-11-16  
**Comparison baseline:** `refactoring_CRAN` (`bd5342e2e1b696a45f00e229940e4fe44904701d`)

<img alt="status" src="https://img.shields.io/badge/status-under%20review-8250df"> 
<!-- <img alt="status" src="https://img.shields.io/badge/status-ready%20for%20merging-1a7f37"> -->

## Purpose

Root feature branch for the `cont_var_test()` statistical-testing improvements subsequently inherited by the post-hoc, boxplot and lineplot branches.

## Implemented in this branch

- Reworks `cont_var_test()` to select tests by number of groups and paired/unpaired design: Wilcoxon rank-sum, Wilcoxon signed-rank, Kruskal–Wallis or Friedman, with corresponding post-hoc procedures.
- Adds a configurable post-hoc `type` and multiplicity adjustment; by default it uses Dunn for unpaired multi-group tests and exact all-pairs comparisons for paired multi-group tests.
- Adds `overall_threshold` so post-hoc tests can be skipped when the overall test does not pass the configured p-adjusted threshold.
- Supports summary statistics such as median, mean, SD, min/max, quartiles, n and range; paired summaries use complete observations.
- Documents the complete-block requirement for Friedman and ID consistency for paired Wilcoxon analyses.
- Updates `R/ContVarTest.R`, `man/cont_var_test.Rd` and the master lineplot vignette.

## Related GitHub issues

| Issue | Relationship to this branch | Status in supplied issue snapshot |
|---|---|---|
| [#19](https://github.com/StheBerga/LandS/issues/19) — ContVarTest enhancement | The commits explicitly target #19. All checklist items shown in the supplied issue text are checked, but the issue itself is still open in the supplied issue snapshot. | OPEN (checklist appears complete) |
|[#46](https://github.com/StheBerga/LandS/issues/46) — ContVarTest documentation | | OPEN (ready for closing after the merging) 

## Remaining work / review checklist

## Relationship to `refactoring_CRAN`

7 commits ahead and 4 behind `refactoring_CRAN`; merge base is `0979769`. The branch is a parent input to `refactor-posthoc`, then to `refactor-boxplot` and `refactor-lineplots`.

## Validation notes

- The analysis is based on the Git objects and branch snapshots contained in `LandS-all-branches.bundle`.
- Issue states above reflect the issue export supplied in this conversation; they were not refreshed from GitHub live.
- No `tests/` directory is present at this branch tip in the supplied bundle.
- R is not available in the inspection runtime, so `R CMD check`, `devtools::test()` and vignette builds were not executed.

## Notes

This is a focused functional branch, not the full CRAN integration branch.
