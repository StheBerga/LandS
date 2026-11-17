# Branch: `refactor-boxplot`

**Remote snapshot SHA:** `340fa1afc468386c4f341aedfa89f5f15b3fdf2e`  
**Snapshot date:** 2026-11-16  
**Comparison baseline:** `refactoring_CRAN` (`bd5342e2e1b696a45f00e229940e4fe44904701d`)

<img alt="status" src="https://img.shields.io/badge/status-under%20development-d29922">

## Purpose

Refactors `Boxplot()` for longitudinal and grouped continuous-variable visualization, including new distribution geometries and statistical annotations.

## Implemented in this branch

- Supports standard boxplots, notched boxplots and violin plots through the `type` argument.
- Adds/retains optional individual observations, paired subject trajectories, median/mean trend lines and overall/post-hoc test annotations.
- Updates bracket positioning/scaling for post-hoc annotations and corrects `.data` usage in the plotting code.
- Integrates the `cont_var_test()` and `posthoc_df()` changes from the roadmap/posthoc branches.
- Introduces the theme helpers in `R/theme.R` and the grid/PPTX parameter helpers used by plot functions; adds a dedicated Boxplot vignette.

## Related GitHub issues

| Issue | Relationship to this branch | Status in supplied issue snapshot |
|---|---|---|
| [#33](https://github.com/StheBerga/LandS/issues/33) — distribution/violin plots for longitudinal data | The branch adds violin visualization and keeps the longitudinal plotting/annotation framework. The issue remains open in the supplied snapshot. | OPEN |
| [#18](https://github.com/StheBerga/LandS/issues/18) — bracket positioning in Boxplot | Commit `3ea4175` explicitly improves bracket positioning and scaling. Issue remains open in the supplied snapshot. | OPEN |
| [#19](https://github.com/StheBerga/LandS/issues/19), #20, #23 — testing and post-hoc workflow | Inherited from `refactor-roadmap` and `refactor-posthoc`. | OPEN |

## Remaining work / review checklist

- [ ] Test the violin and notch modes with small samples, missing observations, outliers and uneven group sizes.
- [ ] Validate bracket placement across variables with very different ranges, multiple groups and many significant comparisons.
- [ ] Run regression tests for all combinations of points, paired ID lines, summary statistics and overall/post-hoc annotations.
- [ ] Bring the branch forward from merge base `0979769` and check for conflicts in `R/Boxplot.R`, `R/ContVarTest.R`, `R/Custom_Function_for_LandS.R` and `R/theme.R`.

## Relationship to `refactoring_CRAN`

17 commits ahead and 4 behind `refactoring_CRAN`; merge base is `0979769`. It contains the posthoc and roadmap work, and is merged into `refactor-integrate`.

## Validation notes

- The analysis is based on the Git objects and branch snapshots contained in `LandS-all-branches.bundle`.
- Issue states above reflect the issue export supplied in this conversation; they were not refreshed from GitHub live.
- No `tests/` directory is present at this branch tip in the supplied bundle.
- R is not available in the inspection runtime, so `R CMD check`, `devtools::test()` and vignette builds were not executed.

## Notes

The tip `340fa1a` is the remote branch SHA in this bundle. It is not yet merged into `refactoring_CRAN`.
