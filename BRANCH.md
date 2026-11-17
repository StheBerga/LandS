# Branch: `refactor-integrate`

**Remote snapshot SHA:** `eda9adc777183cb0699e4ff432050bee55d79101`  
**Snapshot date:** 2026-11-16  
**Comparison baseline:** `refactoring_CRAN` (`bd5342e2e1b696a45f00e229940e4fe44904701d`)

<img alt="status" src="https://img.shields.io/badge/status-under%20development-d29922">

## Purpose

The more recent of the two integration branches in this bundle. It consolidates the main refactoring streams and adds a shared palette utility and common plotting themes.

## Implemented in this branch

- Merges `refactor-correlations`, `refactor-documentation`, the older `refactor-mixed` tip, `refactor-boxplot` and `refactor-lineplots`; the latter branches already contain the roadmap/post-hoc work.
- Adds `theme_lands_grid()`, `theme_lands_pptx()`, `set_params_grid()` and `set_params_pptx()` in `R/theme.R`.
- Adds and exports `get_palette_colors()`, supporting `ggsci` palette names and explicit R colour vectors; integrates it into Boxplot, Lineplots, KM plotting, assay availability and the legacy Distribution workflow.
- Includes the updated Cox `univariate_cox()` source and the sample-availability baseline already present in `refactoring_CRAN`.
- Updates `DESCRIPTION`, `NAMESPACE`, the pkgdown configuration, README artifacts, function documentation and Boxplot/Lineplots/ContVarTest vignettes.

## Related GitHub issues

| Issue | Relationship to this branch | Status in supplied issue snapshot |
|---|---|---|
| [#70](https://github.com/StheBerga/LandS/issues/70) — palette support | A shared helper is implemented and used by several plotting functions. The issue is still open in the supplied snapshot, so coverage and expected behavior should be checked before closing. | OPEN |
| [#48](https://github.com/StheBerga/LandS/issues/48), #56, #42, #44, #40, #37, #55, #58, #59, #64 | Changes from the correlations/documentation branches are included. These issues remain open in the supplied issue snapshot. | OPEN |
| [#18](https://github.com/StheBerga/LandS/issues/18), #19, #20, #23, #33, #39 | Changes from boxplot, lineplot, roadmap and post-hoc branches are included; corresponding issues remain open in the snapshot. | OPEN |
| [#8](https://github.com/StheBerga/LandS/issues/8) — Cox implementation | The updated `R/Univariate.R` blob is included. The issue remains open in the snapshot. | OPEN |

## Remaining work / review checklist

- [ ] Do not assume this integration branch contains the latest remote feature branches: it merged `refactor-mixed` at `5f5e96a`, not the newer remote commits for #72 and #73.
- [ ] Merge/reconcile the latest `missing_availability_changes` branch; the integration branch does not contain `PercentageMissing` or the new availability heatmap.
- [ ] Check `NAMESPACE`, dependencies and generated help after all feature merges, then run package checks and build the vignettes.
- [ ] Add a test suite. No `tests/` directory is present at this branch tip in the supplied bundle.
- [ ] Update the branch from `refactoring_CRAN`: it is one commit behind the current baseline in graph history.
- [ ] Resolve the remaining package-level scope questions in issues #16, #27 and #69.

## Relationship to `refactoring_CRAN`

43 commits ahead and 1 behind `refactoring_CRAN`; merge base is `354a62a` (the samples-availability feature commit). Compared with `refactor-integration`, this branch has 23 commits not in that branch and 12 commits in the reverse direction; they are divergent integration attempts, not a simple parent/child pair.

## Validation notes

- The analysis is based on the Git objects and branch snapshots contained in `LandS-all-branches.bundle`.
- Issue states above reflect the issue export supplied in this conversation; they were not refreshed from GitHub live.
- No `tests/` directory is present at this branch tip in the supplied bundle.
- R is not available in the inspection runtime, so `R CMD check`, `devtools::test()` and vignette builds were not executed.

## Notes

This is the strongest integration candidate in this snapshot because it contains the newer Boxplot/Lineplots theming and #70 palette work. It is not yet a complete aggregation of all remote branch tips.
