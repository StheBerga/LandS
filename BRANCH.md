# Branch: `refactor-integration`

**Remote snapshot SHA:** `0e5cafb4524d7048948bea3de1c9f8b8d2b70a37`  
**Snapshot date:** 2026-11-16  
**Comparison baseline:** `refactoring_CRAN` (`bd5342e2e1b696a45f00e229940e4fe44904701d`)

<img alt="status" src="https://img.shields.io/badge/status-deprecated-cf222e">

## Purpose

An alternative integration branch that consolidates the documentation, mixed, correlations, sample-availability, roadmap, post-hoc, boxplot and lineplot streams.

## Implemented in this branch

- Merges `samples-availability`, `refactor-roadmap`, `refactor-posthoc`, `refactor-boxplot`, `refactor-lineplots`, the older `refactor-mixed`, `refactor-correlations` and `refactor-documentation` histories.
- Adds an earlier version of the shared plotting theme helpers in `R/theme.R`.
- Contains the updated Cox `univariate_cox()` source and the sample-availability baseline.
- Includes the README, pkgdown, documentation and vignette changes inherited from the merged feature branches.

## Related GitHub issues

| Issue | Relationship to this branch | Status in supplied issue snapshot |
|---|---|---|
| [#8](https://github.com/StheBerga/LandS/issues/8), #18, #19, #20, #23, #33, #39, #48 and documentation issues #37/#40/#42/#44/#55/#56/#58/#59/#64 | The branch aggregates changes from the respective feature branches, but the supplied issue snapshot still lists these issues as open. | OPEN |
| [#70](https://github.com/StheBerga/LandS/issues/70) — palette support | The `get_palette_colors()` helper and its broader integration are not present in this branch tip. | NOT IMPLEMENTED HERE |

## Remaining work / review checklist

- [ ] Compare this branch against `refactor-integrate` before choosing an integration line. The branches are divergent: there are 12 commits unique to `refactor-integration` and 23 unique to `refactor-integrate` in the bundle.
- [ ] Consider carrying forward the newer `get_palette_colors()` helper and updated Boxplot/Lineplots theme parameter work from `refactor-integrate`.
- [ ] The branch merged the older `refactor-mixed` tip, not the newer remote commits for #72 and #73; bring those changes in deliberately.
- [ ] Merge/reconcile the latest `missing_availability_changes` branch; the availability heatmap and `PercentageMissing` additions are absent here.
- [ ] Update from `refactoring_CRAN`, regenerate documentation and run CRAN checks after integration.
- [ ] No `tests/` directory is present at this branch tip in the supplied bundle.

## Relationship to `refactoring_CRAN`

32 commits ahead and 1 behind `refactoring_CRAN`; merge base is `354a62a`. This branch and `refactor-integrate` are two distinct, divergent integration attempts.

## Validation notes

- The analysis is based on the Git objects and branch snapshots contained in `LandS-all-branches.bundle`.
- Issue states above reflect the issue export supplied in this conversation; they were not refreshed from GitHub live.
- No `tests/` directory is present at this branch tip in the supplied bundle.
- R is not available in the inspection runtime, so `R CMD check`, `devtools::test()` and vignette builds were not executed.

## Notes

Do not merge this branch wholesale into `refactor-integrate` (or vice versa) without reviewing shared changes and conflicts. Select one integration line and port only missing features.
