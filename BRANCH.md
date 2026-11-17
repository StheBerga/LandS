# Branch: `refactor-documentation`

**Remote snapshot SHA:** `7566c9a08380b1f049308d0640c099947d54c958`  
**Snapshot date:** 2026-11-16  
**Comparison baseline:** `refactoring_CRAN` (`bd5342e2e1b696a45f00e229940e4fe44904701d`)

<img alt="status" src="https://img.shields.io/badge/status-under%20development-d29922">

## Purpose

Reworks documentation and selected implementations across several utility and survival-analysis functions.

## Implemented in this branch

- `plot_km_curve()` is rewritten around `ggsurvfit`, with confidence intervals, censor marks, risk tables, survival-quantile annotations, log-rank p-values and an optional complementary log-log plot for graphical PH assessment.
- Updates `New_Project()`, `Distribution()`, `Outlier_Report()` and `Progress_bar()` implementations/documentation.
- Rewrites or updates help for `print_plot_grid()`, `olink_gsea_map()`, `prot2gene` and `vect.quoted()`.
- Updates `DESCRIPTION` and multiple `.Rd` files; the branch contains several commits explicitly linked to the relevant issues.

## Related GitHub issues

| Issue | Relationship to this branch | Status in supplied issue snapshot |
|---|---|---|
| [#56](https://github.com/StheBerga/LandS/issues/56) — `plot_km_curve` | The branch rewrites the Kaplan–Meier plotting function. | OPEN |
| [#40](https://github.com/StheBerga/LandS/issues/40) — `New_Project` | The branch updates the function and help. | OPEN |
| [#37](https://github.com/StheBerga/LandS/issues/37) — `Distribution` | The legacy points/cutoff distribution function is rewritten; it is not the new three-mode `distribution_plot()` from issue #73. | OPEN |
| [#42](https://github.com/StheBerga/LandS/issues/42) — `Outlier_Report` | The implementation and documentation are rewritten. | OPEN |
| [#44](https://github.com/StheBerga/LandS/issues/44) — `Progress_bar` | The implementation and documentation are rewritten. | OPEN |
| [#55](https://github.com/StheBerga/LandS/issues/55), #59, #64, #58 | Updates cover `olink_gsea_map`, `prot2gene`, `vect.quoted` and `print_plot_grid` documentation. | OPEN |

## Remaining work / review checklist

- [ ] Run examples and validate all `plot_km_curve()` output modes, including risk tables, multiple strata and `check_ph = TRUE`.
- [ ] Review any behavioral changes in the rewritten utility functions against their previous public interfaces.
- [ ] Regenerate documentation from Roxygen and confirm the `NAMESPACE` and `.Rd` files are synchronized.
- [ ] Add tests and bring the branch forward from merge base `0979769`; its changes are merged into both integration branches.

## Relationship to `refactoring_CRAN`

9 commits ahead and 4 behind `refactoring_CRAN`; merge base is `0979769`. It is merged into `refactor-integrate` and `refactor-integration`.

## Validation notes

- The analysis is based on the Git objects and branch snapshots contained in `LandS-all-branches.bundle`.
- Issue states above reflect the issue export supplied in this conversation; they were not refreshed from GitHub live.
- No `tests/` directory is present at this branch tip in the supplied bundle.
- R is not available in the inspection runtime, so `R CMD check`, `devtools::test()` and vignette builds were not executed.

## Notes

This branch contains both code changes and documentation changes. Issue references in commit messages do not imply the issues were closed; the supplied issue snapshot marks them open.
