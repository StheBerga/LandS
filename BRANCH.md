# Branch: `refactor-correlations`

**Remote snapshot SHA:** `9273b90ad9542f2f127392a17f14fb355aa2ea86`  
**Snapshot date:** 2026-11-16  
**Comparison baseline:** `refactoring_CRAN` (`bd5342e2e1b696a45f00e229940e4fe44904701d`)

<img alt="status" src="https://img.shields.io/badge/status-under%20review-8250df">

## Purpose

Reworks `correlations()` and its output/documentation for pairwise analysis of multiple numeric variables.

## Implemented in this branch

- Computes correlation coefficients and raw p-values using Pearson, Kendall or Spearman methods.
- Adds configurable p-value adjustment (FDR by default) and returns correlation, raw-p and adjusted-p matrices.
- Creates a significance-marked correlation matrix and long-format pairwise tables ordered by correlation and absolute correlation.
- Adds formatted result tables and optional Excel export.
- Validates that selected variables are numeric and updates package dependencies and the function help.

## Related GitHub issues

| Issue | Relationship to this branch | Status in supplied issue snapshot |
|---|---|---|
| [#48](https://github.com/StheBerga/LandS/issues/48) — correlations | The branch commit explicitly references #48 and substantially rewrites the function. The issue remains open in the supplied issue snapshot. | OPEN |

## Remaining work / review checklist

- [ ] Add tests for all correlation methods, missingness patterns, constant variables, two-variable input, adjustment methods and Excel output.
- [ ] Review the implementation of the adjusted-p matrix and symmetric entries, particularly with missing or undefined correlations.
- [ ] Check pair sorting semantics: `pair_sort_by_abs_rho` should be confirmed to use the intended descending/ascending order.
- [ ] Bring the branch forward from merge base `0979769`; it has been merged into both integration branches in the supplied bundle.
- [ ] Look for possible two vectors correlations matrix - e.g. correlate all the fatty acids with all the cytokines. 

## Relationship to `refactoring_CRAN`

1 commit ahead and 4 behind `refactoring_CRAN`; merge base is `0979769`. Its changes are included in `refactor-integrate` and `refactor-integration`.

## Validation notes

- The analysis is based on the Git objects and branch snapshots contained in `LandS-all-branches.bundle`.
- Issue states above reflect the issue export supplied in this conversation; they were not refreshed from GitHub live.
- No `tests/` directory is present at this branch tip in the supplied bundle.
- R is not available in the inspection runtime, so `R CMD check`, `devtools::test()` and vignette builds were not executed.

## Notes

This branch is focused: only `DESCRIPTION`, `R/Correlations.R` and `man/correlations.Rd` differ from its common ancestor.
