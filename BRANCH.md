# Branch: `refactor-univariate`

**Remote snapshot SHA:** `41996394e84891e7b4a037ddabe4b78f84351f2c`  
**Snapshot date:** 2026-11-16  
**Comparison baseline:** `refactoring_CRAN` (`bd5342e2e1b696a45f00e229940e4fe44904701d`)

<img alt="status" src="https://img.shields.io/badge/status-under%20development-d29922">

## Purpose

Refactors univariate Cox modelling and brings the percentage-based missingness update onto a dedicated branch.

## Implemented in this branch

- `univariate_cox()` now uses `survival::coxph()` consistently for numeric and categorical predictors.
- Adds `continuous_contrast`, with `"IQR"` (75th versus 25th percentile) as the default and `"unit"` for a one-unit increase.
- Uses likelihood-ratio tests for global predictor p-values and Wald tests for individual non-reference categorical levels.
- Improves handling of missing observations, constant predictors and model-fitting failures; preserves numeric `pval_raw` values and updates the Roxygen help.
- Removes the `rms::datadist()` setup from this univariate workflow.
- Also includes the `PercentageMissing` changes to `missing_matrix()` from commit `ac8dab2`.

## Related GitHub issues

| Issue | Relationship to this branch | Status in supplied issue snapshot |
|---|---|---|
| [#8](https://github.com/StheBerga/LandS/issues/8) — mixed use of `rms::cph()` and `survival::coxph()` | The commit explicitly references #8 and standardizes the univariate model on `survival::coxph()`. The issue is still open in the supplied issue snapshot. | OPEN |
| [#63](https://github.com/StheBerga/LandS/issues/63) — `univariate_cox` | The branch directly modifies this function; the issue remains open in the supplied snapshot. | OPEN |
| [#51](https://github.com/StheBerga/LandS/issues/51) — `fit_cox_model` | Related Cox-model infrastructure exists in the CRAN baseline, but this branch does not by itself prove that issue #51 is closed. | OPEN |
| [#29](https://github.com/StheBerga/LandS/issues/29) — missingness / availability | The branch includes `PercentageMissing`, an extension to the already-closed sample-availability work. | CLOSED (parent issue) |

## Remaining work / review checklist

- [ ] Test the interpretation of IQR hazard ratios for skewed variables, tied values and very small complete-case samples.
- [ ] Add tests for categorical reference levels, binary and multi-level predictors, non-finite values, constant predictors, missingness and model errors.
- [ ] Verify that `rms` remains needed elsewhere in the package before changing `DESCRIPTION`; this branch removes the `datadist` dependency from `univariate_cox`, not necessarily from every function.
- [ ] The `R/Univariate.R` file content is identical in `refactor-univariate`, `refactor-integrate` and `refactor-integration` in this bundle. Avoid re-applying the same source change; the missingness update is the additional delta that those integration branches do not contain.

## Relationship to `refactoring_CRAN`

2 commits ahead and 0 behind `refactoring_CRAN`; merge base is `bd5342e`. The two commits are `ac8dab2` (PercentageMissing) and `4199639` (univariate Cox refactor).

## Validation notes

- The analysis is based on the Git objects and branch snapshots contained in `LandS-all-branches.bundle`.
- Issue states above reflect the issue export supplied in this conversation; they were not refreshed from GitHub live.
- No `tests/` directory is present at this branch tip in the supplied bundle.
- R is not available in the inspection runtime, so `R CMD check`, `devtools::test()` and vignette builds were not executed.

## Notes

The issue snapshot reports #8 and #63 as open even though the implementation changes are present. The implementation status and issue workflow status should be tracked separately.
