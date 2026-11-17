# Branch: `refactor-posthoc`

**Remote snapshot SHA:** `876a64cdab1f17273a5678dc9cd820a1b01f225e`\
**Snapshot date:** 2026-11-16\
**Comparison baseline:** `refactoring_CRAN` (`bd5342e2e1b696a45f00e229940e4fe44904701d`)

<img alt="status" src="https://img.shields.io/badge/status-under%20review-8250df">

## Purpose

Builds on `refactor-roadmap` and refactors the post-hoc result formatter so pairwise p-values are matched to their comparison labels rather than assigned only by vector position.

## Implemented in this branch

- Rewrites `posthoc_df()` to generate the group-pair table and match result columns using comparison names in either direction (for example, `A vs B` or `B vs A`).
- Supports factor and numeric grouping variables, converting numeric groups to factor levels for pair generation.
- Retains filtering by `threshold_posthoc` and formats p-values for plotting.
- Merges the `cont_var_test()` improvements from `refactor-roadmap` and updates documentation for both functions.

## Related GitHub issues

| Issue | Relationship to this branch | Status in supplied issue snapshot |
|------------------------|------------------------|------------------------|
| [#20](https://github.com/StheBerga/LandS/issues/20) — `posthoc_df_LB` compatibility / incorrect p-value assignment | The rewrite matches named pairwise-comparison columns rather than relying solely on positional ordering. The issue remains open in the supplied snapshot. | OPEN |
| [#23](https://github.com/StheBerga/LandS/issues/23) — unify `Posthoc_lineplots()` and `posthoc_df()` group input | The implementation accepts factor or numeric grouping and is part of the response to this concern; issue remains open in the supplied snapshot. | OPEN |
| [#19](https://github.com/StheBerga/LandS/issues/19) — ContVarTest enhancement | Inherited from `refactor-roadmap`. | OPEN (checklist appears complete) |

## Remaining work / review checklist

## Relationship to `refactoring_CRAN`

10 commits ahead and 4 behind `refactoring_CRAN`; merge base is `0979769`. Merged downstream into `refactor-boxplot` and `refactor-lineplots`.

## Validation notes

- The analysis is based on the Git objects and branch snapshots contained in `LandS-all-branches.bundle`.
- Issue states above reflect the issue export supplied in this conversation; they were not refreshed from GitHub live.
- No `tests/` directory is present at this branch tip in the supplied bundle.
- R is not available in the inspection runtime, so `R CMD check`, `devtools::test()` and vignette builds were not executed.

## Notes

The issue #20 fix is visible in the code; the GitHub issue state remains open in the issue snapshot. Do not equate a code change with formal issue closure.
