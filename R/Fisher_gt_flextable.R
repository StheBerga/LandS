#' Create a formatted Fisher's exact test cross-tabulation
#'
#' @description Builds a row-percentage cross-tabulation for two categorical variables,
#' computes Fisher's exact test, and returns the result as a formatted
#' 'flextable' object.
#'
#' @param data A data frame containing the variables to tabulate.
#' @param row_var Column name to use as the row variable.
#' @param col_var Column name to use as the column variable.
#' @param label_row_var Character string used as the display label for
#' 'row_var'.
#' @param label_col_var Character string used as the display label for
#' 'col_var'.
#'
#' @return A flextable object containing the cross-tabulation, row
#' percentages, and Fisher's exact test p-value.
#'
#' @details
#' Missing values are excluded from the cross-tabulation with
#' missing = "no".
#'
#' Percentages are calculated by row. The p-value is computed using
#' stats::fisher.test() through gtsummary::add_p() and formatted with
#' three decimal places.
#'
#' The returned table is formatted for reporting, including centered body text,
#' bold headers, Calibri Light font, automatic column sizing, and vertical
#' borders.
#' @export
#'
#' @author Luca Lalli
#'
#' @examples
#' fisher_gt_flex(
#'   data = mtcars,
#'   row_var = "am",
#'   col_var = "cyl",
#'   label_row_var = "Transmission",
#'   label_col_var = "Number of cylinders"
#' )
fisher_gt_flex <- function(data, row_var, col_var, label_row_var, label_col_var){

  tab <- gtsummary::tbl_cross(data, row = row_var, col = col_var, missing = "no", percent = "row",
                       missing_text = "NA", label = list(row_var ~ label_row_var, col_var ~ label_col_var))
  tab <- gtsummary::add_p(tab, test =  "fisher.test", pvalue_fun = function(x) gtsummary::style_pvalue(x, digits = 3))
  tab <- gtsummary::as_flex_table(tab)
  tab <- flextable::colformat_char(tab, na_str = "")
  tab <- flextable::align(tab, part = "body", align = "center")
  tab <- flextable::bold(tab, part = "header")
  tab <- flextable::bold(tab, j = 1)
  tab <- flextable::autofit(tab)
  tab <- flextable::fit_to_width(tab, max_width = 18, unit = "cm")
  tab <- flextable::font(tab, fontname = "Calibri Light")
  tab <- flextable::vline_right(tab, border = flextable::fp_border_default(color = "black", width = 0.8))
  tab <- flextable::vline_left(tab, border = flextable::fp_border_default(color = "black", width = 0.8))
  tab
}
