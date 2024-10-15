# Funzione che crea marginale colonne e esegue test di fisher

#' Function to build coloumn marginal statistics and Fisher test
#'
#' @param data dataframe
#' @param row_var row variable
#' @param col_var column variable
#' @param label_row_var label for row
#' @param label_col_var label for column
#'
#' @return
#' @export
#'
#' @examples
LL_fisher_gt_flex <- function(data, row_var, col_var, label_row_var, label_col_var){
tab <- tbl_cross(data, row = row_var, col = col_var, missing = "no", percent = "row",
                     missing_text = "NA", label = list(row_var ~ label_row_var, col_var ~ label_col_var))
tab <- add_p(tab, test =  "fisher.test", pvalue_fun = function(x) style_pvalue(x, digits = 3))
tab <- as_flex_table(tab)
tab <- colformat_char(tab, na_str = "")
tab <- align(tab, part = "body", align = "center")
tab <- bold(tab, part = "header")
tab <- bold(tab, j = 1)
tab <- autofit(tab)
tab <- fit_to_width(tab, max_width = 18, unit = "cm")
tab <- font(tab, fontname = "Calibri Light")
tab <- vline_right(tab, border = fp_border_default(color = "black", width = 0.8))
tab <- vline_left(tab, border = fp_border_default(color = "black", width = 0.8))
tab
}
