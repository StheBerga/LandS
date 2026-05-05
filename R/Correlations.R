#' Compute pairwise correlations among numeric variables
#'
#' @description Computes pairwise correlation coefficients and associated p-values
#' for a set of numeric variables in a data frame. The function returns correlation and
#' p-value matrices, plus long-format tables of all variable pairs sorted either
#' by correlation coefficient or by absolute correlation coefficient.
#'
#' Optionally, the returned tables can also be exported to an Excel workbook with
#' one sheet per output table.
#'
#' @param data A data frame containing the variables to correlate.
#' @param variables Character vector. Names of numeric columns in 'data' to use
#' for the correlation analysis.
#' @param method Character string. Correlation method passed to stats::cor.test().
#' Common choices are "spearman", "pearson", and "kendall" (Default = "spearman").
#' @param rho_dec Integer. Number of decimal places used when formatting
#' correlation coefficients (Default = 3).
#' @param pval_dec Integer. Number of decimal places used when formatting unadjusted
#' p-values (Default = 4).
#' @param excel Logical. If TRUE, export the returned list to an Excel workbook
#' using writexl::write_xlsx(), thus having one sheet for each table. Default=FALSE.
#' @param excel_path Character string. Path of the Excel file to create when
#' excel = TRUE. Default to paste0(path_output, "/Results.xlsx").
#'
#' @return A named list with the following elements:
#' \describe{
#'   \item{res_raw_corr}{Square dataframe of raw correlation coefficients.}
#'   \item{res_raw_pval}{Square dataframe of raw p-values.}
#'   \item{res_corr_sig}{Formatted correlation matrix. Statistically significant
#'     correlations at 'p < 0.05' are marked with '*'; diagonal entries are shown
#'     as '-'.}
#'   \item{res_raw_pair}{Long-format table of all variable pairs with raw rho and
#'     p-value, sorted by decreasing rho.}
#'   \item{res_pair_form}{Formatted version of 'res_raw_pair'.}
#'   \item{res_raw_abs}{Long-format table of all variable pairs with raw rho and
#'     p-value, sorted by decreasing absolute rho.}
#'   \item{res_abs_form}{Formatted version of 'res_raw_abs'.}
#'   \item{res_pair_sig}{Formatted table containing only pairs with 'p < 0.05'.}
#' }
#'
#' @details
#' The function checks that all selected variables are numeric before computing
#' correlations. P-values are obtained from stats::cor.test(..., na.rm=T, exact = FALSE) using the selected
#' method.
#'
#' Missing values are handled separately for each variable pair by
#' stats::cor.test(..., na.rm=T, exact = FALSE), so the number of complete observations may differ across
#' pairs.
#'
#' Note that significance is currently defined using an unadjusted p-value
#' threshold of 0.05; no correction for multiple testing is applied.
#' Correlations in res_corr_sig are marked with * when the corresponding
#' unadjusted p-value is < 0.05.
#' @export
#'
#' @author Luca Lalli, Stefano Bergamini
#'
#' @examples
#' correlations(
#'   data = mtcars,
#'   variables = c("mpg", "disp", "hp"),
#'   method = "spearman"
#' )
correlations <- function(data,
                         variables,
                         method = "spearman",
                         rho_dec = 3,
                         pval_dec = 4,
                         excel = FALSE,
                         excel_path = paste0(path_output, "/Results.xlsx")){
  options(width=10000)
  options(max.print=99999)
  options(scipen = 9999) # JG: 99999 is invalid
  require(dplyr)

  start_time <- Sys.time()
  data_tmp <- dplyr::select(data, all_of(variables))

  # check numeric all variables
  for (i in variables) {
    test_numeric <- is.numeric(data_tmp[, i])
    if(test_numeric == F){
      cat("Error, var:", i, " is not numeric", "\n")
      stop()
    }
  }

  # Creo matrici rho e pvalue
  df_corr <- as.data.frame(matrix(, nrow = length(variables), ncol = length(variables) + 1))
  df_pval <- as.data.frame(matrix(, nrow = length(variables), ncol = length(variables) + 1))
  colnames(df_corr) <- c("Var", variables); df_corr$Var <- variables
  colnames(df_pval) <- c("Var", variables); df_pval$Var <- variables


  for (i in variables) {
    for (j in variables) {

      coeff_rho <- cor.test(data_tmp[, i], data_tmp[, j], method = method, na.rm=T, exact = FALSE)$estimate
      df_corr[df_corr$Var == i, colnames(df_corr) == j] <- coeff_rho

      coeff_pval <- cor.test(data_tmp[, i], data_tmp[, j], method = method, na.rm=T, exact = FALSE)$p.value
      df_pval[df_pval$Var == i, colnames(df_pval) == j] <- coeff_pval
    }

    LandS::Progress_bar(current = which(i == variables),
                        total = length(variables),
                        start_time = start_time,
                        bar_fill = "\U2588",
                        bar_void = "\U2591")
  }

  # Creo matrice rho con * dove significativi
  df_corr_sign <- df_corr

  for (i in variables){
    df_corr_sign[, i] <- format(round(df_corr_sign[, i], rho_dec), digits = rho_dec, nsmall = rho_dec, width = 6, scientific=F)
  }


  for (i in variables) {
    for (j in variables) {
      if(df_pval[df_pval$Var == i, colnames(df_pval) == j] < 0.05){
        df_corr_sign[df_corr_sign$Var == i, colnames(df_corr_sign) == j] <-
          paste0(df_corr_sign[df_corr_sign$Var == i, colnames(df_corr_sign) == j], "*")

        if(i == j){
          df_corr_sign[df_corr_sign$Var == i, colnames(df_corr_sign) == j] <- "-"
        }
      }
    }
  }

  df_corr <- as.data.frame(df_corr)
  df_pval <- as.data.frame(df_pval)
  df_corr_sign <- as.data.frame(df_corr_sign)

  # All raw pairs with rho & pvalue
  df_all_pairs <- as.data.frame(t(combn(variables, 2)))
  colnames(df_all_pairs) <- c("Var1", "Var2")
  df_all_pairs$rho <- NA
  df_all_pairs$pval <- NA

  for (i in 1: nrow(df_all_pairs)) {

    df_all_pairs[i, "rho"] <- df_corr[df_corr$Var == df_all_pairs$Var1[i],
                                      colnames(df_corr) == df_all_pairs$Var2[i]]

    df_all_pairs[i, "pval"] <- df_pval[df_corr$Var == df_all_pairs$Var1[i],
                                       colnames(df_corr) == df_all_pairs$Var2[i]]
  }

  # All pairs with rho and pvalue as number
  df_all_pairs$rho <- as.numeric(df_all_pairs$rho)
  df_all_pairs$pval <- as.numeric(df_all_pairs$pval)

  # Sort by rho
  df_all_pairs <- arrange(df_all_pairs, desc(rho))

  # Sort by abs rho
  df_all_pairs_abs_rho <- arrange(df_all_pairs, desc(abs(rho)))

  # Formatted rho
  df_formatted_abs <- df_all_pairs_abs_rho

  df_formatted_abs$rho <- round(df_formatted_abs$rho, digits = rho_dec)

  df_formatted_abs$pval <- case_when(
    df_formatted_abs$pval > 1 ~ NA_character_,
    df_formatted_abs$pval < 0 ~ NA_character_,
    df_formatted_abs$pval == 0 ~ "0",
    df_formatted_abs$pval >= 0.0001 & df_formatted_abs$pval <= 1 ~ format(round(df_formatted_abs$pval, pval_dec), digits = pval_dec, nsmall = pval_dec, width = 6, scientific=F),
    df_formatted_abs$pval < 0.0001 ~ "<0.0001"
  )

  # Formatted rho & pval
  df_formatted <- df_all_pairs

  df_formatted$rho <- round(df_formatted$rho, digits = rho_dec)

  df_formatted$pval <- case_when(
    df_formatted$pval > 1 ~ NA_character_,
    df_formatted$pval < 0 ~ NA_character_,
    df_formatted$pval == 0 ~ "0",
    df_formatted$pval >= 0.0001 & df_formatted$pval <= 1 ~ format(round(df_formatted$pval, pval_dec), digits = pval_dec, nsmall = pval_dec, width = 6, scientific=F),
    df_formatted$pval < 0.0001 ~ "<0.0001"
  )


  # Filter significative correlations
  df_pairs_sign <- df_all_pairs[df_all_pairs$pval < 0.05 , ]

  df_pairs_sign$rho <- round(df_pairs_sign$rho, digits = rho_dec)

  df_pairs_sign$pval <- case_when(
    df_pairs_sign$pval > 1 ~ NA_character_,
    df_pairs_sign$pval < 0 ~ NA_character_,
    df_pairs_sign$pval == 0 ~ "0",
    df_pairs_sign$pval >= 0.0001 & df_pairs_sign$pval <= 1 ~ format(round(df_pairs_sign$pval, pval_dec), digits = pval_dec, nsmall = pval_dec, width = 6, scientific=F),
    df_pairs_sign$pval < 0.0001 ~ "<0.0001"
  )

  list_final_out <- list(res_raw_corr = df_corr,
                         res_raw_pval = df_pval,
                         res_corr_sig = df_corr_sign,
                         res_raw_pair = df_all_pairs,
                         res_pair_form = df_formatted,
                         res_raw_abs = df_all_pairs_abs_rho,
                         res_abs_form = df_formatted_abs,
                         res_pair_sig = df_pairs_sign

  )
  if (excel == FALSE){

    return(list_final_out)

  }else{

    message("Saving excel file")
    writexl::write_xlsx(list_final_out, path = excel_path)
    return(list_final_out)

  }
}
