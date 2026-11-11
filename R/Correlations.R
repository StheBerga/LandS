#' Compute pairwise correlations among numeric variables
#'
#' @description Computes pairwise correlation coefficients and associated raw
#'   and adjusted p-values for a set of numeric variables in a data frame. The
#'   function returns correlation and p-values matrices, plus long-format tables
#'   of all variable pairs sorted either by correlation coefficient or by
#'   absolute correlation coefficient.
#'
#'   Optionally, the returned tables can also be exported to an Excel workbook
#'   with one sheet per output table.
#'
#' @param data A data frame containing the variables to correlate.
#' @param variables Character vector. Names of numeric columns in \code{data} to
#'   use for the correlation analysis.
#' @param method Character string. Correlation method passed to
#'   \code{stats::cor.test()} indicating which correlation coefficient is to be
#'   used for the test. One of \code{"pearson"}, \code{"kendall"}, or
#'   \code{"spearman"}, can be abbreviated. (Default = \code{"spearman"}).
#' @param rho_dec Integer. Number of decimal places used when formatting
#'   correlation coefficients (Default = 3).
#' @param pval_dec Integer. Number of decimal places used when formatting
#'   unadjusted p-values (Default = 4).
#' @param excel Logical. If \code{TRUE}, export the returned list to an Excel
#'   workbook using \code{writexl::write_xlsx()}, thus having one sheet for each
#'   table (Default = \code{FALSE}).
#' @param excel_path Character string. Path of the Excel file to create when
#'   \code{excel = TRUE} (Default = \code{"Results.xlsx"}).
#' @param p_adjust_method Character string specifying the multiplicity
#'   adjustment method. Supports all adjustment methods passed to
#'   \code{p.adjust.methods}. If `NULL` (Default), \code{fdr} is used.
#'
#' @return A named list with the following elements:
#' \describe{
#'   \item{rho}{Data frame of correlation coefficients.}
#'   \item{pvalue}{Data frame of raw p-values}
#'   \item{padj_}{Data frame of adjusted p-values. The name is given by pasting \code{p_adj}, and \code{p_adjust_method}}
#'   \item{rho_star}{Formatted correlation matrix. Statistically significant correlations at \code{adj-p < 0.05} are marked with \code{*}; diagonal entries are shown as \code{-}.}
#'   \item{pairwise_comparisons}{Long-format table of all variable pairs with rho, raw p-value, and adjusted p-value sorted by decreasing rho.}
#'   \item{form_pairwise_comparisons}{Formatted version of \code{pairwise_comparisons}.}
#'   \item{pair_sort_by_abs_rho}{Long-format table of all variable pairs with raw rho and
#'     p-value, sorted by decreasing absolute rho.}
#'   \item{form_pair_sort_by_abs_rho}{Formatted version of \code{pair_sort_by_abs_rho}.}
#'   \item{form_pair_significant}{Formatted table containing only pairs with \code{adj-p < 0.05}}
#' }
#'
#' @details The function checks that all selected variables are numeric before
#' computing correlations. P-values are obtained from \code{stats::cor.test(...,
#' na.rm = TRUE, exact = FALSE)} using the selected method.
#'
#' @export
#'
#' @author Stefano Bergamini, Luca Lalli
#'
#' @examples
#' correlations(
#'   data = mtcars,
#'   variables = c("mpg", "disp", "hp"),
#'   method = "spearman"
#' )
#' @importFrom magrittr %>%
#' @importFrom rlang .data
correlations <- function(data,
                         variables,
                         method = "spearman",
                         p_adjust_method = NULL,
                         rho_dec = 3,
                         pval_dec = 4,
                         excel = FALSE,
                         excel_path = "Results.xlsx"){

  if (is.null(p_adjust_method)) {
    p_adjust_method = "fdr"
  }

  start_time <- Sys.time()
  res <- list()

  # check numeric all variables
  for (i in variables) {
    if(!is.numeric(data[[i]])) {
      stop(paste("Error: \n Variable", i, "is not numeric"))
    }
  }


  # Creating rho and p-values matrices
  mat_r <- mat_p <-
    matrix(nrow = length(variables), ncol = length(variables)) %>%
    `colnames<-`(variables) %>%
    `rownames<-`(variables)

  for (i in variables) {
    for (j in variables) {
      mat_r[i, j] <-
        stats::cor.test(data[[i]], data[[j]], method = method, na.rm = TRUE, exact = FALSE)$estimate
      mat_p[i, j] <-
        stats::cor.test(data[[i]], data[[j]], method = method, na.rm = TRUE, exact = FALSE)$p.value
    }
  }

  # Create p-adjust matrix
  mat_padj <- mat_p
  mat_padj[upper.tri(mat_padj, diag = TRUE)] <- NA
  mat_padj <- stats::p.adjust(mat_padj, method = p_adjust_method) %>%
    matrix(nrow = length(variables), ncol = length(variables)) %>%
    `colnames<-`(variables) %>%
    `rownames<-`(variables)
  mat_padj[upper.tri(mat_padj, diag = FALSE)] <-
    t(mat_padj)[upper.tri(t(mat_padj), diag = FALSE)]

  # Creating matrix with star when adj-p is significant
  mat_star <- mat_r %>%
    as.data.frame() %>%
    dplyr::mutate(dplyr::across(dplyr::everything(), function(x) round(x, digits = rho_dec)))

  for (i in variables) {
    for (j in variables) {
      if (!is.na(mat_padj[i, j]) && mat_padj[i, j] < 0.05) {
        mat_star[i, j] <- paste(mat_star[i, j], "*")
      }
      if (i == j) {
        mat_star[i,j] <- '-'
      }
    }
  }

  # All pairs with rho, p-value, and adjusted p-value
  df_all_pairs <- utils::combn(variables, 2) %>%
    t %>%
    as.data.frame %>%
    `colnames<-`(c("Variable1", "Variable2")) %>%
    dplyr::mutate(rho = NA, pvalue = NA, padj = NA)

  for (i in seq_along(1:nrow(df_all_pairs))) {

    df_all_pairs[i, "rho"] <- mat_r[df_all_pairs$Variable1[i], df_all_pairs$Variable2[i]]
    df_all_pairs[i, "pvalue"] <- mat_p[df_all_pairs$Variable1[i], df_all_pairs$Variable2[i]]
    df_all_pairs[i, "padj"] <- mat_padj[df_all_pairs$Variable1[i], df_all_pairs$Variable2[i]]
  }

  res[["rho"]] <- mat_r %>% as.data.frame %>% tibble::rownames_to_column("Variable")
  res[["pvalue"]] <- mat_p %>% as.data.frame %>% tibble::rownames_to_column("Variable")
  res[[3]] <- mat_padj %>% as.data.frame %>% tibble::rownames_to_column("Variable")
  res[["rho_star"]] <- mat_star %>% tibble::rownames_to_column("Variable")
  res[["pairwise_comparisons"]] <- df_all_pairs %>%
    dplyr::arrange(-.data[["rho"]])
  res[["form_pairwise_comparisons"]] <- df_all_pairs %>%
    dplyr::mutate(dplyr::across(4:5, function(x) LandS::formatz_p(x, digits = pval_dec)),
                  rho = round(.data[["rho"]], digits = rho_dec))
  res[["pair_sort_by_abs_rho"]] <- df_all_pairs %>%
    dplyr::arrange(abs(.data[["rho"]]))
  res[["form_pair_sort_by_abs_rho"]] <- df_all_pairs %>%
    dplyr::mutate(dplyr::across(4:5, function(x) LandS::formatz_p(x, digits = pval_dec))) %>%
    dplyr::arrange(abs(.data[["rho"]])) %>%
    dplyr::mutate(rho = round(.data[["rho"]], digits = rho_dec))
  res[["form_pair_significant"]] <- df_all_pairs %>%
    dplyr::arrange(.data[["padj"]]) %>%
    dplyr::filter(.data[["padj"]] < 0.05) %>%
    dplyr::mutate(dplyr::across(4:5, function(x) LandS::formatz_p(x, digits = pval_dec))) %>%
    dplyr::mutate(rho = round(.data[["rho"]], digits = rho_dec))
  names(res)[[3]] <- paste0("padj_", p_adjust_method)

  if (excel == FALSE){

    return(res)

  }else{

    message("Saving excel file")
    writexl::write_xlsx(res, path = excel_path)
    return(res)

  }
}
