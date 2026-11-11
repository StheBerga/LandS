#' Generate an outlier report using Tukey's fences rule.
#'
#' @description
#' Identifies potential outliers in selected numeric variables using the Tukey's
#' fences rule, based on the interquartile range (IQR). Outliers are flagged
#' using both global variables thresholds, calculated across the full dataset,
#' and - if provided - stratified thresholds, calculated separately within each
#' level of a grouping variable. \cr
#' Optionally, the report can be exported to an Excel file.
#' @param data A data frame containing the variables to be checked.
#' @param variables Character vector giving the names of numeric variables
#' for which outliers should be identified.
#' @param group Character string giving the name of the grouping variable used
#' to compute stratified outlier thresholds (Default = \code{NULL}).
#' @param k Numeric multiplier used to define the IQR outlier thresholds.
#' Values below \eqn{Q_1 - k \times IQR} or above \eqn{Q_3 + k \times IQR}
#' are flagged as outliers (Default = 1.5).
#' @param excel Logical. If \code{TRUE}, the output is written to an Excel file.
#' Default is \code{FALSE}.
#' @param excel_path Character string giving the path where the Excel file
#' should be saved when \code{excel = TRUE}. Default is "Outlier Report.xlsx".
#'
#' @return
#' If \code{group} is not provided, the function returns a single data frame with
#' the same rows as \code{data}, where selected variables are replaced by
#' \code{TRUE} for observations identified as outliers using thresholds calculated
#' across the full dataset, and \code{NA} otherwise. \cr
#' If \code{group} is provided, a named list with two data frames is returned:
#' \describe{
#'   \item{Global}{A data frame with the same architecture when \code{group} is not provided.}
#'   \item{Stratified}{A data frame with the same architecture as \code{Global},
#'   with outliers identified using thresholds calculated separately within each
#'   level of \code{group}.}
#' }
#' Non-selected columns are retained unchanged in both returned dataframes.
#' @export
#'
#' @author Stefano Bergamini, Luca Lalli
#'
#' @details
#' For each variable in \code{variables}, the function computes lower and upper
#' outlier thresholds using:
#' \deqn{[Q_1 - k \times IQR;\ Q_3 + k \times IQR]}
#' where \code{Q1} and \code{Q3} are the first and third quartiles, respectively.
#' John Tukey's fences rule sets \eqn{k = 1.5} to indicate an "outlier", while
#' \eqn{k = 3} identifies data that is "far out".
#'
#' Users should be aware of the following limitations when applying Tukey's
#' fences rule:
#' \describe{
#'   \item{Sensitivity to distribution shape}{The method implicitly assumes
#'   a symmetric distribution. The standard multiplier \code{k} is based on
#'   this assumption. For highly skewed distributions (e.g. log-normal distributions),
#'   the method tends to generate a large number of false positives in the longer tail.}
#'
#'   \item{Sample size effect}{The multiplication factor does not adapt
#'   to the sample size \eqn{n}. As \eqn{n} increases, the probability of incorrectly
#'   classifying normal observations as outliers can grow substantially.}
#'
#'   \item{Risk of information loss}{Observations lying outside the fences
#'   are not necessarily measurement errors; they may represent rare but important
#'   events within the phenomenon under study. Therefore, automatic removal of such
#'   observations is generally discouraged.}
#'
#'   \item{Heuristic nature}{The method was developed within the framework
#'   of exploratory data analysis (EDA) and is not a formal hypothesis test.
#'   Consequently, it does not provide p-values or significance levels.}
#' }
#'
#'
#' @importFrom magrittr %>%
#'
#' @examples
#' data_example <- data.frame(
#'   Time = rep(c("T0", "T1"), times = 5),
#'   biomarker = c(10, 11, 12, 13, 10, 50, 11, 12, 9, 10)
#' )
#'
#' Outlier_Report(
#'   data = data_example,
#'   variables = "biomarker",
#'   group = "Time"
#' )
Outlier_Report <- function (data,
                            variables,
                            group = NULL,
                            k = 1.5,
                            excel = FALSE,
                            excel_path = "Outlier Report.xlsx") {
  res <- list()

  df <- df2 <- data %>% dplyr::mutate(dplyr::across(dplyr::all_of(variables), ~ NA))

  for (p in variables) {

    th_overall <- c(
      # Minimum threshold
      stats::quantile(data[[p]], probs = 0.25, na.rm = TRUE) -
        (k * stats::IQR(data[[p]], na.rm = TRUE)),
      # Maximum threshold
      stats::quantile(data[[p]], probs = 0.75, na.rm = TRUE) +
        (k * stats::IQR(data[[p]], na.rm = TRUE))
    ) %>% `names<-`(c("Min", "Max"))

    if (!is.null(group)) {
      th_stratified <-
        cbind(tapply(data[[p]], data[[group]], FUN = function (x) stats::quantile(x, probs = 0.25, na.rm = TRUE)) -
                (k * tapply(data[[p]], data[[group]], FUN = function (x) stats::IQR(x, na.rm = TRUE))),
              tapply(data[[p]], data[[group]], FUN = function (x) stats::quantile(x, probs = 0.75, na.rm = TRUE)) +
                (k * tapply(data[[p]], data[[group]], FUN = function (x) stats::IQR(x, na.rm = TRUE)))) %>%
        as.data.frame() %>%
        `colnames<-`(c("Min", "Max")) %>%
        tibble::rownames_to_column("group")
    }

    for (i in seq_along(1:nrow(data))) {
      # Check for global outliers
      df[i, p] <-
        ifelse(data[i, p] < th_overall["Min"] | data[i, p] > th_overall["Max"], TRUE, NA)
      # Check for group-specific outliers
      if (!is.null(group)) {
        df[i, p] <-
          ifelse(data[i, p] < th_stratified[th_stratified$group == data[i, group], "Min"] |
                   data[i, p] > th_stratified[th_stratified$group == data[i, group], "Max"],
                 TRUE, NA)
      }
    }
  }
  res[["Global"]] <- df
  if (!is.null(group)) {res[["Stratified"]] <- df2}
  if (excel) {writexl::write_xlsx(res, path = excel_path)}
  if (!is.null(group)) {return(res)} else {return(res[["Global"]])}
}

