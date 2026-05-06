#' Generate an outlier report
#'
#' @description
#' Identifies potential outliers in selected numeric variables using the
#' interquartile range (IQR) rule. Outliers are flagged using both global
#' thresholds, calculated across the full dataset, and stratified thresholds,
#' calculated separately within each level of a grouping variable.
#'
#' Optionally, the report can be exported to an Excel file.
#'
#' @param data A data frame containing the variables to be checked.
#' @param variables Character vector giving the names of numeric variables
#' for which outliers should be identified.
#' @param ID Character string giving the name of the subject or observation
#' identifier column. Default is "ID".
#' @param Group Character string giving the name of the grouping variable used
#' to compute stratified outlier thresholds. Default is "Time".
#' @param k Numeric multiplier used to define the IQR outlier thresholds.
#' Values below `Q1 - k * IQR` or above `Q3 + k * IQR` are flagged as
#' outliers. Default is 1.5.
#' @param excel Logical. If TRUE, the output is written to an Excel file.
#' Default is FALSE.
#' @param excel_path Character string giving the path where the Excel file
#' should be saved when excel = TRUE. Default is "Output/Outlier Report.xlsx".
#'
#' @return
#' A named list with two data frames:
#' \describe{
#'   \item{Global}{A data frame with the same rows as `data`, where selected
#'   variables are replaced by `TRUE` for observations identified as outliers
#'   using thresholds calculated across the full dataset, and `NA` otherwise.}
#'   \item{Stratified}{A data frame with the same rows as `data`, where selected
#'   variables are replaced by `TRUE` for observations identified as outliers
#'   using thresholds calculated separately within each level of `Group`, and
#'   `NA` otherwise.}
#' }
#'
#' Non-selected columns are retained unchanged in both returned dataframes.
#' @export
#'
#' @author Luca Lalli, Stefano Bergamini
#'
#' @details
#' For each variable in `variables`, the function computes lower and upper
#' outlier thresholds using:
#'
#' \deqn{Q_1 - k \times IQR}
#' \deqn{Q_3 + k \times IQR}
#'
#' where `Q1` and `Q3` are the first and third quartiles, respectively.
#'
#' The `Global` output uses thresholds computed on the full variable.
#' The `Stratified` output uses thresholds computed separately within each
#' level of `Group`.
#'
#' @examples
#' data_example <- data.frame(
#'   ID = rep(1:5, each = 2),
#'   Time = rep(c("T0", "T1"), times = 5),
#'   biomarker = c(10, 11, 12, 13, 10, 50, 11, 12, 9, 10)
#' )
#'
#' Outlier_Report(
#'   data = data_example,
#'   variables = "biomarker",
#'   ID = "ID",
#'   Group = "Time"
#' )
Outlier_Report <- function (data,
                            variables,
                            ID = "ID",
                            Group = "Time",
                            k = 1.5,
                            excel = FALSE,
                            excel_path = "Output/Outlier Report.xlsx") {
  require(progress)
  require(dplyr)

  # Progress bar
  pb <- progress_bar$new(format = "[:bar] :current/:total (:percent)", total = length(variables))
  pb$tick(0)

  df <- data; df2 <- data
  df[, variables] <- NA; df2[, variables] <- NA

  for (p in variables){

    tmp <- cbind(tapply(data[, p], data[, Group], FUN = function (x) quantile(x, probs = .25, na.rm = T)) -
                   (k * tapply(data[, p], data[, Group], FUN = function (x) IQR(x, na.rm = T))),
                 tapply(data[, p], data[, Group], FUN = function (x) quantile(x, probs = .75, na.rm = T)) +
                   (k * tapply(data[, p], data[, Group], FUN = function (x) IQR(x, na.rm = T)))) %>%  as.data.frame() %>%
      mutate(Var = rownames(.)) %>%
      `rownames<-`(NULL) %>%
      `colnames<-`(c("Min", "Max", "Group"))

    for (i in unique(df[, Group])){
      for (j in unique(df[, ID])){

        df[df[, ID] == j & df[, Group] == i, p] <-
          ifelse(data[data[, Group] == i & data[, ID] == j, p] < tmp[tmp$Group == i, "Min"] |
                   data[data[, Group] == i & data[, ID] == j, p] > tmp[tmp$Group == i, "Max"], TRUE, NA)

        df2[df2[, ID] == j & df2[, Group] == i, p] <- ifelse(
          data[data[, Group] == i & data[, ID] == j, p] > quantile(data[, p], probs = 0.75, na.rm = T) + (k * IQR(data[, p], na.rm = T)) |
          data[data[, Group] == i & data[, ID] == j, p] < quantile(data[, p], probs = 0.25, na.rm = T) - (k * IQR(data[, p], na.rm = T)),
          TRUE, NA)

      }
    }
    Sys.sleep(0.005)
    pb$tick(1)
    pb
  }
  list <- list("Global" = df2, "Stratified" = df)
  if (excel){writexl::write_xlsx(list, path = excel_path)}
  return(list)
}

