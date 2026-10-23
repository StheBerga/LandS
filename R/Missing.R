#' Missing data matrix for multi-markers longitudinal studies
#'
#' Generates a missing data matrix by counting missing values across a
#' user-specified set of variables. The matrix can be created either at
#' the subject level or at the subject-timepoint level for longitudinal
#' datasets. Selected metadata variables can optionally be retained in
#' the output.
#'
#' @param data A data frame containing the dataset to be evaluated.
#' @param variables Character vector containing the names of the variables
#'   to be evaluated for missingness.
#' @param id Character string identifying the subject identifier variable.
#'   Defaults to `"ID"`.
#' @param time Optional character string identifying the timepoint variable.
#'   When supplied, the output is returned in wide format with one column per
#'  timepoint. Defaults to `NULL`.
#' @param metadata Optional character vector containing metadata variables
#'   to be preserved in the output dataset.
#' @param excel Logical value indicating whether the resulting matrix
#'   should also be exported as an Excel file. Defaults to `FALSE`.
#' @param excel_path Character string specifying the path of the Excel
#'   file to create when `excel = TRUE`.
#'
#' @details
#' For each observation, the function counts the number of missing values
#' across the variables specified in `variables`.
#'
#' When `time = NULL`, missing value counts are aggregated at the subject
#' level, producing a single missingness score for each subject.
#'
#' When a time variable is supplied, missing value counts are aggregated
#' within each unique combination of subject (`id`) and timepoint
#' (`time`) and reshaped into a wide matrix where columns represent
#' timepoints; in this scenario it's preferrable to set the `time` variable
#' as a factor, otherwise timepoint columns will be alphabetically sorted.
#'
#' Only subject-timepoint combinations present in the source dataset are
#' evaluated, therefore, values in the resulting matrix should be
#' interpreted as follows:
#' \itemize{
#'   \item `0`: the observation exists and no selected variables are missing.
#'   \item Integer values greater than `0`: the observation exists and one
#'     or more selected variables are missing.
#'   \item `NA`: the corresponding subject-timepoint combination is absent
#'     from the original dataset and could not be evaluated.
#' }
#'
#' When `metadata` variables are supplied, the first observed value
#' within each aggregation group is retained in the output.
#'
#' @return
#' A data frame containing missingness counts. If a time variable is
#' supplied, the output contains one row per subject and one column per
#' timepoint. Otherwise, a single missingness count is returned for each
#' subject.
#'
#' If `excel = TRUE`, the resulting data frame is additionally exported
#' as an Excel file, named according to the `excel_path` argument.
#'
#' @export
#'
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#'
#' @author Stefano Bergamini
#'
#' @examples
#' set.seed(123)
#' mydata <- data.frame(
#'   ID = rep(c('1', '2', '3'), times = 3),
#'   Time = rep(c('0', '1', '2'), each = 3),
#'   Gender = rep(c("M", "M", "F"), times = 3),
#'   A = rnorm(9),
#'   B = c(NA, rnorm(1), NA, NA, rnorm(5))
#' )
#'
#' # Subject-level missingness
#' missing_matrix(
#'   data = mydata,
#'   variables = c('A', 'B')
#' )
#'
#' # Longitudinal missingness matrix
#' missing_matrix(
#'   data = mydata,
#'   variables = c("A", "B"),
#'   time = "Time",
#'   metadata = c("Gender")
#' )

missing_matrix <- function(data,
                           variables,
                           id = "ID",
                           time = NULL,
                           metadata = NULL,
                           excel = FALSE,
                           excel_path = "MissingMatrix.xlsx") {

  # Checking for input validation

  if (!is.data.frame(data)) {
    stop("'data' must be a data.frame.")
  }

  missing_vars <- setdiff(variables, names(data))

  if (length(missing_vars) > 0) {
    stop(
      sprintf(
        "The following variables were not found in 'data': %s.",
        paste(missing_vars, collapse = ", ")
      ))
  }

  if (anyDuplicated(metadata)) {
    stop("'metadata' contains duplicated variable names.")
  }

  missing_metadata <- setdiff(metadata, names(data))

  if (length(missing_metadata) > 0) {
    stop(
      sprintf(
        "The following metadata variables were not found in 'data': %s.",
        paste(missing_metadata, collapse = ", ")))
  }

  # Time variable missing

  if (is.null(time)) {

    res <- data %>%
      dplyr::group_by(.data[[id]]) %>%
      dplyr::summarise(
        TotalMissing = sum(rowSums(is.na(dplyr::across(dplyr::all_of(variables))))),
        dplyr::across(dplyr::all_of(metadata), dplyr::first),
        .groups = "drop") %>%
      dplyr::relocate(.data[["TotalMissing"]], .after = dplyr::last_col())

  } else {

    res <- data %>%
      dplyr::group_by(dplyr::across(dplyr::all_of(c(id, time)))) %>%
      dplyr::summarise(
        TotalMissing = sum(rowSums(is.na(dplyr::across(dplyr::all_of(variables))))),
        dplyr::across(dplyr::all_of(metadata), dplyr::first),
        .groups = "drop") %>%
      tidyr::pivot_wider(
        names_from = dplyr::all_of(time),
        values_from = .data[["TotalMissing"]])
  }

  if (excel) {
    writexl::write_xlsx(res, path = excel_path)
  }

  return(res)
}
