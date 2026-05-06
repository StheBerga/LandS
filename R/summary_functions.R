#' Summarise a numeric variable by group
#'
#' @description
#' Computes descriptive statistics for a numeric variable within each level of
#' a categorical grouping variable.
#'
#' The returned table includes minimum, first quartile, median, mean, third
#' quartile, maximum, standard deviation, and number of missing values.
#'
#' @param data A dataframe containing the variables to summarise.
#' @param var_quant Character string giving the name of the numeric variable
#' to summarise.
#' @param var_cat Character string giving the name of the categorical grouping
#' variable.
#' @param digits Integer. Number of decimal places used to round the summary
#' statistics. Default is 2.
#'
#' @return
#' A dataframe with one row for each level of 'var_cat' and the following
#' columns:
#' \describe{
#'   \item{var_cat}{Group level. The column name is the value supplied to
#'   'var_cat'.}
#'   \item{Min}{Minimum value.}
#'   \item{Q1}{First quartile.}
#'   \item{Median}{Median value.}
#'   \item{Mean}{Mean value.}
#'   \item{Q3}{Third quartile.}
#'   \item{Max}{Maximum value.}
#'   \item{Stdev}{Standard deviation.}
#'   \item{Miss}{Number of missing values.}
#' }
#'
#' @author Luca Lalli
#'
#' @examples
#' data_example <- data.frame(
#'   group = rep(c("A", "B"), each = 5),
#'   value = c(1, 2, 3, NA, 5, 10, 12, 13, 15, NA)
#' )
#'
#' summary_by_group(
#'   data = data_example,
#'   var_quant = "value",
#'   var_cat = "group"
#' )
#'
#' @export
summary_by_group <- function(data, var_quant, var_cat, digits = 2){

  if (!is.numeric(data[, var_quant])) {stop("The var_quant isn't numeric")}

  num_levels <- nlevels(as.factor(data[, var_cat]))
  df <- as.data.frame(matrix(NA, ncol = 9, nrow = num_levels))
  colnames(df) <- c(var_cat, "Min", "Q1", "Median", "Mean", "Q3", "Max", "Stdev", "Miss")
  df[, 1] <- levels(as.factor(data[, var_cat]))

  tab_t <- tapply(data[, var_quant], data[, var_cat], summary, na.rm=T)
  tab_miss <- tapply(is.na(data[, var_quant]), data[, var_cat], sum)
  tab_std <- tapply(data[, var_quant], data[, var_cat], sd, na.rm = T)

  for (i in df[, 1]) {
    df$Min[df[, 1] == i] <- round(tab_t[[i]]["Min."], digits = digits)
    df$Q1[df[, 1] == i] <- round(tab_t[[i]]["1st Qu."], digits = digits)
    df$Median[df[, 1] == i] <- round(tab_t[[i]]["Median"], digits = digits)
    df$Mean[df[, 1] == i] <- round(tab_t[[i]]["Mean"], digits = digits)
    df$Q3[df[, 1] == i] <- round(tab_t[[i]]["3rd Qu."], digits = digits)
    df$Max[df[, 1] == i] <- round(tab_t[[i]]["Max."], digits = digits)
    df$Miss[df[, 1] == i] <- round(tab_miss[[i]], digits = digits)
    df$Stdev[df[, 1] == i] <- round(tab_std[[i]], digits = digits)
  }
  return(df)
}


#' Summarise multiple numeric variables by group
#'
#' @description
#' Applies `summary_by_group()` to multiple numeric variables and
#' returns a named list of summary tables.
#'
#' @param data A data frame containing the variables to summarise.
#' @param numeric_vars Character vector giving the names of numeric variables
#' to summarise.
#' @param var_cat Character string giving the name of the categorical grouping
#' variable.
#' @param digits Integer. Number of decimal places used to round the summary
#' statistics. Default is 2.
#'
#' @return
#' A named list of data frames. Each element corresponds to one variable in
#' numeric_vars; element names are the variable names.
#'
#' @author Luca Lalli
#'
#' @examples
#' data_example <- data.frame(
#'   group = rep(c("A", "B"), each = 5),
#'   value1 = c(1, 2, 3, NA, 5, 10, 12, 13, 15, NA),
#'   value2 = c(5, 4, 3, 2, 1, 20, 22, NA, 25, 26)
#' )
#'
#' summarise_numeric_vars_by_group(
#'   data = data_example,
#'   numeric_vars = c("value1", "value2"),
#'   var_cat = "group"
#' )
#'
#' @export
summarise_numeric_vars_by_group <- function(data, numeric_vars, var_cat, digits = 2){

  lista_tapply <- list()
  k_tap = 1
  for (i in numeric_vars) {
    lista_tapply[[k_tap]] <- summary_by_group(data, var_quant = i, var_cat = var_cat)
    names(lista_tapply)[k_tap] <- as.character(i)
    k_tap <- k_tap + 1
  }

return(lista_tapply)
}

