#' Collapse selected column names into a single string
#'
#' @description Extracts column names from a data frame based on their positional indices
#' and concatenates them into a single string separated by a specified delimiter.
#'
#' @param data A dataframe.
#' @param vet Integer vector. Positions of the columns whose names should be
#' extracted from 'data'.
#' @param sep Character string used to separate column names (default ",").
#'
#' @return A character scalar containing the selected column names collapsed
#' into a single string.
#' @export
#'
#' @author Luca Lalli
#'
#' @examples
#' # Select column names by position
#' conv_to_string(mtcars, c(1, 3, 5))
#'
#' # Use a custom separator
#' conv_to_string(mtcars, c(1, 3, 5), sep = " | ")
conv_to_string <- function(data, vet, sep= ","){
  p <- list()
  k <- 1
  for (i in vet) {
    p[k] <- colnames(data)[i]
    k <- k+1
  }
  p <- unlist(p)
  p <- toString(p)
  if (sep!= ",") {
    p <- gsub(",", sep, p)
  }else {}
  return(p)
  }
