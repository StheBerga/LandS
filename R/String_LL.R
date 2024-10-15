#'Funzione che riceve in input le posizioni dei nomi di un dataframe e crea una stringa di tali nomi separati da virgola o da altro segno/simbolo
#'
#' @param data datafrane
#' @param vet vector of positions for names in the dataset
#' @param sep symbol to separate names from each other
#'
#' @return
#' @export
#'
#' @examples
Stringa_LL <- function(data, vet, sep= ","){
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