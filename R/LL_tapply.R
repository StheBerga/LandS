#' Function for an easy application of the tapply 
#'
#' @param data dataframe
#' @param var_quant quantitative variable
#' @param var_cat categorial variable
#' @param digits digits to display
#'
#' @return
#' @export
#'
#' @examples
LL_Tapply_f <- function(data, var_quant, var_cat, digits = 2){

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

LL_list_tapply_f <- function(data, vars_numeriche, var_cat, digits = 2){
lista_tapply <- list()
k_tap = 1
for (i in vars_numeriche) {
  lista_tapply[[k_tap]] <- LL_Tapply_f(data, var_quant = i, var_cat = var_cat)
  names(lista_tapply)[k_tap] <- as.character(i)
  k_tap <- k_tap + 1
}
return(lista_tapply)
}



#prova <- LL_Tapply_f(ana01, var_quant = "NLR", var_cat = "Histology")  
#prova <- LL_list_tapply_f(ana01, vars_numeriche = class_df$name_col[class_df$class == "numeric"], var_cat = "Histology")


