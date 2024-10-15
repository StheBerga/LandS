# Funzione per statistiche descrittive

#' Function to build, starting from a dataset, the descriptive statistics of every variable
#'
#' @param dataset dataframe
#' @param path where do you want it to be saved
#'
#' @return
#' @export
#'
#' @examples
LL_Descrittive <- function(dataset, path = NULL){
  
  formatz_p <- function(value){
    if(is.data.frame(value)==T){
      new_frame <- value
      for(i in 1:dim(value)[1]){
        if(value[i,1]> 0.0001){
          new_frame[i,1] <- format(round(value[i,1], 4), digits = 4, nsmall = 4, width = 6, scientific=F, justify = "centre")
        }else{
          new_frame[i,1] <- "<0.0001"
        }
      }
      return(new_frame)
    }else{
      if(value > 0.0001){
        value <- format(round(value, 4), digits = 4, nsmall = 4, width = 6, scientific=F, justify = "centre")
      }else{
        value <- " <0.0001"}
      return(value)
    }
  }
  
  dataset_t <- dataset
  n_var <- dim(dataset_t)[2]
  n_obs <- dim(dataset_t)[1]
  var_cat <- vector()
  var_quan <- vector()
  var_diverse <- vector()
  
  for(i in 1:n_var){
    test_factor <- is.factor(dataset_t[[i]])
    test_character <- is.character(dataset_t[[i]])
    test_quan <- is.numeric(dataset_t[[i]])
    test_logical <- is.logical(dataset_t[[i]])
    if(test_factor==TRUE|test_character==TRUE|test_logical==TRUE){
      dataset_t[,i] <- as.factor(dataset_t[[i]])
      var_cat[i]<-i}
    else if(test_quan==TRUE){
      var_quan[i]<-i}
    else{
      var_diverse[i]<-i}
  }
  
  if(length(var_cat)==0){
  }else{
    var_cat <- var_cat[is.na(var_cat)==FALSE]}
  
  if(length(var_quan)==0){
  }else{
    var_quan <- var_quan[is.na(var_quan)==FALSE]}
  
  if(length(var_diverse)==0){
  }else{
    var_diverse <- var_diverse[is.na(var_diverse)==FALSE]}
  
  if(length(path) > 0){
    sink(path)
  }else{
    
    cat("Numero osservazioni totali:", n_obs,"\n")
    cat("Numero variabili totali:   ", n_var, "\n")
    
    if (length(var_quan) == 0) {
      cat("\n", "VARIABILI QUANTITATIVE: 0")
      index_matrix <- NULL
    } else {
      cat("\n", "VARIABILI QUANTITATIVE:", length(var_quan), "\n", "\n")
      
      index_matrix <- as.data.frame(matrix(, nrow = length(var_quan), ncol = 9))
      colnames(index_matrix) <- c("Var", "Miss", "Min", "1Q", "Median", "Mean", "3Q", "Max", "Stdev")
      index_matrix$Var <- colnames(dataset_t)[var_quan]
      
      k <- 1
      for (i in var_quan) {
        index_matrix[k, "Miss"]    <- sum(is.na(dataset_t[, i]))
        index_matrix[k, "Min"]     <- round(min(dataset_t[, i], na.rm = TRUE), 3)
        index_matrix[k, "1Q"]      <- round(quantile(dataset_t[, i], 0.25, na.rm = TRUE), 3)
        index_matrix[k, "Median"]  <- round(median(dataset_t[, i], na.rm = TRUE), 3)
        index_matrix[k, "Mean"]    <- round(mean(dataset_t[, i], na.rm = TRUE), 3)
        index_matrix[k, "3Q"]      <- round(quantile(dataset_t[, i], 0.75, na.rm = TRUE), 3)
        index_matrix[k, "Max"]     <- round(max(dataset_t[, i], na.rm = TRUE), 3)
        index_matrix[k, "Stdev"]   <- round(sd(dataset_t[, i], na.rm = TRUE), 3)
        
        k <- k + 1
      }
      
      print(index_matrix)
    }
    
    
    cat("\n","\n","\n")
    
    if(length(var_cat)==0){
      cat("\n", "VARIABILI CATEGORICHE: 0")
      mat_level <- NULL
      var_cat_outp <- NULL
    }else{
      cat("\n", "VARIABILI CATEGORICHE:", length(var_cat), "\n", "\n")
      
      mat_level <- as.data.frame(matrix(, nrow = length(var_cat), ncol = 3))
      colnames(mat_level) <- c("Var", "Nlev", "Problem")
      mat_level$Var <- colnames(dataset_t)[var_cat]
      
      k <- 1
      for (i in var_cat) {
        mat_level[k ,2] <- nlevels(dataset_t[, i])
        if(nlevels(dataset_t[, i]) >  (n_obs/3)) {
          mat_level[k ,3] <- "Yes"
        }else{
          mat_level[k ,3] <- "No"
        }
        k <- k + 1
      }
      colnames(mat_level) <- c("Var", " Nlev", " Check")
      
      var_cat_outp <- list()
      k_cat <- 1
      for(k in (var_cat)){
        cat("\n", "-", colnames(dataset_t)[k],":", "\n") 
        nlev_cat <- nlevels(dataset_t[[k]])
        
        mat_cat  <- as.data.frame(matrix(, nrow = nlev_cat + 2, ncol = 3))
        l <- 0
        tab_count <- table(dataset_t[[k]])
        tab_perc  <- round(prop.table(table(dataset_t[[k]]))*100, 2)
        for(l in (1:nlev_cat)){
          mat_cat[l,2] <- tab_count[l] 
          mat_cat[l,3] <- tab_perc[l]
        }
        mat_cat[nlev_cat+1, 2] <- sum(tab_count)
        mat_cat[nlev_cat+1, 3] <- 100
        mat_cat[nlev_cat+2, 2] <- sum(is.na(dataset_t[[k]]))
        tot_miss <- sum(is.na(dataset_t[[k]]))
        tot <- sum(!is.na(dataset_t[[k]])) + tot_miss
        perc_miss <- round((tot_miss/tot)*100, 2)
        mat_cat[nlev_cat+2, 3] <- perc_miss
        colnames(mat_cat) <- c("Levels", "Freq", "%")
        mat_cat$Levels <- c(levels(dataset_t[[k]]), "TOTAL", "*Miss")
        var_cat_outp[[k_cat]] <- mat_cat
        names(var_cat_outp)[k_cat] <- colnames(dataset_t)[k]
        k_cat <- k_cat + 1
      }
      if(length(var_diverse)==0){
        cat("\n", "ALTRE VARIABILI: 0")}
      else{
        cat("\n", "ALTRE VARIABILI:", length(var_diverse), "\n", "\n")
      }
    }
    
    cat("\n","\n", "TABELLA DATI MANCANTI:","\n", "\n")
    
    mat_miss <- as.data.frame(matrix(, nrow = (length(var_quan)+length(var_cat)), ncol =  5))
    colnames(mat_miss) <- c("Var", "Type", "Miss", "% miss", "Completi")
    mat_miss$Var <- c(colnames(dataset_t)[var_quan], colnames(dataset_t)[var_cat])
    mat_miss$Type <- c(rep("Quant", length(var_quan)), rep("Cat", length(var_cat)))
    
    vett_tot <- c(var_quan, var_cat)
    
    k <- 1
    for (i in vett_tot) {
      mat_miss[k, 3] <- sum(is.na(dataset_t[,i]))
      mat_miss[k, 4] <- round( sum(is.na(dataset_t[,i])) / n_obs *100, 2)
      mat_miss[k, 5] <- n_obs - sum(is.na(dataset_t[,i]))
      k <- k+1
    }
    
    colnames(mat_miss) <- c("Var", " Type", "  Miss", "  % miss", " Completi")
    rm(dataset_t)
    list_output <- list(Quant = index_matrix, Cat_level = mat_level, Cat_out = var_cat_outp, Miss = mat_miss)
  }
  return(list_output)
}

