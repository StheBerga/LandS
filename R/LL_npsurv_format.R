# Funzione che richiede in input solamente la npsurv(Surv(time, event) ~ cov_fattore, data)

#' Function to get a cute format of npsurv output.
#'
#' @param fit.npsurv A npsurv(Surv(time, event) ~ cov_factor, data) object
#'
#' @return A cute format of npsurv output
#' @export
#'
#' @examples
LL_Npsurv_format <- function(fit.npsurv){
  strata_len <- length(fit.npsurv$strata)
  if(strata_len == 0){
    res.surv <- as.data.frame(t(summary(fit.npsurv)$table))
    res.surv <- select(res.surv, c("records","events", "median", "0.95LCL","0.95UCL"))
    res.surv[["Var"]] <- "Overall"
    res.surv <- res.surv[,c("Var", "records","events", "median", "0.95LCL","0.95UCL")]
    rownames(res.surv) <- NULL
    colnames(res.surv)[colnames(res.surv) == "records"] <- "Num"
    res.surv[["median"]] <- round(res.surv[["median"]], 2)
    res.surv[["0.95LCL"]] <- round(res.surv[["0.95LCL"]], 2)
    res.surv[["0.95UCL"]] <- round(res.surv[["0.95UCL"]], 2)
    res.surv[["95%IC"]] <- paste0(res.surv[["0.95LCL"]], "-", res.surv[["0.95UCL"]])
    res.surv <- select(res.surv, -c("0.95LCL", "0.95UCL"))
    res.surv[["median"]] <- as.factor(res.surv[["median"]])
  }else{
    Var <- as.character(fit.npsurv$call$formula[[3]])
    res.surv <- as.data.frame(summary(fit.npsurv)$table)
    levs <- rownames(res.surv)
    levs <- gsub(".*=", "", levs)
    res.surv <- select(res.surv, c("records","events", "median", "0.95LCL","0.95UCL"))
    res.surv[[Var]] <- levs
    res.surv <- res.surv[, c(Var, "records","events", "median", "0.95LCL","0.95UCL")]
    rownames(res.surv) <- NULL
    colnames(res.surv)[colnames(res.surv) == "records"] <- "Num"
    res.surv[["median"]] <- round(res.surv[["median"]], 2)
    res.surv[["0.95LCL"]] <- round(res.surv[["0.95LCL"]], 2)
    res.surv[["0.95UCL"]] <- round(res.surv[["0.95UCL"]], 2)
    res.surv[["95%IC"]] <- paste0(res.surv[["0.95LCL"]], "-", res.surv[["0.95UCL"]])
    res.surv <- select(res.surv, -c("0.95LCL", "0.95UCL"))
    res.surv[["median"]] <- as.factor(res.surv[["median"]])
  }
  return(res.surv)
}












