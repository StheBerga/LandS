#' Format rms::npsurv() output
#'
#' @description
#' This function formats the output of:
#'
#' rms::npsurv(Surv(time, event) ~ covariate_factor, data)
#'
#' The format includes number of events, median and 95\% confidence interval
#' for each group (upper-lower bound). If no stratification is present, the
#' values are reported overall.
#'
#' @param fit.npsurv An object of class "npsurv", specifically
#' obtained from the formula rms::npsurv(Surv(time, event) ~ covariate_factor, data).
#'
#' @return
#' A dataframe summarizing median survival estimates from an rms::npsurv() object.
#' The returned dataframe contains:
#' \describe{
#'   \item{Var or stratification variable}{Level of the grouping variable
#'   used in the survival model. If no grouping variable is present,
#'   the value is "Overall".}
#'   \item{Num}{Number of observations (records).}
#'   \item{events}{Number of events.}
#'   \item{median}{Median survival time.}
#'   \item{95\%CI}{95\% confidence interval for the median survival,
#'   formatted as "lower-upper".}
#' }
#'
#' Median survival and confidence interval bounds are rounded to two decimal places.
#' @export
#'
#' @author Luca Lalli
#'
#' @examples
#' if (requireNamespace("rms", quietly = TRUE) &&
#'     requireNamespace("survival", quietly = TRUE)) {
#'
#' # Fit Kaplan-Meier / nonparametric survival estimate
#'   fit <- rms::npsurv(
#'     survival::Surv(time, status) ~ sex,
#'     data = survival::lung
#'   )
#'
#' # Format output
#'   Npsurv_format(fit)
#' }
Npsurv_format <- function(fit.npsurv){
  strata_len <- length(fit.npsurv$strata)
  if(strata_len == 0){
    res.surv <- as.data.frame(t(summary(fit.npsurv)$table))
    res.surv <- dplyr::select(res.surv, c("records","events", "median", "0.95LCL","0.95UCL"))
    res.surv[["Var"]] <- "Overall"
    res.surv <- res.surv[,c("Var", "records","events", "median", "0.95LCL","0.95UCL")]
    rownames(res.surv) <- NULL
    colnames(res.surv)[colnames(res.surv) == "records"] <- "Num"
    res.surv[["median"]] <- round(res.surv[["median"]], 2)
    res.surv[["0.95LCL"]] <- round(res.surv[["0.95LCL"]], 2)
    res.surv[["0.95UCL"]] <- round(res.surv[["0.95UCL"]], 2)
    res.surv[["95%CI"]] <- paste0(res.surv[["0.95LCL"]], "-", res.surv[["0.95UCL"]])
    res.surv <- dplyr::select(res.surv, -c("0.95LCL", "0.95UCL"))
    res.surv[["median"]] <- as.factor(res.surv[["median"]])
  }else{
    Var <- as.character(fit.npsurv$call$formula[[3]])
    res.surv <- as.data.frame(summary(fit.npsurv)$table)
    levs <- rownames(res.surv)
    levs <- gsub(".*=", "", levs)
    res.surv <- dplyr::select(res.surv, c("records","events", "median", "0.95LCL","0.95UCL"))
    res.surv[[Var]] <- levs
    res.surv <- res.surv[, c(Var, "records","events", "median", "0.95LCL","0.95UCL")]
    rownames(res.surv) <- NULL
    colnames(res.surv)[colnames(res.surv) == "records"] <- "Num"
    res.surv[["median"]] <- round(res.surv[["median"]], 2)
    res.surv[["0.95LCL"]] <- round(res.surv[["0.95LCL"]], 2)
    res.surv[["0.95UCL"]] <- round(res.surv[["0.95UCL"]], 2)
    res.surv[["95%CI"]] <- paste0(res.surv[["0.95LCL"]], "-", res.surv[["0.95UCL"]])
    res.surv <- dplyr::select(res.surv, -c("0.95LCL", "0.95UCL"))
    res.surv[["median"]] <- as.factor(res.surv[["median"]])
  }
  return(res.surv)
}

