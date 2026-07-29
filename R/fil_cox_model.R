#' Run Cox proportional hazards models
#'
#' @description
#' Depending on the value of argument "type", this function fits a univariate
#' or multivariate Cox proportional hazards model for a set of
#' candidate predictors.
#'
#' @param data A dataframe containing the survival outcome and predictor variables.
#' @param vars Character vector of predictor variable names to test.
#' @param ptime Character string giving the survival time variable name.
#' @param pevent Character string giving the event indicator variable name.
#' Typically coded as 0/1, where 1 indicates the event occurred.
#' @param type Character string indicating the type of model to run. Options are
#' "univariate" (or "u") and "multivariate" (or "m").
#' @param dec_HR Integer. Number of decimal places used to format hazard ratios
#' and confidence limits in the multivariate model. Default is 4.
#'
#' @return Univariate model returns a dataframe with one or more rows per variable:
#' \describe{
#'   \item{Var}{Variable name or factor level.}
#'   \item{HR}{Hazard ratio, or "ref" for the reference level.}
#'   \item{95\%CI}{Formatted 95\% confidence interval.}
#'   \item{pvalue}{Formatted p-value.}
#'   \item{pval_raw}{Raw numeric p-value where available.}
#' }
#'
#' Multivariate model returns a dataframe with formatted model results.
#' The table contains:
#' \describe{
#'   \item{Var}{Variable name or factor level.}
#'   \item{HR}{Hazard ratio, or "ref" for the reference category.}
#'   \item{Lower}{Lower bound of the 95\% confidence interval.}
#'   \item{Upper}{Upper bound of the 95\% confidence interval.}
#'   \item{Pvalue}{Formatted p-value.}
#' }
#'
#' @author Luca Lalli, Jessica Gliozzo
#'
#' @seealso See also documentation of \code{\link{univariate_cox}} and
#' \code{\link{multivariate_cox}} for details.
#'
#' @examples
#' library(dplyr)
#' library(rms)
#' library(survival)
#'
#' data(cancer, package = "survival")
#'
#' bladder_first <- subset(bladder, enum == 1)
#' bladder_first$rx <- factor(
#'   bladder_first$rx,
#'   levels = c(1, 2),
#'   labels = c("placebo", "thiotepa")
#' )
#'
#' # Univariate Cox model using a continuous variable
#' fit_cox_model(data=bladder_first, vars=c("number", "size"),
#'   ptime="stop", pevent="event", type="u")
#'
#' # Univariate Cox model using a categorical variable
#' fit_cox_model(data=bladder_first, vars=c("rx"),
#'   ptime="stop", pevent="event", type="u")
#'
#' # Multivariate Cox model with one categorical and two continuous predictors
#' fit_cox_model(data=bladder_first, vars=c("rx", "number", "size"),
#'   ptime="stop", pevent="event", type="m")
#'
#' @export
fit_cox_model <- function(data, vars, ptime, pevent, type, dec_HR=4){

  type = tolower(type)

  if(type=="u" || type=="univariate"){
    message("Fitting univariate model.")
    res <- univariate_cox(data=data, vars=vars, ptime=ptime, pevent=pevent)

  } else if(type=="m" || type=="multivariate"){
    message("Fitting multivariate model.")
    res <- multivariate_cox(data=data, vars=vars, ptime=ptime, pevent=pevent,
                            dec_HR = dec_HR)
  }

  return(res)
}




