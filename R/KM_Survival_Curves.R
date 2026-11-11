#' Plot a Kaplan-Meier survival curve
#' @description
#' Creates a Kaplan-Meier survival curve from time-to-event data, either overall
#' or stratified by a categorical variable. The plot includes an optional
#' confidence interval, a risk table, and, when stratified, a log-rank test
#' p-value annotation.
#'
#' @param data A data frame containing the survival time, event, and, if used, the grouping variables.
#' @param time Character string naming the survival time variable.
#' @param event Character string naming the event indicator variable.
#' @param group Character string naming the grouping variable, or "Overall" for
#'   an unstratified curve. Grouping variables must be factors.
#' @param covariate Optional covariate variable for stratification.
#' @param conf_int Logical; show confidence intervals (Default = \code{TRUE}).
#' @param alpha Numeric transparency for the confidence interval (Default = 0.30).
#' @param lwd Numeric line width for the survival curve (Default = 1).
#' @param risktable Logical; include a risk table (Default = \code{TRUE}).
#' @param quantile Optional quantile between 0 and 1 to mark on the curve.
#' @param pvalue Logical; show the log-rank p-value (Default = \code{FALSE}).
#' @param censoring Logical; show censoring marks (Default = \code{TRUE}).
#' @param shape Shape used for censoring marks (Default = 3).
#' @param risktable_group Grouping mode passed to
#'   [ggsurvfit::add_risktable()].
#' @param risktable_symbol Optional symbol for stratified risk-table rows.
#' @param theme Optional theme name: "KMunicate" or "grey-scale".
#' @param check_ph Logical; return the survival curve beside a log-log plot to check the proportional hazards assumption (Default = \code{FALSE}).
#'
#' @return A built survival plot, or a two-panel plot when \code{check_ph = TRUE}.
#' @author Stefano Bergamini, Luca Lalli
#' @export
#' @examples
#' df_colon <- ggsurvfit::df_colon
#' # Overall curve
#' plot_km_curve(df_colon, "time", "status", group = "Overall")
#' 
#' # Compare two groups
#' plot_km_curve(df_colon, "time", "status", group = "surg", pvalue = TRUE)

plot_km_curve <- function(data, time, event, group, covariate = NULL, conf_int = TRUE, alpha = 0.30, lwd = 1, risktable = TRUE, quantile = NULL, pvalue = FALSE, censoring = TRUE, shape = 3, 
  risktable_group = "auto", # risktable_stats = "n.risk", 
                          risktable_symbol = NULL, theme = NULL, check_ph = FALSE) {
  
  # Validate inputs quantile and group
  if (!is.null(quantile) && length(quantile) > 1) {
    stop("Quantile must be exactly one value. Please provide a single numeric value for the quantile.")
  } else if (!is.null(quantile) && quantile < 0) {
    stop("Quantile must be a non-negative value. Please provide a valid numeric value for the quantile.")
  } else if (!is.null(quantile) && quantile > 1) {
    stop("Quantile must be less than or equal to 1. Please provide a valid numeric value for the quantile.")
  }

  if (!group == "Overall" && !is.factor(data[[group]])) {
    stop("The grouping variable must be a factor. Please convert the grouping variable to a factor before proceeding.")
  } else if (!group == "Overall" & pvalue && length(unique(data[[group]])) < 2) {
    stop("The grouping variable must have at least two levels. Please ensure that the grouping variable has at least two distinct levels before proceeding.")
  }

  if (group == "Overall"){
    formula <- stats::formula(paste0("survival::Surv(", time, ",", event, ")~1"))
  } else if (!group == "Overall" && is.null(covariate)) {
    formula <- stats::formula(paste0("survival::Surv(", time, ",", event, ")~", group))
  } else if (!group == "Overall" && !is.null(covariate)) {
    formula <- stats::formula(paste0("survival::Surv(", time, ",", event, ")~",
                                     vect.quoted(covariate, sym = ' + ', quote = FALSE), "strata(", group, ")"))
  }
  if (group == "Overall" & pvalue) {
    pvalue <- FALSE
    warning("No groups to test. `pvalue` set to FALSE")
  }

  p <-
    ggsurvfit::survfit2(formula, data = data) %>%
    ggsurvfit::ggsurvfit(linewidth = lwd) +
    ggsurvfit::scale_ggsurvfit() +
    {
      if (conf_int)
        ggsurvfit::add_confidence_interval(alpha = alpha)
    } + {
      if (risktable)
        ggsurvfit::add_risktable(risktable_group = risktable_group) # , risktable_stats = risktable_stats
    } + {
      if (!is.null(risktable_symbol))
        ggsurvfit::add_risktable_strata_symbol(symbool = risktable_symbol)
    } + {
      if (!is.null(quantile))
        ggsurvfit::add_quantile(y_value = quantile)
    } + {
      if (pvalue)
        ggsurvfit::add_pvalue(pvalue_fun = function(x) formatz_p(x), location = "annotation")
    } + {
      if (censoring)
        ggsurvfit::add_censor_mark(size = lwd, shape = shape)
    }

  if (is.null(theme)) {
    p <- p + ggsurvfit::theme_ggsurvfit_default()
  } else if (!is.null(theme) && theme == "KMunicate") {
    p <- p + ggsurvfit::theme_ggsurvfit_KMunicate()
  } else if (!is.null(theme) && theme == "grey-scale") {
    p <- p + ggplot2::scale_color_grey() + ggplot2::scale_fill_grey()
  }
  
  p <- ggsurvfit::ggsurvfit_build(p)

  if (check_ph) {
    q <- ggsurvfit::survfit2(formula, data = data) %>%
      ggsurvfit::ggsurvfit(linewidth = lwd, type = "cloglog") +
      ggplot2::scale_x_continuous(transform = "log")
    q <- ggsurvfit::ggsurvfit_build(q)
    return(cowplot::plot_grid(p, q, ncol = 2)) # , align = "h", axis = "tb"
  }
  return(p)
}