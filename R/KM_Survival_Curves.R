#' Plot Kaplan-Meier Survival Curves
#'
#' @description
#' Estimates and plots Kaplan-Meier survival curves from time-to-event data,
#' either for the overall population or stratified by a categorical variable.
#' The plot can include confidence intervals, censoring marks, the risk table,
#' survival quantile annotations, and log-rank test p-values.
#'
#' An optional complementary log-log plot can also be displayed alongside
#' the Kaplan-Meier curves to facilitate the graphical assessment of the
#' proportional hazards (PH) assumption.
#'
#' @param data A data frame containing the survival time, event, and, if used, the grouping variables.
#' @param time Character string naming the follow-up time variable.
#' @param event Character string naming the event indicator variable.
#' @param group Character string specifying the name of the categorical grouping variable, or \code{"Overall"} for an unstratified curve. The grouping variable must be a factor.
#@param covariate Optional covariate variable for stratification.
#' @param conf_int Logical; whether to display confidence intervals of the survival estimates (Default = \code{TRUE}).
#' @param alpha Numeric transparency for the confidence interval (Default = 0.30).
#' @param lwd Numeric line width for the survival curves (Default = 1).
#' @param risktable Logical; whether to include the risk table (Default = \code{TRUE}).
#' @param quantile Optional numeric value between 0 and 1 specifying the survival probability at which to draw a quantile annotation (e.g. `0.5` for the median survival probability).
#' @param pvalue Logical; whether to display the log-rank test p-value as a plot annotation. The test is only applicable when survival curves are stratified (Default = \code{FALSE}).
#' @param censoring Logical; whether to display censoring marks (Default = \code{TRUE}).
#' @param shape Numeric or character value specifying the plotting symbol used for censoring marks (Defaults = 3).
#' @param risktable_group Character string indicating the grouping variable for the risk tables. Default = \code{"auto"} and will select \code{"strata"} or \code{"risktable_stats"} based on context. \cr
#' \itemize{
#' \item{\code{"strata"} groups the risk tables per stratum when present.}
#' \item{\code{"risktable_stats"} groups the risk tables per \code{risktable_stats}.}
#' }
#' @param risktable_symbol Optional character string specifying the symbol used to represent strata in the risk table. This option is intended for use with \code{risktable_group = "risktable_stats"}.
#' @param theme Optional character string specifying the graphical theme (Default = \code{NULL}, applies the \code{ggsurvfit::theme_ggsurvfit_default()} theme).
#'   Available options are `"KMunicate"` and `"grey-scale"`.
#'   When `NULL` (default), the standard
#'   [ggsurvfit::theme_ggsurvfit_default()] theme is applied.
#'   `"KMunicate"` applies
#'   [ggsurvfit::theme_ggsurvfit_KMunicate()], whereas
#'   `"grey-scale"` uses grey colour and fill scales.
#' @param check_ph Logical; whether to display a complementary log-log survival plot alongside the Kaplan-Meier curve to facilitate the graphical assessment of the proportional hazards assumption (Default = \code{FALSE}).
#' @param rho A scalar parameter that controls the type of test, check function details for more.
#' @param pvalue_position Character string indicating where to place p-value. Must be one of \code{c("caption", "annotation")} (Default = "annotation").
#' @param conf_int_type Character string indicating the type of confidence interval to draw. Must be one of \code{c("ribbon", "lines")} (Default = "ribbon").
#' @details
#' Survival estimates are calculated using \code{ggsurvfit::survfit2()} and visualized using \code{ggsurvfit::ggsurvfit()}.
#'
#' The survival analysis is performed according to the specified grouping: when \code{group = "Overall"}`, a single Kaplan-Meier survival curve is estimated for the entire population. Otherwise, separate survival curves are estimated for each level of the specified grouping variable.
#'
#' When \code{pvalue = TRUE}, a log-rank test is performed to compare survival distributions between groups, and the resulting p-value is displayed as an annotation on the upper-right corner. With multiple groups, the p-value represents an overall comparison between the survival curves. \cr The \code{rho} argument controls the type of test performed and it's passed to the \code{survival::survdiff(rho=rho)} function: with \code{rho = 0} this is the log-rank or Mantel-Haenszel test, and with \code{rho = 1} it is equivalent to the Peto & Peto modification of the Gehan-Wilcoxon test.
#'
#' When \code{check_ph = TRUE}, a complementary log-log transformation of
#' the survival curves is plotted against logarithmic follow-up time:
#' \deqn{\log[-\log\{\hat{S}(t)\}]}{log(-log(S(t)))}
#' Approximately parallel curves across groups may support the
#' proportional hazards assumption. This graphical assessment does
#' not constitute a formal statistical test.
#'
#' @return
#' When \code{check_ph = FALSE}, returns a built survival plot generated
#' by \code{ggsurvfit::ggsurvfit_build()}, optionally including a risk table.
#'
#' When \code{check_ph = TRUE}, returns a two-column
#' \code{patchwork::wrap_plots()} object containing the Kaplan-Meier
#' plot on the left and the complementary log-log plot on the right.
#' @author Stefano Bergamini, Luca Lalli
#' @export
#' @examples
#' df_colon <- ggsurvfit::df_colon
#'
#' # Overall Kaplan-Meier curve
#' plot_km_curve(
#'   data = df_colon,
#'   time = "time",
#'   event = "status",
#'   group = "Overall"
#' )
#'
#' # Compare survival curves between surgical treatment groups
#' plot_km_curve(
#'   data = df_colon,
#'   time = "time",
#'   event = "status",
#'   group = "surg",
#'   pvalue = TRUE
#' )
#'
#' # Display median survival annotation and
#' # graphical assessment of the proportional hazards assumption
#' plot_km_curve(
#'   data = df_colon,
#'   time = "time",
#'   event = "status",
#'   group = "surg",
#'   quantile = 0.5,
#'   check_ph = TRUE
#' )

plot_km_curve <- function(data,
                          time,
                          event,
                          group,
                          # covariate = NULL,
                          conf_int = TRUE,
                          alpha = 0.30,
                          lwd = 1,
                          risktable = TRUE,
                          quantile = NULL,
                          pvalue = FALSE,
                          censoring = TRUE,
                          shape = 3,
                          risktable_group = "auto", # risktable_stats = "n.risk",
                          risktable_symbol = NULL,
                          theme = NULL,
                          check_ph = FALSE,
                          rho = 0,
                          pvalue_position = "annotation",
                          conf_int_type = "ribbon") {

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
    formula <- stats::formula(paste0("survival::Surv(", time, ",", event, ") ~ 1"))
  } else if (!group == "Overall") { # && is.null(covariate)) {
    formula <- stats::formula(paste0("survival::Surv(", time, ",", event, ") ~ ", group))
  } # else if (!group == "Overall" && !is.null(covariate)) {
  # formula <- stats::formula(paste0("survival::Surv(", time, ",", event, ") ~ ",
  #                                  paste(covariate, collapse = ' + '), " + strata(", group, ")"))
  # }
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
      if (risktable && !is.null(risktable_symbol))
        ggsurvfit::add_risktable_strata_symbol(symbol = risktable_symbol)
    } + {
      if (!is.null(quantile))
        ggsurvfit::add_quantile(y_value = quantile)
    } + {
      if (pvalue)
        ggsurvfit::add_pvalue(pvalue_fun = function(x) formatz_p(x), location = pvalue_position, rho = rho)
    } + {
      if (censoring)
        ggsurvfit::add_censor_mark(size = lwd, shape = shape)
    }

  q <- ggsurvfit::survfit2(formula, data = data) %>%
    ggsurvfit::ggsurvfit(linewidth = lwd, type = "cloglog") +
    ggplot2::scale_x_continuous(transform = "log")

  if (is.null(theme)) {
    p <- p + ggsurvfit::theme_ggsurvfit_default()
    q <- q + ggsurvfit::theme_ggsurvfit_default()
  } else if (!is.null(theme) && theme == "KMunicate") {
    p <- p + ggsurvfit::theme_ggsurvfit_KMunicate()
    q <- q + ggsurvfit::theme_ggsurvfit_KMunicate()
  } else if (!is.null(theme) && theme == "grey-scale") {
    p <- p + ggplot2::scale_color_grey() + ggplot2::scale_fill_grey()
    q <- q + ggplot2::scale_color_grey() + ggplot2::scale_fill_grey()
  }

  p <- ggsurvfit::ggsurvfit_build(p)

  if (check_ph) {
    q <- q + ggplot2::theme(legend.position = "none")
    q <- patchwork::wrap_plots(q, patchwork::plot_spacer(), heights = c(0.70, 0.30))
    return(patchwork::wrap_plots(p, q, ncol = 2))
  }
  return(p)
}