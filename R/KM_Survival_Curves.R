#' Plot a Kaplan-Meier survival curve
#'
#' @description
#' Creates a Kaplan-Meier survival curve from time-to-event data, either overall
#' or stratified by a categorical variable. The plot includes an optional
#' confidence interval, a risk table, and, when stratified, a log-rank test
#' p-value annotation.
#'
#' @param data A data frame containing `Event`, `tEvent`, and, if used, `strata`.
#' @param Event Character string giving the name of the event indicator
#' variable. The variable should be coded as 0/1, where 1 indicates that the
#' event occurred and 0 indicates censoring. Default is "OS_EVENT".
#' @param tEvent Character string giving the name of the survival time variable.
#' Default is "OS".
#' @param strata Character string giving the name of the categorical
#' stratification variable. Use `strata = 1` for an overall Kaplan-Meier curve.
#' Default is 1.
#' @param title Character string giving the plot title. Default is "KM plot".
#' @param xlab Character string giving the x-axis label.
#' Default is "Time in months".
#' @param ylab Character string giving the y-axis label.
#' Default is "Probability of Survival".
#' @param atrisklab Character string giving the legend title and risk-table
#' label. Default is "At risk".
#' @param xlim Numeric vector of length 2 giving the x-axis limits.
#' Default is xlim = c(0, max(data[, tEvent], na.rm = TRUE)).
#' @param lwd_lines Numeric. Line width of the survival curves.
#' Default is 1.
#' @param alpha_CI Numeric. Transparency of the confidence interval. Use `0` to
#' hide the confidence interval. Default is 0.
#' @param size_pval Numeric. Text size of the log-rank p-value annotation.
#' Default is 4.
#' @param x_pval Numeric. X-axis coordinate for the p-value annotation.
#' Default is 5.
#' @param y_pval Numeric between 0 and 1. Y-axis coordinate for the p-value
#' annotation. Default is 0.5.
#' @param size_title Numeric. Plot title size. Default is 14.
#' @param size_title_x Numeric. X-axis title size. Default is 12.
#' @param size_title_y Numeric. Y-axis title size. Default is 12.
#' @param size_text_x Numeric. X-axis tick-label size. Default is 10.
#' @param size_text_y Numeric. Y-axis tick-label size. Default is 10.
#' @param size_legend_title Numeric. Legend-title size. Default is 5.
#' @param size_legend_text Numeric. Legend-text size. Default is 12.
#' @param at_risk_title_size Numeric. Risk-table title size. Default is 10.75.
#' @param at_risk_size Numeric. Risk-table text size.Default is 3.
#' @param breaks_by Numeric. Spacing between x-axis breaks and risk-table time
#' points. Default is 3.
#'
#' @return
#' A `ggsurvfit`/`ggplot` object containing the Kaplan-Meier curve and risk
#' table. The object can be printed directly or further modified with ggplot2
#' layers.
#'
#' @author Luca Lalli, Stefano Bergamini
#'
#' @examples
#' library(survival)
#'
#' data(cancer, package="survival")
#' lung$status01 <- as.integer(lung$status == 2)
#' lung$sex <- factor(lung$sex, labels = c("Male", "Female"))
#'
#' # Subset dataset for quick example
#' lung <- lung[1:100,]
#'
#' # Overall Kaplan-Meier curve
#' plot_km_curve(
#'   data = lung,
#'   Event = "status01",
#'   tEvent = "time",
#'   strata = 1,
#'   title = "Overall survival",
#'   breaks_by=30,
#'   xlab="Time",
#'   size_text_x=5
#' )
#'
#' # Stratified Kaplan-Meier curve
#' plot_km_curve(
#'   data = lung,
#'   Event = "status01",
#'   tEvent = "time",
#'   strata = "sex",
#'   title = "Overall survival by sex",
#'   breaks_by=30,
#'   xlab="Time",
#'   size_text_x=5
#' )
#'
#'
#' @export
plot_km_curve <- function(data,
                          Event = "OS_EVENT",
                          tEvent = "OS",
                          strata = 1,
                          title = "KM plot",
                          xlab = "Time in months",
                          ylab = "Probability of Survival",
                          atrisklab = "At risk",
                          xlim = c(0, max(data[, tEvent], na.rm = TRUE)),
                          lwd_lines = 1,
                          alpha_CI = 0,
                          size_pval = 4,
                          x_pval = 5,
                          y_pval = .5,
                          size_title = 14,
                          size_title_x = 12,
                          size_title_y = 12,
                          size_text_x = 10,
                          size_text_y = 10,
                          size_legend_title = 5,
                          size_legend_text = 12,
                          at_risk_title_size = 10.75,
                          at_risk_size = 3,
                          breaks_by = 3){

  frm <- formula(paste0("survival::Surv(", tEvent, ",", Event, ")~", strata))
  surv_fit = ggsurvfit::survfit2(frm, data = data)


  KM.fit <-
    ggsurvfit::ggsurvfit(surv_fit, size = lwd_lines) +
    ggsci::scale_color_jco() +
    ggsurvfit::add_confidence_interval(alpha = alpha_CI) +
    ggsurvfit::add_risktable_strata_symbol(size = 5) +
    ggsurvfit::add_risktable(risktable_stats = "n.risk", risktable_height = 0.15, size = at_risk_size,
                  theme = ggsurvfit::theme_risktable_default(plot.title.size = at_risk_title_size)) +

    {
      if (strata != 1)
        ggsurvfit::add_pvalue("annotation", size = size_pval, x = x_pval, y = y_pval, pvalue_fun = function(x) LandS::formatz_p(x))
    } +
    ggplot2::scale_x_continuous(breaks = seq(0, xlim[2], breaks_by), expand = ggplot2::expansion(mult = c(0.02))) +
    ggplot2::scale_y_continuous(labels = scales::percent_format(accuracy = 1), expand = ggplot2::expansion(mult = c(0, 0.01))) +
    ggplot2::coord_cartesian(xlim = xlim, ylim = c(0,1)) +
    ggplot2::labs(
      title = title,
      x = xlab,
      y = ylab,
      legend.title = atrisklab
    ) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(size = size_title, hjust = 0.5, face = "bold"),
      legend.position = "top",
      axis.title.x = ggplot2::element_text(size = size_title_x, colour = "black"),
      axis.title.y = ggplot2::element_text(size = size_title_y, colour = "black"),
      axis.text.x = ggplot2::element_text(size = size_text_x, colour = "black"),
      axis.text.y = ggplot2::element_text(size = size_text_y, colour = "black"),
      legend.text = ggplot2::element_text(size = size_legend_text, colour = "black"),
      legend.title = ggplot2::element_text(size = size_legend_title, colour = "black"),
      panel.background = ggplot2::element_rect(fill = "transparent"),
      panel.grid.major = ggplot2::element_line(linewidth = .1),
      panel.grid.minor = ggplot2::element_line(linewidth = 0)
    )
  return(KM.fit)
}


