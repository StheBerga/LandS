#' This function allows to create a KM survival curve overall or splitted by a categorical variable
#'
#' @param Event Event variable
#' @param tEvent Survival Time Variable
#' @param strata Variable to stratify (Default = 1)
#' @param data dataframe
#' @param title Graph title (Default = "Prova")
#' @param xlab x-axis title (Default = "Time in months")
#' @param ylab y-axis title (Default = "Probaility of Surv")
#' @param xlim limits of x-axis (Default is from 0 to maximum observed time)
#' @param breaks_by breaks of risk table (Default = 3)
#' @param legendlab title of at risk table (Default = "At risk")
#' @param lwd_lines linewidth of survival curves
#' @param alpha_CI transparency of CI of survival curves. To not display CI set it to 0 (default)
#' @param size_pval size of log-rank-test p-value
#' @param x_pval x-axis coordinates of pvalue, same scale of tEvent
#' @param y_pval y-axis coordinates of pvalue, between 0 and 1
#' @param size_title size of title
#' @param size_title_x size of x-axis title
#' @param size_title_y size of y-axis title
#' @param size_text_x size of x-axis text
#' @param size_text_y size of y-axis text
#' @param size_legend_title size of legend title
#' @param size_legend_text size of legend text
#' @param at_risk_title_size size of at risk table title
#' @param at_risk_size size of at risk table text
#'
#' @return a KM graph
#' @export
#'
#' @examples
KM_LB <- function(Event = "OS_EVENT",
                  tEvent = "OS",
                  strata = 1,
                  data = data,
                  title = "Prova",
                  xlab = "Time in months",
                  ylab = "Probability of Survival",
                  atrisklab = "At risk",
                  xlim = c(0, max(data[, tEvent], na.rm = T)),
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

  require(survival)
  require(ggsurvfit)
  require(ggplot2)
  require(ggsci)

  frm <- formula(paste0("Surv(", tEvent, ",", Event, ")~", strata))
  surv_fit = survfit2(frm, data = data)


  KM.fit <-
    ggsurvfit(surv_fit, size = lwd_lines) +
    scale_color_jco() +
    add_confidence_interval(alpha = alpha_CI) +
    add_risktable_strata_symbol(size = 5) +
    add_risktable(risktable_stats = "n.risk", risktable_height = 0.15, size = at_risk_size,
                  theme = theme_risktable_default(plot.title.size = at_risk_title_size)) +

    {
      if (strata != 1)
        add_pvalue("annotation", size = size_pval, x = x_pval, y = y_pval, pvalue_fun = function(x) LandS::formatz_p(x))
    } +
    scale_x_continuous(breaks = seq(0, xlim[2], breaks_by), expand = expansion(mult = c(0.02))) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1), expand = expansion(mult = c(0, 0.01))) +
    coord_cartesian(xlim = xlim, ylim = c(0,1)) +
    labs(
      title = title,
      x = xlab,
      y = ylab,
      legend.title = atrisklab
    ) +
    theme(
      plot.title = element_text(size = size_title, hjust = 0.5, face = "bold"),
      legend.position = "top",
      axis.title.x = element_text(size = size_title_x, colour = "black"),
      axis.title.y = element_text(size = size_title_y, colour = "black"),
      axis.text.x = element_text(size = size_text_x, colour = "black"),
      axis.text.y = element_text(size = size_text_y, colour = "black"),
      legend.text = element_text(size = size_legend_text, colour = "black"),
      legend.title = element_text(size = size_legend_title, colour = "black"),
      panel.background = element_rect(fill = "transparent"),
      panel.grid.major = element_line(linewidth = .1),
      panel.grid.minor = element_line(linewidth = 0)
    )
  return(KM.fit)
}


