#' Create longitudinal summary line plots
#'
#' @description
#' Creates one line plot for each selected numeric variable measured over time.
#' The function can display mean or median trajectories, optional interquartile
#' range ribbons, observed data points, and individual subject trajectories.
#'
#' Plots can be generated overall or stratified by a grouping variable. The
#' output is either a list of 'ggplot' objects or, if 'PPTX = TRUE', a
#' PowerPoint file.
#'
#' @param data A data frame containing the variables to plot.
#' @param variables Character vector with the names of numeric variables to plot.
#' @param time Character string giving the name of the time variable. This
#' variable should be numeric if `scale_x_continuous()` is used.
#' @param breaks Numeric vector of x-axis break positions. Default is the unique
#' values of `time`.
#' @param label Character or numeric vector of x-axis labels. Must have the same
#' length as `breaks`. Default is the unique values of `time`.
#' @param group Character string giving the name of the grouping variable used
#' to stratify lines, colours, and ribbons. Use `group = 1` for no grouping.
#' Default = 1.
#' @param col_lines Character vector of colours used for groups. Its length
#' should match the number of group levels when `group != 1`.
#' Default=c("salmon", "royalblue").
#'
#' @param stat_line Character string indicating the summary statistic to plot.
#' Allowed values are `"median"` and `"mean"`. Default="median".
#' @param smooth_line Logical. If `TRUE`, applies a LOESS-smoothed line to the
#' summary trajectory. Default is `FALSE`.
#' @param span_line Numeric value controlling the LOESS smoothing span. Default
#' is `0.3`.
#' @param lw_reg Numeric. Line width of the summary trajectory. Default is 1.
#' @param alpha_line Numeric. Transparency of the summary line. Default is 1.
#'
#' @param ylim Numeric vector of length 2 giving the lower and upper quantiles
#' used to determine the y-axis limits. Default is `c(0.2, 0.8)`.
#'
#' @param ribbon Logical. If `TRUE`, adds a ribbon representing the interquartile
#' range, from Q1 to Q3. Default is `TRUE`.
#' @param alpha_ribbon Numeric. Transparency of the ribbon. Default is 0.05.
#'
#' @param ID_lines Logical. If `TRUE`, adds individual subject trajectories.
#' Default is `FALSE`.
#' @param ID Character string giving the subject identifier column. Required
#' when `ID_lines = TRUE`, default is "ID".
#' @param alpha_ID_line Numeric. Transparency of individual subject lines.
#' Default is 0.3.
#' @param lw_ID_line Numeric. Line width of individual subject lines.
#' Default is 0.2.
#'
#' @param Point Logical. If `TRUE`, adds observed data points. Default is FALSE.
#' @param alpha_point Numeric. Transparency of observed data points.
#' Default is 0.3.
#' @param size_point Numeric. Size of observed data points. Default is 0.3.
#'
#' @param col_title Logical. If `TRUE`, fills plot-title boxes using colours
#' returned by `fill_title`. Default is FALSE.
#' @param fill_title Function returning a fill colour for each variable name.
#' Required when `col_title = TRUE`. Default is NULL.
#' @param size_title Numeric. Size of the plot title. Default is 7.
#' @param alpha_fill_title Numeric. Transparency of the title fill colour.
#' Default is 0.2.
#' @param label_title Character string used as the legend/title panel label when
#' multiple variables are plotted.
#' @param size_label_title Numeric. Text size of `label_title`. Default is 2.5.
#'
#' @param size_axis_x Numeric. Size of x-axis tick labels. Default is 5.
#' @param size_axis_y Numeric. Size of y-axis tick labels. Default is 6.
#'
#' @param Overall Logical. If `TRUE`, annotates each plot with an overall
#' p-value from `Test_results`. Default is FALSE.
#' @param Test_results Data frame containing statistical test results used for
#' annotations. Required when `Overall = TRUE` or `Posthoc = TRUE`.
#' Default is NULL.
#' @param Posthoc Logical. If `TRUE`, adds post-hoc comparison annotations.
#' Default is FALSE.
#' @param threshold_posthoc Numeric. P-value threshold used to filter post-hoc
#' comparisons. Default is 0.1.
#' @param posthoc_test_size Numeric. Text size for p-value annotations.
#' Default is 2.
#'
#' @param grid Logical. If `TRUE`, uses the default grid-oriented theme.
#' Default is TRUE.
#' @param ratio Numeric. Aspect ratio of each plot panel. Default is 1.
#' @param PPTX Logical. If `TRUE`, saves the plots to a PowerPoint file instead
#' of returning them. Default is FALSE.
#'
#' @param pptx_width Numeric. Width of the PowerPoint plot area. Default is 8.5.
#' @param pptx_height Numeric. Height of the PowerPoint plot area.
#' Default is 5.5.
#' @param target Character string giving the output path for the PowerPoint file.
#' Default is "Output/Lineplots.pptx".
#'
#' @param extra Logical. If `TRUE`, adds extra ggplot layers returned by
#' `extra_text`. Default is FALSE.
#' @param extra_text Function taking a variable name and returning one or more
#' ggplot layers. Default is NULL.
#' @param verbose Logical. If `TRUE`, prints progress messages. Default is TRUE.
#'
#' @return
#' If `PPTX = FALSE`, a list of `ggplot` objects. When more than one variable is
#' supplied, the first element is a legend/title panel and subsequent elements
#' are the plots for each variable.
#'
#' If `PPTX = TRUE`, the function writes a PowerPoint file to `target` and
#' returns `NULL` invisibly.
#'
#' @author Luca Lalli, Stefano Bergamini
#'
#' @examples
#' data_example <- data.frame(
#'   ID = rep(1:20, each = 4),
#'   Time = rep(0:3, times = 20),
#'   Group = rep(rep(c("A", "B"), each = 10), each = 4),
#'   marker1 = rnorm(80, mean = rep(0:3, times = 20)),
#'   marker2 = rnorm(80, mean = rep(0:3, times = 20) / 2)
#' )
#'
#' # One variable, overall median trajectory
#' plots <- Lineplots(
#'   data = data_example,
#'   variables = "marker1",
#'   time = "Time",
#'   group = 1
#' )
#'
#' plots[[1]]
#'
#' # Multiple variables stratified by group
#' plots_grouped <- Lineplots(
#'   data = data_example,
#'   variables = c("marker1", "marker2"),
#'   time = "Time",
#'   group = "Group",
#'   col_lines = c("salmon", "royalblue"),
#'   ID_lines = TRUE,
#'   Point = TRUE
#' )
#'
#' plots_grouped[[2]]
#'
#' # Save to PowerPoint
#' \dontrun{
#' dir.create("Output")
#' Lineplots(
#'   data = data_example,
#'   variables = c("marker1", "marker2"),
#'   time = "Time",
#'   group = "Group",
#'   PPTX = TRUE,
#'   target = "Output/Lineplots.pptx"
#' )
#' }
#'
#' @export
Lineplots <- function (data,
                       variables,
                       time,
                       breaks = unique(data[, time]),
                       label = unique(data[, time]),
                       group = 1,
                       col_lines = c("salmon", "royalblue"),

                       stat_line = "median",
                       smooth_line = FALSE,
                       span_line = 0.3,
                       lw_reg = 1,
                       alpha_line = 1,

                       ylim = c(0.2, 0.8),

                       ribbon = TRUE,
                       alpha_ribbon = 0.05,

                       ID_lines = FALSE,
                       ID = "ID",
                       alpha_ID_line = 0.3,
                       lw_ID_line = 0.2,

                       Point = FALSE,
                       alpha_point = 0.3,
                       size_point = 0.3,

                       col_title = FALSE,
                       fill_title = NULL,
                       size_title = 7,
                       alpha_fill_title = 0.2,
                       label_title = paste0("Lineplots by ", group, "\n", format(Sys.Date(), "%d/%m/%Y")),
                       size_label_title = 2.5,

                       size_axis_x = 5,
                       size_axis_y = 6,

                       Overall = FALSE,
                       Test_results = NULL,
                       Posthoc = FALSE,
                       threshold_posthoc = 0.1,
                       posthoc_test_size = 2,

                       grid = TRUE,
                       ratio = 1,
                       PPTX = FALSE,

                       pptx_width = 8.5,
                       pptx_height = 5.5,
                       target = "Output/Lineplots.pptx",

                       extra = FALSE,
                       extra_text = NULL,

                       verbose = TRUE)


{


  if (Posthoc == FALSE & Overall == FALSE) {
    Test_results = data.frame(matrix(NA))
  }

  if ((Overall || Posthoc) && is.null(Test_results)) {
    stop("`Test_results` must be supplied when `Overall = TRUE` or `Posthoc = TRUE`.")
  }

  if (!is.numeric(data[[time]])) {
    stop("`time` must be numeric because the function uses scale_x_continuous().")
  }

  if (verbose) message(paste0("Creation ", length(variables), " lineplots with: \n",
                 "-Split by ", group, "\n",
                 "-Break time ", paste(breaks, collapse = ", "), "\n",
                 "-Label time ", paste(label, collapse = ", "), "\n",
                 "-Stat Line: ", stat_line, "\n",
                 "-Smooth Line: ", smooth_line, "\n",
                 "-Ribbon: ", ribbon, "\n",
                 "-Points: ", Point, "\n"
  ))

  start_time <- Sys.time()

  # Define Themes ----
  themegrid <- ggplot2::theme(axis.text.x = ggplot2::element_text(size = size_axis_x, colour = "black", vjust = -0),
                     plot.margin = ggplot2::margin(2, 2, 2, 2, "mm"),
                     axis.text.y = ggplot2::element_text(size = size_axis_y, colour = "black"),
                     panel.border = ggplot2::element_rect(linetype = "solid", colour = "black", linewidth = 0.1, fill = NA),
                     axis.ticks = ggplot2::element_line(linewidth = 0.1),
                     axis.ticks.length = grid::unit(0.25, "mm"),
                     panel.background = ggplot2::element_rect(fill = "transparent"),
                     plot.background = ggplot2::element_rect(fill = "transparent"),
                     panel.grid.major = ggplot2::element_blank(), panel.grid.minor = ggplot2::element_blank(),
                     legend.title = ggplot2::element_blank(), panel.spacing.x = grid::unit(1.5, "mm"), aspect.ratio = ratio)

  themePPTX <- ggplot2::theme(axis.text.x = ggplot2::element_text(size = size_axis_x, colour = "black", vjust = -0),
                     plot.margin = ggplot2::margin(2, 2, 2, 2, "mm"),
                     axis.text.y = ggplot2::element_text(size = size_axis_y, colour = "black"),
                     panel.border = ggplot2::element_rect(linetype = "solid", colour = "black", linewidth = 0.1, fill = NA),
                     axis.ticks = ggplot2::element_line(linewidth = 0.1),
                     axis.ticks.length = grid::unit(0.25, "mm"),
                     panel.background = ggplot2::element_rect(fill = "transparent"),
                     plot.background = ggplot2::element_rect(fill = "transparent"),
                     panel.grid.major.y = ggplot2::element_blank(),
                     panel.grid.major.x = ggplot2::element_line(colour = "grey70", linewidth = 0.1, linetype = 2),
                     panel.grid.minor = ggplot2::element_blank(),
                     legend.key.width = grid::unit(2, "cm"),
                     panel.spacing.x = grid::unit(1.5, "mm"))
  list_reg <- list()


  # Creating a legend as the first graph ----
  if (length(variables) > 1) {
    list_reg[[1]] <- ggplot2::ggplot(data, ggplot2::aes_string(x = 1, y = 1)) +
      {
        if (group != 1)
          ggplot2::geom_point(ggplot2::aes_string(colour = group), shape = NA,
                     show.legend = TRUE)
      } + {
        if (group == 1)
          ggplot2::geom_point(shape = NA, show.legend = TRUE)
      } + {
        if (group != 1)
          ggplot2::scale_color_manual(values = col_lines)
      } + {
        if (group == 1)
          ggplot2::scale_color_manual(values = col_lines[1])
      } + {
        if (group != 1)
          ggplot2::scale_fill_manual(values = col_lines)
      } + {
        if (group == 1)
          ggplot2::scale_fill_manual(values = col_lines[1])
      } + ggpubr::theme_transparent() +

      ggplot2::annotate(geom = "text", x = 1, y = 1.001,
               size = size_label_title, label = label_title,
               vjust = 1.25, fontface = "bold") +
      ggplot2::guides(color = ggplot2::guide_legend(override.aes = list(size = 7.5, shape = 20))) +
      ggplot2::theme(legend.position = "inside", legend.justification = c(0.5, 0.3), legend.title = ggplot2::element_blank())
  } else { }

  # For cycle for all variables ----
  for (i in variables) {
    k <- which(variables == i)

    # Smooth line

    if (smooth_line == T) {
      if (stat_line == "median") {

        if (group == 1) {
          smooth_data <- aggregate(data[[i]], by = list(Time_num = data[, time]),
                                   FUN = function(x) c(median = median(x, na.rm = TRUE),
                                                       lower = quantile(x, 0.25, na.rm = TRUE),
                                                       upper = quantile(x, 0.75, na.rm = TRUE)))
          smooth_data <- do.call(data.frame, smooth_data)
          colnames(smooth_data) <- c("Time_num", "smooth_y", "Lower", "Upper")
        } else {
          smooth_data <- aggregate(data[[i]], by = list(Time_num = data[, time], Group = data[, group]),
                                   FUN = function(x) c(median = median(x, na.rm = TRUE),
                                                       lower = quantile(x, 0.25, na.rm = TRUE),
                                                       upper = quantile(x, 0.75, na.rm = TRUE)))
          smooth_data <- do.call(data.frame, smooth_data)
          colnames(smooth_data) <- c("Time_num", group, "smooth_y", "Lower", "Upper")
        }

      } else if (stat_line == "mean") {

        if (group == 1) {
          smooth_data <- aggregate(data[[i]], by = list(Time_num = data[, time]),
                                   FUN = function(x) c(mean = mean(x, na.rm = TRUE),
                                                       lower = quantile(x, 0.25, na.rm = TRUE),
                                                       upper = quantile(x, 0.75, na.rm = TRUE)))
          smooth_data <- do.call(data.frame, smooth_data)
          colnames(smooth_data) <- c("Time_num", "smooth_y", "Lower", "Upper")
        } else {
          smooth_data <- aggregate(data[[i]], by = list(Time_num = data[, time], Group = data[, group]),
                                   FUN = function(x) c(mean = mean(x, na.rm = TRUE),
                                                       lower = quantile(x, 0.25, na.rm = TRUE),
                                                       upper = quantile(x, 0.75, na.rm = TRUE)))
          smooth_data <- do.call(data.frame, smooth_data)
          colnames(smooth_data) <- c("Time_num", group, "smooth_y", "Lower", "Upper")
        }

      }
    }

    # If you want to add the brackets for the posthoc tests
    if (Posthoc == T) {
      posthoc_df <- Posthoc_lineplots(Test_results = Test_results,
                                      data = data, time = time, threshold_posthoc = threshold_posthoc, i)
    }

    Quantili <- data.frame(Tempo = unique(data[, time]),
                           Inferior = tapply(data[, i], data[, time], FUN = function(z) quantile(z, ylim[1], na.rm = T)),
                           Superior = tapply(data[, i], data[, time], FUN = function(z) quantile(z, ylim[2], na.rm = T)))

    # Define the graph
    gg <- ggplot2::ggplot(data = data, ggplot2::aes_string(x = time, y = data[, i])) +
      ggplot2::coord_cartesian(ylim = c(min(Quantili$Inferior), max(Quantili$Superior)))

    if (col_title) {
      if (group != 1) {

        colour_title <- fill_title(i)
        gg <- gg + ggplot2::aes_string(colour = group, fill = group) +
          ggplot2::scale_color_manual(values = col_lines, drop = F) +
          ggplot2::scale_fill_manual(values = col_lines, drop = F, guide = FALSE)

        if (ID_lines == TRUE) {

          gg <- gg + ggplot2::geom_line(ggplot2::aes_string(y = data[, i], group = ID), alpha = alpha_ID_line, linewidth = lw_ID_line)

        } else { }

      } else {
        colour_title <- fill_title(i)
        gg <- gg + ggplot2::aes(colour = "forestgreen", fill = "forestgreen") +
          ggplot2::scale_color_manual(values = col_lines[1], drop = F) +
          ggplot2::scale_fill_manual(values = col_lines[1], drop = F, guide = FALSE)

        if (ID_lines == TRUE) {
          gg <- gg + ggplot2::geom_line(ggplot2::aes_string(y = data[, i], group = ID), alpha = alpha_ID_line, linewidth = lw_ID_line, colour = "black")

        } else { }
      }
    } else {
      if (group != 1) {
        colour_title <- "transparent"
        gg <- gg + ggplot2::aes_string(colour = group, fill = group) +
          ggplot2::scale_color_manual(values = col_lines, drop = F) +
          ggplot2::scale_fill_manual(values = col_lines, drop = F, guide = FALSE)
        if (ID_lines == TRUE) {
          gg <- gg + ggplot2::geom_line(ggplot2::aes_string(y = data[, i], group = ID), alpha = alpha_ID_line, linewidth = lw_ID_line)
        } else { }

      } else {
        colour_title <- "transparent"
        gg <- gg + ggplot2::aes(colour = "forestgreen", fill = "forestgreen") +
          ggplot2::scale_color_manual(values = col_lines[1], drop = F) +
          ggplot2::scale_fill_manual(values = col_lines[1], drop = F,
                            guide = FALSE)
        if (ID_lines == TRUE) {
          gg <- gg + ggplot2::geom_line(ggplot2::aes_string(y = data[,
                                                   i], group = ID), alpha = alpha_ID_line, linewidth = lw_ID_line,
                               colour = "black")
        } else { }
      }
    }
    if (Point) {
      gg <- gg + ggplot2::geom_point(size = size_point, alpha = alpha_point)
    }
    if (stat_line == "median") {
      if (smooth_line) {
        gg <- gg + ggplot2::stat_smooth(data = smooth_data, ggplot2::aes(x = Time_num, y = smooth_y), geom = "line", alpha = alpha_line,
                               size = lw_reg, span = span_line, show.legend = F, method = "loess", formula = 'y ~ x')
      } else {
        gg <- gg + ggplot2::stat_summary(geom = "line", fun = median,
                                alpha = alpha_line, linewidth = lw_reg, show.legend = FALSE) +
          ggplot2::stat_summary(geom = "point", fun = median,
                       size = NA, show.legend = TRUE)
      }
    } else if (stat_line == "mean") {
      if (smooth_line) {
        gg <- gg + ggplot2::stat_smooth(data = smooth_data, ggplot2::aes(x = Time_num, y = smooth_y), geom = "line", alpha = alpha_line,
                               size = lw_reg, span = span_line, show.legend = F, method = "loess", formula = 'y ~ x')
      } else {
        gg <- gg + ggplot2::stat_summary(geom = "line", fun = mean,
                                alpha = alpha_line, linewidth = lw_reg, show.legend = FALSE) +
          ggplot2::stat_summary(geom = "point", fun = mean,
                       size = NA, show.legend = TRUE)
      }
    }
    # else if (stat_line == "both") {
    #    gg <- gg + stat_summary(geom = "line", fun = median,
    #                            alpha = alpha_line, linewidth = lw_reg, show.legend = TRUE) +
    #       stat_summary(geom = "line", fun = mean, alpha = alpha_line,
    #                    linewidth = lw_reg, linetype = 2, show.legend = TRUE)
    # }
    if (ribbon == TRUE) {
      if(smooth_line){
        if(group == 1){
          gg <- gg + ggplot2::stat_smooth(data = smooth_data, ggplot2::aes(x = Time_num, y = Upper), geom = "line", alpha = alpha_line,
                                 size = lw_reg, span = span_line, show.legend = F, colour = NA, method = "loess", formula = 'y ~ x')

          gg <- gg + ggplot2::stat_smooth(data = smooth_data, ggplot2::aes(x = Time_num, y = Lower), geom = "line", alpha = alpha_line,
                                 size = lw_reg, span = span_line, show.legend = F, colour = NA, method = "loess", formula = 'y ~ x')

          gg1 <- ggplot2::ggplot_build(gg)

          df2 <- data.frame(x = gg1$data[[1]]$x, ymin = gg1$data[[2]]$y, ymax = gg1$data[[3]]$y)

          gg <- gg + ggplot2::geom_ribbon(data = df2, ggplot2::aes(x = x, ymin = ymin, ymax = ymax, y = 0),
                                 fill = col_lines[1], alpha = alpha_ribbon, show.legend = F, linewidth = NA)
        } else if (group != 1) {

          gg <- gg + ggplot2::stat_smooth(data = smooth_data, ggplot2::aes(x = Time_num, y = Upper), geom = "line", alpha = alpha_line,
                                 size = lw_reg, span = span_line, show.legend = F, colour = NA, method = "loess", formula = 'y ~ x')

          gg <- gg + ggplot2::stat_smooth(data = smooth_data, ggplot2::aes(x = Time_num, y = Lower), geom = "line", alpha = alpha_line,
                                 size = lw_reg, span = span_line, show.legend = F, colour = NA, method = "loess", formula = 'y ~ x')

          gg1 <- ggplot2::ggplot_build(gg)

          df2 <- data.frame(x = gg1$data[[1]]$x, ymin = gg1$data[[2]]$y, ymax = gg1$data[[3]]$y, group = gg1$data[[1]]$group)
          df2$group <- factor(df2$group, levels = 1:nlevels(data[, group]), labels = levels(data[, group]))
          colnames(df2)[which(colnames(df2) == "group")] <- group

          gg <- gg + ggplot2::geom_ribbon(data = df2, ggplot2::aes(x = x, ymin = ymin, ymax = ymax, y = 0), alpha = 0.1, show.legend = F,
                                 linewidth = NA)

        }
      } else {
        gg <- gg + ggplot2::stat_summary(geom = "ribbon", fun.min = function(z) {
          quantile(z, 0.25)
        }, fun.max = function(z) {
          quantile(z, 0.75)
        }, linewidth = NA, alpha = alpha_ribbon, show.legend = F)
      }
    } else { }
    if (extra) {
      gg <- gg + extra_text(i)
    }
    gg <- gg + ggplot2::labs(title = i, x = NULL, y = NULL) +
      ggplot2::scale_x_continuous(breaks = breaks, labels = label, guide = ggplot2::guide_axis(check.overlap = TRUE))

    {
      if (Overall)
        gg <- gg + ggplot2::annotate(geom = "text", x = -Inf,
                            y = Inf, hjust = -0.1, vjust = 1.5, size = posthoc_test_size,
                            label = LandS::formatz_p(Test_results[Test_results[, 1] == i, 2]), colour = "black")
      }
    {
      if (Posthoc)
        if (nrow(posthoc_df) > 0 & !all(is.na(posthoc_df)))
          gg <- gg + ggpubr::stat_pvalue_manual(posthoc_df, label = "p = {pval}",
                                                y.position = max(tapply(data[, i], data[, time], median, na.rm = T)),
                                                step.increase = 0.08, size = posthoc_test_size)
    }
    if (grid == TRUE) {
      gg <- gg + themegrid + ggplot2::theme(plot.title = ggtext::element_textbox_simple(
        size = size_title, box.colour = "black", face = "bold",
        linewidth = .1, linetype = 1,
        hjust = 0, halign = .5,
        padding = ggplot2::margin(6, 5, 4, 5),
        margin = ggplot2::margin(0, 0, 0, 0),
        fill = scales::alpha(colour_title, alpha_fill_title)),
        legend.position = "none",
        legend.title = ggplot2::element_blank())
    }
    if (PPTX) {
      gg <- gg + themePPTX + ggplot2::theme(plot.title = ggtext::element_textbox_simple(
        size = size_title, box.colour = "black", face = "bold",
        linewidth = .1, linetype = 1,
        hjust = 0, halign = .5,
        padding = ggplot2::margin(6, 5, 4, 5),
        margin = ggplot2::margin(0, 0, 0, 0),
        fill = scales::alpha(colour_title, alpha_fill_title)),
        legend.position = "none",
        legend.title = ggplot2::element_blank(),
        legend.background = ggplot2::element_rect(fill = "transparent"))
    }
    if (length(variables) > 1) {
      list_reg[[k + 1]] <- gg
    }
    else {
      list_reg[[k]] <- gg
    }
    if (verbose) LandS::Progress_bar(current = which(i == variables),
                           total = length(variables),
                           start_time = start_time,
                           bar_fill = "\U2588",
                           bar_void = "\U2591")
  }
  if (PPTX == T) {
    ppt <- officer::read_pptx()
    if (verbose) message("Printing PowerPoint")
    for (i in 1:length(list_reg)) {
      list_reg[[i]] <- rvg::dml(ggobj = list_reg[[i]])
      ppt = officer::add_slide(ppt, layout = "Title and Content")
      officer::ph_with(ppt, list_reg[[i]], officer::ph_location(width = pptx_width,
                                              height = pptx_height))
    }
    print(ppt, target = target)
  }
  else {
    return(list_reg)
  }
}
