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
#' @param variables Character vector containing all numeric variables of interest.
#' @param time Character string giving the name of the time variable. This
#' variable should be numeric if \code{scale_x_continuous()} is used.
#' @param breaks Numeric vector of x-axis break positions (Default = levels(data[[time]]))
#' @param label Character or numeric vector of x-axis labels. Must have the same
#' length as \code{breaks} (Default = \code{levels(data[[time]])})
#' @param group Character scalar identifying the grouping variable.
#' Use \code{"Overall"} to generate a single distribution without stratification.
#' @param palette Character vector of colours used for groups. Its length
#' must match the number of group levels when \code{group != "Overall"} (Default = \code{"black"}).
#'
#' @param stat_line Character string indicating the summary statistic to plot.
#' Allowed values are \code{"median"} and \code{"mean"} (Default = \code{"median"}.
#' @param smooth_line Logical. If \code{TRUE}, applies a LOESS-smoothed line to the
#' summary trajectory (Default = \code{FALSE}).
#' @param span_loess Controls the amount of smoothing for the default loess smoother.
#' Smaller numbers produce wigglier lines, larger numbers produce smoother lines (Default = 1).
#' @param lwd_line Numeric. Line width of the summary trajectory (Default = 1).
#' @param alpha_line Numeric. Transparency of the summary line (Default = 1).
#'
#' @param ylim Numeric vector of length 2 giving the lower and upper quantiles
#' used to determine the y-axis limits (Default = \code{c(0, 1)}).
#'
#' @param ribbon Logical. If \code{TRUE}, adds a ribbon representing the interquartile
#' range, from Q1 to Q3 (Default = \code{TRUE}).
#' @param alpha_ribbon Numeric. Transparency of the ribbon (Default = 0.05).
#'
#' @param id_lines Logical. If \code{TRUE}, adds individual subject trajectories (Default = \code{FALSE}).
#' @param id Column name of id variable used to connect paired observations (Default = \code{"ID"}).
#' @param alpha_id_lines Numeric. Transparency of individual subject lines (Default = 0.3).
#' @param lwd_id_lines Numeric. Linewidth of individual subject lines (Default = 0.2).
#'
#' @param Point Logical. If \code{TRUE}, adds observed data points (Default = \code{FALSE}).
#' @param alpha_point Numeric. Transparency of observed data points (Default = 0.3).
#' @param size_point Numeric. Size of observed data points (Default = 0.3).
#'
#' @param col_title Logical. If \code{TRUE}, fills plot-title boxes using colours
#' returned by \code{fill_title} (Default = \code{FALSE}).
#' @param fill_title Function returning a fill colour for each variable name.
#' Required when \code{col_title = TRUE} (Default = \code{NULL}).
#' @param size_title Numeric. Size of the plot title (Default = 7).
#' @param alpha_fill_title Numeric. Transparency of the title fill colour (Default = 0.2).
#' @param label_legend_title A title for your list.
#' Default sets "Lineplots by grouping variable" and the current date.
#' @param size_label_title Numeric. Text size of `label_title` (Default = 2.5).
#'
#' @param size_axis_x Numeric. Size of x-axis tick labels (Default = 5).
#' @param size_axis_y Numeric. Size of y-axis tick labels (Default = 6).
#'
#' @param Overall Logical. If \code{TRUE}, annotates each plot with an overall
#' p-value from \code{Test_results} (Default = \code{FALSE}.
#' @param Test_results Data frame containing statistical test results used for
#' annotations. Required when \code{Overall = TRUE} or \code{Posthoc = TRUE} (Default = \code{NULL}).
#' @param Posthoc Logical. If \code{TRUE}, adds post-hoc comparison annotations (Default = \code{FALSE}).
#' @param threshold_posthoc Numeric. P-value threshold used to filter post-hoc
#' comparisons (Default = 0.1).
#' @param posthoc_test_size Numeric. Text size for p-value annotations (Default = 2).
#'
#' @param grid Logical. If \code{TRUE}, uses the default grid-oriented theme (Default = \code{TRUE}).
#' @param ratio Numeric. Aspect ratio of each plot panel (Default = 1).
#' @param pptx Logical. If \code{TRUE}, saves the plots to a PowerPoint file instead
#' of returning them (Default = \code{FALSE}).
#'
#' @param pptx_width Numeric. Width of the PowerPoint plot area (Default = 8.5).
#' @param pptx_height Numeric. Height of the PowerPoint plot area (Default = 5.5).
#' @param target Character string giving the output path for the PowerPoint file (Default = \code{"Lineplots.pptx"}).
#'
#' @param extra Logical. If \code{TRUE}, adds extra ggplot layers returned by
#' \code{extra_text} (Default = \code{FALSE}).
#' @param extra_text Function taking a variable name and returning one or more
#' ggplot layers (Default = \code{NULL}).
#' @param verbose Logical. If \code{TRUE}, prints progress messages (Default = \code{TRUE}).
#' @param overall_p Character identifying the p-value column of
#' \code{Test_results} to display as overall test (Default = \code{"Overall.padj"}).
#'
#'
#' @return
#' If \code{PPTX = FALSE}, a list of \code{ggplot} objects. When more than one variable is
#' supplied, the first element = a legend/title panel and subsequent elements
#' are the plots for each variable.
#'
#' If \code{PPTX = TRUE}, the function writes a PowerPoint file to \code{target} and
#' returns \code{NULL} invisibly.
#'
#' @author Stefano Bergamini, Luca Lalli
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
#' Lineplots(
#'   data = data_example,
#'   variables = "marker1",
#'   time = "Time",
#'   group = "Overall",
#'   verbose = FALSE
#' )
#'
#' # Multiple variables stratified by group
#' plots_grouped <- Lineplots(
#'   data = data_example,
#'   variables = c("marker1", "marker2"),
#'   time = "Time",
#'   group = "Group",
#'   palette = c("salmon", "royalblue"),
#'   id_lines = TRUE,
#'   Point = TRUE,
#'   verbose = FALSE
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
#'   verbose = FALSE,
#'   PPTX = TRUE,
#'   target = "Output/Lineplots.pptx"
#' )
#' }
#'
#' @export
#' @importFrom rlang .data
Lineplots <- function (data,
                       variables,
                       time,
                       breaks = unique(data[[time]]),
                       label = unique(data[[time]]),
                       group,
                       palette = "black",

                       stat_line = "median",
                       smooth_line = FALSE,
                       span_loess = 1,
                       lwd_line = 1,
                       alpha_line = 1,

                       ylim = c(0.2, 0.8),

                       ribbon = TRUE,
                       alpha_ribbon = 0.05,

                       id_lines = FALSE,
                       id = "ID",
                       alpha_id_lines = 0.3,
                       lwd_id_lines = 0.2,

                       Point = FALSE,
                       alpha_point = 0.3,
                       size_point = 0.3,

                       col_title = FALSE,
                       fill_title = NULL,
                       size_title = 7,
                       alpha_fill_title = 0.2,
                       label_legend_title = paste0("Lineplots by ", group, "\n", format(Sys.Date(), "%d/%m/%Y")),
                       size_label_title = 2.5,

                       size_axis_x = 5,
                       size_axis_y = 6,

                       Overall = FALSE,
                       Test_results = NULL,
                       Posthoc = FALSE,
                       threshold_posthoc = 0.1,
                       posthoc_test_size = 2,
                       overall_p = "Overall.padj",

                       grid = TRUE,
                       ratio = 1,
                       pptx = FALSE,

                       pptx_width = 8.5,
                       pptx_height = 5.5,
                       target = "Lineplots.pptx",

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
        if (!group == "Overall")
          ggplot2::geom_point(ggplot2::aes_string(colour = group), shape = NA,
                              show.legend = TRUE)
      } + {
        if (group == "Overall")
          ggplot2::geom_point(shape = NA, show.legend = TRUE)
      } + {
        if (!group == "Overall")
          ggplot2::scale_color_manual(values = palette)
      } + {
        if (group == "Overall")
          ggplot2::scale_color_manual(values = palette[1])
      } + {
        if (!group == "Overall")
          ggplot2::scale_fill_manual(values = palette)
      } + {
        if (group == "Overall")
          ggplot2::scale_fill_manual(values = palette[1])
      } + ggpubr::theme_transparent() +

      ggplot2::annotate(geom = "text", x = 1, y = 1.001,
                        size = size_label_title, label = label_legend_title,
                        vjust = 1.25, fontface = "bold") +
      ggplot2::guides(color = ggplot2::guide_legend(override.aes = list(size = 7.5, shape = 20))) +
      ggplot2::theme(legend.position = "inside", legend.justification = c(0.5, 0.3), legend.title = ggplot2::element_blank())
  } else { }

  for (i in variables) {
    if (col_title == TRUE) {
      fill.title <- fill_title(i)
    } else {
      fill.title <- "transparent"
    }
    k <- which(variables == i)
    if (Posthoc) {
      posthoc_df <- posthoc_df(Test_results = Test_results, data = data, group = time, threshold_posthoc = threshold_posthoc, i = i)
    }

    if (smooth_line) {
      grouping <- time

      if (!group == "Overall") {
        grouping <- c(time, group)
      }
      # Define smooth data
      smooth_data <- data %>%
        dplyr::group_by(dplyr::across(dplyr::all_of(grouping))) %>%
        dplyr::summarise(
          smooth_y =
            if (stat_line == "median") {
              stats::median(.data[[i]], na.rm = TRUE)
            } else {
              mean(.data[[i]], na.rm = TRUE)
            },
          Lower = stats::quantile(.data[[i]], 0.25, na.rm = TRUE),
          Upper = stats::quantile(.data[[i]], 0.75, na.rm = TRUE),
          .groups = "drop")
    }

    Quantiles <- data.frame(Times = unique(data[[time]]),
                            Inferior = tapply(data[[i]], data[[time]], FUN = function(z) stats::quantile(z, ylim[1], na.rm = TRUE)),
                            Superior = tapply(data[[i]], data[[time]], FUN = function(z) stats::quantile(z, ylim[2], na.rm = TRUE)))
    # Define ribbon data
    if (ribbon && smooth_line && group == "Overall") {
      loess_upper <- stats::loess(stats::as.formula(paste("Upper ~", time)), data = smooth_data, span = span_loess)
      loess_lower <- stats::loess(stats::as.formula(paste("Lower ~", time)), data = smooth_data, span = span_loess)
      xseq <- seq(min(smooth_data[[time]]), max(smooth_data[[time]]), length.out = 50)
      ribbon_data <- data.frame(x = xseq,
                                ymin = stats::predict(loess_lower, xseq),
                                ymax = stats::predict(loess_upper, xseq))
    } else if (ribbon && smooth_line && !group == "Overall") {
      ribbon_data <- smooth_data %>%
        dplyr::group_by(.data[[group]]) %>%
        dplyr::group_modify(~{
          loess_upper <- stats::loess(Upper ~ .x[[time]], data = .x, span = span_loess)
          loess_lower <- stats::loess(Lower ~ .x[[time]], data = .x, span = span_loess)
          xseq <- seq(min(.x[[time]]), max(.x[[time]]), length.out = 50)
          data.frame(x = xseq, y = i,
                     ymin = predict(loess_lower, xseq), ymax = predict(loess_upper, xseq))}
        ) %>%
        dplyr::ungroup()
    }

    # Define the graph
    gg <-
      ggplot2::ggplot(data = data, ggplot2::aes(x = .data[[time]], y = .data[[i]])) +
      ggplot2::coord_cartesian(ylim = c(min(Quantiles$Inferior), max(Quantiles$Superior))) +
      {
        if (group == "Overall")
          ggplot2::aes(colour = "black")
      } + {
        if (!group == "Overall")
          ggplot2::aes(colour = .data[[group]], fill = .data[[group]])
      } + {
        if (Point)
          ggplot2::geom_point(size = size_point, alpha = alpha_point, shape = 19)
      } + {
        if (id_lines)
          ggplot2::geom_line(ggplot2::aes(y = .data[[i]], group = .data[[id]]),
                             alpha = alpha_id_lines, linewidth = lwd_id_lines,
                             colour = "black")
      } + {
        if (smooth_line)
          ggplot2::stat_smooth(data = smooth_data,
                               ggplot2::aes(x = .data[[time]], y = .data[["smooth_y"]]),
                               geom = "line", alpha = alpha_line,
                               size = lwd_line, span = span_loess,
                               method = "loess", formula = 'y ~ x',
                               show.legend = FALSE)
      } + {
        if (!smooth_line & stat_line == "median")
          ggplot2::stat_summary(geom = "line", fun = stats::median,
                                alpha = alpha_line, linewidth = lwd_line,
                                show.legend = FALSE)
      } + {
        if (!smooth_line & stat_line == "mean")
          ggplot2::stat_summary(geom = "line", fun = mean,
                                alpha = alpha_line, linewidth = lwd_line,
                                show.legend = FALSE)

      } + {
        if (Overall)
          ggplot2::annotate(geom = "text", x = -Inf, y = Inf,
                            hjust = -0.1, vjust = 1.5,
                            size = posthoc_test_size,
                            label = LandS::formatz_p(Test_results[Test_results[["Variable"]] == i, overall_p]),
                            colour = "black")
      } + {
        if (Posthoc)
          if (nrow(posthoc_df) > 0 & !all(is.na(posthoc_df)))
            ggpubr::stat_pvalue_manual(posthoc_df, label = "p = {pvalue}",
                                       y.position = max(tapply(data[[i]], data[[time]], stats::median, na.rm = T)),
                                       step.increase = 0.08, size = posthoc_test_size)
      } + {
        if (extra)
          extra_text(i)
      } + {
        if (ribbon && smooth_line && group == "Overall")
          ggplot2::geom_ribbon(data = ribbon_data, ggplot2::aes(x = .data[["x"]], ymin = .data[["ymin"]], ymax = .data[["ymax"]]),
                               alpha = alpha_ribbon, fill = palette, show.legend = FALSE, colour = NA, inherit.aes = FALSE)
      } + {
        if (ribbon && smooth_line && !group == "Overall")
          ggplot2::geom_ribbon(data = ribbon_data, ggplot2::aes(x = .data[["x"]], ymin = .data[["ymin"]], ymax = .data[["ymax"]], fill = .data[[group]]),
                               alpha = alpha_ribbon, show.legend = FALSE, colour = NA, inherit.aes = FALSE)
      } + {
        if (ribbon & !smooth_line)
          ggplot2::stat_summary(ggplot2::aes(x = .data[[time]], y = .data[[i]]),
                                geom = "ribbon", alpha = alpha_ribbon, linewidth = NA,
                                fun.min = function(z) stats::quantile(z, 0.25),
                                fun.max = function(z) stats::quantile(z, 0.75), inherit.aes = FALSE)

      } +
      ggplot2::scale_color_manual(values = palette, drop = FALSE) +
      ggplot2::scale_fill_manual(values = palette, drop = FALSE, guide = "none") +
      ggplot2::labs(title = i, x = NULL, y = NULL) +
      ggplot2::scale_x_continuous(breaks = breaks, labels = label,
                                  guide = ggplot2::guide_axis(check.overlap = TRUE)) +
      {
        if (grid)
          themegrid + ggplot2::theme(plot.title = ggtext::element_textbox_simple(
            size = size_title, box.colour = "black", face = "bold",
            linewidth = .1, linetype = 1,
            hjust = 0, halign = .5,
            padding = ggplot2::margin(6, 5, 4, 5),
            margin = ggplot2::margin(0, 0, 0, 0),
            fill = scales::alpha(fill.title, alpha_fill_title)),
            legend.position = "none",
            legend.title = ggplot2::element_blank())
      } + {
        if (pptx)
          themePPTX + ggplot2::theme(plot.title = ggtext::element_textbox_simple(
            size = size_title, box.colour = "black", face = "bold",
            linewidth = .1, linetype = 1,
            hjust = 0, halign = .5,
            padding = ggplot2::margin(6, 5, 4, 5),
            margin = ggplot2::margin(0, 0, 0, 0),
            fill = scales::alpha(fill.title, alpha_fill_title)),
            legend.position = "none",
            legend.title = ggplot2::element_blank(),
            legend.background = ggplot2::element_rect(fill = "transparent"))
      }

    if (length(variables) > 1) {
      list_reg[[k + 1]] <- gg
    } else {
      list_reg[[k]] <- gg
    }
    if (verbose) LandS::Progress_bar(current = which(i == variables),
                                     total = length(variables),
                                     start_time = start_time,
                                     bar_fill = "\U2588",
                                     bar_void = "\U2591")
  }
  if (pptx) {
    ppt <- officer::read_pptx()
    if (verbose) message("Printing PowerPoint")
    for (i in 1:length(list_reg)) {
      list_reg[[i]] <- rvg::dml(ggobj = list_reg[[i]])
      ppt = officer::add_slide(ppt, layout = "Title and Content")
      officer::ph_with(ppt, list_reg[[i]], officer::ph_location(width = pptx_width,
                                                                height = pptx_height))
    }
    print(ppt, target = target)
    if (verbose) message("Done printing :)")

  } else if (length(list_reg) == 1 && !pptx) {

    return(list_reg[[1]])

  } else {
    return(list_reg)
  }
}
