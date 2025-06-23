#' Function to build the lineplots
#'
#' @param data A long-formatted dataframe
#' @param variables Vector of variables to plot
#' @param time Numeric variable to plot on the x-axis
#' @param breaks Numeric vector with x-axis breaks. Default: unique(data[, time])
#' @param label Vector with x-axis labels. Must be the same length of breaks. Default: unique(data[, time])
#' @param group Factor variable to group the lines
#' @param col_lines Colours for the lines. Default: "salmon" & "royalblue"
#' @param stat_line Statistic method for the line. "median" or "mean".
#' @param lw_reg Linewidth for regression line
#' @param alpha_line Alpha for regression line
#' @param ylim Limits for the y-axis to plot. Default: c(0.20, 0.80)
#' @param ribbon Whether to show or not ribbons
#' @param alpha_ribbon Alpha for ribbons. Default: 0.05
#' @param ID_lines Whether to show ID lines for every patient
#' @param ID ID variable. Default: "ID"
#' @param alpha_ID_line Alpha for ID lines. Default: 0.3
#' @param lw_ID_line Linewidth for ID lines. Default: 0.2
#' @param col_title Whether to personalize the colour of title. Default = FALSE
#' @param colour_title A function to personalize the colour of title. See vignette for more.
#' @param size_title Size of title. If grid recommended 7, if PPTX recommended 20
#' @param size_axis_x x-axis text size. If grid recommended 5, if PPTX recommended 14
#' @param size_axis_y y-axis text size. If grid recommended 6, if PPTX recommended 14
#' @param Overall Whether to add overall test. Default = FALSE. See vignette for more.
#' @param Test_results Dataframe for overall and posthoc tests. See vignette for more.
#' @param Posthoc Whether to add posthoc tests with brackets.  Default = FALSE. See vignette for more.
#' @param threshold_posthoc Threshold to display posthoc brackets
#' @param posthoc_test_size Size for annotations of posthoc p-values. Default = 2
#' @param grid Whether to build a grid pdf or a PPTX file. Default = TRUE
#' @param ratio Graph ratio when grid = TRUE
#' @param PPTX Whether to build PPTX or a grid pdf file. Default = FALSE. Must change grid = FALSE
#' @param pptx_width Graph dimensions for PPTX in inches
#' @param pptx_height Graph dimensions for PPTX in inches
#' @param target Path where to save the PPTX file
#' @param extra Whether to add an extra text function. Default = FALSE
#' @param extra_text A function to add extra functions to the graphs. See vignette for more.
#' @param label_title A title for your list. Default sets "Lineplots by grouping variable" and the current date
#' @param smooth_line Whether to show smooth or spline regression line. Defaul = FALSE
#' @param span_line The span of smooth line. Default = 0.3
#' @param Point Whether to show points. Default = FALSE
#' @param alpha_point Alpha points. Default = 0.3
#' @param size_point Size points. Default = 0.3
#' @param size_label_title Size for your list's title. Default = 2.5
#'
#' @returns When grid = TRUE returns a list of ggplots. When PPTX = TRUE and grid = FALSE returns a PPTX file in the target folder
#' @export
#'
#' @examples
Lineplots_LB <- function (data, variables, time, breaks = unique(data[, time]),
                          label = unique(data[, time]), group = 1, col_lines = c("salmon", "royalblue"), stat_line = "median", smooth_line = FALSE,
                          span_line = 0.3, lw_reg = 1, alpha_line = 1, ylim = c(0.2, 0.8), ribbon = TRUE, alpha_ribbon = 0.05, ID_lines = FALSE,
                          ID = "ID", alpha_ID_line = 0.3, lw_ID_line = 0.2, Point = FALSE,
                          alpha_point = 0.3, size_point = 0.3, col_title = FALSE, colour_title = NULL,
                          size_title = 7, size_axis_x = 5, size_axis_y = 6, Overall = F,
                          Test_results = Test_results, Posthoc = F, threshold_posthoc = 0.1,
                          posthoc_test_size = 2, grid = T, ratio = 1, PPTX = F, pptx_width = 8.5,
                          pptx_height = 5.5, target = "Output/Lineplots.pptx",
                          label_title = paste0("Lineplots by ", group, "\n", format(Sys.Date(), "%d/%m/%Y")),
                          size_label_title = 2.5, extra = F, extra_text = NULL)
{
  require(ggplot2)
  require(ggpubr)
  require(dplyr)
  require(ggh4x)
  require(grid)
  require(gridExtra)
  require(svMisc)
  require(officer)
  require(rvg)
  require(progress)
  if (Posthoc == FALSE & Overall == FALSE) {
    Test_results = data.frame(matrix(NA))
  }

  # Define Themes ----
  themegrid <- theme(axis.text.x = element_text(size = size_axis_x, colour = "black", vjust = -0),
                     plot.margin = margin(2, 2, 2, 2, "mm"),
                     axis.text.y = element_text(size = size_axis_y, colour = "black"),
                     panel.border = element_rect(linetype = "solid", colour = "black", linewidth = 0.1, fill = NA),
                     axis.ticks = element_line(linewidth = 0.1),
                     axis.ticks.length = unit(0.25, "mm"),
                     panel.background = element_rect(fill = "transparent"),
                     plot.background = element_rect(fill = "transparent"),
                     panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                     legend.title = element_blank(), panel.spacing.x = unit(1.5, "mm"), aspect.ratio = ratio)

  themePPTX <- theme(axis.text.x = element_text(size = size_axis_x, colour = "black", vjust = -0),
                     plot.margin = margin(2, 2, 2, 2, "mm"),
                     axis.text.y = element_text(size = size_axis_y, colour = "black"),
                     panel.border = element_rect(linetype = "solid", colour = "black", linewidth = 0.1, fill = NA),
                     axis.ticks = element_line(linewidth = 0.1),
                     axis.ticks.length = unit(0.25, "mm"),
                     panel.background = element_rect(fill = "transparent"),
                     plot.background = element_rect(fill = "transparent"),
                     panel.grid.major.y = element_blank(),
                     panel.grid.major.x = element_line(colour = "grey70", linewidth = 0.1, linetype = 2),
                     panel.grid.minor = element_blank(),
                     legend.key.width = unit(2, "cm"),
                     panel.spacing.x = unit(1.5, "mm"))
  list_reg <- list()
  pb <- progress::progress_bar$new(format = "[:bar] :current/:total (:percent)", total = length(variables))
  pb$tick(0)

  # Creating a legend as the first graph ----
  if (length(variables) > 1) {
    list_reg[[1]] <- ggplot(data, aes_string(x = 1, y = 1)) +
      {
        if (group != 1)
          geom_point(aes_string(colour = group), shape = NA,
                     show.legend = TRUE)
      } + {
        if (group == 1)
          geom_point(shape = NA, show.legend = TRUE)
      } + {
        if (group != 1)
          scale_color_manual(values = col_lines)
      } + {
        if (group == 1)
          scale_color_manual(values = col_lines[1])
      } + {
        if (group != 1)
          scale_fill_manual(values = col_lines)
      } + {
        if (group == 1)
          scale_fill_manual(values = col_lines[1])
      } + theme_transparent() +

      annotate(geom = "text", x = 1, y = 1.001,
               size = size_label_title, label = label_title,
               vjust = 1.25, fontface = "bold") +
      guides(color = guide_legend(override.aes = list(size = 7.5, shape = 20))) +
      theme(legend.position = "inside", legend.justification = c(0.5, 0.3), legend.title = element_blank())
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
      posthoc_df <- Posthoc_lineplots_LB(Test_results = Test_results,
                                         data = data, time = time, threshold_posthoc = threshold_posthoc, i)
    }

    Quantili <- data.frame(Tempo = unique(data[, time]),
                           Inferior = tapply(data[, i], data[, time], FUN = function(z) quantile(z, ylim[1], na.rm = T)),
                           Superior = tapply(data[, i], data[, time], FUN = function(z) quantile(z, ylim[2], na.rm = T)))

    # Define the graph
    gg <- ggplot(data = data, aes_string(x = time, y = data[, i])) +
      coord_cartesian(ylim = c(min(Quantili$Inferior), max(Quantili$Superior)))

    if (col_title) {
      if (group != 1) {

        colour_title <- colour_title(i)
        gg <- gg + aes_string(colour = group, fill = group) +
          scale_color_manual(values = col_lines, drop = F) +
          scale_fill_manual(values = col_lines, drop = F, guide = FALSE)

        if (ID_lines == TRUE) {

          gg <- gg + geom_line(aes_string(y = data[, i], group = ID), alpha = alpha_ID_line, linewidth = lw_ID_line)

        } else { }

      } else {
        colour_title <- colour_title(i)
        gg <- gg + aes(colour = "forestgreen", fill = "forestgreen") +
          scale_color_manual(values = col_lines[1], drop = F) +
          scale_fill_manual(values = col_lines[1], drop = F, guide = FALSE)

        if (ID_lines == TRUE) {
          gg <- gg + geom_line(aes_string(y = data[, i], group = ID), alpha = alpha_ID_line, linewidth = lw_ID_line, colour = "black")

        } else { }
      }
    } else {
      if (group != 1) {
        colour_title <- "black"
        gg <- gg + aes_string(colour = group, fill = group) +
          scale_color_manual(values = col_lines, drop = F) +
          scale_fill_manual(values = col_lines, drop = F, guide = FALSE)
        if (ID_lines == TRUE) {
          gg <- gg + geom_line(aes_string(y = data[, i], group = ID), alpha = alpha_ID_line, linewidth = lw_ID_line)
        } else { }

      } else {
        colour_title <- "black"
        gg <- gg + aes(colour = "forestgreen", fill = "forestgreen") +
          scale_color_manual(values = col_lines[1], drop = F) +
          scale_fill_manual(values = col_lines[1], drop = F,
                            guide = FALSE)
        if (ID_lines == TRUE) {
          gg <- gg + geom_line(aes_string(y = data[,
                                                   i], group = ID), alpha = alpha_ID_line, linewidth = lw_ID_line,
                               colour = "black")
        } else { }
      }
    }
    if (Point) {
      gg <- gg + geom_point(size = size_point, alpha = alpha_point)
    }
    if (stat_line == "median") {
      if (smooth_line) {
        gg <- gg + stat_smooth(data = smooth_data, aes(x = Time_num, y = smooth_y), geom = "line", alpha = alpha_line,
                               size = lw_reg, span = span_line, show.legend = F, method = "loess", formula = 'y ~ x')
      } else {
        gg <- gg + stat_summary(geom = "line", fun = median,
                                alpha = alpha_line, linewidth = lw_reg, show.legend = FALSE) +
          stat_summary(geom = "point", fun = median,
                       size = NA, show.legend = TRUE)
      }
    } else if (stat_line == "mean") {
      if (smooth_line) {
        gg <- gg + stat_smooth(data = smooth_data, aes(x = Time_num, y = smooth_y), geom = "line", alpha = alpha_line,
                               size = lw_reg, span = span_line, show.legend = F, method = "loess", formula = 'y ~ x')
      } else {
        gg <- gg + stat_summary(geom = "line", fun = mean,
                                alpha = alpha_line, linewidth = lw_reg, show.legend = FALSE) +
          stat_summary(geom = "point", fun = mean,
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
          gg <- gg + stat_smooth(data = smooth_data, aes(x = Time_num, y = Upper), geom = "line", alpha = alpha_line,
                                 size = lw_reg, span = span_line, show.legend = F, colour = NA, method = "loess", formula = 'y ~ x')

          gg <- gg + stat_smooth(data = smooth_data, aes(x = Time_num, y = Lower), geom = "line", alpha = alpha_line,
                                 size = lw_reg, span = span_line, show.legend = F, colour = NA, method = "loess", formula = 'y ~ x')

          gg1 <- ggplot_build(gg)

          df2 <- data.frame(x = gg1$data[[1]]$x, ymin = gg1$data[[2]]$y, ymax = gg1$data[[3]]$y)

          gg <- gg + geom_ribbon(data = df2, aes(x = x, ymin = ymin, ymax = ymax, y = 0),
                                 fill = col_lines[1], alpha = alpha_ribbon, show.legend = F, linewidth = NA)
        } else if (group != 1) {

          gg <- gg + stat_smooth(data = smooth_data, aes(x = Time_num, y = Upper), geom = "line", alpha = alpha_line,
                                 size = lw_reg, span = span_line, show.legend = F, colour = NA, method = "loess", formula = 'y ~ x')

          gg <- gg + stat_smooth(data = smooth_data, aes(x = Time_num, y = Lower), geom = "line", alpha = alpha_line,
                                 size = lw_reg, span = span_line, show.legend = F, colour = NA, method = "loess", formula = 'y ~ x')

          gg1 <- ggplot_build(gg)

          df2 <- data.frame(x = gg1$data[[1]]$x, ymin = gg1$data[[2]]$y, ymax = gg1$data[[3]]$y, group = gg1$data[[1]]$group)
          df2$group <- factor(df2$group, levels = 1:nlevels(data[, group]), labels = levels(data[, group]))
          colnames(df2)[which(colnames(df2) == "group")] <- group

          gg <- gg + geom_ribbon(data = df2, aes(x = x, ymin = ymin, ymax = ymax, y = 0), alpha = 0.1, show.legend = F,
                                 linewidth = NA)

        }
      } else {
        gg <- gg + stat_summary(geom = "ribbon", fun.min = function(z) {
          quantile(z, 0.25)
        }, fun.max = function(z) {
          quantile(z, 0.75)
        }, linewidth = NA, alpha = alpha_ribbon, show.legend = F)
      }
    } else { }
    if (extra) {
      gg <- gg + extra_text(i)
    }
    gg <- gg + labs(title = i, x = NULL, y = NULL) +
      scale_x_continuous(breaks = breaks, labels = label, guide = guide_axis(check.overlap = TRUE))

    {
      if (Overall)
        gg <- gg + annotate(geom = "text", x = -Inf,
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
      gg <- gg + themegrid + theme(plot.title = element_text(hjust = 0.5, size = size_title, vjust = -0.5, face = "bold",
                                                             colour = colour_title), legend.position = "none",
                                   legend.title = element_blank())
    }
    if (PPTX) {
      gg <- gg + themePPTX + theme(plot.title = element_text(hjust = 0.5, size = 20, vjust = -0.5, face = "bold", colour = colour_title),
                                   legend.position = "none", legend.title = element_blank(),
                                   legend.background = element_rect(fill = "transparent"))
    }
    if (length(variables) > 1) {
      list_reg[[k + 1]] <- gg
    }
    else {
      list_reg[[k]] <- gg
    }
    Sys.sleep(0.01)
    pb$tick(1)
    pb
  }
  if (PPTX == T) {
    ppt <- read_pptx()
    message("Printing PowerPoint")
    for (i in 1:length(list_reg)) {
      list_reg[[i]] <- rvg::dml(ggobj = list_reg[[i]])
      ppt = add_slide(ppt, layout = "Title and Content")
      ph_with(ppt, list_reg[[i]], ph_location(width = pptx_width,
                                              height = pptx_height))
    }
    print(ppt, target = target)
  }
  else {
    return(list_reg)
  }
}
