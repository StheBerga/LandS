#' Create boxplots for multiple continuous variables
#'
#' @description
#' Creates one boxplot per numeric variable in 'variables', optionally stratified
#' by a grouping variable. Observation-level points, paired-observation lines,
#' median trends, global test annotations, and post-hoc comparison brackets can
#' be added. Multiple plots are returned as a list and can optionally be exported
#' to a PowerPoint file.
#'
#'
#' @param data Dataframe containing numeric variables to plot.
#' @param variables Character vector containing all numeric variables of interest.
#' @param group Column name identifying factor grouping variable for split
#' boxplots, or 1 for no grouping.
#' @param ID Column name of ID variable used to connect paired observations. Default = "ID".
#' @param rm.outliers Whether to remove outliers from display. Default = FALSE.
#' @param th.outliers Numeric multiplier of the interquartile range (IQR) used
#' to define outlier thresholds; observations outside [Q1−th.outliers×IQR,Q3+th.outliers×IQR]
#' are treated as outliers. Default = 1.5.
#' @param Point Whether to display observation points. Default = FALSE.
#' @param size_point Size for points. Default = 0.3
#' @param alpha_point Alpha for points transparency. Default = 0.3
#' @param alpha_box Alpha parameter for the boxes transparency. Default = 0.1.
#' @param width_box Width parameter for the boxes. Default = 0.2.
#' @param lwd_box Linewidth for the boxes. Default = 0.1.
#' @param notch Whether to display notches for the boxes. Default = FALSE.
#' @param notchwidth Width parameter of the notch. Default = 0.5.
#' @param Median_line Whether to display the line connecting boxes median values. Default = FALSE.
#' @param lwd_median_line Median line linewidth. Default = 0.8.
#' @param col_median_line Colour for the median line. Default = "red".
#' @param ID_lines Whether to display the lines connecting paired observations. Default = FALSE.
#' @param lwd_ID_line Linewidth parameter for lines connecting paired observations. Default = 0.2.
#' @param alpha_ID_line Alpha parameter for paired observations lines transparency. Default = 0.3.
#' @param Overall Whether to display overall test in the upper-left corner. Default = FALSE.
#' @param Posthoc Whether to display posthoc tests brackets. Default = FALSE.
#' @param Test_results Dataframe containing the results of global and posthoc tests computed
#' using LandS::cont_var_test() function, in particular objects "KW_ph_pval" or "Friedman_ph_pval"
#' or "no_corrected_ph". Default = NULL.
#' @param threshold_posthoc Threshold for displaying post-hoc tests brackets. Default = 0.05.
#' @param posthoc_test_size Font size for annotations of post-hoc p-values. Default = 3.88.
#' @param bracket_shorten Width of the bracket in range [0,1]. Default = 0.
#' @param bracket.nudge.y Vertical adjustment to nudge brackets by.
#' Useful to move up or move down the bracket. If positive value, brackets will be moved up;
#' if negative value, brackets are moved down. Default = 0.
#' @param axis_x_title Title for the x-axis. Default = NULL.
#' @param size_axis_x Font size for the x-axis title. Default = 6.
#' @param axis_y_title Title for the y-axis. Default = NULL.
#' @param size_axis_y Font size for the y-axis title. Default = 6.
#' @param col_title Whether to personalize the colour background fill of boxplots title.
#' Default = FALSE.
#' @param alpha_fill_title Alpha value for title background. Default = 0.2.
#' @param fill_title A function to personalize the fill of each boxplot title background.
#' The argument fill_title is defined as a function mapping for each variable the
#' background color. See examples for further information. JG: AGGIUNGERE ESEMPIO.
#' @param title_leg Whether to personalize the title of the variables. Default = FALSE.
#' @param title_legend A function to personalize the title of the variables.
#' The argument title_legend is defined as a function mapping for each variable the
#' novel name to be displayed as title. Default = NULL.
#' See examples for further information. JG: AGGIUNGERE ESEMPIO.
#' @param size_title Font size for the title. Default = 8.
#' @param breaks_axis_x Character vector defining the subset and order of factor
#' levels to display on the x-axis (passed to scale_x_discrete(breaks = ...)).
#' Default = levels(data[, group]).
#' @param labels_axis_x Character vector defining the subset and order labels to
#' display on the x-axis (passed to scale_x_discrete(labels = ...)).
#' Default = levels(data[, group]).
#' @param grid Logical. If TRUE and 'PPTX = FALSE', returns plots as a list
#' suitable for grid display. Default=TRUE.
#' @param PPTX Logical. If TRUE, plots are exported to a PowerPoint file and
#' no plots are returned. This option takes precedence over 'grid argument.
#' Default=FALSE.
#' @param pptx_width Plot width dimension for PPTX in inches. Default = 7.5.
#' @param pptx_height Plot height dimension for PPTX in inches. Default = 5.5.
#' @param extra Whether to add an extra text function. Default = FALSE.
#' @param extra_text A function of one argument (variable) that returns one or
#' more ggplot2 layers to be added to each plot (e.g., annotations or custom geoms).
#' The function is evaluated inside the plotting loop.
#' See examples for further information. JG: AGGIUNGERE ESEMPIO.
#' @param palette Colours for the boxes. Character vector of colors with length
#' equal to number of groups. Default = rep("transparent", nlevels(data[, group])).
#' @param label_legend_title A title for your list.
#' Default sets "Boxplot by grouping variable" and the current date.
#' @param size_legend_title Font size for the list's title. Default = 3.
#' @param size_legend_text Font size for the list's text. Default = 3.
#' @param size_legend_circle Font size for the list's circles defining groups legend.
#' Default = 4.
#' @param target Path where to save the PPTX file.
#' @param ratio Aspect ratio when grid = TRUE. Default = 1.
#' @param telegram Whether to send a telegram message when the job is completed.
#' Default = "none" to not send any message.
#' @param verbose Print progress bar and messages (Default=TRUE).
#'
#' @return If length(variables) > 1 returns a list of boxplot where the first one is
#' coverpage. Otherwise returns a ggplot. When grid = TRUE returns a list of ggplots.
#' When PPTX = TRUE and grid = FALSE returns a PPTX file in the target path.
#' @export
#'
#' @details
#' Overall and post-hoc tests can be displayed, but they need to be computed using
#' LandS::cont_var_test() and then formatted using LandS::posthoc_df() function.
#' Boxplots can be saved in a panel image using pptx format or as a list of
#' ggplots for later saving in pdf format.
#'
#' Optionally, a Telegram message can be send when the job is complete.
#'
#' @author Luca Lalli, Stefano Bergamini
#'
#' @examples
#' mtcars$vs <- factor(mtcars$vs)
#' Boxplot(mtcars, c('mpg', 'disp'), 'vs')
Boxplot <- function (data,
                     variables,
                     group,
                     ID = "ID",

                     rm.outliers = F,
                     th.outliers = 1.5,

                     Point = F,
                     size_point = 0.3,
                     alpha_point = 0.3,

                     alpha_box = 0.1,
                     width_box = 0.2,
                     lwd_box = 0.1,
                     notch = F,
                     notchwidth = 0.5,
                     Median_line = F,
                     lwd_median_line = 0.8,
                     col_median_line = "red",

                     ID_lines = FALSE,
                     lwd_ID_line = 0.2,
                     alpha_ID_line = 0.3,

                     Overall = F,
                     Posthoc = FALSE,
                     Test_results = NULL,
                     threshold_posthoc = 0.05,
                     posthoc_test_size = 3.88,
                     bracket_shorten = 0,
                     bracket.nudge.y = 0,

                     axis_x_title = NULL,
                     size_axis_x = 6,
                     axis_y_title = NULL,
                     size_axis_y = 6,

                     col_title = FALSE,
                     alpha_fill_title = 0.2,
                     fill_title = NULL,

                     title_leg = FALSE,
                     title_legend = NULL,
                     size_title = 8,

                     breaks_axis_x = levels(data[, group]),
                     labels_axis_x = levels(data[, group]),

                     grid = TRUE,
                     PPTX = FALSE,
                     pptx_width = 7.5,
                     pptx_height = 5.5,

                     extra = FALSE,
                     extra_text = NULL,

                     palette = rep("transparent", nlevels(data[, group])),
                     label_legend_title = paste0("Boxplots by ", group, "\n", format(Sys.Date(), "%d/%m/%Y")),
                     size_legend_title = 3,
                     size_legend_text = 3,
                     size_legend_circle = 4,
                     target = "Output/Boxplot.pptx",
                     ratio = 1,
                     telegram = "none",
                     verbose = TRUE)
{
  require(ggplot2)
  require(officer)
  if (telegram != "none") {
    start_time <<- Sys.time()
  }
  start_time <- Sys.time()

  if (verbose) message(paste0("Creazione ", length(variables), " boxplots con: \n",
                              "-Split by ", group, "\n",
                              "-Points: ", Point, "\n",
                              "-Outliers: ", rm.outliers, "\n",
                              "-Posthoc: ", Posthoc, "\n",
                              "-ID_lines: ", ID_lines
  ))

  theme_PPTX <- theme(axis.text.x = element_text(size = 14, colour = "black", vjust = -0),
                      plot.margin = margin(2, 2, 2, 2, "mm"),
                      axis.text.y = element_text(size = 14, colour = "black"),
                      panel.border = element_rect(linetype = "solid", colour = "black", linewidth = 0.1, fill = NA),
                      axis.ticks = element_line(linewidth = 0.1),
                      axis.ticks.length = unit(0.25, "mm"),
                      panel.background = element_rect(fill = "transparent"),
                      plot.background = element_rect(fill = "transparent"),
                      panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                      panel.spacing.x = unit(1.5, "mm"))
  themegrid <- theme(axis.text.x = element_text(size = size_axis_x, colour = "black", vjust = -0),
                     plot.margin = margin(2, 2, 2, 2, "mm"),
                     axis.text.y = element_text(size = size_axis_y,colour = "black"),
                     panel.border = element_rect(linetype = "solid", colour = "black", linewidth = 0.1, fill = NA),
                     axis.ticks = element_line(linewidth = 0.1),
                     axis.ticks.length = unit(0.25, "mm"),
                     panel.background = element_rect(fill = "transparent"),
                     plot.background = element_rect(fill = "transparent"),
                     panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                     legend.position = c(0.87, 0.15), legend.title = element_blank(),
                     panel.spacing.x = unit(1.5, "mm"), aspect.ratio = ratio)
  boxplot <- list()

  if (length(variables) > 1) {
    boxplot[[1]] <- ggplot(data, aes_string(x = 1, y = 1)) +
      {
        if (group != 1)
          geom_point(aes_string(colour = group), shape = NA,
                     show.legend = TRUE)
      } + {
        if (group == 1)
          geom_point(shape = NA, show.legend = TRUE)
      } + {
        if (group != 1)
          scale_color_manual(values = palette)
      } + {
        if (group == 1)
          scale_color_manual(values = palette[1])
      } + {
        if (group != 1)
          scale_fill_manual(values = palette)
      } + {
        if (group == 1)
          scale_fill_manual(values = palette[1])
      } + ggpubr::theme_transparent() +
      theme(legend.text = element_text(size = size_legend_text, face = "bold")) +
      annotate(geom = "text", x = 1,
               y = 1.001, size = size_legend_title, label = label_legend_title,
               vjust = 1.25, fontface = "bold") +
      guides(color = guide_legend(override.aes = list(size = size_legend_circle, shape = 20))) +
      theme(legend.position = "inside",
            legend.justification = c(0.5, 0.3), legend.title = element_blank())
  }
  else { }

  for (i in variables) {
    if (col_title == TRUE) {
      fill_title <- fill_title(i)
    } else {
      fill_title <- "transparent"
    }
    k <- which(variables == i)
    if (Posthoc == T) {
      posthoc_df <- LandS::posthoc_df(Test_results = Test_results, data = data, group = group, threshold_posthoc = threshold_posthoc, i)
    }
    A = tapply(data[, i], data[, group], quantile, c(0.75), na.rm = T)
    B = tapply(data[, i], data[, group], quantile, c(0.25), na.rm = T)
    C = tapply(data[, i], data[, group], IQR, na.rm = T)
    ymax <- max(A + th.outliers * C, na.rm = T)
    ymin <- min(B - th.outliers * C, na.rm = T)
    data[, "Y"] <- data[, i]
    if (rm.outliers == TRUE) {
      data[, "Y"][data[, i] > ymax] <- NA
      data[, "Y"][data[, i] < ymin] <- NA
    }
    else {
    }
    gg <- ggplot(data = data, aes_string(x = data[, group])) +
      geom_boxplot(aes_string(y = data[, i], fill = data[, group]), width = width_box, alpha = alpha_box,
                   notch = notch, notchwidth = notchwidth, outlier.shape = NA,
                   lwd = lwd_box, show.legend = F) +
      {
        if (Point)
          geom_point(aes_string(y = data[, "Y"]), size = size_point,
                     alpha = alpha_point)
      } + {
        if (ID_lines)
          geom_line(aes_string(y = data[, i], group = ID),
                    alpha = alpha_ID_line, linewidth = lwd_ID_line,
                    colour = "black")
      } + {
        if (Median_line)
          stat_summary(aes_string(y = data[, i], group = 1),
                       geom = "line", fun = median, linewidth = lwd_median_line,
                       colour = col_median_line, linetype = "solid", show.legend = F)
      } + {
        if (rm.outliers)
          coord_cartesian(ylim = c(min(data[, "Y"], na.rm = T),
                                   min(max(data[, "Y"], ymax, na.rm = T))))
      } + {

        if (title_leg){
          labs(title = title_legend(i), x = axis_x_title, y = axis_y_title)
        } else {
          labs(title = i, x = axis_x_title, y = axis_y_title)
        }

      } +
      scale_color_manual(values = palette) +
      scale_fill_manual(values = palette, guide = FALSE) +
      scale_x_discrete(breaks = breaks_axis_x, labels = labels_axis_x) +
      {
        if (Posthoc)
          if (nrow(posthoc_df) > 0 & !all(is.na(posthoc_df)))
            ggpubr::stat_pvalue_manual(posthoc_df, label = "p = {pval}", bracket.shorten = bracket_shorten, bracket.nudge.y = bracket.nudge.y,
                                       y.position =
                                         max(tapply(data[, i], data[, group], quantile, na.rm = T, probs = 0.75) +
                                               tapply(data[, i], data[, group], IQR, na.rm = T)),
                                       step.increase = 0.08, size = posthoc_test_size)
      } + {
        if (Overall)
          annotate(geom = "text", x = -Inf, y = Inf, hjust = -0.1,
                   vjust = 1.5, size = posthoc_test_size, label = LandS::formatz_p(Test_results[, 2][Test_results[, 1] == i]), colour = "black")
      } + {
        if (extra)
          extra_text(i)
      } + {
        if (grid)
          themegrid + theme(plot.title = ggtext::element_textbox_simple(
            size = size_title, box.colour = "black", face = "bold",
            linewidth = .1, linetype = 1,
            hjust = 0, halign = .5,
            padding = margin(6, 5, 4, 5),
            margin = margin(0, 0, 0, 0),
            fill = scales::alpha(fill_title, alpha_fill_title)))
      } + {
        if (PPTX)
          theme_PPTX + theme(plot.title = ggtext::element_textbox_simple(
            size = size_title, box.colour = "black", face = "bold",
            linewidth = .1, linetype = 1,
            hjust = 0, halign = .5,
            padding = margin(6, 5, 4, 5),
            margin = margin(0, 0, 0, 0),
            fill = scales::alpha(fill_title, alpha_fill_title)))
      }

    if (length(variables) > 1) {
      boxplot[[k + 1]] <- gg
    }
    else {
      boxplot[[k]] <- gg
    }

    if (verbose) LandS::Progress_bar(current = which(i == variables),
                                     total = length(variables),
                                     start_time = start_time,
                                     bar_fill = "\U2588",
                                     bar_void = "\U2591")
  }
  if (PPTX == T) {
    ppt <- read_pptx()
    if (verbose) message("Printing PowerPoint")
    for (i in 1:length(boxplot)) {
      boxplot[[i]] <- rvg::dml(ggobj = boxplot[[i]])
      ppt = add_slide(ppt, layout = "Title and Content")
      ph_with(ppt, boxplot[[i]], ph_location(width = pptx_width, height = pptx_height))

    }
    print(ppt, target = target)
    if (verbose) message("Done printing :)")
    if (telegram != "none") {
      LandS::telegram_mess(dest = telegram, script = "Boxplot")
    }
  }
  else {
    if (telegram != "none") {
      LandS::telegram_mess(dest = telegram, script = "Boxplot")
    }
    return(boxplot)
  }
}

