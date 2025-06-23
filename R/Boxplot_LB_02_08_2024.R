#' This function creates a list of boxplot
#'
#' @param data Dataframe
#' @param variables Vector containing all variables of interest
#' @param group Factor variable splitting the data. Default = 1, overall distribution
#' @param rm.outliers Whether to remove outliers from display. Default = FALSE
#' @param th.outliers Times between Q1, Q3 and IQR to remove outliers. Default = 1.5
#' @param Point Whether to display observation points. Default = FALSE
#' @param size_point Size for points. Default = 0.3
#' @param alpha_point Alpha for points. Default = 0.3
#' @param alpha_box Alpha parameter for the boxes. Default = 0.1
#' @param width_box Width parameter for the boxes. Default = 0.2
#' @param lwd_box Linewidth for the boxes. Default = 0.1
#' @param notch Whether to display notches for the boxes. Default = FALSE
#' @param notchwidth Width parameter of the notch. Default = 0.5
#' @param Median_line Whether to display the line connecting median values. Default = FALSE
#' @param lwd_median_line Median line linewidth. Default = 0.8
#' @param col_median_line Colour for the median line. Default = "red"
#' @param ID_lines Whether to display the lines for paired observations. Default = FALSE
#' @param ID ID variable for paired observations. Default = "ID"
#' @param lwd_ID_line Linewidth parameter for paired observations lines. Default = 0.2
#' @param alpha_ID_line Alpha parameter for paired observations lines. Default = 0.3
#' @param Overall Whether to display overall test in the upper-left corner. Default = FALSE
#' @param Posthoc Whether to display posthoc tests brackets. Default = FALSE
#' @param Test_results Dataframe for global and posthoc tests for the LandS::cont_var_test_LB function
#' @param threshold_posthoc Threshold for displaying posthoc tests brackets. Default = 0.1
#' @param posthoc_test_size Size for annotations of posthoc p-values. Default = 3.88
#' @param bracket_shorten Width of the bracket [0,1]. Default = 0
#' @param bracket.nudge.y Vertical adjustment to nudge brackets by. Useful to move up or move down the bracket. If positive value, brackets will be moved up; if negative value, brackets are moved down. Default = 0
#' @param axis_x_title Title for the x axis
#' @param size_axis_x Dimensions for the x axis title. Default = 6
#' @param axis_y_title Title for the y axis
#' @param size_axis_y Dimensions for the y axis title. Default = 6
#' @param col_title Whether to personalize the colour of title. Default = FALSE
#' @param colour_title A function to personalize the colour of title. See vignette for more
#' @param title_leg Whether to personalize the title of the variables. Default = FALSE
#' @param title_legend A function to personalize the title of the variables. See vignette for more
#' @param size_title Dimensions for the title. Default = 8
#' @param breaks_axis_x Levels to be displayed in the graph
#' @param labels_axis_x Labels of the levels
#' @param grid Whether to build a grid pdf or a PPTX file. Default = TRUE
#' @param PPTX Whether to build PPTX or a grid pdf file. Must change grid = FALSE. Default = FALSE
#' @param pptx_width Graph dimensions for PPTX in inches. Default = 7.5
#' @param pptx_height Graph dimensions for PPTX in inches. Default = 5.5
#' @param target Path where to save the PPTX file
#' @param extra Whether to add an extra text function. Default = FALSE
#' @param extra_text A function to add extra functions to the graphs. See vignette for more
#' @param palette Colours for the boxes. Must be a vector of the same length as the nlevels. Default = "transparent"
#' @param label_legend_title A title for your list. Default sets "Boxplot by grouping variable" and the current date
#' @param size_legend_title Size for the list's title. Default = 3
#' @param size_legend_text Size for the list's text. Default = 3
#' @param size_legend_circle Size for the list's circles Default = 4
#' @param ratio Aspect ratio when grid = TRUE. Default = 1
#' @param telegram Whether to send a telegram message when finished. Default = "none" to not send any message
#' @return If length(variables) > 1 returns a list of boxplot where the first one is a coverpage. Otherwise returns a ggplot. \n When grid = TRUE returns a list of ggplots. When PPTX = TRUE and grid = FALSE returns a PPTX file in the target path
#' @export
#'
#' @examples
Boxplot_LB <- function (data, variables, group,
                        rm.outliers = F, th.outliers = 1.5,
                        Point = F, size_point = 0.3, alpha_point = 0.3,
                        alpha_box = 0.1, width_box = 0.2, lwd_box = 0.1,
                        notch = F, notchwidth = 0.5,
                        Median_line = F, lwd_median_line = 0.8, col_median_line = "red",
                        ID_lines = FALSE, ID = "ID", lwd_ID_line = 0.2, alpha_ID_line = 0.3,
                        Overall = F, Posthoc = FALSE,  Test_results = NULL, threshold_posthoc = 0.1,
                        posthoc_test_size = 3.88, bracket_shorten = 0, bracket.nudge.y = 0,
                        axis_x_title = NULL, size_axis_x = 6,
                        axis_y_title = NULL, size_axis_y = 6,
                        col_title = FALSE, colour_title = NULL,
                        title_leg = FALSE, title_legend = NULL, size_title = 8,
                        breaks_axis_x = levels(data[, group]), labels_axis_x = levels(data[, group]),
                        grid = TRUE, PPTX = FALSE, pptx_width = 7.5, pptx_height = 5.5,
                        extra = FALSE, extra_text = NULL,
                        palette = rep("transparent", nlevels(data[, group])),
                        label_legend_title = paste0("Boxplots by ", group, "\n", format(Sys.Date(), "%d/%m/%Y")),
                        size_legend_title = 3, size_legend_text = 3, size_legend_circle = 4,
                        target = "Output/Boxplot.pptx", ratio = 1, telegram = "none")
{
  require(ggplot2)
  require(officer)
  if (telegram != "none") {
    start_time <<- Sys.time()
  }
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
  pb <- progress::progress_bar$new(format = "[:bar] :current/:total (:percent)",
                                   total = length(variables))
  pb$tick(0)

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
      } + theme_transparent() +
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
      colour_title <- colour_title(i)
    } else {
      colour_title <- "black"
    }
    k <- which(variables == i)
    if (Posthoc == T) {
      posthoc_df <- LandS::posthoc_df_LB(Test_results = Test_results, data = data, group = group, threshold_posthoc = threshold_posthoc, i)
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
          themegrid + theme(plot.title = element_text(hjust = 0.5, size = size_title, vjust = -0.5, face = "bold", colour = colour_title))
      } + {
        if (PPTX)
          theme_PPTX + theme(plot.title = element_text(hjust = 0.5, size = 20, vjust = -0.5, face = "bold", colour = colour_title))
      }

    if (length(variables) > 1) {
      boxplot[[k + 1]] <- gg
    }
    else {
      boxplot[[k]] <- gg
    }

    Sys.sleep(0.01)
    pb$tick(1)
    pb
  }
  if (PPTX == T) {
    ppt <- read_pptx()
    message("Printing PowerPoint")
    for (i in 1:length(boxplot)) {
      boxplot[[i]] <- rvg::dml(ggobj = boxplot[[i]])
      ppt = add_slide(ppt, layout = "Title and Content")
      ph_with(ppt, boxplot[[i]], ph_location(width = pptx_width, height = pptx_height))

    }
    print(ppt, target = target)
    message("Done printing :)")
    if (telegram != "none") {
      LandS::telegram_mess_LB(dest = telegram, script = "Boxplot")
    }
  }
  else {
    if (telegram != "none") {
      LandS::telegram_mess_LB(dest = telegram, script = "Boxplot")
    }
    return(boxplot)
  }
}

