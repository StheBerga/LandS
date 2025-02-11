#' This function creates a list of boxplot
#'
#' @param data dataframe
#' @param variables vector containing all variables of interest
#' @param group factor variable splitting the data
#' @param ID_lines whether to print the lines for paired observations
#' @param Posthoc whether to display Posthoc tests
#' @param Point whether to display observation points
#' @param Median_line whether to display the line connecting medians
#' @param rm.outliers whether to remove outliers from display
#' @param alpha_box alpha parameter for the boxes
#' @param width_box box's width
#' @param size_median_line median linewidth
#' @param lwd_box box linewidth
#' @param lwd_ID_line linewidth for paired observations
#' @param alpha_ID_line alpha for paired observations
#' @param alpha_point alpha for points
#' @param size_point size for points
#' @param Test_results dataframe for global and posthoc tests, see cont_var_test_LB
#' @param threshold_posthoc threshold for displaying posthoc tests
#' @param axis_y_title axis y title
#' @param axis_x_title axis x title
#' @param size_axis_x axis y title dimension
#' @param size_axis_y axis x title dimension
#' @param ID ID variable
#' @param legend_cod
#' @param breaks_axis_x
#' @param labels_axis_x
#' @param grid
#' @param PPTX
#' @param pptx_width
#' @param pptx_height
#' @param extra
#' @param extra_text
#' @param palette_boxplot
#' @param palette_title
#' @param size_title
#' @param target
#' @param ratio
#' @param telegram
#' @param posthoc_test_size
#' @param Overall
#' @param notch add notch to boxplot
#' @param notchwidth width of the notch
#' @param label_legend_title
#' @param size_legend_title
#' @param size_legend_text
#' @param size_legend_circle
#'
#' @return Una lista di boxplot
#' @export
#'
#' @examples
Boxplot_LB <- function (data, variables, group, ID_lines = FALSE, Posthoc = FALSE,
                        Point = F, Median_line = F, rm.outliers = F, alpha_box = 0.1,
                        width_box = 0.2, size_median_line = 0.8, lwd_box = 0.1, lwd_ID_line = 0.2,
                        alpha_ID_line = 0.3, alpha_point = 0.3, size_point = 0.3,
                        Test_results = NULL, threshold_posthoc = 0.1, posthoc_test_size = 3.88,
                        Overall = F, notch = F, notchwidth = 0.5, axis_y_title = NULL,
                        axis_x_title = NULL, size_axis_x = 6, size_axis_y = 6, ID = "ID",
                        legend_cod = NULL, breaks_axis_x = levels(data[, group]),
                        labels_axis_x = levels(data[, group]), grid = TRUE, PPTX = FALSE,
                        pptx_width = 7.5, pptx_height = 5.5, extra = F, extra_text = NULL,
                        palette_boxplot = c("salmon", "royalblue", "forestgreen", "gold"),
                        label_legend_title = paste0("Boxplots by ", group, "\n", format(Sys.Date(), "%d/%m/%Y")),
                        palette_title = "black", size_title = 3,
                        size_legend_title = 3, size_legend_text = 3, size_legend_circle = 4,
                        target = paste0(path_out, "/Boxplot.pptx"), ratio = 1, telegram = "none")
{
  require(ggplot2)
  require(officer)
  if (telegram != "none") {
    start_time <<- Sys.time()
  }
  theme_PPTX <- theme(axis.text.x = element_text(size = 14,
                                                 colour = "black", vjust = -0), plot.margin = margin(2,
                                                                                                     2, 2, 2, "mm"), axis.text.y = element_text(size = 14,
                                                                                                                                                colour = "black"), panel.border = element_rect(linetype = "solid",
                                                                                                                                                                                               colour = "black", linewidth = 0.1, fill = NA), axis.ticks = element_line(linewidth = 0.1),
                      axis.ticks.length = unit(0.25, "mm"), panel.background = element_blank(),
                      panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                      panel.spacing.x = unit(1.5, "mm"))
  themegrid <- theme(axis.text.x = element_text(size = size_axis_x,
                                                colour = "black", vjust = -0), plot.margin = margin(2,
                                                                                                    2, 2, 2, "mm"), axis.text.y = element_text(size = size_axis_y,
                                                                                                                                               colour = "black"), panel.border = element_rect(linetype = "solid",
                                                                                                                                                                                              colour = "black", linewidth = 0.1, fill = NA), axis.ticks = element_line(linewidth = 0.1),
                     axis.ticks.length = unit(0.25, "mm"), panel.background = element_blank(),
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
          scale_color_manual(values = palette_boxplot)
      } + {
        if (group == 1)
          scale_color_manual(values = palette_boxplot[1])
      } + {
        if (group != 1)
          scale_fill_manual(values = palette_boxplot)
      } + {
        if (group == 1)
          scale_fill_manual(values = palette_boxplot[1])
      } + theme_transparent() +
      theme(legend.text = element_text(size = size_legend_text, face = "bold")) +
      annotate(geom = "text", x = 1,
               y = 1.001, size = size_legend_title, label = label_legend_title,
               vjust = 1.25, fontface = "bold") +
      guides(color = guide_legend(override.aes = list(size = size_legend_circle,
                                                      shape = 20))) +
      theme(legend.position = "inside",
            legend.justification = c(0.5, 0.3), legend.title = element_blank())
  }
  else {
  }

  for (i in variables) {
    if (palette_title == 1) {
      col_title <- legend_name$Colore[legend_name$new_name ==
                                        i]
    }
    else {
      col_title <- "black"
    }
    k <- which(variables == i)
    if (Posthoc == T) {
      posthoc_df <- LandS::posthoc_df_LB(Test_results = Test_results,
                                         data = data, group = group, threshold_posthoc = threshold_posthoc,
                                         i)
    }
    A = tapply(data[, i], data[, group], quantile, c(0.75),
               na.rm = T)
    B = tapply(data[, i], data[, group], quantile, c(0.25),
               na.rm = T)
    C = tapply(data[, i], data[, group], IQR, na.rm = T)
    ymax <- max(A + 1.5 * C, na.rm = T)
    ymin <- min(B - 1.5 * C, na.rm = T)
    data[, "Y"] <- data[, i]
    if (rm.outliers == TRUE) {
      data[, "Y"][data[, i] > ymax] <- NA
      data[, "Y"][data[, i] < ymin] <- NA
    }
    else {
    }

    if (length(variables) > 1){
      boxplot[[k + 1]] <- ggplot(data = data, aes_string(x = data[, group])) +
        geom_boxplot(aes_string(y = data[, i],
                                fill = data[, group]), width = width_box, alpha = alpha_box,
                     notch = notch, notchwidth = notchwidth, outlier.shape = NA,
                     lwd = lwd_box, show.legend = F) + {
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
                                      geom = "line", fun = median, linewidth = size_median_line,
                                      colour = "red", linetype = "solid", show.legend = F)
                     } + {
                       if (rm.outliers)
                         coord_cartesian(ylim = c(min(data[, "Y"], na.rm = T),
                                                  min(max(data[, "Y"], ymax, na.rm = T))))
                     } + {
                       if (is.null(legend_cod)) {
                         labs(title = i, y = axis_y_title, x = axis_x_title)
                       }
                     } + {
                       if (!is.null(legend_cod)) {
                         labs(title = legend_cod$Original[legend_cod$Newname ==
                                                            i], y = axis_y_title, x = axis_x_title)
                       }
                     } + scale_color_manual(values = palette_boxplot) +
        scale_fill_manual(values = palette_boxplot,
                          guide = FALSE) +
        scale_x_discrete(breaks = breaks_axis_x,
                         labels = labels_axis_x) + {
                           if (Posthoc)
                             if (nrow(posthoc_df) > 0 & !all(is.na(posthoc_df)))
                               ggpubr::stat_pvalue_manual(posthoc_df, label = "p = {pval}",
                                                          y.position = max(tapply(data[, i], data[,
                                                                                                  group], quantile, na.rm = T, probs = 0.75) +
                                                                             tapply(data[, i], data[, group], IQR, na.rm = T)),
                                                          step.increase = 0.08, size = posthoc_test_size)
                         } + {
                           if (Overall)
                             annotate(geom = "text", x = -Inf, y = Inf, hjust = -0.1,
                                      vjust = 1.5, size = posthoc_test_size, label = LandS::formatz_p(Test_results[,
                                                                                                                   2][Test_results[, 1] == i]), colour = "black")
                         } + {
                           if (extra)
                             extra_text(i)
                         } + {
                           if (grid)
                             themegrid + theme(plot.title = element_text(hjust = 0.5,
                                                                         size = size_title, vjust = -0.5, face = "bold",
                                                                         colour = col_title))
                         } + {
                           if (PPTX)
                             theme_PPTX + theme(plot.title = element_text(hjust = 0.5,
                                                                          size = 20, vjust = -0.5, face = "bold", colour = col_title))
                         }
    }else{

      boxplot[[k]] <- ggplot(data = data, aes_string(x = data[, group])) +
        geom_boxplot(aes_string(y = data[, i],
                                fill = data[, group]), width = width_box, alpha = alpha_box,
                     notch = notch, notchwidth = notchwidth, outlier.shape = NA,
                     lwd = lwd_box, show.legend = F) + {
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
                                      geom = "line", fun = median, linewidth = size_median_line,
                                      colour = "red", linetype = "solid", show.legend = F)
                     } + {
                       if (rm.outliers)
                         coord_cartesian(ylim = c(min(data[, "Y"], na.rm = T),
                                                  min(max(data[, "Y"], ymax, na.rm = T))))
                     } + {
                       if (is.null(legend_cod)) {
                         labs(title = i, y = axis_y_title, x = axis_x_title)
                       }
                     } + {
                       if (!is.null(legend_cod)) {
                         labs(title = legend_cod$Original[legend_cod$Newname ==
                                                            i], y = axis_y_title, x = axis_x_title)
                       }
                     } + scale_color_manual(values = palette_boxplot) +
        scale_fill_manual(values = palette_boxplot,
                          guide = FALSE) +
        scale_x_discrete(breaks = breaks_axis_x,
                         labels = labels_axis_x) + {
                           if (Posthoc)
                             if (nrow(posthoc_df) > 0 & !all(is.na(posthoc_df)))
                               ggpubr::stat_pvalue_manual(posthoc_df, label = "p = {pval}",
                                                          y.position = max(tapply(data[, i], data[,
                                                                                                  group], quantile, na.rm = T, probs = 0.75) +
                                                                             tapply(data[, i], data[, group], IQR, na.rm = T)),
                                                          step.increase = 0.08, size = posthoc_test_size)
                         } + {
                           if (Overall)
                             annotate(geom = "text", x = -Inf, y = Inf, hjust = -0.1,
                                      vjust = 1.5, size = posthoc_test_size, label = LandS::formatz_p(Test_results[,
                                                                                                                   2][Test_results[, 1] == i]), colour = "black")
                         } + {
                           if (extra)
                             extra_text(i)
                         } + {
                           if (grid)
                             themegrid + theme(plot.title = element_text(hjust = 0.5,
                                                                         size = size_title, vjust = -0.5, face = "bold",
                                                                         colour = col_title))
                         } + {
                           if (PPTX)
                             theme_PPTX + theme(plot.title = element_text(hjust = 0.5,
                                                                          size = 20, vjust = -0.5, face = "bold", colour = col_title))
                         }
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
      ph_with(ppt, boxplot[[i]], ph_location(width = pptx_width,
                                             height = pptx_height))

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

