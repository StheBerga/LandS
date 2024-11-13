#' Function to build the lineplots
#'
#' @param data dataset
#' @param variables vector of variables
#' @param time x-axis variable
#' @param group factor variable to group
#' @param split whether to split in two windows the lines
#' @param lw_reg lwd of regression line
#' @param size_point size of points
#' @param size_title size of title
#' @param col_title colour of title
#' @param size_axis_x x-axis text size
#' @param size_axis_y y-axis text size
#' @param size_title_grid size of title in the grid
#' @param breaks breaks of x-axis
#' @param label labels of x-axis
#' @param ylim ylim to display in the graph
#' @param Posthoc whether to display posthoc tests
#' @param Friedman friedman overall test dataset
#' @param Test_results posthoc test dataset
#' @param grid whether to build a grid or a pptx
#' @param ratio graph ratio
#' @param extra do you want to add extra option?
#' @param extra_text write your additional options
#' @param PPTX whether to build a pptx or a grid
#' @param pptx_width inch
#' @param pptx_height inch
#' @param threshold_posthoc threshold to display posthoc brackets
#' @param check check the correctness of your graph
#' @param target where do you want your pptx to be saved
#' @param col_lines splitted lines colour
#'
#' @return
#' @export
#'
#' @examples
Lineplots_LB <- function(data,
                         variables,
                         time,
                         group = 1,
                         split=F,
                         lw_reg = 1,
                         size_point = 0.6,
                         size_title = 12,
                         col_title = 1,
                         size_axis_x = 5,
                         size_axis_y = 6,
                         size_title_grid = 7,
                         breaks = unique(data[, time]),
                         label = unique(data[, time]),
                         ylim = c(0.20, 0.80),
                         Posthoc = F,
                         Friedman = F,
                         Test_results = Test_results,
                         grid = T,
                         ratio = 1,
                         extra = F,
                         extra_text = NULL,
                         PPTX = F,
                         pptx_width = 8.5,
                         pptx_height = 5.5,
                         threshold_posthoc = 0.1,
                         check = F,
                         target = paste0(path, "/file.pptx"),
                         col_lines = c("salmon", "royalblue"))
{

  require(ggplot2)
  require(ggpubr)
  require(dplyr)
  require(survival)
  require(ggh4x)
  require(ggformula)
  require(grid)
  require(gridExtra)
  require(svMisc)
  require(officer)
  require(rvg)
  require(progress)

  if (Posthoc == F) {
    Test_results = data.frame(matrix(NA))
  }

  themegrid <- theme(
    axis.text.x = element_text(size = size_axis_x, colour = "black", vjust = -0.0),
    plot.margin = margin(2, 2, 2, 2, "mm"),
    axis.text.y = element_text(size = size_axis_y, colour = "black"),
    panel.border = element_rect(linetype = "solid", colour = "black", linewidth = 0.1, fill = NA),
    axis.ticks = element_line(linewidth = 0.1),
    axis.ticks.length = unit(0.25, "mm"),
    panel.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.title = element_blank(),
    panel.spacing.x = unit(1.5, "mm"),
    aspect.ratio = ratio
  )

  themePPTX <- theme(
    axis.text.x = element_text(size = 14, colour = "black", vjust = -0.0),
    plot.margin = margin(2, 2, 2, 2, "mm"),
    axis.text.y = element_text(size = 14, colour = "black"),
    panel.border = element_rect(linetype = "solid", colour = "black", linewidth = 0.1, fill = NA),
    axis.ticks = element_line(linewidth = 0.1),
    axis.ticks.length = unit(0.25, "mm"),
    panel.background = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_line(colour = "grey70", linewidth = 0.1, linetype = 2),
    panel.grid.minor = element_blank(),
    legend.key.width = unit(2, 'cm'),
    panel.spacing.x = unit(1.5, "mm")
  )

  # Progress bar
  pb <- progress_bar$new(format = "[:bar] :current/:total (:percent)", total = length(variables))
  pb$tick(0)

  # Define list_reg
  list_reg <- list()

  # Define PPTX
  if (PPTX == T) {
    ppt <- read_pptx()
  }

  for (i in variables) {

    # If Posthoc = TRUE create a df for Posthoc significant tests
    if (Posthoc) {
      postmodel <- Test_results[Test_results[, 1] == i, ]
      posthoc_df <- as.data.frame(t(combn(levels(data[, "Time"]), 2)))
      colnames(posthoc_df) <- c("group1", "group2")
      posthoc_df$y <- i
      posthoc_df$pval <- NA
      posthoc_df$pval <- as.numeric(as.vector(postmodel[, (ncol(postmodel) + 1 - nrow(posthoc_df)):ncol(postmodel)]))
      posthoc_df <- posthoc_df[!posthoc_df$pval >= threshold_posthoc, ]

      if (nrow(posthoc_df) == 0) {
        posthoc_df$pval <- formatz_p(posthoc_df$pval)
      }
    }

    k <- which(variables == i)

    # Define ylim df
    Quantili <- data.frame(Tempo = unique(data[, time]),
                           Inferior = tapply(data[, i], data[, time],
                                             FUN = function (z) quantile(z, ylim[1], na.rm = T)),
                           Superior = tapply(data[, i], data[, time],
                                             FUN = function (z) quantile(z, ylim[2], na.rm = T)))

    # Define gg
    gg <- ggplot(data = data, aes_string(x = time, y = data[, i])) +
      coord_cartesian(ylim = c(min(Quantili$Inferior), max(Quantili$Superior)))

    # Add layers based on conditions
    if (col_title != 1) {
      if (group != 1) {

        colour_title <- legend_name$Colore[legend_name$new_name == i]
        gg <- gg + aes_string(colour = group, fill = group) +
          scale_color_manual(values = col_lines, drop = F) +
          scale_fill_manual(values = col_lines, drop = F, guide = FALSE)

      } else {

        colour_title <- legend_name$Colore[legend_name$new_name == i]
        gg <- gg + aes_string(colour = col_lines[1], fill = col_lines[1]) +
          scale_color_manual(values = col_lines[1], drop = F) +
          scale_fill_manual(values = col_lines[1], drop = F, guide = FALSE)

      }
    } else {
      if (group != 1) {

        colour_title <- "black"
        gg <- gg + aes_string(colour = group, fill = group) +
          scale_color_manual(values = col_lines, drop = F) +
          scale_fill_manual(values = col_lines, drop = F, guide = FALSE)

      } else {

        colour_title <- "black"
        gg <- gg + aes(colour = "forestgreen", fill = "forestgreen") +
          scale_color_manual(values = col_lines[1], drop = F) +
          scale_fill_manual(values = col_lines[1], drop = F, guide = FALSE)

      }
    }

    # Stat line and other graphical adjustments
    gg <- gg +
      stat_summary(geom = "line", fun = median, linewidth = lw_reg, show.legend = F) +
      stat_summary(geom = "ribbon", fun.min = function(z) {quantile(z, 0.25)},
                                    fun.max = function(z) {quantile(z, 0.75)},
                   linewidth = NA, alpha = 0.05, show.legend = F)

    if (check) {
      gg <- gg +
        # Every obs
        geom_point(alpha = 1, size = 1, position = position_dodge(width = 3), show.legend = F) +
        # Median points
        stat_summary(geom = "point", position = position_dodge(width = 3),
                     fun = median, show.legend = F, shape = 17, size = 4) +
        # IQR errorbars
        stat_summary(geom = "errorbar", position = position_dodge(width = 3),
                     fun.min = function(z) {quantile(z, 0.25)},
                     fun.max = function(z) {quantile(z, 0.75)},
                     show.legend = F)
    }

    if (extra) {
      gg <- gg + extra_text(i)
    }

    gg <- gg +
      labs(title = i, x = NULL, y = NULL) +
      scale_x_continuous(breaks = breaks, labels = label, guide = guide_axis(check.overlap = T)) +
      guides(color = guide_legend(override.aes = list(linewidth = 2.5, size = 5))) +
      guides(fill = guide_legend(override.aes = list(linewidth = 2.5, size = 5)))

    if (split) {
      gg <- gg + facet_grid2(cols = vars(group), scales = "fixed",
                             strip = strip_themed(background_x = element_rect(fill = "gray95"),
                                                  text_x = element_text(size = size_title_grid,
                                                                        face = "bold",
                                                                        color = "black")))
    }

    if (Friedman) {

      if (as.numeric(postmodel[, 2]) < 0.05) {
        gg <- gg +
          annotate("text", x = -Inf, y = Inf, hjust = -0.1, vjust = 1.5,
                            label = paste0("p: ", formatz_p(postmodel[, 2])), colour = "red")

      } else {

        gg <- gg +
          annotate("text", x = -Inf, y = Inf, hjust = -0.1, vjust = 1.5,
                            label = paste0("p: ", formatz_p(postmodel[, 2])), colour = "black")

      }
    }

    if (Posthoc && nrow(posthoc_df) > 0) {
      gg <- gg +
        stat_pvalue_manual(posthoc_df, label = "pval", step.increase = 0.08,
                           y.position = max(tapply(data[, i], data[, group], quantile, na.rm = T, probs = 0.75) +
                                            tapply(data[, i], data[, group], IQR, na.rm = T)))
    }

    if (grid) {

      if (k == 1) {

        gg <- gg + themegrid +
          theme(plot.title = element_text(hjust = 0.5, size = size_title_grid, vjust = -0.5,
                                          face = "bold", colour = colour_title),
                legend.justification = c(1, 1), legend.position = c(1, 1),
                legend.background = element_rect(fill = "transparent"),
                legend.title = element_blank(), legend.key = element_blank())


      } else {

        gg <- gg + themegrid +
          theme(plot.title = element_text(hjust = 0.5, size = size_title_grid, vjust = -0.5,
                                          face = "bold", colour = colour_title),
                legend.position = "none", legend.title = element_blank())

      }
    }

    if (PPTX) {

      gg <- gg + themePPTX +
        theme(plot.title = element_text(hjust = 0.5, size = 20, vjust = -0.5, face = "bold",
                                        colour = colour_title),
              legend.position = "bottom", legend.title = element_blank(),
              legend.background = element_rect(fill = "transparent"))
    }

    list_reg[[k]] <- gg
    pb$tick(1)
  }

  if (PPTX == T) {

    message("Printing PowerPoint")
    pb <- progress_bar$new(format = "[:bar] :current/:total (:percent)", total = length(variables))
    pb$tick(0)

    for (i in 1:length(list_reg)) {
      list_reg[[i]] <- rvg::dml(ggobj = list_reg[[i]])
      ppt = add_slide(ppt, layout = 'Title and Content')
      ph_with(ppt, list_reg[[i]], ph_location(width = pptx_width, height = pptx_height))

      pb$tick(1)
    }
    print(ppt, target = target)
    message("Done printing :)")
  } else {
    return(list_reg)
  }
}
