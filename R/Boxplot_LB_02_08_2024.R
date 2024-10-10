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
#' @param axis_y_title
#' @param axis_x_title
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
#'
#' @return Una lista di boxplot
#' @export
#'
#' @examples
Boxplot_LB <- function(data,
                       variables,
                       group,
                       ID_lines = FALSE,
                       Posthoc = FALSE,
                       Point = F,
                       Median_line = F,
                       rm.outliers = F,
                       alpha_box = 0.1,
                       width_box = 0.2,
                       size_median_line = 0.8,
                       lwd_box = 0.1,
                       lwd_ID_line = 0.2,
                       alpha_ID_line = 0.3,
                       alpha_point = 0.3,
                       size_point = 0.3,
                       Test_results = NULL,
                       threshold_posthoc = 0.1,
                       axis_y_title = NULL,
                       axis_x_title = NULL,
                       size_axis_x  = 6,
                       size_axis_y = 6,
                       ID = "ID",
                       legend_cod = NULL,
                       breaks_axis_x = levels(data[, group]),
                       labels_axis_x = levels(data[, group]),
                       grid = TRUE,
                       PPTX = FALSE,
                       pptx_width = 7.5,
                       pptx_height = 5.5,
                       extra = F,
                       extra_text = NULL,
                       palette_boxplot = c("salmon", "royalblue", "forestgreen", "gold"),
                       palette_title = "black",
                       size_title = 12,
                       target = paste0(path_out, "/Boxplot.pptx"), ratio = 1,
                       telegram = "none")
{

  if(telegram != "none"){
    start_time <<- Sys.time()
  }

  formatz_p <- function(value){
    if(is.data.frame(value)==T){
      new_frame <- value
      for(i in 1:dim(value)[1]){
        if(value[i,1]> 0.0001){
          new_frame[i,1] <- format(round(value[i,1], 4), digits = 4, nsmall = 4, width = 6, scientific=F, justify = "centre")
        }else{
          new_frame[i,1] <- "<0.0001"
        }
      }
      return(new_frame)
    }else{
      if(value > 0.0001){
        value <- format(round(value, 4), digits = 4, nsmall = 4, width = 6, scientific=F, justify = "centre")
      }else{
        value <- " <0.0001"}
      return(value)
    }
  }

  formatz_p_vett <- function(vett){
    new_vett <- c()
    for (i in 1:length(vett)) {
      p <- formatz_p(vett[i])
      new_vett <- c(new_vett,p)
    }
    return(new_vett)
  }

  theme_PPTX <- theme(axis.text.x = element_text(size= 14, colour = "black", vjust=-0.0),
                      plot.margin = margin(2, 2, 2, 2, "mm"),
                      axis.text.y = element_text(size= 14, colour = "black"),
                      panel.border = element_rect(linetype = "solid", colour = "black", linewidth =0.1, fill=NA),
                      axis.ticks = element_line(linewidth = 0.1),
                      axis.ticks.length = unit(0.25, "mm"),
                      panel.background = element_blank(),
                      panel.grid.major = element_blank(),
                      panel.grid.minor = element_blank(),
                      panel.spacing.x = unit(1.5, "mm"))
  themegrid <-
    theme(axis.text.x = element_text(size = size_axis_x, colour = "black", vjust=-0.0),
          plot.margin = margin(2, 2, 2, 2, "mm"),
          axis.text.y = element_text(size= size_axis_y, colour = "black"),
          panel.border = element_rect(linetype = "solid", colour = "black", linewidth =0.1, fill=NA),
          axis.ticks = element_line(linewidth = 0.1),
          axis.ticks.length = unit(0.25, "mm"),
          panel.background = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          legend.position = c(0.87, 0.15),
          legend.title = element_blank(),
          panel.spacing.x = unit(1.5, "mm"),
          aspect.ratio = ratio)


  boxplot <- list()
  pb <- progress_bar$new(format = "[:bar] :current/:total (:percent)", total = length(variables))
  pb$tick(0)

  for (i in variables){

    if (palette_title == 1 ){
      col_title <- legend_name$Colore[legend_name$new_name == i]
    } else {
      col_title <- "black"
    }
    # postmodel <- Test_results[Test_results[, 1] == i, ]
    # posthoc_df <- as.data.frame(t(combn(levels(data[, group]),2)))
    # colnames(posthoc_df) <- c("group1", "group2")
    # posthoc_df$y <- i
    # posthoc_df$pval <- NA
    # posthoc_df$pval <- as.numeric(as.vector(postmodel[, (ncol(postmodel)+1-nrow(posthoc_df)):ncol(postmodel)]))
    # posthoc_df <- posthoc_df[!posthoc_df$pval >= threshold_posthoc, ]
    #
    # if(nrow(posthoc_df) == 0) {}else{
    # posthoc_df$pval <- formatz_p_vett(posthoc_df$pval)
    # }

    k <- which(variables == i)

    A = tapply(data[, i], data[, group], quantile, c(0.75), na.rm = T)
    B = tapply(data[, i], data[, group], quantile, c(0.25), na.rm = T)
    C = tapply(data[, i], data[, group], IQR, na.rm = T)

    ymax <- max(A+1.5*C, na.rm = T)
    ymin <- min(B-1.5*C, na.rm = T)
    data[, "Y"] <- data[, i]

    if (rm.outliers == TRUE){
      data[, "Y"][data[, i] > ymax] <- NA
      data[, "Y"][data[, i] < ymin] <- NA
    }else{}

    boxplot[[k]] <- ggplot(data = data, aes_string(x = data[, group])) +

      geom_boxplot(aes_string(y = data[, i], fill = data[, group]),
                   width = width_box, alpha = alpha_box, outlier.shape = NA, lwd = lwd_box, show.legend = F)+

      {if (Point)
        geom_point(aes_string(y = data[, "Y"]),
                   size = size_point, alpha = alpha_point)}+

      {if (ID_lines)
        geom_line(aes_string(y = data[, i], group = ID),
                  alpha = alpha_ID_line, linewidth = lwd_ID_line, colour = "black")}+

      {if (Median_line)
        # stat_summary(aes_string(y = data[, i], group = group),
        #              geom = "line", fun = median, linewidth = 1.5, colour = "red", linetype = "dashed", show.legend = F)
        stat_summary(aes_string(y = data[, i], group = 1), geom = "line", fun = median,
                     linewidth = size_median_line, colour = "red", linetype = "solid", show.legend = F)
      }+

      {if (rm.outliers)
        coord_cartesian(ylim = c(min(data[,"Y"], na.rm = T),
                                 min(max(data[, "Y"], ymax, na.rm = T))))}+

      {if (is.null(legend_cod)){
        labs(title = i, y = axis_y_title, x = axis_x_title)}}+

      {if (!is.null(legend_cod)){
        labs(title = legend_cod$Original[legend_cod$Newname == i], y = axis_y_title, x = axis_x_title)}}+

      scale_color_manual(values = palette_boxplot)+

      scale_fill_manual(values = palette_boxplot, guide=FALSE)+

      scale_x_discrete(breaks = breaks_axis_x, labels = labels_axis_x)+


      # {if (Posthoc)
      #   if (nrow(posthoc_df) > 0)
      #     stat_pvalue_manual(posthoc_df, label = "pval",
      #                        y.position = max(tapply(data[,i], data[, group], quantile, na.rm = T, probs = 0.60)+
      #                                           tapply(data[,i], data[, group], IQR, na.rm = T)),
      #                        step.increase = 0.05)
      # }+
      #
      # {if (as.numeric(postmodel[, 2]) < 0.05)
      #   annotate("text", x = -Inf, y = Inf, hjust = -0.1, vjust = 1.5, label = paste0("p: ", formatz_p(postmodel[, 2])), colour = "red")
      # }+
      #
      # {if (as.numeric(postmodel[, 2]) >= 0.05)
      #   annotate("text", x = -Inf, y = Inf, hjust = -0.1, vjust = 1.5, label = paste0("p: ", formatz_p(postmodel[, 2])), colour = "black")
      # }+

      {if (extra)
        extra_text(i)}+

      {if (grid)
        themegrid +
          theme(plot.title = element_text(hjust = 0.5, size= size_title, vjust = -0.5,
                                          face = "bold",
                                          colour = col_title))
      }+
      {if (PPTX)
        theme_PPTX +
          theme(plot.title = element_text(hjust = 0.5, size= 20, vjust = -0.5,
                                          face = "bold",
                                          colour = col_title))
      }
    Sys.sleep(0.01)
    pb$tick(1)
    pb
  }
  if (PPTX == T){
    ppt <- read_pptx()
    message("Printing PowerPoint")
    pb <- progress_bar$new(format = "[:bar] :current/:total (:percent)", total = length(variables))
    pb$tick(0)

    for (i in 1: length(boxplot)){
      boxplot[[i]] <- rvg::dml(ggobj = boxplot[[i]])
      ppt = add_slide(ppt, layout = 'Title and Content')
      ph_with(ppt, boxplot[[i]], ph_location(width = pptx_width, height = pptx_height))

      pb$tick(1)
      pb
    }
    print(ppt, target = target)
    message("Done printing :)")

    if(telegram != "none"){
      telegram_mess_LB(dest = telegram, script = "Boxplot")
    }

  } else {

    if(telegram != "none"){
      telegram_mess_LB(dest = telegram, script = "Boxplot")
    }

    return(boxplot)
  }


}

