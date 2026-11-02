#' LandS grid Theme
#'
#' A lightweight ggplot2 theme designed for LandS graphics. The theme applies a
#' clean appearance with bordered panels, transparent backgrounds, customizable
#' title formatting, and compact spacing.
#'
#' @param ... Additional arguments used to customize theme elements:
#' \describe{
#'   \item{size_title}{Numeric. Font size of the plot title (Default = 8).}
#'   \item{size_axis_x}{Numeric. Font size of x-axis tick labels (Default = 6).}
#'   \item{size_axis_y}{Numeric. Font size of y-axis tick labels (Default = 6).}
#'   \item{ratio}{Numeric. Aspect ratio of the plot (Default = 1).}
#'   \item{fill.title}{Character. Fill color of the title background box (Default = \code{"transparent"}).}
#'   \item{alpha_fill_title}{Numeric between 0 and 1. Transparency applied
#'   to the title background fill (Default = 0.2).}
#' }
#'
#' @return A \code{ggplot2} theme object that can be added to a plot with
#' \code{+}.
#'
#' @details The theme uses \code{ggtext::element_textbox_simple()} for
#' formatting the plot title, allowing a boxed title with optional background
#' color and transparency. Panel and plot backgrounds are set to transparent.
#'
#' @export
#'
#' @examples
#' library(ggplot2)
#'
#' ggplot(mtcars, aes(wt, mpg)) +
#'   geom_point() +
#'   labs(title = "Fuel efficiency by vehicle weight") +
#'   theme_lands_grid()
#'
theme_lands_grid <- function (...) {

  size_title <- list(...)$size_title
  size_axis_x <- list(...)$size_axis_x
  size_axis_y <- list(...)$size_axis_y
  ratio <- list(...)$ratio
  fill.title <- list(...)$fill.title
  alpha_fill_title <- list(...)$alpha_fill_title

  list_param <- list(
    size_title = ifelse(!is.null(size_title), size_title, 8),
    size_axis_x = ifelse(!is.null(size_axis_x), size_axis_x, 6),
    size_axis_y = ifelse(!is.null(size_axis_y), size_axis_y, 6),
    ratio = ifelse(!is.null(ratio), ratio, 1),
    fill.title = fill.title,
    alpha_fill_title = ifelse(!is.null(alpha_fill_title), alpha_fill_title, 0.2)
  )

  ggplot2::theme(
    plot.title = ggtext::element_textbox_simple(
      size = list_param[["size_title"]], box.colour = "black", face = "bold",
      linewidth = .1, linetype = 1,
      hjust = 0, halign = .5,
      padding = ggplot2::margin(6, 5, 4, 5),
      margin = ggplot2::margin(0, 0, 0, 0),
      fill = scales::alpha(list_param[["fill.title"]], list_param[["alpha_fill_title"]])),
    axis.text.x = ggplot2::element_text(size = list_param[["size_axis_x"]], colour = "black", vjust = -0),
    plot.margin = ggplot2::margin(2, 2, 2, 2, "mm"),
    axis.text.y = ggplot2::element_text(size = list_param[["size_axis_y"]], colour = "black"),
    panel.border = ggplot2::element_rect(linetype = "solid", colour = "black", linewidth = 0.1, fill = NA),
    axis.ticks = ggplot2::element_line(linewidth = 0.1),
    axis.ticks.length = grid::unit(0.25, "mm"),
    panel.background = ggplot2::element_rect(fill = "transparent"),
    plot.background = ggplot2::element_rect(fill = "transparent"),
    panel.grid.major = ggplot2::element_blank(),
    panel.grid.minor = ggplot2::element_blank(),
    legend.title = ggplot2::element_blank(),
    legend.position = "none",
    panel.spacing.x = grid::unit(1.5, "mm"),
    aspect.ratio = list_param[["ratio"]])
}

#' LandS pptx Theme
#'
#' A lightweight ggplot2 theme designed for LandS graphics. The theme applies a
#' clean appearance with bordered panels, transparent backgrounds, customizable
#' title formatting, and compact spacing, ideal for PowerPoint export
#'
#' @param ... Additional arguments used to customize theme elements:
#' \describe{
#'   \item{size_title}{Numeric. Font size of the plot title (Default = 20).}
#'   \item{size_axis_x}{Numeric. Font size of x-axis tick labels (Default = 14).}
#'   \item{size_axis_y}{Numeric. Font size of y-axis tick labels (Default = 14).}
#'   \item{fill.title}{Character. Fill color of the title background box (Default = \code{"transparent"}).}
#'   \item{alpha_fill_title}{Numeric between 0 and 1. Transparency applied
#'   to the title background fill (Default = 0.2).}
#' }
#'
#' @return A \code{ggplot2} theme object that can be added to a plot with
#' \code{+}.
#'
#' @details The theme uses \code{ggtext::element_textbox_simple()} for
#' formatting the plot title, allowing a boxed title with optional background
#' color and transparency. Panel and plot backgrounds are set to transparent.
#'
#' @export
#'
#' @examples
#' library(ggplot2)
#'
#' ggplot(mtcars, aes(wt, mpg)) +
#'   geom_point() +
#'   labs(title = "Fuel efficiency by vehicle weight") +
#'   theme_lands_grid()
#'
theme_lands_pptx <- function(...) {
  size_title <- list(...)$size_title
  size_axis_x <- list(...)$size_axis_x
  size_axis_y <- list(...)$size_axis_y
  ratio <- list(...)$ratio
  fill.title <- list(...)$fill.title
  alpha_fill_title <- list(...)$alpha_fill_title

  list_param <- list(
    size_title = ifelse(!is.null(size_title), size_title, 20),
    size_axis_x = ifelse(!is.null(size_axis_x), size_axis_x, 14),
    size_axis_y = ifelse(!is.null(size_axis_y), size_axis_y, 14),
    fill.title = fill.title,
    alpha_fill_title = ifelse(!is.null(alpha_fill_title), alpha_fill_title, 0.2)
  )

  ggplot2::theme(
    plot.title = ggtext::element_textbox_simple(
      size = list_param[["size_title"]], box.colour = "black", face = "bold",
      linewidth = .1, linetype = 1,
      hjust = 0, halign = .5,
      padding = ggplot2::margin(6, 5, 4, 5),
      margin = ggplot2::margin(0, 0, 0, 0),
      fill = scales::alpha(list_param[["fill.title"]], list_param[["alpha_fill_title"]])),
    axis.text.x = ggplot2::element_text(size = list_param[["size_axis_x"]], colour = "black", vjust = -0),
    plot.margin = ggplot2::margin(2, 2, 2, 2, "mm"),
    axis.text.y = ggplot2::element_text(size = list_param[["size_axis_y"]], colour = "black"),
    panel.border = ggplot2::element_rect(linetype = "solid", colour = "black", linewidth = 0.1, fill = NA),
    axis.ticks = ggplot2::element_line(linewidth = 0.1),
    axis.ticks.length = grid::unit(0.25, "mm"),
    panel.background = ggplot2::element_rect(fill = "transparent"),
    plot.background = ggplot2::element_rect(fill = "transparent"),
    panel.grid.major = ggplot2::element_blank(),
    panel.grid.minor = ggplot2::element_blank(),
    legend.title = ggplot2::element_blank(),
    legend.position = "none",
    panel.spacing.x = grid::unit(1.5, "mm"),
    aspect.ratio = list_param[["ratio"]])
}


#' Set Default Graphical Parameters for grid plotting
#'
#' Internal utility function that collects graphical parameters supplied through
#' `...` and returns a list of plotting options with package defaults applied
#' when arguments are not provided.
#'
#' @param ...
#' Additional graphical parameters:
#' \describe{
#'   \item{alpha_id_lines}{Numeric. Transparency of subject-specific lines (Default = 0.3).}
#'   \item{lwd_id_lines}{Numeric. Line width of subject-specific lines (Default = 0.2).}
#'   \item{alpha_point}{Numeric. Transparency of plotted points (Default = 0.3).}
#'   \item{size_point}{Numeric. Point size (Default = 0.3).}
#'   \item{size_label_title}{Numeric. Font size used for variable labels or
#'   panel titles(Default = 2.5).}
#'   \item{breaks}{Numeric or character vector specifying x-axis break
#'   positions(Default = NULL).}
#'   \item{labels}{Character or numeric vector specifying x-axis labels.
#'   Must have the same length as `breaks`(Default = NULL`.}
#'   \item{posthoc_test_size}{Numeric. Text size used for post-hoc test
#'   annotations(Default = 3.88).}
#'   \item{bracket_shorten}{Numeric. Amount by which statistical brackets are
#'   shortened(Default = 0).}
#'   \item{bracket.nudge.y}{Numeric. Vertical adjustment applied to
#'   statistical brackets(Default = 0).}
#'   \item{lwd_statistic}{Numeric. Line width of statistical annotation
#'   brackets(Default = 1).}
#'   \item{col_statistic}{Character. Color used for statistical annotations (Default = "red").}
#' }
#'
#' @return
#' A named list containing plotting parameters with user-supplied values
#' replacing package defaults.
#'
#' @details
#' This function is intended for internal use by plotting functions to ensure a
#' consistent graphical appearance while allowing selected aesthetics to be
#' customized through `...`.
#'
#' @keywords internal
set_params_grid <- function(...){
  alpha_id_lines <- list(...)$alpha_id_lines
  lwd_id_lines <- list(...)$lwd_id_lines
  alpha_point <- list(...)$alpha_point
  size_point <- list(...)$size_point
  size_label_title <- list(...)$size_label_title
  breaks_axis <- list(...)$breaks
  labels_axis <- list(...)$labels
  posthoc_test_size <- list(...)$posthoc_test_size
  bracket_shorten <- list(...)$bracket_shorten
  bracket.nudge.y <- list(...)$bracket.nudge.y
  lwd_statistic <- list(...)$lwd_statistic
  col_statistic <- list(...)$col_statistic

  list_param <- list(
    alpha_id_lines = ifelse(!is.null(alpha_id_lines), alpha_id_lines, 0.3),
    lwd_id_lines = ifelse(!is.null(lwd_id_lines), lwd_id_lines, 0.2),
    alpha_point = ifelse(!is.null(alpha_point), alpha_point, 0.3),
    size_point = ifelse(!is.null(size_point), size_point, 0.3),
    size_label_title = ifelse(!is.null(size_label_title), size_label_title, 2.5),
    breaks = NA,
    labels = NA,
    posthoc_test_size = ifelse(!is.null(posthoc_test_size), posthoc_test_size, 3.88),
    bracket_shorten = ifelse(!is.null(bracket_shorten), bracket_shorten, 0),
    bracket.nudge.y = ifelse(!is.null(bracket.nudge.y), bracket.nudge.y, 0),
    lwd_statistic = ifelse(!is.null(lwd_statistic), lwd_statistic, 1),
    col_statistic = ifelse(!is.null(col_statistic), col_statistic, "red")
  )
  # This is a little confusing I don't get why can't I assign it directly,
  # but this is the only way it seems to work
  if (!is.null(breaks_axis)) {
    list_param[["breaks"]] <- breaks_axis
  }
  if (!is.null(labels_axis)) {
    list_param[["labels"]] <- labels_axis
  }
  return(list_param)
}

#' Set Default Graphical Parameters for pptx plotting
#'
#' Internal utility function that collects graphical parameters supplied through
#' `...` and returns a list of plotting options with package defaults applied
#' when arguments are not provided.
#'
#' @param ...
#' Additional graphical parameters:
#' \describe{
#'   \item{alpha_id_lines}{Numeric. Transparency of subject-specific lines (Default = 0.3).}
#'   \item{lwd_id_lines}{Numeric. Line width of subject-specific lines (Default = 0.2).}
#'   \item{alpha_point}{Numeric. Transparency of plotted points (Default = 0.3).}
#'   \item{size_point}{Numeric. Point size (Default = 0.3).}
#'   \item{size_label_title}{Numeric. Font size used for variable labels or
#'   panel titles(Default = 2.5).}
#'   \item{breaks}{Numeric or character vector specifying x-axis break
#'   positions(Default = NULL).}
#'   \item{labels}{Character or numeric vector specifying x-axis labels.
#'   Must have the same length as `breaks`(Default = NULL`.}
#'   \item{posthoc_test_size}{Numeric. Text size used for post-hoc test
#'   annotations(Default = 3.88).}
#'   \item{bracket_shorten}{Numeric. Amount by which statistical brackets are
#'   shortened(Default = 0).}
#'   \item{bracket.nudge.y}{Numeric. Vertical adjustment applied to
#'   statistical brackets(Default = 0).}
#'   \item{lwd_statistic}{Numeric. Line width of statistical annotation
#'   brackets(Default = 1).}
#'   \item{col_statistic}{Character. Color used for statistical annotations (Default = "red").}
#'   \item{pptx_width}{Numeric. Width in inches of the plot for PowerPoint (Default = 8.5).}
#'   \item{pptx_height}{Numeric. Height in inches of the plot for PowerPoint (Default = 5.5).}
#' }
#'
#' @return
#' A named list containing plotting parameters with user-supplied values
#' replacing package defaults.
#'
#' @details
#' This function is intended for internal use by plotting functions to ensure a
#' consistent graphical appearance while allowing selected aesthetics to be
#' customized through `...`.
#'
#' @keywords internal
set_params_pptx <- function(...){
  alpha_id_lines <- list(...)$alpha_id_lines
  lwd_id_lines <- list(...)$lwd_id_lines
  alpha_point <- list(...)$alpha_point
  size_point <- list(...)$size_point
  size_label_title <- list(...)$size_label_title
  breaks_axis <- list(...)$breaks
  labels_axis <- list(...)$labels
  posthoc_test_size <- list(...)$posthoc_test_size
  bracket_shorten <- list(...)$bracket_shorten
  bracket.nudge.y <- list(...)$bracket.nudge.y
  lwd_statistic <- list(...)$lwd_statistic
  col_statistic <- list(...)$col_statistic
  pptx_width <- list(...)$pptx_width
  pptx_height <- list(...)$pptx_height

  list_param <- list(
    alpha_id_lines = ifelse(!is.null(alpha_id_lines), alpha_id_lines, 0.3),
    lwd_id_lines = ifelse(!is.null(lwd_id_lines), lwd_id_lines, 0.2),
    alpha_point = ifelse(!is.null(alpha_point), alpha_point, 0.3),
    size_point = ifelse(!is.null(size_point), size_point, 0.3),
    size_label_title = ifelse(!is.null(size_label_title), size_label_title, 2.5),
    breaks = NA,
    labels = NA,
    posthoc_test_size = ifelse(!is.null(posthoc_test_size), posthoc_test_size, 3.88),
    bracket_shorten = ifelse(!is.null(bracket_shorten), bracket_shorten, 0),
    bracket.nudge.y = ifelse(!is.null(bracket.nudge.y), bracket.nudge.y, 0),
    lwd_statistic = ifelse(!is.null(lwd_statistic), lwd_statistic, 1),
    col_statistic = ifelse(!is.null(col_statistic), col_statistic, "red"),
    pptx_width = ifelse(!is.null(pptx_width), pptx_width, 8.5),
    pptx_height = ifelse(!is.null(pptx_height), pptx_height, 5.5)
  )
  # This is a little confusing I don't get why can't I assign it directly,
  # but this is the only way it seems to work
  if (!is.null(breaks_axis)) {
    list_param[["breaks"]] <- breaks_axis
  }
  if (!is.null(labels_axis)) {
    list_param[["labels"]] <- labels_axis
  }
  return(list_param)
}
