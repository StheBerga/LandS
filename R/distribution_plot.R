#' Plot the distribution of a numeric variable
#'
#' @description
#' Visualizes a numeric vector using either a horizontal point plot,
#' a vertical boxplot with individual observations, or a kernel density plot.
#' When a cutoff is provided, points are coloured according to their
#' relationship to the cutoff. In density mode, the two areas under a single
#' estimated density curve are coloured separately.
#'
#' @param x A one-dimensional numeric vector.
#' @param cutoff NULL (default) or a finite numeric threshold.
#' @param type One of "points" (default), "boxplot", or "density".
#' @param palette One colour without cutoff; two colours with cutoff.
#' @param point_size Point size (default 2).
#' @param point_alpha Point transparency (default 0.7).
#' @param point_jitter Perpendicular jitter (default 0.08).
#' @param box_width Boxplot width (default 0.20).
#' @param box_fill Boxplot fill (default "grey85").
#' @param cutoff_colour Cutoff line colour (default "grey30").
#' @param cutoff_linetype Cutoff line type (default "dashed").
#' @param density_adjust Kernel density bandwidth multiplier (default 1).
#'   Larger values give smoother density estimates.
#' @param density_alpha Density area transparency (default 0.5).
#' @param legend Show cutoff legend (default FALSE).
#' @param title Optional plot title.
#' @param xlab Optional numeric variable label.
#'
#' @return A ggplot object.
#'
#' @details
#' Non-finite observations are removed with a warning.
#' Density plots require at least three finite observations and two
#' distinct values. The density is always estimated on the full sample.
#' The cutoff only changes the area colours, without fitting separate
#' densities for the two subsets.
#'
#' @author Luca Lalli, Stefano Bergamini
#' @export
#'
#' @examples
#' distribution_plot(mtcars$mpg)
#' distribution_plot(mtcars$mpg, cutoff = 23)
#' distribution_plot(mtcars$mpg, type = "boxplot", cutoff = 23)
#' distribution_plot(mtcars$mpg, type = "density")
#' distribution_plot(mtcars$mpg, type = "density", cutoff = 23)

distribution_plot <- function(
    x,
    cutoff = NULL,
    type = c("points", "boxplot", "density"),
    palette = NULL,
    point_size = 2,
    point_alpha = 0.7,
    point_jitter = 0.08,
    box_width = 0.20,
    box_fill = "grey85",
    cutoff_colour = "grey30",
    cutoff_linetype = "dashed",
    density_adjust = 1,
    density_alpha = 0.5,
    legend = FALSE,
    title = NULL,
    xlab = NULL) {

  type <- match.arg(type)
  x_expression <- paste(deparse(substitute(x)), collapse = "")

  if (!is.numeric(x) || is.factor(x) || !is.null(dim(x))) {
    stop("'x' must be a one-dimensional numeric vector.",
         call. = FALSE)
  }

  if (!is.null(cutoff) &&
      (!is.numeric(cutoff) || length(cutoff) != 1L ||
       !is.finite(cutoff))) {
    stop("'cutoff' must be NULL or one finite numeric value.",
         call. = FALSE)
  }

  check_number <- function(z, name, min = 0, max = Inf,
                           strictly_positive = FALSE) {

    if (!is.numeric(z) || length(z) != 1L || !is.finite(z) ||
        (if (strictly_positive) z <= min else z < min) ||
        z > max) {
      stop("Invalid value for '", name, "'.", call. = FALSE)
    }
  }

  check_number(point_size, "point_size", strictly_positive = TRUE)
  check_number(point_alpha, "point_alpha", max = 1)
  check_number(point_jitter, "point_jitter", max = 0.3)
  check_number(box_width, "box_width", max = 0.9,
               strictly_positive = TRUE)
  check_number(density_adjust, "density_adjust",
               strictly_positive = TRUE)
  check_number(density_alpha, "density_alpha", max = 1)

  if (!is.logical(legend) || length(legend) != 1L ||
      is.na(legend)) {
    stop("'legend' must be TRUE or FALSE.", call. = FALSE)
  }

  for (nm in c("title", "xlab")) {
    z <- get(nm)

    if (!is.null(z) &&
        (!is.character(z) || length(z) != 1L || is.na(z))) {
      stop("'", nm, "' must be NULL or a single character string.",
           call. = FALSE)
    }
  }

  if (!is.character(cutoff_linetype) ||
      length(cutoff_linetype) != 1L ||
      is.na(cutoff_linetype)) {
    stop("'cutoff_linetype' must be one valid linetype string.",
         call. = FALSE)
  }

  valid <- is.finite(x)

  if (any(!valid)) {
    warning(sum(!valid), " non-finite observation(s) removed.",
            call. = FALSE)
  }

  x <- x[valid]

  if (length(x) == 0L) {
    stop("No finite observations to plot.", call. = FALSE)
  }

  if (type == "density" &&
      (length(x) < 3L || length(unique(x)) < 2L)) {
    stop(
      "A density plot requires at least 3 finite observations ",
      "and 2 distinct values.",
      call. = FALSE
    )
  }

  if (is.null(palette)) {
    palette <- if (is.null(cutoff)) {
      "black"
    } else {
      c("salmon", "royalblue")
    }
  }

  n_colours <- if (is.null(cutoff)) 1L else 2L

  if (!is.character(palette) || anyNA(palette) ||
      length(palette) != n_colours) {
    stop("'palette' must contain ", n_colours, " colour(s).",
         call. = FALSE)
  }

  for (colour in c(palette, box_fill, cutoff_colour)) {
    tryCatch(
      grDevices::col2rgb(colour),
      error = function(e) {
        stop("Invalid colour: ", colour, call. = FALSE)
      }
    )
  }

  if (is.null(xlab)) xlab <- x_expression

  cut_levels <- c("At or below", "Above")

  cut_labels <- if (is.null(cutoff)) {
    NULL
  } else {
    c(paste0("<= ", cutoff), paste0("> ", cutoff))
  }

  plot_data <- data.frame(value = x)

  if (!is.null(cutoff)) {
    plot_data$cut_group <- factor(
      ifelse(plot_data$value <= cutoff,
             cut_levels[1], cut_levels[2]),
      levels = cut_levels
    )
  }

  if (type == "points") {

    p <- ggplot2::ggplot(
      plot_data,
      ggplot2::aes(x = value, y = 0)
    ) +
      ggplot2::theme_classic(base_size = 11)

    if (!is.null(cutoff)) {
      p <- p + ggplot2::geom_vline(
        xintercept = cutoff,
        colour = cutoff_colour,
        linetype = cutoff_linetype,
        linewidth = 0.5
      )
    }

    jitter_position <- ggplot2::position_jitter(
      width = 0,
      height = point_jitter,
      seed = 123
    )

    if (is.null(cutoff)) {

      p <- p + ggplot2::geom_point(
        position = jitter_position,
        colour = palette[1],
        size = point_size,
        alpha = point_alpha
      )

    } else {

      p <- p + ggplot2::geom_point(
        ggplot2::aes(colour = cut_group),
        position = jitter_position,
        size = point_size,
        alpha = point_alpha
      )
    }

    p <- p +
      ggplot2::scale_y_continuous(
        breaks = NULL,
        limits = c(-0.4, 0.4),
        expand = ggplot2::expansion(mult = 0)
      ) +
      ggplot2::labs(
        x = xlab,
        y = NULL,
        title = title
      ) +
      ggplot2::theme(
        axis.line.y = ggplot2::element_blank(),
        axis.ticks.y = ggplot2::element_blank(),
        axis.text.y = ggplot2::element_blank()
      )

  } else if (type == "boxplot") {

    p <- ggplot2::ggplot(
      plot_data,
      ggplot2::aes(x = 1, y = value)
    ) +
      ggplot2::theme_classic(base_size = 11) +
      ggplot2::geom_boxplot(
        ggplot2::aes(group = 1),
        orientation = "x",
        width = box_width,
        outlier.shape = NA,
        colour = "grey25",
        fill = box_fill,
        alpha = 0.8
      )

    if (!is.null(cutoff)) {
      p <- p + ggplot2::geom_hline(
        yintercept = cutoff,
        colour = cutoff_colour,
        linetype = cutoff_linetype,
        linewidth = 0.5
      )
    }

    jitter_position <- ggplot2::position_jitter(
      width = point_jitter,
      height = 0,
      seed = 123
    )

    if (is.null(cutoff)) {

      p <- p + ggplot2::geom_point(
        position = jitter_position,
        colour = palette[1],
        size = point_size,
        alpha = point_alpha
      )

    } else {

      p <- p + ggplot2::geom_point(
        ggplot2::aes(colour = cut_group),
        position = jitter_position,
        size = point_size,
        alpha = point_alpha
      )
    }

    p <- p +
      ggplot2::scale_x_continuous(
        breaks = NULL,
        limits = c(0.5, 1.5),
        expand = ggplot2::expansion(mult = 0)
      ) +
      ggplot2::labs(
        x = NULL,
        y = xlab,
        title = title
      ) +
      ggplot2::theme(
        axis.line.x = ggplot2::element_blank(),
        axis.ticks.x = ggplot2::element_blank(),
        axis.text.x = ggplot2::element_blank()
      )

  } else {

    kde <- stats::density(
      x,
      adjust = density_adjust,
      n = 1024L
    )

    density_data <- data.frame(
      value = kde$x,
      density = kde$y
    )

    p <- ggplot2::ggplot() +
      ggplot2::theme_classic(base_size = 11)

    if (is.null(cutoff)) {

      p <- p +
        ggplot2::geom_area(
          data = density_data,
          ggplot2::aes(x = value, y = density),
          stat = "identity",
          fill = palette[1],
          alpha = density_alpha
        ) +
        ggplot2::geom_line(
          data = density_data,
          ggplot2::aes(x = value, y = density),
          colour = palette[1],
          linewidth = 0.6
        )

    } else {

      left <- density_data[
        density_data$value <= cutoff, , drop = FALSE
      ]

      right <- density_data[
        density_data$value >= cutoff, , drop = FALSE
      ]

      if (cutoff > min(density_data$value) &&
          cutoff < max(density_data$value) &&
          !any(density_data$value == cutoff)) {

        cut_height <- stats::approx(
          x = density_data$value,
          y = density_data$density,
          xout = cutoff
        )$y

        boundary <- data.frame(
          value = cutoff,
          density = cut_height
        )

        left <- rbind(left, boundary)
        right <- rbind(boundary, right)
      }

      area_parts <- list()

      if (nrow(left) >= 2L) {
        left$cut_group <- cut_levels[1]
        area_parts[[length(area_parts) + 1L]] <- left
      }

      if (nrow(right) >= 2L) {
        right$cut_group <- cut_levels[2]
        area_parts[[length(area_parts) + 1L]] <- right
      }

      area_data <- do.call(rbind, area_parts)

      area_data$cut_group <- factor(
        area_data$cut_group,
        levels = cut_levels
      )

      p <- p +
        ggplot2::geom_area(
          data = area_data,
          ggplot2::aes(
            x = value,
            y = density,
            fill = cut_group,
            group = cut_group
          ),
          stat = "identity",
          position = "identity",
          alpha = density_alpha,
          show.legend = legend
        ) +
        ggplot2::geom_line(
          data = density_data,
          ggplot2::aes(x = value, y = density),
          colour = "grey25",
          linewidth = 0.6
        ) +
        ggplot2::geom_vline(
          xintercept = cutoff,
          colour = cutoff_colour,
          linetype = cutoff_linetype,
          linewidth = 0.5
        ) +
        ggplot2::scale_fill_manual(
          values = stats::setNames(
            palette, cut_levels
          ),
          breaks = cut_levels,
          labels = cut_labels,
          name = "Cutoff",
          drop = FALSE
        )
    }

    p <- p +
      ggplot2::labs(
        x = xlab,
        y = "Density",
        title = title
      )
  }

  if (!is.null(cutoff) && type != "density") {

    p <- p +
      ggplot2::scale_colour_manual(
        values = stats::setNames(
          palette, cut_levels
        ),
        breaks = cut_levels,
        labels = cut_labels,
        name = "Cutoff",
        drop = FALSE
      )
  }

  # Final theme

  p +
    ggplot2::theme(
      legend.position = if (is.null(cutoff) || !legend) {
        "none"
      } else {
        "bottom"
      },
      plot.title = ggplot2::element_text(
        hjust = 0.5,
        face = "bold"
      ),
      panel.background = ggplot2::element_rect(
        fill = "transparent",
        colour = NA
      ),
      plot.background = ggplot2::element_rect(
        fill = "transparent",
        colour = NA
      )
    )
}
