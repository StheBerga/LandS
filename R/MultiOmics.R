#' Plot assay availability in multi-omics longitudinal datasets
#'
#' Creates a dot plot describing assay availability across subjects and
#' timepoints. Each point represents the availability of a given assay
#' for a specific subject-timepoint combination.
#'
#' @param data A data frame where each row represents a unique
#'   subject-timepoint combination.
#' @param assays Character vector containing assay column names.
#' @param id Character string identifying the subject identifier variable.
#'   Defaults to `"ID"`.
#' @param time Character string identifying the timepoint variable.
#'   Defaults to `"Time"`.
#' @param metadata Optional character vector used for nested faceting and
#'  point fill.
#' @param size Numeric of size of points. Defaults to `3`.
#' @param palette Character vector containing fill colours for points.
#' @param ncol Number of facet columns. Defaults to `4`.
#'
#' @section Recommended Data Structure:
#'
#' The function assumes that each row represents a unique combination of
#' subject and timepoint.
#'
#' A complete subject-timepoint grid is recommended when visualizing
#' longitudinal assay availability, as subject-timepoint combinations
#' absent from the source dataset cannot be distinguished from omitted
#' visits.
#'
#' Example:
#'
#' \preformatted{
#' ID  Time   Assay1  Assay2  Assay3
#' 01  PRE         1       1       1
#' 01  POST        1      NA       1
#' 02  PRE         1       1       1
#' 02  POST       NA      NA      NA
#' }
#'
#' @details
#'
#' Assay columns are internally converted to binary availability
#' indicators:
#'
#' \itemize{
#'   \item non-missing value = assay available;
#'   \item missing value (`NA`) = assay unavailable.
#' }
#'
#' When metadata variables are supplied, nested subject panels are
#' generated using \code{ggh4x::facet_nested_wrap()}.

#'
#' @return
#' A ggplot object that can be further customized using standard
#' ggplot2 syntax.
#'
#' @examples
#' multiomics <- data.frame(
#'   ID = rep(c("1", "2", "3", "4"), times = 2),
#'   Time = rep(c("Pre", "Post"), each = 4),
#'   Gender = rep(c("M", "F", "F", "M"), times = 2),
#'   Assay1 = rep(1, times = 8),
#'   Assay2 = rep(c(1, NA), times = 4),
#'   Assay3 = c(rep(1, times = 4), rep(NA, times = 4))
#' )
#' multiomics$Time <- factor(multiomics$Time, levels = c("Pre", "Post"))
#'
#' # Scenario without metadata
#'
#' plot_assay_availability(
#'   data = multiomics,
#'   assays = c(
#'     "Assay1",
#'     "Assay2",
#'     "Assay3"
#'   ),
#'   id = "ID",
#'   time = "Time",
#'   size = 5,
#'   palette = "red"
#' )
#'
#' # Scenario with metadata
#'
#' plot_assay_availability(
#'   data = multiomics,
#'   assays = c(
#'     "Assay1",
#'     "Assay2",
#'     "Assay3"
#'   ),
#'   id = "ID",
#'   time = "Time",
#'   metadata = "Gender",
#'   palette = c("red", "blue")
#' )
#'
#' @export
plot_assay_availability <- function(
    data,
    assays,
    id = "ID",
    time = "Time",
    metadata = NULL,
    size = 3,
    palette = "transparent",
    ncol = 4
) {

  if (!is.data.frame(data)) {
    stop("'data' must be a data.frame.", call. = FALSE)
  }

  plot_data <- data %>%
    tidyr::pivot_longer(
      cols = dplyr::all_of(assays),
      names_to = "Assay",
      values_to = "Available"
    ) %>%
    dplyr::mutate(
      Available = !is.na(.data$Available),
      Assay = factor(
        .data$Assay
      )
    )

  p <- ggplot2::ggplot(
    plot_data,
    ggplot2::aes(
      x = .data[[time]],
      y = .data$Assay
    )
  )

  if (is.null(metadata)) {

    p <- p +
      ggplot2::geom_point(
        data = function(x) x[x$Available, ],
        fill = palette,
        size = size,
        shape = 21
      )

  } else {

    p <- p +
      ggplot2::geom_point(
        data = function(x) x[x$Available, ],
        ggplot2::aes(fill = .data[[metadata]]),
        size = size,
        shape = 21
      ) +
      ggplot2::scale_fill_manual(
        values = palette
      )

  }

  p <- p +
    ggplot2::labs(
      x = NULL,
      y = NULL
    ) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = .5),
      strip.background = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_text(
        angle = 45,
        hjust = 1
      )
    )

  if (is.null(metadata)) {

    p <- p +
      ggplot2::facet_wrap(
        stats::as.formula(
          paste("~", id)
        ),
        ncol = ncol
      )

  } else {

    facet_formula <- stats::as.formula(
      paste(paste(metadata, collapse = " + "), "~", id))

    p <- p +
      ggh4x::facet_nested_wrap(
        facet_formula,
        dir = "v",
        ncol = ncol,
        strip = ggh4x::strip_nested(
          text_x = list(ggplot2::element_blank(),
                        ggplot2::element_text()),
          background_x = list(ggplot2::element_blank(),
                              ggplot2::element_rect()),
          by_layer_x = TRUE)
      )
  }

  return(p)
}
