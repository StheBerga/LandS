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
#' @param metadata Optional character string identifying one metadata variable
#'   used for nested faceting and point fill.
#' @param size Numeric of size of points. Defaults to `3`.
#' @param palette Character vector containing fill colours for points. It must
#'   contain one colour when `metadata = NULL`; otherwise, it must provide a
#'   colour for each observed metadata level.
#' @param ncol Number of facet columns. Defaults to `4`.
#'
#' @section Recommended Data Structure:
#'
#' Each row must represent a unique combination of subject and timepoint.
#' When supplied, metadata must be constant within each subject.
#'
#' A complete subject-timepoint grid is recommended when visualizing
#' longitudinal assay availability, as subject-timepoint combinations
#' absent from the source dataset cannot be distinguished from omitted
#' timepoints.
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
#' When metadata variables are supplied, subject panels are grouped and
#' labelled hierarchically by their metadata values using
#' \code{ggh4x::facet_nested_wrap()}.
#'
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

  if (!is.character(assays) || length(assays) == 0L ||
      anyNA(assays) || any(!nzchar(assays))) {
    stop("'assays' must be a non-empty character vector of column names.",
         call. = FALSE)
  }

  if (!is.character(id) || length(id) != 1L || is.na(id) || !nzchar(id)) {
    stop("'id' must name exactly one column.", call. = FALSE)
  }

  if (!is.character(time) || length(time) != 1L || is.na(time) || !nzchar(time)) {
    stop("'time' must name exactly one column.", call. = FALSE)
  }

  if (!is.null(metadata) &&
      (!is.character(metadata) || length(metadata) != 1L ||
       is.na(metadata) || !nzchar(metadata))) {
    stop("'metadata' must be NULL or name exactly one column.", call. = FALSE)
  }

  if (anyDuplicated(assays)) {
    stop("'assays' must not contain duplicate names.", call. = FALSE)
  }

  required_columns <- c(id, time, assays, metadata)
  missing_columns <- setdiff(required_columns, names(data))

  if (length(missing_columns) > 0L) {
    stop(
      "The following columns were not found in 'data': ",
      paste(missing_columns, collapse = ", "),
      call. = FALSE
    )
  }

  if (any(assays %in% c(id, time, metadata))) {
    stop("'assays' cannot include the ID, time, or metadata columns.",
         call. = FALSE)
  }

  if (!is.character(palette) || length(palette) == 0L || anyNA(palette)) {
    stop("'palette' must be a non-empty character vector of colours.",
         call. = FALSE)
  }

  if (is.null(metadata) && length(palette) != 1L) {
    stop("'palette' must contain exactly one colour when 'metadata' is NULL.",
         call. = FALSE)
  }

  if (anyDuplicated(data[c(id, time)])) {
    stop("Each ID-time combination must occur at most once.", call. = FALSE)
  }

  if (!is.null(metadata)) {
    metadata_per_id <- data %>%
      dplyr::group_by(.data[[id]]) %>%
      dplyr::summarise(
        n_metadata = dplyr::n_distinct(.data[[metadata]], na.rm = FALSE),
        .groups = "drop"
      )

    if (any(metadata_per_id$n_metadata > 1L)) {
      stop("'metadata' must be constant within each subject.", call. = FALSE)
    }

    metadata_levels <- unique(data[[metadata]][!is.na(data[[metadata]])])

    if (is.null(names(palette)) && length(palette) < length(metadata_levels)) {
      stop("'palette' must provide at least one colour for each metadata level.",
           call. = FALSE)
    }

    if (!is.null(names(palette))) {
      missing_colours <- setdiff(as.character(metadata_levels), names(palette))

      if (length(missing_colours) > 0L) {
        stop("'palette' is missing colours for: ",
             paste(missing_colours, collapse = ", "), call. = FALSE)
      }
    }
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

#' Plot assay availability as a heatmap
#'
#' Creates a heatmap showing whether each selected assay is available for each
#' subject-timepoint combination. An assay is available when its value is not
#' missing.
#'
#' @param data A data frame where each row represents a unique
#'   subject-timepoint combination.
#' @param assays Character vector containing assay column names.
#' @param id Character string identifying the subject identifier variable.
#'   Defaults to `"ID"`.
#' @param time Character string identifying the timepoint variable.
#'   Defaults to `"Time"`.
#' @param by_time Logical value indicating the facet variable. When `TRUE`,
#'   the heatmap is faceted by timepoint and assays are shown on the x-axis.
#'   When `FALSE`, it is faceted by assay and timepoints are shown on the
#'   x-axis. Defaults to `TRUE`.
#' @param nrow Optional positive whole number specifying the number of facet
#'   rows. Defaults to `NULL` to let ggplot2 determine the number.
#' @param ncol Optional positive whole number specifying the number of facet
#'   columns. Defaults to `NULL`, which lets ggplot2 determine the number.
#'
#' @aliases Jessica Gliozzo
#'
#' @return
#' A ggplot object that can be further customized using standard ggplot2
#' syntax.
#'
#' @examples
#' multiomics <- data.frame(
#'   ID = rep(c("1", "2", "3"), times = 2),
#'   Time = rep(c("Pre", "Post"), each = 3),
#'   Assay1 = c(1, 1, NA, 1, NA, 1),
#'   Assay2 = c(NA, 1, 1, 1, 1, NA)
#' )
#'
#' plot_assays_avaliability_heatmap(
#'   data = multiomics,
#'   assays = c("Assay1", "Assay2")
#' )
#'
#' plot_assays_avaliability_heatmap(
#'   data = multiomics,
#'   assays = c("Assay1", "Assay2"),
#'   by_time = FALSE
#' )
#'
#' @export
plot_assays_avaliability_heatmap <- function(
    data,
    assays,
    id = "ID",
    time = "Time",
    by_time = TRUE,
    nrow = NULL,
    ncol = NULL
) {

  if (!is.data.frame(data)) {
    stop("'data' must be a data.frame.", call. = FALSE)
  }

  if (!is.character(assays) || length(assays) == 0L ||
      anyNA(assays) || any(!nzchar(assays))) {
    stop("'assays' must be a non-empty character vector of column names.",
         call. = FALSE)
  }

  if (!is.character(id) || length(id) != 1L || is.na(id) || !nzchar(id)) {
    stop("'id' must name exactly one column.", call. = FALSE)
  }

  if (!is.character(time) || length(time) != 1L || is.na(time) || !nzchar(time)) {
    stop("'time' must name exactly one column.", call. = FALSE)
  }

  if (!is.logical(by_time) || length(by_time) != 1L || is.na(by_time)) {
    stop("'by_time' must be TRUE or FALSE.", call. = FALSE)
  }

  if (!is.null(nrow) &&
      (!is.numeric(nrow) || length(nrow) != 1L || is.na(nrow) ||
       !is.finite(nrow) || nrow <= 0 || nrow != as.integer(nrow))) {
    stop("'nrow' must be NULL or a positive whole number.", call. = FALSE)
  }

  if (!is.null(ncol) &&
      (!is.numeric(ncol) || length(ncol) != 1L || is.na(ncol) || !is.finite(ncol) ||
       ncol <= 0 || ncol != as.integer(ncol))) {
    stop("'ncol' must be NULL or a positive whole number.", call. = FALSE)
  }

  if (anyDuplicated(assays)) {
    stop("'assays' must not contain duplicate names.", call. = FALSE)
  }

  required_columns <- c(id, time, assays)
  missing_columns <- setdiff(required_columns, names(data))

  if (length(missing_columns) > 0L) {
    stop(
      "The following columns were not found in 'data': ",
      paste(missing_columns, collapse = ", "),
      call. = FALSE
    )
  }

  if (any(assays %in% c(id, time))) {
    stop("'assays' cannot include the ID or time columns.", call. = FALSE)
  }

  if (anyDuplicated(data[c(id, time)])) {
    stop("Each ID-time combination must occur at most once.", call. = FALSE)
  }

  plot_data <- data %>%
    tidyr::pivot_longer(
      cols = dplyr::all_of(assays),
      names_to = "Assay",
      values_to = "AvailabilityValue"
    ) %>%
    dplyr::mutate(
      Assay = factor(.data$Assay, levels = assays),
      Availability = factor(
        dplyr::if_else(is.na(.data$AvailabilityValue), "Missing", "Available"),
        levels = c("Available", "Missing")
      )
    )

  if (by_time) {
    p <- ggplot2::ggplot(
      plot_data,
      ggplot2::aes(x = .data$Assay, y = .data[[id]], fill = .data$Availability)
    ) +
      ggplot2::facet_wrap(stats::reformulate(time), nrow = nrow, ncol = ncol)
  } else {
    p <- ggplot2::ggplot(
      plot_data,
      ggplot2::aes(x = .data[[time]], y = .data[[id]], fill = .data$Availability)
    ) +
      ggplot2::facet_wrap(~Assay, nrow = nrow, ncol = ncol)
  }

  p +
    ggplot2::geom_tile(colour = "white", linewidth = 0.1) +
    ggplot2::scale_fill_manual(
      values = c("Available" = "#457B9D", "Missing" = "#D9D9D9")
    ) +
    ggplot2::labs(
      title = "Assay availability",
      x = NULL,
      y = "Subject",
      fill = NULL
    ) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 90, hjust = 1, size = 7),
      axis.text.y = ggplot2::element_text(size = 7),
      strip.background = ggplot2::element_blank(),
      legend.position = "bottom",
      plot.title = ggplot2::element_text(hjust = 0.5)
    )
}
