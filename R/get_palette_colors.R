#' Get colors for plot groups
#'
#' Returns colors ready to use in ggplot2 manual scales, from either a
#' \pkg{ggsci} palette name or a vector of valid R colors.
#'
#' @param group A grouping vector. The number of observed, non-missing groups
#'   and their names are derived from this vector. Use \code{NULL} for a
#'   single overall color.
#' @param palette A \pkg{ggsci} palette name, a vector of R colors, or
#'   \code{NULL} to use \code{default}.
#' @param default Color or colors to use when \code{palette} is \code{NULL}.
#'
#' @return A character vector of colors, named by the observed group values
#'   when \code{group} is supplied.
#' @export
#'
#' @examples
#' get_palette_colors(factor(c("Control", "Low", "High")), palette = "npg")
#' get_palette_colors(c("Control", "Low"), palette = c("salmon", "royalblue"))
get_palette_colors <- function(group = NULL, palette = NULL, default = "black") {
  group_levels <- if (is.null(group)) {
    NULL
  } else {
    levels(droplevels(factor(group[!is.na(group)])))
  }
  n <- if (is.null(group)) 1L else length(group_levels)
  if (n < 1L) {
    stop("`group` must contain at least one observed, non-missing value.", call. = FALSE)
  }

  if (is.null(palette)) {
    palette <- default
  }
  if (!is.character(palette) || length(palette) < 1L || anyNA(palette) || any(!nzchar(palette))) {
    stop("`palette` must be a `ggsci` palette name or a non-empty vector of colors.", call. = FALSE)
  }

  palette_scale <- if (length(palette) == 1L) paste0("scale_color_", palette)
  exports <- getNamespaceExports("ggsci")
  if (!is.null(palette_scale) && palette_scale %in% exports) {
    colors <- getExportedValue("ggsci", palette_scale)()$palette(n)
  } else {
    valid_colors <- vapply(
      palette,
      function(color) !inherits(try(grDevices::col2rgb(color), silent = TRUE), "try-error"),
      logical(1)
    )
    if (!all(valid_colors)) {
      if (length(palette) == 1L) {
        available_palettes <- sub("^scale_color_", "", grep("^scale_color_", exports, value = TRUE))
        stop(
          "Unknown `palette`: ", palette, ". Choose one of: ",
          paste(available_palettes, collapse = ", "), ".",
          call. = FALSE
        )
      }
      stop("Every value in `palette` must be a valid R color.", call. = FALSE)
    }
    if (length(palette) == 1L && n > 1L) {
      colors <- rep(palette, n)
    } else if (length(palette) == n) {
      colors <- palette
    } else {
      stop("The number of colors in `palette` must match `n`.", call. = FALSE)
    }
  }

  if (!is.null(group)) {
    names(colors) <- group_levels
  }
  colors
}