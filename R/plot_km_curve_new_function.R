#' Plot Kaplan-Meier survival curves with an optional risk table
#'
#' @description
#' Fits overall or stratified Kaplan-Meier curves, with optional confidence
#' intervals, censoring marks, survival quantiles, a risk table and a log-rank
#' test. Supports ggsci palettes or manual colours, customizable grids and borders,
#' follow-up display limits, and editable vector PowerPoint export.
#'
#' @param data Data frame containing the survival variables.
#' @param time,event Character names of the follow-up time and event columns.
#'   Events must be coded as 0 (censored) and 1 (event).
#' @param group Grouping column name, or "Overall" (default). Factors,
#'   character and logical grouping variables are accepted.
#' @param conf_int Display confidence intervals (default TRUE).
#' @param alpha Confidence ribbon opacity (default 0.20).
#' @param lwd Survival curve linewidth (default 1).
#' @param conf_int_type Either "ribbon" (default) or "lines".
#' @param risktable Include the risk table (default TRUE).
#' @param risktable_stats Risk-table statistics; default "n.risk". Other options
#'   include "cum.event", "cum.censor", "n.event" and "n.censor".
#' @param risktable_group One of "auto", "strata" or "risktable_stats".
#' @param risktable_symbol Optional symbol to identify strata in the risk table.
#' (e.g. Unicode symbols u25CF, put reversed slash before)
#' @param risktable_height Proportion of figure height reserved for the risk
#'   table (default 0.2).
#' @param risktable_size Size of risk-table numbers (default 3.5).
#' @param risktable_axis_text_size Risk-table row-label font size in points.
#' @param risktable_title_size Risk-table title font size in points.
#' @param quantile Optional survival probability at which to mark a quantile;
#'   for example, 0.5 for the median survival time.
#' @param pvalue Display a stratified log-rank test (default FALSE). The
#'   ordinary log-rank test is always used (rho = 0).
#' @param pvalue_x,pvalue_y Coordinates of the p-value annotation in follow-up
#'   time and survival probability units; both default to 0.
#' @param pvalue_size P-value annotation font size (default 4).
#' @param censoring Display censoring marks (default TRUE).
#' @param shape Symbol for censoring marks (default 3).
#' @param grid_major,grid_minor,panel_border Optional TRUE/FALSE values to
#'   show or hide major grid lines, minor grid lines and the panel border.
#'   NULL (default) keeps the standard ggsurvfit appearance. These options
#'   affect the KM panel only, not the risk table.
#' @param colors NULL (default), a ggsci palette name (e.g. "jco", "jama",
#'   "npg", "nejm", "lancet") or a valid R colour/vector of colours.
#'   For palettes with multiple variants use "family:variant", e.g.
#'   "d3:category20". Available palette families correspond to exported
#'   ggsci `pal_*()` generators that return a discrete palette function.
#'   A single colour is allowed for an Overall curve; stratified curves
#'   require one colour per observed group unless a palette is specified.
#'   Named colours must match the factor levels (e.g., c(F = "red", M = "blue")).
#'   Overall confidence intervals and censoring marks use the selected colour.
#' @param title,legend_title,axis_x_title,axis_y_title Optional plot labels.
#' @param size_title,size_axis_title,size_axis_text,size_legend_title,
#'   size_legend_text Font sizes in points.
#' @param legend_position Legend location: "bottom" (default), "top", "left",
#'   "right" or "none".
#' @param xlim NULL (default) shows the full follow-up. A positive finite
#'   number (for example 30) sets the maximum displayed follow-up time.
#'   This is a visual limit: all patients and events are retained in the
#'   Kaplan-Meier estimates and the log-rank test. The risk table is shown
#'   only at tick times within the displayed interval.
#' @param x_breaks Positive number defining the spacing of time-axis ticks,
#'   in the same units as time. Default 6 gives 0, 6, 12, 18, ... .
#'   The same time points are used in the risk table. If xlim is not a
#'   multiple of x_breaks, the last tick is the last multiple below xlim.
#' @param y_breaks Positive number (up to 1) defining the spacing of survival
#'   probability ticks. Default 0.2 gives 0, 0.2, ..., 1.
#' @param file Optional output filename (.pptx, .pdf, .svg, .png, .tif,
#'   .tiff, .jpg, .jpeg); NULL (default) does not save a file.
#' @param width,height Output plot dimensions in inches (default 10 x 7),
#'   including for PPTX. For PPTX the entire KM + risk-table composition is
#'   centred within the default PowerPoint slide; it must fit the slide.
#'
#' @details
#' The curve and risk table are combined with
#' ggsurvfit::ggsurvfit_build(combine_plots = TRUE) before rendering or export,
#' to preserve their shared horizontal alignment. For editable PowerPoint,
#' rvg and officer generate DrawingML vector elements rather than a bitmap.
#' These are editable shapes/text, not a native PowerPoint chart with data.
#'
#' @return A ggplot2 or patchwork object, depending on whether a risk table
#'   is included.
#' @author Stefano Bergamini, Luca Lalli
#' @export
#'
#' @examples
#' # Public dataset included in ggsurvfit:
#' # time = follow-up (years); status = 0/1; surg = grouping factor.
#' dat <- ggsurvfit::df_colon
#'
#' # Overall survival with a custom curve colour
#' plot_km_curve(dat, "time", "status", colors = "steelblue",
#'               x_breaks = 2)
#'
#' # Stratified survival with a log-rank test and risk table
#' plot_km_curve(dat, "time", "status", group = "surg",
#'               pvalue = TRUE, x_breaks = 2, y_breaks = 0.2,
#'               xlim = 8, grid_major = TRUE, panel_border = TRUE)
#'
#' # Custom colours mapped to the observed strata
#' group_colors <- stats::setNames(c("salmon", "royalblue"),
#'                                 levels(dat$surg))
#' plot_km_curve(dat, "time", "status", group = "surg",
#'               colors = group_colors, x_breaks = 2)
#'
#' # JCO/JAMA scientific palettes (optional ggsci dependency)
#' if (requireNamespace("ggsci", quietly = TRUE)) {
#'   plot_km_curve(dat, "time", "status", group = "surg",
#'                 colors = "jco", x_breaks = 2, xlim = 8)
#'   plot_km_curve(dat, "time", "status", group = "surg",
#'                 colors = "jama", x_breaks = 2)
#' }
#'
#' \dontrun{
#' # Editable PowerPoint export (requires officer and rvg)
#' plot_km_curve(dat, "time", "status", group = "surg",
#'               colors = c("salmon", "royalblue"), x_breaks = 2,
#'               file = "KM_colon.pptx", width = 7, height = 5.5)
#' }
plot_km_curve <- function(
    data,
    time,
    event,
    group = "Overall",
    conf_int = TRUE,
    alpha = 0.20,
    lwd = 1,
    conf_int_type = c("ribbon", "lines"),
    risktable = TRUE,
    risktable_stats = "n.risk",
    risktable_group = c("auto", "strata", "risktable_stats"),
    risktable_symbol = NULL,
    risktable_height = 0.2,
    risktable_size = 3.5,
    risktable_axis_text_size = 10,
    risktable_title_size = 10.75,
    quantile = NULL,
    pvalue = FALSE,
    pvalue_x = 0,
    pvalue_y = 0,
    pvalue_size = 4,
    censoring = TRUE,
    shape = 3,
    grid_major = NULL,
    grid_minor = NULL,
    panel_border = NULL,
    colors = NULL,
    title = NULL,
    legend_title = NULL,
    axis_x_title = NULL,
    axis_y_title = NULL,
    size_title = 14,
    size_axis_title = 11,
    size_axis_text = 10,
    size_legend_title = 11,
    size_legend_text = 10,
    legend_position = c("bottom", "top", "left", "right", "none"),
    x_breaks = 6,
    y_breaks = 0.2,
    xlim = NULL,
    file = NULL,
    width = 10,
    height = 7) {

  if (!is.data.frame(data)) {
    stop("'data' must be a data frame.", call. = FALSE)
  }

  for (nm in c("time", "event", "group")) {
    z <- get(nm)
    if (!is.character(z) || length(z) != 1L ||
        is.na(z) || !nzchar(z)) {
      stop("'", nm, "' must be a single column name.", call. = FALSE)
    }
  }

  is_overall <- identical(group, "Overall")
  required <- c(time, event, if (!is_overall) group)
  missing_cols <- setdiff(required, names(data))
  if (length(missing_cols)) {
    stop("Missing columns: ", paste(missing_cols, collapse = ", "),
         call. = FALSE)
  }

  check_flag <- function(z, nm, allow_null = FALSE) {
    if (allow_null && is.null(z)) return(invisible(NULL))
    if (!is.logical(z) || length(z) != 1L || is.na(z)) {
      stop("'", nm, "' must be TRUE or FALSE.", call. = FALSE)
    }
  }
  for (nm in c("conf_int", "risktable", "pvalue", "censoring")) {
    check_flag(get(nm), nm)
  }
  for (nm in c("grid_major", "grid_minor", "panel_border")) {
    check_flag(get(nm), nm, allow_null = TRUE)
  }

  check_number <- function(z, nm, low = 0, high = Inf,
                           strictly_positive = FALSE) {
    if (!is.numeric(z) || length(z) != 1L || !is.finite(z) ||
        (if (strictly_positive) z <= low else z < low) || z > high) {
      stop("Invalid value for '", nm, "'.", call. = FALSE)
    }
  }
  check_number(alpha, "alpha", high = 1)
  check_number(lwd, "lwd", strictly_positive = TRUE)
  check_number(risktable_height, "risktable_height", high = 1,
               strictly_positive = TRUE)
  if (risktable_height >= 1) {
    stop("'risktable_height' must be below 1.", call. = FALSE)
  }
  for (nm in c("risktable_size", "risktable_axis_text_size",
               "risktable_title_size", "pvalue_size", "size_title",
               "size_axis_title", "size_axis_text", "size_legend_title",
               "size_legend_text", "width", "height")) {
    check_number(get(nm), nm, strictly_positive = TRUE)
  }
  if (!is.null(quantile)) check_number(quantile, "quantile", high = 1)
  check_number(pvalue_x, "pvalue_x")
  check_number(pvalue_y, "pvalue_y", high = 1)
  check_number(x_breaks, "x_breaks", strictly_positive = TRUE)
  check_number(y_breaks, "y_breaks", high = 1, strictly_positive = TRUE)
  if (!is.null(xlim)) {
    check_number(xlim, "xlim", strictly_positive = TRUE)
  }

  for (nm in c("title", "legend_title", "axis_x_title", "axis_y_title")) {
    z <- get(nm)
    if (!is.null(z) &&
        (!is.character(z) || length(z) != 1L || is.na(z))) {
      stop("'", nm, "' must be NULL or a single string.", call. = FALSE)
    }
  }

  conf_int_type <- match.arg(conf_int_type)
  risktable_group <- match.arg(risktable_group)
  legend_position <- match.arg(legend_position)

  allowed_stats <- c("n.risk", "cum.event", "cum.censor",
                     "n.event", "n.censor")
  if (!is.character(risktable_stats) || !length(risktable_stats) ||
      anyNA(risktable_stats) || any(!risktable_stats %in% allowed_stats)) {
    stop("Invalid 'risktable_stats'.", call. = FALSE)
  }

  if (!is.numeric(data[[time]])) {
    stop("The time column must be numeric.", call. = FALSE)
  }
  if (!(is.numeric(data[[event]]) || is.logical(data[[event]]))) {
    stop("The event column must contain 0/1 or FALSE/TRUE values.",
         call. = FALSE)
  }
  observed_events <- unique(data[[event]][!is.na(data[[event]])])
  if (any(!is.finite(observed_events)) ||
      any(!observed_events %in% c(0, 1))) {
    stop("The event column must be coded 0/1.", call. = FALSE)
  }

  valid <- stats::complete.cases(data[, required, drop = FALSE]) &
    is.finite(data[[time]])
  if (any(!valid)) {
    warning(sum(!valid), " incomplete/non-finite row(s) removed.",
            call. = FALSE)
  }
  workdata <- data[valid, , drop = FALSE]
  if (!nrow(workdata)) stop("No valid observations.", call. = FALSE)
  if (any(workdata[[time]] < 0)) {
    stop("Follow-up times must be non-negative.", call. = FALSE)
  }

  if (!is_overall) {
    grp <- workdata[[group]]
    if (!(is.factor(grp) || is.character(grp) || is.logical(grp))) {
      stop("Grouping variable must be factor, character or logical.",
           call. = FALSE)
    }
    grp <- if (is.factor(grp)) droplevels(grp) else factor(grp)
    if (is.ordered(grp)) {
      grp <- factor(as.character(grp), levels = levels(grp))
    }
    workdata[[group]] <- grp
    n_strata <- nlevels(grp)
    if (n_strata < 2L && pvalue) {
      stop("At least two observed groups are needed for the log-rank test.",
           call. = FALSE)
    }
    strata_names <- levels(grp)
  } else {
    n_strata <- 1L
    strata_names <- "Overall"
    if (pvalue) {
      warning("pvalue ignored for Overall survival.", call. = FALSE)
      pvalue <- FALSE
    }
  }

  max_time <- max(workdata[[time]])
  axis_end <- if (is.null(xlim)) {
    max(x_breaks, ceiling(max_time / x_breaks) * x_breaks)
  } else {
    xlim
  }

  n_intervals <- floor(axis_end / x_breaks)
  if (!is.finite(n_intervals) || n_intervals > 1000) {
    stop("More than 1000 time breaks requested; increase 'x_breaks'.",
         call. = FALSE)
  }

  time_ticks <- seq(0, by = x_breaks, length.out = n_intervals + 1L)
  survival_ticks <- sort(unique(c(seq(0, 1, by = y_breaks), 1)))

  curve_colors <- NULL

  if (!is.null(colors)) {
    if (!is.character(colors) || !length(colors) ||
        anyNA(colors) || any(!nzchar(colors))) {
      stop("'colors' must be NULL, a ggsci palette name, or valid colours.",
           call. = FALSE)
    }

    is_r_color <- function(z) {
      isTRUE(tryCatch({
        grDevices::col2rgb(z)
        TRUE
      }, error = function(e) FALSE))
    }

    is_palette <- length(colors) == 1L && !is_r_color(colors)

    if (is_palette) {
      if (!is.null(names(colors)) && nzchar(names(colors)[1])) {
        stop("A ggsci palette name cannot be a named colour.",
             call. = FALSE)
      }
      if (!requireNamespace("ggsci", quietly = TRUE)) {
        stop("Install 'ggsci' to use its palettes.", call. = FALSE)
      }

      specification <- strsplit(tolower(colors), ":", fixed = TRUE)[[1]]
      if (length(specification) > 2L ||
          length(specification) < 1L ||
          any(!nzchar(specification))) {
        stop("Invalid ggsci palette specification: ", colors,
             call. = FALSE)
      }
      family <- sub("^pal_", "", specification[1])
      palette_function <- paste0("pal_", family)

      generator <- tryCatch(
        getExportedValue("ggsci", palette_function),
        error = function(e) NULL
      )
      if (!is.function(generator)) {
        stop("Unknown ggsci palette: ", colors,
             ". Use a valid R colour or a ggsci pal_* family.",
             call. = FALSE)
      }

      palette_args <- if (length(specification) == 2L) {
        list(palette = specification[2])
      } else {
        list()
      }

      selected <- tryCatch({
        pal_fun <- do.call(generator, palette_args)
        if (!is.function(pal_fun)) {
          stop("The palette generator did not return a function.")
        }
        suppressWarnings(pal_fun(n_strata))
      }, error = function(e) {
        stop("Cannot generate colours from ggsci palette '", colors,
             "': ", conditionMessage(e), call. = FALSE)
      })

      if (!is.character(selected) || length(selected) != n_strata ||
          anyNA(selected) || !all(vapply(selected, is_r_color, logical(1)))) {
        stop("The ggsci palette '", colors, "' cannot provide ",
             n_strata, " valid colours. Choose another palette or ",
             "supply custom colours.", call. = FALSE)
      }
      curve_colors <- stats::setNames(unname(selected), strata_names)

    } else {

      if (length(colors) != n_strata) {
        stop("For manual colours, supply ", n_strata,
             " colour(s), or specify a ggsci palette name.",
             call. = FALSE)
      }
      if (!all(vapply(colors, is_r_color, logical(1)))) {
        stop("'colors' contains an invalid R colour.", call. = FALSE)
      }

      nm <- names(colors)
      if (!is.null(nm) && any(nzchar(nm))) {
        if (any(!nzchar(nm)) || anyDuplicated(nm) ||
            !setequal(nm, strata_names)) {
          stop("Named 'colors' must match the observed group levels exactly.",
               call. = FALSE)
        }
        curve_colors <- colors[strata_names]
      } else {
        curve_colors <- stats::setNames(unname(colors), strata_names)
      }
    }
  }

  quote_col <- function(nm) {
    if (grepl("`", nm, fixed = TRUE)) {
      stop("Backticks in column names are not supported.", call. = FALSE)
    }
    paste0("`", nm, "`")
  }
  formula <- stats::as.formula(
    paste0("survival::Surv(", quote_col(time), ", ", quote_col(event),
           ") ~ ", if (is_overall) "1" else quote_col(group)),
    env = environment()
  )
  fit <- ggsurvfit::survfit2(formula, data = workdata)

  km_theme <- ggsurvfit::theme_ggsurvfit_default()
  km_args <- list(x = fit, linewidth = lwd, theme = km_theme)
  if (is_overall && !is.null(curve_colors)) {
    km_args$colour <- unname(curve_colors[1])
  }
  p <- do.call(ggsurvfit::ggsurvfit, km_args)

  p <- p + ggsurvfit::scale_ggsurvfit(
    x_scales = list(
      breaks = time_ticks,
      limits = c(0, axis_end),
      oob = scales::oob_keep,
      expand = c(0.05, 0)
    ),
    y_scales = list(breaks = survival_ticks)
  )

  if (conf_int) {
    ci_args <- if (conf_int_type == "ribbon") {
      list(type = "ribbon", alpha = alpha)
    } else {
      list(type = "lines")
    }

    if (is_overall && !is.null(curve_colors)) {
      if (conf_int_type == "ribbon") {
        ci_args$fill <- unname(curve_colors[1])
      } else {
        ci_args$colour <- unname(curve_colors[1])
      }
    }
    p <- p + do.call(ggsurvfit::add_confidence_interval, ci_args)
  }
  if (censoring) {
    censor_args <- list(shape = shape, size = lwd)
    if (is_overall && !is.null(curve_colors)) {
      censor_args$colour <- unname(curve_colors[1])
    }
    p <- p + do.call(ggsurvfit::add_censor_mark, censor_args)
  }
  if (!is.null(quantile)) {
    p <- p + ggsurvfit::add_quantile(y_value = quantile)
  }
  if (pvalue) {
    p <- p + ggsurvfit::add_pvalue(
      location = "annotation",
      rho = 0,  # ordinary log-rank test
      x = pvalue_x,
      y = pvalue_y,
      hjust = 0,
      vjust = 0,
      size = pvalue_size
    )
  }

  if (!is_overall && !is.null(curve_colors)) {
    p <- p + ggplot2::scale_colour_manual(values = curve_colors,
                                          drop = FALSE) +
      ggplot2::scale_fill_manual(values = curve_colors, drop = FALSE)
  }

  plot_labels <- list()
  if (!is.null(title)) plot_labels$title <- title
  if (!is.null(axis_x_title)) plot_labels$x <- axis_x_title
  if (!is.null(axis_y_title)) plot_labels$y <- axis_y_title
  if (!is.null(legend_title)) {
    plot_labels$colour <- legend_title
    plot_labels$fill <- legend_title
  }
  if (length(plot_labels)) {
    p <- p + do.call(ggplot2::labs, plot_labels)
  }

  p <- p + ggplot2::theme(
    plot.title = ggplot2::element_text(size = size_title, face = "bold"),
    axis.title = ggplot2::element_text(size = size_axis_title),
    axis.text = ggplot2::element_text(size = size_axis_text),
    legend.title = ggplot2::element_text(size = size_legend_title),
    legend.text = ggplot2::element_text(size = size_legend_text),
    legend.position = legend_position
  )

  custom_theme <- list()
  if (!is.null(grid_major)) {
    custom_theme$panel.grid.major <- if (grid_major) {
      ggplot2::element_line(colour = "grey85", linewidth = 0.3)
    } else ggplot2::element_blank()
  }
  if (!is.null(grid_minor)) {
    custom_theme$panel.grid.minor <- if (grid_minor) {
      ggplot2::element_line(colour = "grey92", linewidth = 0.2)
    } else ggplot2::element_blank()
  }
  if (!is.null(panel_border)) {
    custom_theme$panel.border <- if (panel_border) {
      ggplot2::element_rect(colour = "black", fill = NA, linewidth = 0.5)
    } else ggplot2::element_blank()
  }
  if (length(custom_theme)) {
    p <- p + do.call(ggplot2::theme, custom_theme)
  }

  if (risktable) {
    p <- p + ggsurvfit::add_risktable(
      times = time_ticks,
      risktable_stats = risktable_stats,
      risktable_group = risktable_group,
      risktable_height = risktable_height,
      size = risktable_size,
      theme = ggsurvfit::theme_risktable_default(
        axis.text.y.size = risktable_axis_text_size,
        plot.title.size = risktable_title_size
      )
    )
    if (!is.null(risktable_symbol)) {
      p <- p + ggsurvfit::add_risktable_strata_symbol(
        symbol = risktable_symbol
      )
    }
  }
  built <- ggsurvfit::ggsurvfit_build(p, combine_plots = TRUE)

  if (!is.null(file)) {
    if (!is.character(file) || length(file) != 1L ||
        is.na(file) || !nzchar(file)) {
      stop("'file' must be NULL or one valid filename.", call. = FALSE)
    }
    ext <- tolower(tools::file_ext(file))
    if (!ext %in% c("pptx", "pdf", "svg", "png", "tif", "tiff",
                    "jpg", "jpeg")) {
      stop("Unsupported export format: ", ext, call. = FALSE)
    }
    outdir <- dirname(file)
    if (!dir.exists(outdir)) dir.create(outdir, recursive = TRUE)
    if (!dir.exists(outdir)) {
      stop("Cannot create output directory.", call. = FALSE)
    }

    if (ext == "pptx") {
      if (!requireNamespace("officer", quietly = TRUE) ||
          !requireNamespace("rvg", quietly = TRUE)) {
        stop("PPTX export requires 'officer' and 'rvg'.", call. = FALSE)
      }
      ppt <- officer::read_pptx()
      layouts <- officer::layout_summary(ppt)
      blank <- which(layouts$layout == "Blank")
      if (!length(blank)) {
        stop("The default PPTX needs a 'Blank' slide layout.",
             call. = FALSE)
      }
      ppt <- officer::add_slide(
        ppt, layout = "Blank", master = layouts$master[blank[1]]
      )

      sz <- officer::slide_size(ppt)
      slide_w <- sz[["width"]]
      slide_h <- sz[["height"]]
      if (width > slide_w || height > slide_h) {
        stop(sprintf(
          paste0("Requested plot size (%.2f x %.2f in) exceeds ",
                 "PowerPoint slide size (%.2f x %.2f in). ",
                 "Reduce width/height to fit the default slide."),
          width, height, slide_w, slide_h
        ), call. = FALSE)
      }

      ppt <- officer::ph_with(
        ppt,
        value = rvg::dml(code = print(built), editable = TRUE),
        location = officer::ph_location(
          left = (slide_w - width) / 2,
          top = (slide_h - height) / 2,
          width = width,
          height = height
        )
      )
      print(ppt, target = file)
    } else {
      ggplot2::ggsave(
        filename = file, plot = built,
        width = width, height = height, units = "in", bg = "white"
      )
    }
  }

  built
}
