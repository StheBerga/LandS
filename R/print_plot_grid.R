#' Save a paginated grid of ggplot objects
#'
#' @description
#' Combines a list of ggplots into printable pages using cowplot. Pages can be
#' returned as ggplots, exported as a multipage PDF, editable PowerPoint slides,
#' or individual image/vector files.
#'
#' @param plot_list Non-empty list of ggplot objects.
#' @param path_print Output path without extension. Default "plot_grid".
#' @param nrow,ncol Positive integers defining the number of plots per page.
#' @param ext One of "pdf", "pptx", "png", "tiff", "jpeg", "svg", "emf".
#'   "jpg" and "tif" are also accepted. Default "pdf".
#' @param width_pg,height_pg Page dimensions in centimeters; A4 portrait by default.
#' @param dpi Resolution for raster output (default 300).
#' @param align,axis Alignment settings passed to cowplot::align_plots().
#' @param plot.spacing Gap between adjacent plot cells in millimeters (default 4).
#' @param page.margin Page margins in millimeters: top, right, bottom, left.
#'   A single number applies to all four margins (default 10).
#' @param scale Scale factor for plots within grid cells (default 1).
#' @param labels NULL (no labels), "AUTO" (A, B, ...), "auto" (a, b, ...),
#'   "NUM" (1, 2, ...), or one character label per plot. Automatic labels
#'   continue sequentially across pages.
#' @param label.size,label.x,label.y Panel-label size and relative position.
#' @param page_num Whether to display "Page N" above each page (default FALSE).
#' @param bg Page background colour (default "transparent").
#'   Note that individual ggplots may retain their own panel backgrounds.
#' @param progress Whether to display a text progress bar in the R console
#'   during plot assembly and export (default TRUE).
#' @param save Whether to write files (default TRUE).
#' @param return_plot Whether to return the assembled page ggplots (default FALSE).
#' @param suppress_warning Suppress warnings generated during processing.
#' @param verbose Print output file messages (default TRUE).
#'
#' @details
#' PDF output is one multipage file. PPTX output is one editable DrawingML slide
#' per page, at the exact dimensions of width_pg x height_pg. For the PowerPoint
#' dimensions, the function updates the XML of the officer presentation object:
#' officer does not currently expose a public slide-size setter. This technique
#' depends on officer internals and should be retested after officer upgrades.
#' All other formats generate one file per page, named *_page_01, etc.
#' The source plots are not modified. For PPTX, 'transparent' means the
#' DrawingML page background is transparent over the slide; it does not
#' automatically change the underlying slide's default background.
#'
#' @return A named list of ggplot objects if return_plot = TRUE; otherwise
#'   invisible(NULL). Files are written only if save = TRUE.
#' @author Luca Lalli, Stefano Bergamini
#' @export
#' @examples
#' \dontrun{
#' p <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg)) +
#'   ggplot2::geom_point()
#' print_plot_grid(list(p, p, p), nrow = 2, ncol = 2,
#'                 ext = "pptx", labels = "NUM", page_num = TRUE,
#'                 path_print = "example_grid")
#' }
print_plot_grid <- function(
    plot_list,
    path_print = "plot_grid",
    nrow = 3,
    ncol = 2,
    ext = "pdf",
    width_pg = 21,
    height_pg = 29.7,
    dpi = 300,
    align = "hv",
    axis = "tblr",
    plot.spacing = 4,
    page.margin = c(10, 10, 10, 10),
    scale = 1,
    labels = NULL,
    label.size = 12,
    label.x = 0.02,
    label.y = 0.98,
    page_num = FALSE,
    bg = "transparent",
    save = TRUE,
    return_plot = FALSE,
    suppress_warning = FALSE,
    verbose = TRUE,
    progress = TRUE) {

  # Input checks -------------------------------------------------------------
  if (!is.list(plot_list) || length(plot_list) == 0L ||
      !all(vapply(plot_list, inherits, logical(1), what = "ggplot"))) {
    stop("'plot_list' must be a non-empty list of ggplot objects.")
  }

  check_number <- function(x, nm, allow_zero = FALSE) {
    if (!is.numeric(x) || length(x) != 1L || !is.finite(x) ||
        (if (allow_zero) x < 0 else x <= 0)) {
      stop("'", nm, "' must be a finite ",
           if (allow_zero) "non-negative" else "positive", " number.")
    }
  }
  check_flag <- function(x, nm) {
    if (!is.logical(x) || length(x) != 1L || is.na(x)) {
      stop("'", nm, "' must be TRUE or FALSE.")
    }
  }

  for (nm in c("nrow", "ncol")) {
    val <- get(nm)
    check_number(val, nm)
    if (val != floor(val) || val > .Machine$integer.max) {
      stop("'", nm, "' must be a positive integer.")
    }
  }
  nrow <- as.integer(nrow)
  ncol <- as.integer(ncol)
  per_page <- as.double(nrow) * as.double(ncol)
  if (per_page > (.Machine$integer.max / 4)) {
    stop("The requested grid is too large.")
  }

  for (nm in c("width_pg", "height_pg", "dpi", "scale", "label.size")) {
    check_number(get(nm), nm)
  }
  if (scale > 1) stop("'scale' must be in (0, 1].")
  check_number(plot.spacing, "plot.spacing", allow_zero = TRUE)

  if (!is.numeric(page.margin) || !length(page.margin) %in% c(1L, 4L) ||
      anyNA(page.margin) || any(!is.finite(page.margin)) ||
      any(page.margin < 0)) {
    stop("'page.margin' must be one or four non-negative mm values.")
  }
  if (length(page.margin) == 1L) page.margin <- rep(page.margin, 4L)
  names(page.margin) <- c("top", "right", "bottom", "left")

  for (nm in c("save", "return_plot", "page_num", "suppress_warning",
               "verbose", "progress")) {
    check_flag(get(nm), nm)
  }
  if (!save && !return_plot) {
    stop("Set 'save' and/or 'return_plot' to TRUE.")
  }
  for (nm in c("label.x", "label.y")) {
    val <- get(nm)
    if (!is.numeric(val) || length(val) != 1L || !is.finite(val) ||
        val < 0 || val > 1) {
      stop("'", nm, "' must be a number between 0 and 1.")
    }
  }

  if (!is.character(bg) || length(bg) != 1L || is.na(bg) || !nzchar(bg)) {
    stop("'bg' must be a colour string.")
  }
  if (!is.character(path_print) || length(path_print) != 1L ||
      is.na(path_print) || !nzchar(path_print)) {
    stop("'path_print' must be a non-empty file path.")
  }
  if (identical(path_print, ".")) path_print <- file.path(".", "plot_grid")

  if (is.null(ext)) ext <- "pdf"
  if (!is.character(ext) || length(ext) != 1L || is.na(ext)) {
    stop("'ext' must be one supported format.")
  }
  ext <- tolower(ext)
  if (ext == "jpg") ext <- "jpeg"
  if (ext == "tif") ext <- "tiff"
  if (!ext %in% c("pdf", "pptx", "png", "tiff", "jpeg", "svg", "emf")) {
    stop("Unsupported output format: ", ext)
  }
  path_print <- sub("\\.(pdf|pptx|png|tiff|tif|jpeg|jpg|svg|emf)$",
                    "", path_print, ignore.case = TRUE)

  align <- match.arg(align, c("none", "h", "v", "hv"))
  axis <- match.arg(axis, c("none", "l", "r", "t", "b", "lr", "tb", "tblr"))

  # Page geometry in mm ------------------------------------------------------
  page_w <- width_pg * 10
  page_h <- height_pg * 10
  title_mm <- if (page_num) 8 else 0
  content_w <- page_w - page.margin["left"] - page.margin["right"]
  content_h <- page_h - page.margin["top"] - page.margin["bottom"] - title_mm
  cells_w <- content_w - (ncol - 1L) * plot.spacing
  cells_h <- content_h - (nrow - 1L) * plot.spacing
  if (cells_w <= 0 || cells_h <= 0) {
    stop("Margins and plot spacing leave no room for the panels.")
  }

  total <- length(plot_list)
  n_pages <- ceiling(total / per_page)

  # Panel labels; continue A, B, ... across pages ----------------------------
  if (!is.null(labels)) {
    if (!is.character(labels) || anyNA(labels)) {
      stop("'labels' must be NULL or a character vector without NAs.")
    }
    if (length(labels) == 1L && labels %in% c("AUTO", "auto", "NUM", "num", "numeric")) {
      to_letters <- function(k) {
        out <- ""
        while (k > 0L) {
          k <- k - 1L
          out <- paste0(LETTERS[(k %% 26L) + 1L], out)
          k <- k %/% 26L
        }
        out
      }
      if (labels %in% c("NUM", "num", "numeric")) {
        labels <- as.character(seq_len(total))
      } else {
        lower <- identical(labels, "auto")
        labels <- vapply(seq_len(total), to_letters, character(1))
        if (lower) labels <- tolower(labels)
      }
    } else if (length(labels) != total) {
      stop("Provide one label per plot, or use 'AUTO', 'auto' or 'NUM'.")
    }
  }

  # Physical gap cells, spacing is measured in millimeters ----------------
  with_gaps <- function(sizes, gap) {
    if (length(sizes) == 1L) return(sizes)
    out <- rep(gap, length(sizes) * 2L - 1L)
    out[seq.int(1L, length(out), by = 2L)] <- sizes
    out
  }
  col_layout <- with_gaps(rep(cells_w / ncol, ncol), plot.spacing)
  row_layout <- with_gaps(rep(cells_h / nrow, nrow), plot.spacing)

  core_execution <- function() {
    # Count each assembled plot and exported page, plus final file completion.
    # The progress bar also works when save = FALSE (preview only).
    total_steps <- total + if (save) n_pages + 1L else 0L
    done_steps <- 0L

    if (progress) {
      pb <- utils::txtProgressBar(min = 0, max = total_steps, style = 3)
      on.exit(close(pb), add = TRUE)
    }

    advance <- function() {
      done_steps <<- done_steps + 1L
      if (progress) utils::setTxtProgressBar(pb, done_steps)
      invisible(NULL)
    }

    pages <- vector("list", n_pages)
    names(pages) <- sprintf("page_%02d", seq_len(n_pages))

    for (page in seq_len(n_pages)) {
      indices <- seq.int(as.integer((page - 1L) * per_page + 1L),
                         as.integer(min(total, page * per_page)))

      aligned <- cowplot::align_plots(
        plotlist = plot_list[indices], align = align, axis = axis
      )

      nr <- 2L * nrow - 1L
      nc <- 2L * ncol - 1L
      grid_items <- vector("list", nr * nc)

      for (j in seq_along(aligned)) {
        r <- (j - 1L) %/% ncol
        c <- (j - 1L) %% ncol
        slot <- (2L * r) * nc + (2L * c) + 1L
        item <- aligned[[j]]

        if (!is.null(labels) && nzchar(labels[indices[j]])) {
          item <- cowplot::ggdraw(item) +
            cowplot::draw_label(labels[indices[j]],
                                x = label.x, y = label.y,
                                hjust = 0, vjust = 1,
                                size = label.size, fontface = "bold")
        }
        grid_items[slot] <- list(item)
        advance()
      }

      grid_plot <- cowplot::plot_grid(
        plotlist = grid_items, nrow = nr, ncol = nc,
        rel_widths = col_layout, rel_heights = row_layout,
        align = "none", scale = scale
      )

      page_plot <- cowplot::ggdraw() +
        cowplot::draw_plot(
          grid_plot,
          x = unname(page.margin["left"] / page_w),
          y = unname(page.margin["bottom"] / page_h),
          width = unname(content_w / page_w),
          height = unname(content_h / page_h)
        )

      if (page_num) {
        page_plot <- page_plot +
          cowplot::draw_label(
            paste("Page", page),
            x = 0.5,
            y = unname((page_h - page.margin["top"] - title_mm / 2) / page_h),
            hjust = 0.5, vjust = 0.5,
            size = 12, fontface = "bold"
          )
      }

      pages[[page]] <- page_plot +
        ggplot2::theme(
          plot.background = ggplot2::element_rect(fill = bg, colour = NA)
        )
    }

    if (save) {
      out_dir <- dirname(path_print)
      if (!dir.exists(out_dir) &&
          !dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)) {
        stop("Could not create output directory: ", out_dir)
      }

      if (ext == "pdf") {
        filename <- paste0(path_print, ".pdf")
        write_pdf <- function() {
          grDevices::pdf(
            file = filename, width = width_pg / 2.54,
            height = height_pg / 2.54, onefile = TRUE,
            bg = bg, useDingbats = FALSE
          )
          on.exit(grDevices::dev.off(), add = TRUE)
          for (p in pages) {
            print(p)
            advance()
          }
        }
        write_pdf()
        advance() # PDF file successfully closed
        if (verbose) message("Saved PDF (", n_pages, " pages): ", filename)

      } else if (ext == "pptx") {
        if (!requireNamespace("officer", quietly = TRUE) ||
            !requireNamespace("rvg", quietly = TRUE) ||
            !requireNamespace("xml2", quietly = TRUE)) {
          stop("PPTX export requires officer, rvg and xml2.")
        }

        ppt <- officer::read_pptx()

        # Set the REAL slide size; no external template is required.
        # Note: this relies on officer internals, not a public setter.
        presentation_xml <- ppt$presentation$get()
        size_node <- xml2::xml_find_first(
          presentation_xml, "//*[local-name()='sldSz']"
        )
        if (inherits(size_node, "xml_missing")) {
          stop("Cannot locate PowerPoint slide size (p:sldSz). ",
               "The internal structure of officer may have changed.")
        }
        xml2::xml_set_attr(size_node, "cx", as.character(round(width_pg * 360000)))
        xml2::xml_set_attr(size_node, "cy", as.character(round(height_pg * 360000)))
        xml2::xml_set_attr(size_node, "type", "custom")

        dim_ppt <- officer::slide_size(ppt)
        if (!isTRUE(all.equal(as.numeric(dim_ppt[["width"]]), width_pg / 2.54,
                              tolerance = 1e-6)) ||
            !isTRUE(all.equal(as.numeric(dim_ppt[["height"]]), height_pg / 2.54,
                              tolerance = 1e-6))) {
          stop("PowerPoint slide size verification failed.")
        }

        for (p in pages) {
          ppt <- officer::add_slide(ppt, layout = "Blank")
          ppt <- officer::ph_with(
            ppt,
            value = rvg::dml(ggobj = p, editable = TRUE),
            location = officer::ph_location(
              left = 0, top = 0,
              width = width_pg / 2.54,
              height = height_pg / 2.54
            )
          )
          advance()
        }

        filename <- paste0(path_print, ".pptx")
        print(ppt, target = filename)
        advance() # PPTX file successfully written
        if (verbose) message("Saved editable PPTX (", n_pages,
                             " slides): ", filename)

      } else {
        if (ext == "svg" && !requireNamespace("svglite", quietly = TRUE)) {
          stop("SVG export requires 'svglite'.")
        }
        if (ext == "emf" && !requireNamespace("devEMF", quietly = TRUE)) {
          stop("EMF export requires 'devEMF'.")
        }
        emf_device <- function(filename, width, height, bg = "transparent", ...) {
          devEMF::emf(file = filename, width = width, height = height, bg = bg)
        }
        digits <- max(2L, nchar(as.character(n_pages)))

        for (page in seq_along(pages)) {
          suffix <- if (n_pages == 1L) "" else
            sprintf(paste0("_page_%0", digits, "d"), page)
          filename <- paste0(path_print, suffix, ".", ext)
          ggplot2::ggsave(
            filename = filename, plot = pages[[page]],
            device = if (ext == "emf") emf_device else ext,
            width = width_pg, height = height_pg,
            units = "cm", dpi = dpi, bg = bg
          )
          advance()
          if (verbose) message("Saved: ", filename)
        }
        advance() # All page files successfully written
      }
    }

    if (return_plot) return(pages)
    invisible(NULL)
  }

  if (suppress_warning) suppressWarnings(core_execution()) else core_execution()
}
