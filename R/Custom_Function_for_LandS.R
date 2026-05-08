# Custom functions for LandS package ----

#' Format p-values for reporting
#'
#' @description
#' Formats numeric p-values for display in tables and plot annotations.
#' Values greater than or equal to '0.0001' are rounded to four decimal places
#' and printed with four digits after the decimal point. Values below '0.0001'
#' are displayed as "<0.0001". Missing values are preserved as NA.
#'
#' The function accepts a single numeric value, a numeric vector, or a data frame.
#' If a data frame is supplied, only the first column is formatted and the other
#' columns are returned unchanged.
#'
#' @param value A numeric scalar, numeric vector, or data frame containing
#'   p-values to format. For data frames, p-values must be in the first column.
#'
#' @return
#' An object of the same general type as 'value': a formatted character value,
#' a character vector, or a data frame with its first column formatted.
#' @export
#'
#' @examples
#' formatz_p(0.03215)
#' formatz_p(c(0.2, 0.0499, 0.00001, NA))
#' formatz_p(data.frame(p = c(0.03, 0.00001, NA)))
formatz_p <- function(value){
  if(is.data.frame(value)==T){
    new_frame <- value

    for(i in 1:dim(value)[1]){

      if(is.na(value[i, 1]) == T) {

        new_frame[i, 1] = NA
      }

      else if(value[i, 1] >= 0.0001){
        new_frame[i, 1] <- format(round(value[i,1], 4), digits = 4, nsmall = 4, width = 6, scientific=F, justify = "centre")
      }else if (value[i, 1] < 0.0001){
        new_frame[i, 1] <- "<0.0001"
      }
    }
    return(new_frame)
  }else if (is.vector(value) == TRUE){
    new_vett <- c()

    for (i in 1:length(value)) {


      if (is.na(value[i])){

        p <- NA

      }else if (value[i] >= 0.0001){

        p <- format(round(value[i], 4), digits = 4, nsmall = 4, width = 6, scientific=F, justify = "centre")

      }else if (value[i] < 0.0001){

        p <- "<0.0001"
      }
      new_vett <- c(new_vett,p)
    }
    return(new_vett)
  }else{

    if(is.na(value) == T) {

      value = NA
    }
    else if(value >= 0.0001){
      value <- format(round(value, 4), digits = 4, nsmall = 4, width = 6, scientific=F, justify = "centre")
    }else if(value < 0.0001){
      value <- "<0.0001"}
    return(value)
  }
}

#' Collapse a vector into a formatted string
#'
#' @description Format a vector as a single string with a custom separator,
#' optionally quoting each element. The result is printed to the console.
#'
#' @param vect A vector of values to format.
#' @param sym A character string used as separator between elements.
#' Defaults to ", ".
#' @param quote Logical. If TRUE, each element is wrapped in double quotes
#' (Default = T).
#'
#' @return Invisibly returns NULL. The formatted string is printed to the console.
#' @export
#'
#' @examples
#' vect.quoted(1:3, sym = " | ", quote = FALSE)
#' vect.quoted(1:3, sym = " | ", quote = TRUE)
vect.quoted <- function(vect, sym = ", ", quote = T){
  if (quote == T){
    cat(dQuote(vect, q = '"'), sep = sym)
  } else {
    cat(vect, sep = sym)
  }
}

#' Create a new project with a standard folder structure
#'
#' New_Project() creates a new project directory inside a platform-specific
#' root folder and optionally initializes it as an RStudio project. By default,
#' it creates the subdirectories "Analysis", "Data", "Data/Original", and
#' "Output", but users can provide their own folder structure through
#' 'subdirs'.
#'
#' @param project_name Character string. Name of the project directory to create.
#' @param rstudio Logical. If TRUE, create an RStudio project file using
#'   [usethis::use_rstudio()]. If FALSE, create a '.here' sentinel file.
#' @param open Logical. If TRUE, activate the newly created project using
#'   [usethis::proj_activate()].
#' @param root_x86_64 Character string. Default root directory used when
#'   'Sys.info()[["machine"]]' is "x86-64".
#' @param root_arm64 Character string. Default root directory used when
#'   'Sys.info()[["machine"]]' is "arm64".
#' @param root_manual Character string or NULL. Manually supplied root
#'   directory used when the machine architecture is not recognized.
#' @param subdirs Character vector. Subdirectories to create inside the project.
#'   Nested directories can be supplied using /, for example
#'   "data/raw" or "results/figures".
#'
#' @return Invisibly returns the path to the newly created project.
#' @export
#'
#' @author Luca Lalli, Stefano Bergamini
#'
#' @examples
#' \dontrun{
#' New_Project(
#'   "my_project",
#'   root_x86_64 = "~/Projects/",
#'   subdirs = c("data/raw", "data/processed", "R", "outputs")
#' )
#' }
New_Project <- function(project_name,
                        rstudio = rstudioapi::isAvailable(),
                        open = rlang::is_interactive(),
                        root_x86_64 = "//irccs-int.local/int/Bioimmunol/Projects/",
                        root_arm64 = "/Volumes/biomimmunol/Projects/",
                        root_manual = NULL,
                        subdirs = c("Analysis", "Data", "Data/Original", "Output"))
{

  if(Sys.info()["machine"] %in% c("x86-64", "x86_64")){
    root <- root_x86_64
  } else if (Sys.info()["machine"] == "arm64"){
    root <- root_arm64
  } else {
    if(!is.null(root_manual)){
      root <- root_manual
      warning("No architecture recognized, using root_manual argument.")
    } else{
      stop("No architecture recognized, set root_manual!")
    }
  }


  path <- usethis:::user_path_prep(paste0(root, project_name))
  name <- fs::path_file(fs::path_abs(path))
  usethis:::challenge_nested_project(fs::path_dir(path), name)
  usethis:::challenge_home_directory(path)
  usethis:::create_directory(path)
  usethis:::local_project(path, force = TRUE)

  # Add subdirectories
  for (dir in subdirs) {
    usethis::use_directory(dir)
  }

  if (rstudio) {
    usethis::use_rstudio()
  }
  else {
    usethis:::ui_bullets(c(v = "Writing a sentinel file {.path {pth('.here')}}.",
                 `_` = "Build robust paths within your project via {.fun here::here}.",
                 i = "Learn more at {.url https://here.r-lib.org}."))
    fs::file_create(usethis::proj_path(".here"))
  }
  if (open) {
    if (usethis::proj_activate(usethis::proj_get())) {
      withr::deferred_clear()
    }
  }
  invisible(usethis::proj_get())
}


#' Save a grid of plots
#'
#' Combines a list of ggplot objects into one or more grids using
#' cowplot::plot_grid() and saves the result to disk. Supported output formats
#' are "pdf", "tiff", "jpeg", "svg", "png", and "emf".
#'
#' For PDF output, plots are split across multiple pages when the number of
#' plots exceeds nrow * ncol. For other formats, all plots are arranged in a
#' single grid.
#'
#' @param plot_list A list of ggplot objects.
#' @param path_print Character string. Output file path without extension.
#' Default is ".".
#' @param nrow Integer. Number of rows in the plot grid. Default is 8.
#' @param ncol Integer. Number of columns in the plot grid. Default is 6.
#' @param ext Character string. Output file extension. One of "pdf",
#' "tiff", "jpeg", "svg", "png", or "emf". If NULL, "pdf" is used.
#' @param width_pg Numeric. Output width in centimeters. Default is 21.
#' @param height_pg Numeric. Output height in centimeters. Default is 29.7.
#' @param return_plot Logical. If TRUE, returns the list of grid plots created
#' by cowplot::plot_grid(). If FALSE, returns NULL.
#'
#' @return
#' Invisibly returns NULL if return_plot = FALSE. If return_plot = TRUE,
#' returns a list of grid plot objects. In both cases, a file is written to disk.
#'
#' @author Luca Lalli, Stefano Bergamini
#' @export
#'
#' @examples
#' \dontrun{
#' library(ggplot2)
#'
#' plots <- list(
#'   ggplot(mtcars, aes(wt, mpg)) +
#'     geom_point() +
#'     labs(title = "MPG vs weight"),
#'
#'   ggplot(mtcars, aes(factor(cyl), mpg)) +
#'     geom_boxplot() +
#'     labs(title = "MPG by cylinders", x = "Cylinders"),
#'
#'   ggplot(mtcars, aes(hp, mpg)) +
#'     geom_point() +
#'     labs(title = "MPG vs horsepower"),
#'
#'   ggplot(mtcars, aes(factor(gear))) +
#'     geom_bar() +
#'     labs(title = "Number of cars by gears", x = "Gears")
#' )
#'
#' print_plot_grid(
#'   plot_list = plots,
#'   path_print = file.path(".", "mtcars_grid"),
#'   nrow = 2,
#'   ncol = 2,
#'   ext = "pdf"
#' )
#' }
print_plot_grid <- function (plot_list,
                             path_print = ".",
                             nrow = 8,
                             ncol = 6,
                             ext = NULL,
                             width_pg = 21,
                             height_pg = 29.7,
                             return_plot = FALSE)
{

  if(is.null(ext)){ext <- "pdf"}

  path_print <- paste0(path_print, ".", ext)
  variables <- length(plot_list)
  graphs <- list()

  if (ext == "pdf"){
    npag <- ceiling(variables/(nrow * ncol))
    for (i in 1:npag) {

      graphs[[i]] <- cowplot::plot_grid(plotlist = plot_list[(((i - 1) * nrow * ncol) + 1) : min(variables, (i * nrow * ncol))],
                                        nrow = nrow, ncol = ncol)

      graphs[[i]] <- graphs[[i]] + ggplot2::theme_minimal() +
        ggplot2::labs(title = paste0("Page ", i)) + ggplot2::theme(plot.title = ggplot2::element_text(hjust = .5, face = "bold"))

    }
  } else {

    graphs[[1]] <- cowplot::plot_grid(plotlist = plot_list, nrow = nrow, ncol = ncol)

  }
  message(paste0("cowplot::plot_grid done! \nSaving in ", ext))

  if(ext == "tiff"){tiff(filename = path_print, width = width_pg, height = height_pg, units = "cm", res = 300)}

  if(ext == "jpeg"){jpeg(file = path_print, width = width_pg, height = height_pg, units = "cm", res = 300)}

  if(ext == "pdf"){pdf(file = path_print, width = (width_pg/2.54), height = (height_pg/2.54))}

  if(ext == "svg"){svg(file = path_print, width = (width_pg/2.54), height = (height_pg/2.54))}

  if(ext == "png"){png(file = path_print, width = width_pg, height = height_pg, units = "cm", res = 300)}

  if(ext == "emf"){devEMF::emf(file = path_print, width = (width_pg/2.54), height = (height_pg/2.54))}

  for (i in 1:length(graphs)) {
    plot(graphs[[i]])
  }
  dev.off()

  if (return_plot == TRUE){
    return(graphs)
  }
}


#' Build path to file
#'
#' @description
#' This function builds a complete path to file including:
#' output path + filename + extension.
#'
#' @param filename string. Name of the file, default="example".
#' @param extension string. File extension, default=".png".
#' @param output string. Output path, default=".".
#' @param datetime logical. Whether to append the datetime in format.
#' Default FALSE.
#'
#' @return String of the filename.
#' @export
#'
#' @author Luca Lalli, Stefano Bergamini
#'
#' @examples
#' build_filename(filename = "example", extension = ".png", output = ".",
#' datetime = F)
#' build_filename(filename = "example", extension = ".png", output = ".",
#' datetime = T)
build_filename <- function(filename = "example",
                     extension = ".png",
                     output = ".",
                     datetime = F){
  if (datetime == F){
    return(paste0(output, "/", filename, extension))
  }else {
    return(paste0(output, "/", filename, "_",
                  format(Sys.time(), "%m_%d_%Y__%H_%M"), extension))
  }

}

#' Format current system time as a string
#'
#' @description
#' Returns the current system time (Sys.time()) formatted as a character
#' string using the pattern "month_day_Year__Hour_Minute". This format is convenient
#' for file names or logs because it avoids spaces and special characters.
#'
#' Example: "05_05_2026__14_32"
#'
#' @return A character string representing the current system time.
#'
#' @export
#'
#' @author Luca Lalli, Stefano Bergamini
#'
#' @examples
#' # Get current time as formatted string
#'format_sys_time()
#'
#' # Use in a filename
#' paste0("results_", format_sys_time(), ".csv")
format_sys_time <- function()
{
  format(Sys.time(), "%m_%d_%Y__%H_%M", zero.print = F)
}


#' Format p-values for Boxplots
#'
#' @description
#' This is an internal function used to format p-values to display in boxplots.
#' If p-value < threshold_posthoc, the corresponding row in the output dataframe
#' is removed.
#'
#' @param Test_results Dataframe containing the results of global and posthoc tests computed
#' using LandS::cont_var_test() function, in particular objects "KW_ph_pval" or "Friedman_ph_pval"
#' or "no_corrected_ph". Default = NULL.
#' @param data Dataframe containing numeric variables to plot.
#' @param group string. Column name identifying factor grouping variable for split
#' boxplots.
#' @param threshold_posthoc numeric. Threshold for post-hoc tests.
#' @param i string. Name of variable to consider.
#'
#' @return Dataframe having has columns: group1, group2, y (i.e. the variable)
#' and pval (i.e. formatted p-value).
#'
#' @author Luca Lalli, Stefano Bergamini
#'
#' @examples
#' res <- cont_var_test(data = iris, variables = c("Sepal.Length", "Sepal.Width"),
#' group = "Species", paired = FALSE)
#'
#' # Formatted results
#' formatted_list <- list()
#' for(n in res$KW_ph_pval$Var){
#'   formatted_list[[n]] <- LandS:::posthoc_df(res$KW_ph_pval, iris, group="Species",
#'   threshold_posthoc=0.01, n)
#' }
#'
posthoc_df <- function(Test_results, data, group, threshold_posthoc, i){

  postmodel <- Test_results[Test_results[, 1] == i, ]
  posthoc_df <- as.data.frame(t(combn(levels(data[, group]),2)))
  colnames(posthoc_df) <- c("group1", "group2")
  posthoc_df$y <- i
  posthoc_df$pval <- NA
  posthoc_df$pval <- as.numeric(as.vector(postmodel[, (ncol(postmodel)+1-nrow(posthoc_df)):ncol(postmodel)]))
  posthoc_df <- posthoc_df[!posthoc_df$pval >= threshold_posthoc, ]

  if(nrow(posthoc_df) == 0) {}else{
    posthoc_df$pval <- LandS::formatz_p(posthoc_df$pval)
  }
  return(posthoc_df)

}

#' Plot variable distribution
#'
#' @description
#' This function outputs a ggplot showing the distribution of a single variable,
#' where points are optionally divided by a cut-off value (cutoff).
#'
#'
#' @param data A dataframe containing the variable to plot.
#' @param var string. Name of the variable to plot.
#' @param split logical. If TRUE, the variable is split by a cut-off value.
#' Default=FALSE.
#' @param cutoff numeric. Cut-off value to split the variable. Default=NULL.
#'
#' @return A ggplot object.
#' @export
#'
#' @author Luca Lalli, Stefano Bergamini
#'
#' @examples
#' Distribution(data = mtcars, var = "mpg", split = TRUE, cutoff = 23)
Distribution <- function(data, var, split = FALSE, cutoff = NULL){

  if(split==TRUE && is.null(cutoff)){
    stop("Please provide a cutoff value if you use split=TRUE")
  }


  ggplot2::ggplot(data = data, ggplot2::aes_string(x = data[, var], y = 1))+
    {if (split == FALSE)
      ggplot2::geom_point(size = 1.5)}+

    {if (split == TRUE)
      ggplot2::geom_point(aes(colour = data[, var] > cutoff), size = 1.5)}+

    {if (split == TRUE)
      ggplot2::geom_vline(xintercept = cutoff, linetype = 2)}+

    ggplot2::scale_colour_manual(values=c("salmon", "cornflowerblue"))+

    {if (split == FALSE)
      ggplot2::labs(x = NULL, y = NULL)}+

    {if (split == TRUE)
      ggplot2::labs(x = paste0("Cut-off: ", cutoff), y = NULL)}+

    # coord_fixed(ratio = 50/2)+
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5, size = 6, face = "bold"),
          axis.text.x = ggplot2::element_text(size = 12, colour = "black"),
          axis.text.y.left = ggplot2::element_blank(),
          axis.ticks.y.left = ggplot2::element_blank(),
          panel.background = ggplot2::element_blank(),
          panel.grid.major = ggplot2::element_blank(),
          panel.grid.minor = ggplot2::element_blank(),
          legend.position = "bottom",
          legend.title = ggplot2::element_blank(),
          panel.border = ggplot2::element_rect(linetype = "solid", colour = "black", size=0.2, fill=NA))
}


#' Format p-values for Lineplots
#'
#' @description
#' This is an internal function used to format p-values to display in lineplots.
#' If p-value < threshold_posthoc, the corresponding row in the output dataframe
#' is removed.
#'
#' @param Test_results Dataframe containing the results of global and posthoc
#' tests computed using LandS::cont_var_test() function, in particular objects
#' "KW_ph_pval" or "Friedman_ph_pval" or "no_corrected_ph".
#' @param data Dataframe containing numeric variables to plot.
#' @param time string. Name of the numeric variable containing time.
#' @param threshold_posthoc numeric. Threshold for post-hoc tests.
#' @param i string. Name of variable to consider.
#'
#' @returns
#' Dataframe having has columns: group1, group2, y (i.e. the variable)
#' and pval (i.e. formatted p-value).
#'
#' @author Luca Lalli, Stefano Bergamini
#'
#' @examples
#' mtcars$gear <- as.factor(mtcars$gear)
#' res <- cont_var_test(data = mtcars, variables = c("mpg", "disp"),
#' group = "gear", paired = FALSE)
#'
#' # Formatted results
#' mtcars$gear <- as.numeric(levels(mtcars$gear))[mtcars$gear]
#' formatted_list <- list()
#' for(n in res$KW_ph_pval$Var){
#'   formatted_list[[n]] <- LandS:::Posthoc_lineplots(res$KW_ph_pval, mtcars, time="gear",
#'                                             threshold_posthoc=0.01, n)
#' }
Posthoc_lineplots <- function (Test_results, data, time, threshold_posthoc, i) {

  postmodel <- Test_results[Test_results[, 1] == i, ]
  posthoc_df <- combn(levels(factor(data[, time])), 2) %>%
    t() %>% as.data.frame() %>%
    `colnames<-`(c("group1", "group2")) %>%
    mutate(across(1:2, as.numeric))
  posthoc_df$y <- i
  posthoc_df$pval <- NA
  posthoc_df$pval <- as.numeric(as.vector(postmodel[, (ncol(postmodel) + 1 - nrow(posthoc_df)):ncol(postmodel)]))
  posthoc_df <- posthoc_df[!posthoc_df$pval >= threshold_posthoc, ]
  if (nrow(posthoc_df) == 0) {
  } else {
    posthoc_df$pval <- LandS::formatz_p(posthoc_df$pval)
  }
  return(posthoc_df)
}


#' Create a Standardized and Formatted Flextable
#'
#' @description
#' Converts a `data.frame` into a beautifully formatted `flextable`. It applies
#' standard styling including center alignment, bold headers, customizable fonts,
#' and borders. It also offers optional features like highlighting significant p-values,
#' bolding specific columns, and adding a styled table caption.
#'
#' @param data A `data.frame` to be converted into a table.
#' @param fontname Character string for the font family. Default is `"Calibri Light"`.
#' @param bold_cols Character vector or numeric indices of columns to be bolded.
#' Default is `NULL` (no extra bold columns).
#' @param pval_col Character string specifying the exact name of the p-value column.
#' If provided, p-values < 0.05 will be colored red. Default is `NULL`.
#' @param max_width Numeric. Maximum width of the table in centimeters. Default is `18`.
#' @param width_border Numeric. Line width for the left and right outer borders. Default is `0.8`.
#' @param caption Character string for the table caption. Default is `NULL`.
#'
#' @return A formatted `flextable` object.
#'
#' @export
#'
#' @examples
#' df <- data.frame(Variable = c("Value_A", "Value_B", "Value_C"), p_val = c(0.01, 0.20, 0.002))
#'
#' # Advanced usage with new parameters
#' Flex_format(
#'   data = df,
#'   fontname = "Arial",
#'   bold_cols = 1,
#'   pval_col = "p_val",
#'   max_width = 15,
#'   width_border = 1.2,
#'   caption = "Table 1: Patient Characteristics"
#' )
Flex_format <- function(data,
                        fontname = "Calibri Light",
                        bold_cols = NULL,
                        pval_col = NULL,
                        max_width = 18,
                        width_border = 0.8,
                        caption = NULL) {

  # Initialize flextable
  ft <- flextable::flextable(data)

  # General standard formatting
  ft <- flextable::align(ft, part = "all", align = "center")
  ft <- flextable::bold(ft, part = "header")
  ft <- flextable::font(ft, fontname = fontname, part = "all")

  # Optional: Bold specific columns
  if (!is.null(bold_cols)) {
    ft <- flextable::bold(ft, j = bold_cols)
  }

  # Optional: Highlight significant p-values (p < 0.05 in red)
  if (!is.null(pval_col) && pval_col %in% colnames(data)) {

    # First, color the entire p-value column red
    ft <- flextable::color(ft, color = "red", j = pval_col)

    # Helper function to safely evaluate p-values, ignoring strings like "<0.001"
    is_non_significant <- function(x) {
      num_x <- suppressWarnings(as.numeric(gsub("[<>]", "", x)))
      return(is.na(num_x) | num_x >= 0.05)
    }

    # Identify row indices where the p-value is NOT significant (>= 0.05)
    row_idx <- which(is_non_significant(data[[pval_col]]))

    # Revert those specific rows back to black
    if (length(row_idx) > 0) {
      ft <- flextable::color(ft, i = row_idx, j = pval_col, color = "black")
    }
  }

  # Layout and sizing
  ft <- flextable::autofit(ft)
  ft <- flextable::fit_to_width(ft, max_width = max_width, unit = "cm")

  # Borders (using flextable namespace)
  border_style <- flextable::fp_border_default(color = "black", width = width_border)
  ft <- flextable::vline_right(ft, border = border_style)
  ft <- flextable::vline_left(ft, border = border_style)

  # Optional: Add Caption
  if (!is.null(caption)) {
    ft <- flextable::set_caption(
      ft,
      caption = flextable::as_paragraph(
        flextable::as_chunk(caption, props = flextable::fp_text_default(font.family = fontname))
      ),
      word_stylename = "Table Caption"
    )
  }

  return(ft)
}


#' Export and Quickly Open Tables or Plots in Word or PowerPoint
#'
#' @description
#' This function takes a `data.frame`, a `ggplot` object, or a `flextable`,
#' formats it (applying `Flex_format` if necessary), saves it to a temporary file,
#' and automatically opens the file using the system's default application.
#'
#' @param x The object to export. Can be a `data.frame`, a `ggplot` object,
#' or a `flextable` compatible object.
#' @param type string. The desired export format: `"docx"` (default) or `"pptx"`.
#' Note: if `x` is a `ggplot` object, the export will be forced to `"pptx"`.
#' @param pptx_width Numeric. Width of the plot in inches for PowerPoint export.
#' Default: 4.
#' @param pptx_height Numeric. Height of the plot in inches for PowerPoint export.
#' Default: 5.
#' @param as_dml Logical. If `TRUE` (default), `ggplot` objects are exported as
#' editable vectorized graphic (DML).
#' If `FALSE`, they are exported as static PNG images to save memory and
#' processing time for heavy plots.
#' @param ... Additional arguments passed to the `Flex_format` function
#' (e.g., `caption`, `fontname`, `bold_cols`, `max_width`). See function
#' `Flex_format` for the complete list of arguments.
#'
#' @return Returns `invisible(NULL)`. The function is called for its side
#' effect (creating and opening a file).
#'
#' @export
#'
#' @seealso Flex_format
#'
#' @examples
#' \dontrun{
#' # Basic export of a data.frame to docx
#' df <- data.frame(Variable = c("Age", "Weight"), p_val = c(0.01, 0.20))
#' Printable(df, type = "docx")
#'
#' # Export a heavy ggplot as a static image to avoid huge file sizes
#' library(ggplot2)
#' p <- ggplot(diamonds, aes(carat, price)) + geom_point()
#' Printable(p, as_dml = FALSE, pptx_width = 7, pptx_height = 5)
#' }
Printable <- function(x, type = c("docx", "pptx"), pptx_width = 4, pptx_height = 5,
                      as_dml = TRUE, ...) {

  # Validate the 'type' argument (defaults to the first element: "docx")
  type <- match.arg(type)

  # --- Internal helper function to open files across different operating systems ---
  open_file <- function(path) {
    if (.Platform$OS.type == "windows") {
      shell.exec(normalizePath(path))
    } else if (Sys.info()["sysname"] == "Darwin") {
      system2("open", args = shQuote(normalizePath(path)))
    } else {
      system2("xdg-open", args = shQuote(normalizePath(path)))
    }
  }

  # --- Handle ggplot objects ---
  if (inherits(x, "ggplot")) {
    tmp_path <- tempfile(fileext = ".pptx")
    ppt <- officer::read_pptx()
    ppt <- officer::add_slide(ppt, layout = "Blank")

    # Check if user wants a vectorized (editable) plot or a static image
    if (as_dml) {
      val <- rvg::dml(ggobj = x)
    } else {
      tmp_img <- tempfile(fileext = ".png")
      ggplot2::ggsave(filename = tmp_img, plot = x, width = pptx_width, height = pptx_height, units = "in", dpi = 300)
      val <- officer::external_img(src = tmp_img, width = pptx_width, height = pptx_height)
    }

    ppt <- officer::ph_with(
      x = ppt,
      value = val,
      location = officer::ph_location(width = pptx_width, height = pptx_height)
    )
    print(ppt, target = tmp_path)

    open_file(tmp_path)
    return(invisible(NULL))
  }

  # --- Handle data.frame objects ---
  if (is.data.frame(x)) {
    # Apply the Flex_format function, passing any additional arguments (...)
    x <- Flex_format(data = x, ...)
  }

  # --- Handle Flextable export ---
  if (type == "docx") {
    tmp_path <- tempfile(fileext = ".docx")
    flextable::save_as_docx(x, path = tmp_path)
    open_file(tmp_path)
  } else if (type == "pptx") {
    tmp_path <- tempfile(fileext = ".pptx")
    flextable::save_as_pptx(x, path = tmp_path)
    open_file(tmp_path)
  }

  invisible(NULL)
}

