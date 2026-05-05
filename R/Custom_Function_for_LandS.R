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
#' @param vettore A vector of values to format.
#' @param sym A character string used as separator between elements.
#' Defaults to ", ".
#' @param quote Logical. If TRUE, each element is wrapped in double quotes
#' (Default = T).
#'
#' @return Invisibly returns NULL. The formatted string is printed to the console.
#' @export
#'
#' @examples
#' vett.quoted(1:3, sym = " | ", quote = FALSE)
#' vett.quoted(1:3, sym = " | ", quote = TRUE)
vett.quoted <- function(vettore, sym = ", ", quote = T){
  if (quote == T){
    cat(dQuote(vettore, q = '"'), sep = sym)
  } else {
    cat(vettore, sep = sym)
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
#' New_Project("my_project")
#'
#' New_Project(
#'   "my_project",
#'   root_manual = "~/Projects",
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
  require(usethis); require(fs)

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
  name <- fs::path_file(path_abs(path))
  usethis:::challenge_nested_project(path_dir(path), name)
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
  invisible(proj_get())
}

#' Function to send a Telegram message with BiostatUO9 bot. NB: must create a start_time before running it
#'
#' @param dest Who is going to receive the message
#' @param script The title of the message
#' @param rm_start_time If you want the start_time item to be removed after the message is sent
#' @param timestamp Do you want the time in your message
#'
#' @return Nothing
#' @export
#'
#' @author Luca Lalli, Stefano Bergamini
#'
#' @examples
telegram_mess <- function(dest = "both",
                          script = 0,
                          rm_start_time = TRUE,
                          timestamp = TRUE){

  if(timestamp){
    process_time <- format(lubridate::seconds_to_period(round(as.numeric(difftime(Sys.time(), start_time, units = "secs")))), "%H:%M:%S")
  }

  bot = telegram.bot::Bot(token = telegram.bot::bot_token("BiostatUO9_bot"))
  updates = bot$getUpdates()

  chat_id <- switch(dest,
                    "both" = c(629518490, 285721593),
                    "Ste" = 629518490,
                    "Luca" = 285721593)

  for (i in chat_id) {

    if(timestamp){

      if(script == 0){
        bot$sendMessage(chat_id = i,
                        text = paste0("Lo script ha runnato correttamente in ", process_time))
      } else {

        bot$sendMessage(chat_id = i,
                        text = paste0("Lo script ", script,  " ha runnato correttamente in ", process_time))

      }

    }else{

      if(script == 0){
        bot$sendMessage(chat_id = i,
                        text = paste0("Lo script ha runnato correttamente"))
      } else {

        bot$sendMessage(chat_id = i, text = script)

      }

    }

  }

  if(rm_start_time == T & timestamp == T){
    rm(start_time, envir = .GlobalEnv)
  }
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
  require(ggplot2)
  if(is.null(ext)){ext <- "pdf"}

  path_print <- paste0(path_print, ".", ext)
  variables <- length(plot_list)
  graphs <- list()

  if (ext == "pdf"){
    npag <- ceiling(variables/(nrow * ncol))
    for (i in 1:npag) {

      graphs[[i]] <- cowplot::plot_grid(plotlist = plot_list[(((i - 1) * nrow * ncol) + 1) : min(variables, (i * nrow * ncol))],
                                        nrow = nrow, ncol = ncol)

      graphs[[i]] <- graphs[[i]] + theme_minimal() +
        labs(title = paste0("Pag. ", i)) + theme(plot.title = element_text(hjust = .5, face = "bold"))

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

#' Distribution function
#'
#'
#'
#' @param data a dataframe
#' @param var variable
#' @param split does the variable need to be splitted
#' @param split_rule the splitting rule
#'
#' @return a plot with the variable
#' @export
#'
#' @author Luca Lalli, Stefano Bergamini
#'
#' @examples Distribution(data = mtcars, var = "mpg", split = TRUE, split_rule = 23)
Distribution <- function(data, var, split = FALSE, split_rule = NULL){

  require(ggplot2)

  ggplot(data = data, aes_string(x = data[, var], y = 1))+
    {if (split == FALSE)
      geom_point(size = 1.5)}+

    {if (split == TRUE)
      geom_point(aes(colour = data[, var] > split_rule), size = 1.5)}+

    {if (split == TRUE)
      geom_vline(xintercept = split_rule, linetype = 2)}+

    scale_colour_manual(values=c("salmon", "cornflowerblue"))+

    {if (split == FALSE)
      labs(x = NULL, y = NULL)}+

    {if (split == TRUE)
      labs(x = paste0("Cutoff: ", split_rule), y = NULL)}+

    # coord_fixed(ratio = 50/2)+
    theme(plot.title = element_text(hjust = 0.5, size = 6, face = "bold"),
          axis.text.x = element_text(size = 12, colour = "black"),
          axis.text.y.left = element_blank(),
          axis.ticks.y.left = element_blank(),
          panel.background = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          legend.position = "bottom",
          legend.title = element_blank(),
          panel.border = element_rect(linetype = "solid", colour = "black", size=0.2, fill=NA))
}


#' Sending a Pushover notification on your device
#'
#' @param dest Who is going to receive the notification (Default = both)
#' @param script Text of your message
#' @param timestamp If you want the time printed in your notification, if TRUE it requires a start_time in the .GlobalEnv
#' @param priority Priority of your notification (-2; 2)
#' @param app Your app API key
#' @param title Your title notification
#' @param attachment Path to an image, if you want to attach it
#' @param start_time Start time (Default = NULL)
#'
#' @return
#' @export
#'
#' @author Luca Lalli, Stefano Bergamini
#'
#' @examples
Pushover <- function(dest = "both", script = 0, timestamp = TRUE, priority = 0,
                     app = "ayje1n4x8fi64bupdn5shjnwd8ut95", title = "RStudio",
                     attachment = NULL, start_time = NULL) {
  chat_id <- switch(dest,
                    both = c("grrzfbyxunvhecu46cr5m8mdiv9pno"),
                    Ste = "uwtaoa255uoeauaktnh8cprn6xw3aa",
                    Luca = "u6gu5qc6aujne8csirhbqmghsxzsud")

  if (timestamp) {
    if (!is.null(start_time)) {
      process_time <- format(lubridate::seconds_to_period(
        round(as.numeric(difftime(Sys.time(), start_time, units = "secs")))), "%H:%M:%S")
    } else {
      stop("Argument 'start_time' must be provided when 'timestamp' is TRUE.")
    }

    for (i in chat_id) {
      if (script == 0) {
        pushoverr::pushover(user = i, app = app,
                            message = paste0("Lo script ha runnato correttamente in ", process_time),
                            priority = priority, title = title, attachment = attachment)
      } else {
        pushoverr::pushover(user = i, app = app,
                            message = paste0("Lo script ", script, " ha runnato correttamente in ", process_time),
                            priority = priority, title = title, attachment = attachment)
      }
    }
  } else {
    for (i in chat_id) {
      if (script == 0) {
        pushoverr::pushover(user = i, app = app,
                            message = paste0("Lo script ha runnato correttamente"),
                            priority = priority, title = title, attachment = attachment)
      } else {
        pushoverr::pushover(user = i, app = app,
                            message = script, priority = priority, title = title, attachment = attachment)
      }
    }
  }
}



#' Title
#'
#' @param Test_results Test_results
#' @param data data
#' @param Time Time
#' @param threshold_posthoc Threshold ph tests
#' @param i i
#'
#' @returns
#' @export
#'
#' @author Luca Lalli, Stefano Bergamini
#'
#' @examples
Posthoc_lineplots <- function (Test_results, data, time, threshold_posthoc, i) {
  require(dplyr)
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

