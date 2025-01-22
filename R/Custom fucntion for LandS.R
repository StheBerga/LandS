# Custom functions for LandS package ----

#' Function to get a formatted p-value for a number o a vector of numbers
#'
#' @param value a number or a vector of numbers to be formatted
#'
#' @return a number or a vector of numbers formatted with 4 digits
#' @export
#'
#' @examples formatz_p(c(1.000, 0.75643242, 0.000032431, 0.00214))
#'
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

      }else if (value[i] > 0.0001){

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
      value <- " <0.0001"}
    return(value)
  }
}

# Stringa di vettori
#' Funzione che permette partendo da un vettore, di riscrivere quel vettore in varie forme
#'
#' @param vettore Starting vector
#' @param sym Symbol of separation (Default ", ")
#' @param quote Vector elements to be quoted or not (Default = T)
#'
#' @return Una stringa di elementi formattati al meglio
#' @export
#'
#' @examples
vett.quoted <- function(vettore, sym = ", ", quote = T){
  if (quote == T){
    cat(dQuote(vettore, q = '"'), sep = sym)
  } else {
    cat(vettore, sep = sym)
  }
}

# Funzione per creare progetto e sottocartelle
#' Function to create a new project in the default folder
#'
#' @param project_name The name of the Project
#' @param rstudio If `TRUE`, calls [use_rstudio()] to make the new package or
#'   project into an [RStudio
#'   Project](https://r-pkgs.org/workflow101.html#sec-workflow101-rstudio-projects).
#'    If `FALSE` and a non-package project, a sentinel `.here` file is placed so
#'   that the directory can be recognized as a project by the
#'   [here](https://here.r-lib.org) or
#'   [rprojroot](https://rprojroot.r-lib.org) packages.
#' @param open If `TRUE`, [activates][proj_activate()] the new project:
#'
#'   * If using RStudio desktop, the package is opened in a new session.
#'   * If on RStudio server, the current RStudio project is activated.
#'   * Otherwise, the working directory and active project is changed.
#'
#'
#' @return Returns a folder in Projects with Analisi, Dati and Output subfolders
#' @export
#'
#' @examples
New_Project_LB <- function(project_name, rstudio = rstudioapi::isAvailable(), open = rlang::is_interactive())
{
  require(usethis); require(fs)

  if(Sys.info()["machine"] == "x86-64"){
    root <- "//fileserversvc/biomimmunol/Projects/"
  } else if (Sys.info()["machine"] == "arm64"){
    root <- "/Volumes/biomimmunol/Projects/"
  }
  path <- usethis:::user_path_prep(paste0(root, project_name))
  name <- fs::path_file(path_abs(path))
  usethis:::challenge_nested_project(path_dir(path), name)
  usethis:::challenge_home_directory(path)
  usethis:::create_directory(path)
  usethis:::local_project(path, force = TRUE)
  usethis::use_directory("Analisi")
  usethis::use_directory("Dati")
  usethis::use_directory("Dati/Original")
  usethis::use_directory("Output")
  if (rstudio) {
    use_rstudio()
  }
  else {
    ui_bullets(c(v = "Writing a sentinel file {.path {pth('.here')}}.",
                 `_` = "Build robust paths within your project via {.fun here::here}.",
                 i = "Learn more at {.url https://here.r-lib.org}."))
    file_create(proj_path(".here"))
  }
  if (open) {
    if (proj_activate(proj_get())) {
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
#' @examples
telegram_mess_LB <- function(dest = "both", script = 0, rm_start_time = TRUE, timestamp = TRUE){

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


#' Function to print the PDF with the grid.arrange function
#'
#' @param plot_list The list you want to be plotted
#' @param path_print The path where you want your PDF to be printed
#' @param nrow Rows of your grid
#' @param ncol Columns of your grid
#' @param ext File extention
#' @param width_pg page width in cm
#' @param height_pg page height in cm
#' @param return if you want to assign your grid
#'
#' @return A pdf in the path_output
#' @export
#'
#' @examples

Print_LB <- function (plot_list, path_print = path_print,
                      nrow = 8, ncol = 6, ext = c("pdf", "svg", "png", "emf", "tiff", "jpeg"),
                      width_pg = 21, height_pg = 29.7, return = FALSE)
{
  require(ggplot2)

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
  message(paste0("Grid arrange Done! \nSaving in ", ext))

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

  if (return == TRUE){
    return(graphs)
  }
}

#' Function to print the output of the AIM function with Biomarker, Direction and Cutoff as a data frame model
#'
#' @param res.index An output from the AIM package function
#' @param aim.data Data where the function was run on
#'
#' @return A dataframe-like object
#' @export
#'
#' @examples
output.aim.f <- function(res.index, aim.data){
  output.aim <- as.data.frame(res.index$res[[length(res.index$res)]])
  vars_pos <- output.aim$jmax
  vars_model <- colnames(aim.data)[vars_pos]
  output.aim$jmax <- vars_model
  output.aim <- select(output.aim, jmax, maxdir, cutp)
  output.aim$maxdir <- ifelse(output.aim$maxdir == 1, ">", "<")
  colnames(output.aim) <- c("Vars", "Dir", "Cutoff")
  return(output.aim)
}

#' Function to print the histogram of the AIM::cv.cox.main output
#'
#' @param kmax.cycle The vector of values of the best biomarkers
#'
#' @return an histogram
#' @export
#'
#' @examples
Kmax_aim_LB <- function(kmax.cycle = kmax.cycle){
  require(ggplot)

  df_kmax_cycle <- as.data.frame(table(kmax.cycle))
  df_kmax_cycle$Perc <- round(prop.table(table(kmax.cycle))*100,1)
  plot_kmax_cycle <- ggplot(data=df_kmax_cycle, aes(x=kmax.cycle, y=Perc)) +
    geom_histogram(stat="identity", fill = "royalblue", alpha = 0.7, color = "black")+
    labs(x = "Nvar selected", y = "%")+
    theme(axis.text.x = element_text(size = 12, colour = "black", vjust = -0.0),
          plot.margin = margin(2, 2, 2, 2, "mm"),
          axis.text.y = element_text(size = 12, colour = "black"),
          panel.border = element_rect(linetype = "solid", colour = "black", linewidth = 0.1, fill = NA),
          axis.ticks = element_line(linewidth = 0.1),
          axis.ticks.length = unit(0.25, "mm"),
          panel.background = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          legend.title = element_blank(),
          panel.spacing.x = unit(1.5, "mm"),
          aspect.ratio = 1)
  return(plot_kmax_cycle)
}

#' This function returns the filename to be outputted
#'
#' @param filename name of the file
#' @param extention file extention
#' @param output the main output path
#' @param datetime whether to print the datetime in a cute format
#'
#' @return
#' @export
#'
#' @examples
filename_LB <- function(filename = "Prova",
                        extention = ".png",
                        output = path_output,
                        datetime = F){
  if (datetime == F){
    print(paste0(output, "/", filename, extention))
  }else {
    return(paste0(output, "/", filename, "_", format(Sys.time(), "%m_%d_%Y__%H_%M"), extention))
  }

}

#' Function to get the Sys.time() in a cute and nice format
#'
#' @return The Sys.time() in a cute format
#' @export
#'
#' @examples
Sys_Time_LB <- function()
{
  format(Sys.time(), "%m_%d_%Y__%H_%M", zero.print = F)
}


#' Function for Boxplot_LB
#'
#' @param Test_results Test_results
#' @param data data
#' @param group group
#' @param threshold_posthoc threshold posthoc tests
#' @param i i
#'
#' @return nothing
#' @export
#'
#' @examples
posthoc_df_LB <- function(Test_results, data, group, threshold_posthoc, i){

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
#' @examples Distribution_LB(data = mtcars, var = "mpg", split = TRUE, split_rule = 23)
Distribution_LB <- function(data, var, split = FALSE, split_rule = NULL){

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
#' @examples
Pushover_LB <- function(dest = "both", script = 0, timestamp = TRUE, priority = 0,
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
#' @examples
Posthoc_lineplots_LB <- function (Test_results, data, time, threshold_posthoc, i) {
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
