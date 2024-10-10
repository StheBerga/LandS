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
#' @param name_project The name of the Project
#' @param pc Which pc are we operating
#'
#' @return Returns a folder in Projects with Analisi, Dati and Output subfolders
#' @export
#'
#' @examples
New_Project_LB <- function(name_project, pc = c("Luca", "Stefano")){

  if (pc == "Luca"){
    path_project = "//fileserversvc/biomimmunol/Projects/"
  } else if (pc == "Stefano"){
    path_project = "/Volumes/biomimmunol/Projects/"
  }

  main_path <- paste0(path_project, name_project)
  dir.create(path = main_path)
  dir.create(path = paste0(main_path, "/Analisi"))
  dir.create(path = paste0(main_path, "/Dati"))
  dir.create(path = paste0(main_path, "/Output"))
  dir.create(path = paste0(main_path, "/Dati/Original"))

}

#' Function to send a Telegram message with BiostatUO9 bot. NB: must create a start_time before running it
#'
#' @param process_time Just don't modify it
#' @param dest Who is going to receive the message
#' @param script The title of the message
#' @param rm_start_time If you want the start_time item to be removed after the message is sent
#'
#' @return Nothing
#' @export
#'
#' @examples
telegram_mess_LB <- function(process_time = {
  format(lubridate::seconds_to_period(round(as.numeric(difftime(Sys.time(), start_time, units = "secs")))), "%H:%M:%S")
}, dest = "both", script = 0, rm_start_time = T){

  bot = telegram.bot::Bot(token = telegram.bot::bot_token("BiostatUO9_bot"))
  updates = bot$getUpdates()

  chat_id <- switch(dest,
                    "both" = c(629518490, 285721593),
                    "Ste" = 629518490,
                    "Luca" = 285721593)

  for (i in chat_id) {

    if(script == 0){
      bot$sendMessage(chat_id = i,
                      text = paste0("Lo script ha runnato correttamente in ", process_time))
    } else {

      bot$sendMessage(chat_id = i,
                      text = paste0("Lo script ", script,  " ha runnato correttamente in ", process_time))

    }

  }

  if(rm_start_time == T){
    rm(start_time, envir = .GlobalEnv)
  }
}


#' Function to print the PDF with the grid.arrange function
#'
#' @param plot_list The list you want to be plotted
#' @param path_print The path where you want your PDF to be printed
#' @param nrow Rows of your grid
#' @param ncol Columns of your grid
#' @param variables Number of total graphs to be printed
#'
#' @return A pdf in the path_output
#' @export
#'
#' @examples
PDF_print_LB <- function(plot_list, path_print = path_print, nrow = 8, ncol = 6, variables = vett_all_markers){

  npag <- ceiling(length(variables)/(nrow*ncol))
  graphs <- list()

  for (i in 1:npag){

    graphs[[i]] <- grid.arrange(grobs=plot_list[(((i-1)*nrow*ncol)+1):min(length(variables),
                                                                          (i*nrow*ncol))], nrow=nrow, ncol=ncol,  top=paste0("Pag. ", i))

  }

  message("Grid arrange Done!")

  pdf(file = path_print, width = 21, height = 29.7)
  for (i in 1:length(graphs)) {
    plot(graphs[[i]])
  }

  dev.off()
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

