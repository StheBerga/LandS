#' Extract selected variables and thresholds from AIM output
#'
#' @description
#' Extracts the final model from an object returned by the AIM algorithm and
#' converts it into a readable data frame containing:
#' \itemize{
#'   \item variable names,
#'   \item decision direction (">" or "<"),
#'   \item cutoff values.
#' }
#'
#' The function assumes that res.index$res is a list where the last element
#' corresponds to the final model, and that it contains at least the columns
#' jmax (variable indices), maxdir (direction encoded as 1 or -1), and
#' cutp (cutoff values).
#'
#' @param res.index A list-like object returned by the AIM function. Must contain
#'   a component res, which is a list of intermediate models. The last element
#'   is treated as the final model.
#' @param aim.data A data frame used to fit the AIM model. Column names are used
#'   to map variable indices (jmax) to variable names.
#'
#' @return A data frame with three columns:
#' \describe{
#'   \item{Vars}{Character vector of variable names selected by the model}
#'   \item{Dir}{Character vector indicating direction of the rule (">" or "<")}
#'   \item{Cutoff}{Numeric vector of cutoff values}
#' }
#'
#' @details
#' The function performs the following steps:
#' \enumerate{
#'   \item Extracts the final model from res.index$res.
#'   \item Maps variable indices (jmax) to column names in aim.data.
#'   \item Converts direction encoding (1 / other) into ">" and "<".
#'   \item Returns a simplified data frame.
#' }
#'
#' @export
#'
#' @examples
#' # Example with mtcars (mocking AIM output structure)
#'
#' library(dplyr)
#'
#' # Simulated AIM-like result
#' res.index <- list(
#'   res = list(
#'     data.frame(
#'       jmax = c(1, 3),
#'       maxdir = c(1, -1),
#'       cutp = c(20, 150)
#'     )
#'   )
#' )
#'
#' # Use mtcars as input data
#' output.aim.f(res.index, mtcars)
#'
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
#' @author Luca Lalli, Stefano Bergamini
#'
#' @examples
Kmax_aim <- function(kmax.cycle = kmax.cycle){
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
