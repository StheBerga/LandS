#' Display a console progress bar with timing and memory usage
#'
#' @description Prints a dynamic progress bar in the console for iterative processes,
#' including percentage completion, elapsed time, estimated time remaining (ETA),
#' and current memory usage.
#'
#' @param current Integer. Current iteration index (e.g., loop counter).
#' @param total Integer. Total number of iterations.
#' @param start_time POSIXct. Start time of the process, typically obtained with
#'   'Sys.time()' before entering the loop.
#' @param bar_length Integer. Width of the progress bar in characters (default 80).
#' @param bar_fill Symbol used to represent completed progress (default "=").
#' @param bar_void Character. Symbol used to represent remaining progress
#' (default = " ").
#' @param logfile Character or 'NULL'. If a file path is provided, progress
#' messages are appended to this file. Default to 'NULL' (no logging).
#'
#' @returns Invisibly returns `NULL`. The function is used for its side effects
#' (console output and optional logging).
#' @export
#'
#' @author Luca Lalli, Stefano Bergamini
#'
#' @examples
#' start_time <- Sys.time()
#' total <- 30
#' for (i in 1:total) {
#'   Sys.sleep(0.005)  # simulate work
#'   Progress_bar(
#'     current = i,
#'     total = total,
#'     start_time = start_time
#'   )
#'}
Progress_bar <- function (current,
                          total,
                          start_time,
                          bar_length = 80,
                          bar_fill = "=",
                          bar_void = " ",
                          logfile = NULL)
{
   # if (!requireNamespace("pryr", quietly = TRUE))
   #    install.packages("pryr")
   # library(pryr)
   format_hms <- function(seconds) {
      hrs <- floor(seconds/3600)
      mins <- floor((seconds%%3600)/60)
      secs <- round(seconds%%60)
      sprintf("%02d:%02d:%02d", hrs, mins, secs)
   }
   elapsed <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
   percent <- current/total
   filled_length <- round(bar_length * percent)
   avg_time <- if (current > 0)
      elapsed/current
   else 0
   eta <- if (current < total)
      avg_time * (total - current)
   else 0
   ram_gb <- round(as.numeric(pryr::mem_used())/1024^3, 2)
   bar <- paste0("\033[92m", strrep(bar_fill, filled_length),
                 "\033[90m", strrep(bar_void, bar_length - filled_length),
                 "\033[0m")
   cat(sprintf("\r%s %3d%%  %3d/%d | %s | ETA: %s | RAM: %.2fGb%s",
               bar,
               round(percent * 100), current, total,
               format_hms(elapsed), format_hms(eta), ram_gb,
               if (current == total) "\n" else ""))


   flush.console()
   current_time <- format(Sys.time(), "%H:%M:%S")
   if (!is.null(logfile)) {
      msg <- sprintf("[%s] Ciclo %d/%d %s - Elapsed: %s - RAM: %.2fGb",
                     current_time, current, total, if (current == total)
                        "COMPLETATO"
                     else "completato", format_hms(elapsed), ram_gb)
      write(msg, file = logfile, append = TRUE)
   }
}
