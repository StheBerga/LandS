#' Nice progress bar for cycles with different visual outputs
#'
#' @param current The current iteractive value in the cycle
#' @param total The total number of variables in the cycle
#' @param start_time Start time of the cycle
#' @param bar_length Bar width in the console (default 80)
#' @param bar_fill The bar fill character
#' @param bar_void The empty bar character
#' @param logfile If not null e.g. 'Log.txt' prints the loop log
#'
#' @returns Only visual output in the console
#' @export
#'
#' @examples

Progress_bar_LB <- function (current, total, start_time, bar_length = 80, bar_fill = "=",
                             bar_void = " ", logfile = NULL)
{
   if (!requireNamespace("pryr", quietly = TRUE))
      install.packages("pryr")
   library(pryr)
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
