#' Nice progress bar for cycles with different visual outputs
#'
#' @param current The current iteractive value in the cycle
#' @param total The total number of variables in the cycle
#' @param start_time Start time of the cycle
#' @param bar_length Bar width in the console
#' @param bar_fill The bar fill character
#' @param bar_void The empty bar character
#' @param logfile If not null e.g. 'Log.txt' prints the loop log
#'
#' @returns Only visual output in the console
#' @export
#'
#' @examples

Progress_bar_LB <- function(current, total, start_time, bar_length = 90,
                            bar_fill = "=", bar_void = " ",
                            logfile = NULL) {

   if (!requireNamespace("pryr", quietly = TRUE)) install.packages("pryr")
   library(pryr)

   # Funzione per formattare secondi in hh:mm:ss
   format_hms <- function(seconds) {
      hrs <- floor(seconds / 3600)
      mins <- floor((seconds %% 3600) / 60)
      secs <- round(seconds %% 60)
      sprintf("%02d:%02d:%02d", hrs, mins, secs)
   }

   # Calcolo tempo e percentuale completata
   elapsed <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
   percent <- current / total
   filled_length <- round(bar_length * percent)

   # Stima ETA
   avg_time <- if (current > 0) elapsed / current else 0
   eta <- if (current < total) avg_time * (total - current) else 0

   # Calcola RAM usata (in GB)
   ram_gb <- round(as.numeric(pryr::mem_used()) / 1024^3, 2)

   # Costruzione della barra visiva colorata
   bar <- paste0(
      "\033[92m", strrep(bar_fill, filled_length),               # verde
      "\033[90m", strrep(bar_void, bar_length - filled_length),  # grigio chiaro
      "\033[0m"  # reset colori
   )

   # Stampa su console con elapsed, ETA e RAM
   cat(sprintf("\r%s %3d%%  %s | ETA: %s | RAM: %.2fGb",
               bar,
               round(percent * 100),
               format_hms(elapsed),
               format_hms(eta),
               ram_gb))
   flush.console()

   # Orario corrente nel giorno (hh:mm:ss)
   current_time <- format(Sys.time(), "%H:%M:%S")

   if (!is.null(logfile)) {
      # Prepara il messaggio da scrivere nel log (con RAM in GB)
      msg <- sprintf("[%s] Ciclo %d/%d %s - Elapsed: %s - RAM: %.2fGb",
                     current_time,
                     current, total,
                     if (current == total) "COMPLETATO" else "completato",
                     format_hms(elapsed),
                     ram_gb)

      # Scrive nel file di log
      write(msg, file = logfile, append = TRUE)
   }
}
