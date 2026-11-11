#' Display a console progress bar with timing and memory usage
#'
#' @description Prints a dynamic progress bar in the console for iterative processes,
#' including percentage completion, elapsed time, Estimated Residual Time (ERT),
#' and current memory usage.
#'
#' @param current Integer. Current iteration index (e.g., loop counter).
#' @param total Integer. Total number of iterations.
#' @param start_time POSIXct. Start time of the process, typically obtained with
#' \code{Sys.time()} before entering the loop.
#' @param bar_length Integer. Width of the progress bar in characters (Default = 50).
#' @param bar_fill Symbol used to represent completed progress (Default \code{"="}).
#' @param bar_void Character. Symbol used to represent remaining progress
#' (Default = \code{" "}).
#' @param logfile Character or \code{NULL}. If a file path is provided, progress
#' messages are appended to this file. Default to \code{NULL}. (no logging).
#'
#' @details
#' A \code{start_time} object must be defined in the \code{.GlobalEnv} to run the function.
#'
#'
#' @returns Invisibly returns \code{NULL}. The function is used for its side effects
#' (console output and optional logging).
#' @export
#'
#' @author Luca Lalli, Stefano Bergamini
#'
#' @examples
#' start_time <- Sys.time()
#' total <- 30
#' for (i in seq_along(1:total)) {
#'   # simulate work
#'   Sys.sleep(0.005)
#'   Progress_bar(
#'     current = i,
#'     total = total,
#'     start_time = start_time
#'   )
#'}
Progress_bar <- function (current,
                          total,
                          start_time,
                          bar_length = 50,
                          bar_fill = "=",
                          bar_void = " ",
                          logfile = NULL)
{
  # Compute elapsed time from start_time
  elapsed <-
    difftime(Sys.time(), start_time, units = 's') %>%
    as.numeric() %>%
    round(digits = 0)

  average_time <- elapsed / current
  etr <- round(average_time * (total - current), digits = 0)
  fill_length <- floor(bar_length * current / total)

  # Not sure what does it mean to represent the amount of bytes used by R
  # I mean it's not the amount of memory that the function is currently using
  ram_gb <- round(as.numeric(lobstr::mem_used())/1024^3, 2)
  bar <- paste0("\033[92m", strrep(bar_fill, fill_length),
                "\033[90m", strrep(bar_void, bar_length - fill_length),
                "\033[0m")
  cat(sprintf("\r%s %.2f%%  %3d/%d | %s | ERT: %s | RAM: %.2fGb%s",
              bar,
              round(100 * current / total, 2),
              current,
              total,
              hms::hms(lubridate::seconds_to_period(elapsed)),
              hms::hms(lubridate::seconds_to_period(etr)),
              ram_gb,
              if (current == total) {"\n"} else {""}))

  utils::flush.console()
  current_time <- format(Sys.time(), "%H:%M:%S")
  if (!is.null(logfile)) {
    msg <-
      sprintf("[%s] Cycle %d/%d %s - Elapsed: %s - RAM: %.2fGb",
              format(Sys.time(), "%H:%M:%S"), current, total,
              if (current == total) {"COMPLETED"} else {"completed"},
              hms::hms(lubridate::seconds_to_period(elapsed)),
              ram_gb)
    write(msg, file = logfile, append = TRUE)
  }
}
