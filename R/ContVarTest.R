#' Statistical tests for continuous variables split by categories
#'
#' @description
#' The test is selected automatically according to the number of groups and
#' whether observations are paired:
#' \itemize{
#'   \item 2 groups, unpaired: Wilcoxon rank-sum test / Mann-Whitney U test.
#'   \item 2 groups, paired: Wilcoxon signed-rank test.
#'   \item >2 groups, unpaired: Kruskal-Wallis rank-sum test, with Dunn post-hoc tests by default.
#'   \item >2 groups, paired: Friedman rank-sum test, with exact post-hoc tests by default.
#' }
#'
#' You can also perform post-hoc tests without correcting for test multiplicity (option no.adj = T),
#' even if it is not recommended.
#' The following summary statistics can be computed:
#' `median`, `mean`, `sd`, `min`, `max`, `q1`, `q3`, `n`, and `range`.
#'
#' @param data dataframe containing the variables to test.
#' @param variables Character vector with the names of continuous numeric
#' variables to test.
#' @param paired Logical. If `TRUE`, observations are treated as paired or
#' repeated measures across levels of group. (Default=`FALSE`).
#' @param group Character string giving the name of the grouping variable.
#' This column must be a factor with at least two levels.
#' @param statistic Specifies summary statistics to display for each variable. Default = "\{mean\} (\{sd\})".
#' Format is defined by `gtsummary` package.
#' @param id Character string giving the name of the subject identifier column,
#' required when `paired = TRUE`. Default = "ID".
#' @param num_dec Decimal number for summary statistics (Default = 2).
#' @param p_adjust_method Character string specifying the multiplicity
#' adjustment method used for post-hoc tests. Supports all adjustment methods passed to `stats::p.adjust.method`.
#' Passed to the `PMCMRplus` post-hoc test functions. If `NULL` (Default), "fdr" is used.
#' @param excel Logical. If `TRUE`, writes the result list to an Excel workbook
#' with one sheet per result table (Default = `FALSE`).
#' @param excel_path Path where you want to save the Excel file (Default "Results.xlsx").
#' @param verbose  Print progress bar and messages (Default=`TRUE`).
#' @param type Choose the posthoc test to perform when `levels(group)>2`.
#' The function supports all the tests from the `PMCMRplus` package,
#' as well as Pairwise Wilcoxon Rank Sum tests and Pairwise Wilcoxon Signed Rank Tests.
#' By default is set the Exact All-Pairs Comparisons Test when `paired=TRUE`,
#' and the Dunn's All-Pairs Rank Comparison Test when `paired=FALSE`.
#' @param overall_threshold Sets a threhold to not perform posthoc comparisons when the overall test's raw p-value is over (Default = 0.1).
#'
#' @return
#' A named list of data frames. The exact elements depend on the selected test:
#' \describe{
#'   \item{Raw_tests}{Summary statistics and unformatted p-values.}
#'   \item{Form_tests}{Summary statistics and formatted p-values.}
#'   \item{MW or WS _type}{Returned for two-group analyses. }
#'   \item{Friedman_ph_pval}{Friedman global and post-hoc p-values; returned
#'   for paired analyses with more than two groups.}
#'   \item{KW_ph_pval}{Kruskal-Wallis global and post-hoc p-values; returned for
#'   unpaired analyses with more than two groups.}
#' }
#' The name of the last data frame is given by the overall test, posthoc comparisons if performed, and p-adjust method
#' @export
#'
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#'
#' @details
#' For paired analyses, subjects with missing values for a tested variable are
#' removed from that variable-specific analysis to preserve complete pairs or
#' repeated-measure blocks.
#' @author Stefano Bergamini, Luca Lalli
#'
#' @examples
#' cont_var_test(data = iris, variables = c("Sepal.Length", "Sepal.Width"),
#'               group = "Species", paired = FALSE)
cont_var_test <- function(data,
                          variables,
                          paired = FALSE,
                          group,
                          type = NULL,
                          statistic = "{mean} ({sd})",
                          id = "ID",
                          num_dec = 2,
                          overall_threshold = 0.1,
                          p_adjust_method = NULL,
                          excel = FALSE,
                          excel_path = "Results.xlsx",
                          verbose = TRUE) {

  if (is.null(p_adjust_method)) {
    p_adjust_method = "fdr"
  }

  start_time <- Sys.time()
  res <- list()

  # Check if data is a data frame
  if ("tbl_df" %in% base::class(data)) {
    stop(paste(base::substitute(data), "is not a data frame, please make it a data frame!"))
  }

  # Error check: split variable numeric
  if (!is.factor(data[[group]])) {
    stop(paste("Variable", group, "is not factor, please make it factor"))
  }

  # Error check: T/F assigned to something else
  if (T != TRUE || F != FALSE) {
    stop("T or F are not assigned as TRUE or FALSE. Check your .GlobalEnv")
  }

  # Error check: split variable with just one level
  if (nlevels(data[[group]]) == 1) {
    stop(paste("Variable", group, "has just 1 level"))
  }

  statistic <- stringr::str_to_lower(statistic)

  compute.stats <- function(data, group, name, levels, statistic, num_dec) {

    stats <- sapply(levels, function(lvl) {

      x <- data %>%
        dplyr::filter(.data[[group]] == lvl) %>%
        dplyr::pull(name)

      vals <- list(
        mean   = round(mean(x, na.rm = TRUE), num_dec),
        sd     = round(stats::sd(x, na.rm = TRUE), num_dec),
        median = round(stats::median(x, na.rm = TRUE), num_dec),
        iqr    = round(stats::IQR(x, na.rm = TRUE), num_dec),
        min    = round(min(x, na.rm = TRUE), num_dec),
        max    = round(max(x, na.rm = TRUE), num_dec),
        q1     = round(stats::quantile(x, 0.25, na.rm = TRUE), num_dec),
        q3     = round(stats::quantile(x, 0.75, na.rm = TRUE), num_dec),
        n      = sum(!is.na(x)),
        range  = paste0(
          round(min(x, na.rm = TRUE), num_dec),
          "-",
          round(max(x, na.rm = TRUE), num_dec)
        )
      )
      glue::glue_data(vals, statistic)
    })

    return(stats)
  }

  # Split variable dicotomica
  if (nlevels(data[[group]]) == 2) {

    levels <- levels(data[[group]])
    table.p <- matrix(nrow = length(variables), ncol = 5) %>%
      as.data.frame %>%
      `colnames<-`(c("Variable",
                     paste0(stringr::str_to_sentence(gsub("[{}]", "", statistic)), ": ", levels[1]),
                     paste0(stringr::str_to_sentence(gsub("[{}]", "", statistic)), ": ", levels[2]),
                     "pvalue", "padj")) %>%
      dplyr::mutate(Variable = variables)

    # Mann-Whitney Test
    if (paired == FALSE) {

      for (i in seq_along(length(variables))) {
        name <- variables[i]

        stats <- compute.stats(data = data, group = group, name = name, levels = levels, statistic = statistic, num_dec = num_dec)

        table.p[table.p$Variable == name, 2] <- stats[1]
        table.p[table.p$Variable == name, 3] <- stats[2]
        table.p[table.p$Variable == name, "pvalue"] <- stats::wilcox.test(data[[name]] ~ data[[group]])$p.value
        table.p[table.p$Variable == name, "padj"] <-
          stats::p.adjust(table.p[table.p$Variable == name, "pvalue"], method = p_adjust_method, n = length(variables))

        if (verbose) LandS::Progress_bar(current = which(name == variables),
                                         total = length(variables), start_time = start_time,
                                         bar_fill = "\u2588", bar_void = "\u2591")
      }

      if (verbose) message("Mann-Whitney Rank Sum test used")
    }

    # Wilcoxon Test
    if (paired == TRUE) {

      if (sum(is.na(data[, variables])) > 0) {
        warning("IDs with NA will be removed \n Summary statistics are computed on filtered data")
      }

      for (i in seq_along(length(variables))) {
        name <- variables[i]

        if (sum(is.na(data[[name]])) > 0) {
          vett_excl <- data %>% dplyr::filter(is.na(name)) %>% dplyr::pull(id)
          data_filtered <- data %>% dplyr::filter(!id %in% vett_excl)
        } else {
          data_filtered <- data
        }

        stats <- compute.stats(data = data_filtered, group = group, name = name, levels = levels, statistic = statistic, num_dec = num_dec)

        pre <- data_filtered %>% dplyr::filter(.data[[group]] == levels[1]) %>% dplyr::pull(name)
        names(pre) <- data_filtered %>% dplyr::filter(.data[[group]] == levels[1]) %>% dplyr::pull(id)
        post <- data_filtered %>% dplyr::filter(.data[[group]] == levels[2]) %>% dplyr::pull(name)
        names(post) <- data_filtered %>% dplyr::filter(.data[[group]] == levels[2]) %>% dplyr::pull(id)

        stopifnot("IDs are not sorted in the appropriate order within group variable. \n Please refer to function description for details" =
                    names(pre) != names(post))

        table.p[table.p$Variable == name, 2] <- stats[1]
        table.p[table.p$Variable == name, 3] <- stats[2]
        table.p[table.p$Variable == name, "pvalue"] <- stats::wilcox.test(pre, post, paired = TRUE)$p.value
        table.p[table.p$Variable == name, "padj"] <-
          stats::p.adjust(table.p[table.p$Variable == name, "pvalue"], method = p_adjust_method, n = length(variables))

        if (verbose) LandS::Progress_bar(current = which(name == variables),
                                         total = length(variables), start_time = start_time,
                                         bar_fill = "\u2588", bar_void = "\u2591")
      }

      if (verbose) message("Wilcoxon signed rank test used")
    }

  } else {

    # Posthoc test to perform
    if (!is.null(type)) {
      type <- tolower(type)
    }
    levels <- data[[group]] %>% levels()

    matrix_contrasts <- levels %>% utils::combn(2) %>% t()
    vett_contrasts <- paste(matrix_contrasts[, 1], "vs", matrix_contrasts[, 2])

    # Dataframe results
    table.p <- matrix(nrow = length(variables), ncol = 2 + nlevels(data[[group]]) + length(vett_contrasts)) %>%
      as.data.frame %>%
      `colnames<-`(c("Variable",
                     paste0(stringr::str_to_sentence(gsub("[{}]", "", statistic)), ": ", levels(data[[group]])),
                     "Overall", vett_contrasts)) %>%
      dplyr::mutate(Variable = variables)

    # Friedman test and post hoc tests
    if (paired == TRUE) {

      if (sum(is.na(data[, variables])) > 0) {
        warning("IDs with NA will be removed \n Summary statistics are computed on filtered data")
      }

      for (i in seq_along(length(variables))) {
        name <- variables[i]

        if (sum(is.na(data[[name]])) > 0) {
          vett_excl <- data %>% dplyr::filter(is.na(name)) %>% dplyr::pull(id)
          data_filtered <- data %>% dplyr::filter(!id %in% vett_excl)
        } else {
          data_filtered <- data
        }

        if (nrow(data_filtered) > 0) {
          table.p[table.p$Variable == name, 1+(1:nlevels(data[[group]]))] <-
            compute.stats(data = data_filtered, group = group, name = name, levels = levels, statistic = statistic, num_dec = num_dec)

          # Friedman test
          table.p[table.p$Variable == name, "Overall"] <- stats::friedman.test(y = data_filtered[[name]], groups = data_filtered[[group]], blocks = data_filtered[[id]])$p.value

        } else {
          table.p[table.p$Variable == name, "Overall"] <- NA
        }

        if(stats::friedman.test(y = data_filtered[[name]], groups = data_filtered[[group]], blocks = data_filtered[[id]])$p.value > overall_threshold) {
          # If the overall test is not significant, do not perform pairwise comparisons
        } else {
          # If the overall test is significant, perform pairwise comparisons
          if (is.null(type)) {type <- "e"}

          if (type == "c" || type == "conover") {
            mat_posthoc <- PMCMRplus::frdAllPairsConoverTest(y = data_filtered[[name]], groups = data_filtered[[group]], blocks = data_filtered[[id]], p_adjust_method = p_adjust_method)$p.value
            posthoc = "Conover's All-Pairs Rank Comparison Test"
          } else if(type == "e" || type == "exact") {
            mat_posthoc <- PMCMRplus::frdAllPairsExactTest(y = data_filtered[[name]], groups = data_filtered[[group]], blocks = data_filtered[[id]], p_adjust_method = p_adjust_method)$p.value
            posthoc = "Exact All-Pairs Comparisons Test"
          } else if(type == "n" || type == "nemenyi") {
            mat_posthoc <- PMCMRplus::frdAllPairsNemenyiTest(y = data_filtered[[name]], groups = data_filtered[[group]], blocks = data_filtered[[id]], p_adjust_method = p_adjust_method)$p.value
            posthoc = "Nemenyi's All-Pairs Rank Comparison Test"
          } else if(type == "m" || type == "miller") {
            mat_posthoc <- PMCMRplus::frdAllPairsMillerTest(y = data_filtered[[name]], groups = data_filtered[[group]], blocks = data_filtered[[id]], p_adjust_method = p_adjust_method)$p.value
            posthoc = "Millers's All-Pairs Comparisons Test"
          } else if(type == "w" || type == "wilcoxon") {
            listIDs <- base::split(data_filtered, data_filtered[[group]]) %>% base::lapply(function(x) stats::setNames(x[[name]], x$id))
            stopifnot("IDs are not sorted in the appropriate order within group variable. \n Please refer to function description for details" =
                        all(vapply(listIDs[-1], function(x) identical(names(x), names(listIDs[[1]])), logical(1))))
            mat_posthoc <- stats::pairwise.wilcox.test(data[[name]], g = data[[group]], p_adjust_method = p_adjust_method)$p.value
            posthoc = "Pairwise signed Wilcoxon Rank Sum Tests"
          } else {
            stop("Pairwise comparison is not defined properly \n Please refer to function description for details")
          }

          for (contrast in vett_contrasts) {
            x <- strsplit(contrast, " vs ")[[1]]
            table.p[table.p$Variable == name, contrast] <- mat_posthoc[x[2], x[1]]
          }
        }
        if (verbose) LandS::Progress_bar(current = i,
                                         total = length(variables), start_time = start_time,
                                         bar_fill = "\u2588", bar_void = "\u2591")
      }

      if (verbose) message(paste("Friedman rank sum overall and", posthoc, "were used"))
    }

    # Kruskal Wallis and post hoc tests
    if(paired == FALSE) {

      for (i in seq_along(length(variables))) {
        name <- variables[i]

        table.p[table.p$Variable == name, 1+(1:nlevels(data[[group]]))] <-
          compute.stats(data = data, group = group, name = name, levels = levels, statistic = statistic, num_dec = num_dec)

        # Kruskal-Wallis test
        table.p[table.p$Variable == name, "Overall"] <- stats::kruskal.test(x = data[[i]], g = data[[group]])$p.val


        if(stats::kruskal.test(x = data[[name]], g = data[[group]])$p.val > overall_threshold) {
          # If the overall test is not significant, do not perform pairwise comparisons
        } else {
          # If the overall test is significant, perform pairwise comparisons
          if (is.null(type)) {type <- "d"}

          if (type == "c" || type == "conover") {
            mat_posthoc <- PMCMRplus::kwAllPairsConoverTest(x = data[[name]], g = data[[group]], p_adjust_method = p_adjust_method)$p.value
            posthoc = "Conover's All-Pairs Rank Comparison Test"
          } else if (type == "d" || type == "dunn") {
            mat_posthoc <- PMCMRplus::kwAllPairsDunnTest(x = data[[name]], g = data[[group]], p_adjust_method = p_adjust_method)$p.value
            posthoc = "Dunn's All-Pairs Rank Comparison Test"
          } else if (type == "n" || type == "nemenyi") {
            mat_posthoc <- PMCMRplus::kwAllPairsNemenyiTest(x = data[[name]], g = data[[group]], p_adjust_method = p_adjust_method)$p.value
            posthoc = "Nemenyi's All-Pairs Rank Comparison Test"
          } else if (type == "m" || type == "mann-whitney" || type == "mannwhitney" || type == "wilcoxon" || type == "w") {
            mat_posthoc <- stats::pairwise.wilcox.test(data[[name]], g = data[[group]], p_adjust_method = p_adjust_method)$p.value
            posthoc = "Pairwise Wilcoxon Rank Sum Tests"
          } else {
            stop("Pairwise comparison is not defined properly \n Please refer to function description for details")
          }

          for (contrast in vett_contrasts) {
            x <- strsplit(contrast, " vs ")[[1]]
            table.p[table.p$Variable == name, contrast] <- mat_posthoc[x[2], x[1]]
          }

        }

        if (verbose) LandS::Progress_bar(current = i,
                                         total = length(variables), start_time = start_time,
                                         bar_fill = "\u2588", bar_void = "\u2591")
      }

      if (verbose) message(paste("Kruskal-Wallis rank sum overall and", posthoc, "were used"))
    }

    res[["Raw_tests"]] <- table.p
    res[["Form_tests"]] <- table.p %>%
      dplyr::arrange(.data[[if ("Overall" %in% names(table.p)) "Overall" else "padj"]]) %>%
      dplyr::mutate(dplyr::across((2+length(levels)):dplyr::last_col(), function(x) LandS::formatz_p(x)))
    res[[3]] <- table.p %>%
      dplyr::select(.data$Variable, (2+length(levels)):dplyr::last_col()) %>%
      `names<-`(dplyr::case_when(nlevels(data[[group]]) == 2 & paired == FALSE ~ paste('MW', type, sep = '_'),
                                 nlevels(data[[group]]) == 2 & paired == TRUE ~ paste('WS', type, sep = '_'),
                                 nlevels(data[[group]]) > 2 & paired == FALSE ~ paste('KW', type, p_adjust_method, sep = '_'),
                                 nlevels(data[[group]]) > 2 & paired == TRUE ~ paste('FR', type, p_adjust_method, sep = '_')))

  }
  if (excel == FALSE) {
    return(res)
  }else{
    writexl::write_xlsx(res, path = excel_path)
    return(res)
  }
}
