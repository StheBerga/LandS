#' Compare similarity between longitudinal biomarker kinetics
#'
#' @description
#' Compares the temporal profiles of biomarkers using median values at each
#' time point. The function can compare biomarker kinetics overall, or compare
#' the same biomarker between two groups.
#'
#' In the overall mode, all pairwise combinations of `variables` are compared.
#' In the stratified mode, each biomarker is compared between the two levels of
#' `grouping_var`.
#'
#' @param df_long A dataframe in long format. It must contain one row per
#' observation and include at least a biomarker column named `Biomarker`, a
#' numeric measurement column named `Value`, and a time variable.
#' @param variables Character vector of biomarker names to compare. These values
#' must match entries in `df_long$Biomarker`.
#' @param grouping_var Optional character string giving the name of a two-level
#' grouping variable. If `NULL`, biomarker kinetics are compared overall.
#' If provided, each biomarker is compared between the two groups.
#' @param time_var Character string giving the name of the time variable.
#' Default is "Time".
#' @param trend Character string. Either "similar" or "correlated".
#' "similar" performs a one-sided permutation test for positive similarity.
#' "correlated" performs a two-sided permutation test based on absolute
#' correlation.
#' @param n_perm Integer. Number of permutations used to estimate p-values.
#' Default is `10000`.
#' @param seed Integer. Random seed for reproducibility. Default is `123`.
#' @param excel Character string giving the path of the Excel file to write.
#' Default is "similarity_output.xlsx".
#'
#' @return A dataframe with similarity estimates, raw p-values,
#' FDR-adjusted p-values, and a logical column indicating whether
#' `p_adj < 0.05`. The same table is also written to `excel`.
#'
#' @details
#' Similarity is calculated as the Pearson correlation between standardized
#' median biomarker curves across time points. For each biomarker and time point,
#' the median of `Value` is used.
#'
#' Missing values in `Value` are ignored when calculating medians.
#'
#' @author Luca Lalli, Stefano Bergamini
#'
#' @export
#'
#' @examples
#'
#' library(dplyr)
#'
#' set.seed(1)
#'
#' df_long <- expand.grid(
#'   Subject = seq_len(20),
#'   Time = c(0, 1, 2, 4, 8),
#'   Biomarker = c("IL6", "CRP", "TNF")
#' )
#'
#' df_long$Group <- rep(c("Control", "Treatment"), length.out = nrow(df_long))
#'
#' df_long$Value <- with(
#'   df_long,
#'   ifelse(
#'     Biomarker == "IL6",
#'     10 + 2 * Time + rnorm(nrow(df_long), 0, 1),
#'     ifelse(
#'       Biomarker == "CRP",
#'       8 + 1.8 * Time + rnorm(nrow(df_long), 0, 1),
#'       12 - 1.5 * Time + rnorm(nrow(df_long), 0, 1)
#'     )
#'   )
#' )
#'
#' # Overall comparison among biomarker kinetics
#' similarity_kinetic(
#'   df_long = df_long,
#'   variables = c("IL6", "CRP", "TNF"),
#'   time_var = "Time",
#'   trend = "correlated",
#'   n_perm = 100,
#'   excel = tempfile(fileext = ".xlsx")
#' )
#'
#' # Stratified comparison between two groups for each biomarker
#' similarity_kinetic(
#'   df_long = df_long,
#'   variables = c("IL6", "CRP", "TNF"),
#'   grouping_var = "Group",
#'   time_var = "Time",
#'   trend = "correlated",
#'   n_perm = 100,
#'   excel = tempfile(fileext = ".xlsx")
#' )
similarity_kinetic <- function(df_long,
                               variables,
                               grouping_var = NULL,
                               time_var = "Time",
                               trend = "similar",
                               n_perm = 10000,
                               seed = 123,
                               excel = "similarity_output.xlsx") {

  set.seed(seed)

  if (!trend %in% c("similar", "correlated")) {
    stop('trend has to be set to "similar" or "correlated".')
  }

  # === Modalità OVERALL (confronto tra biomarcatori) ===
  if (is.null(grouping_var)) {

    test_similarity_perm <- function(marker1, marker2) {
      curve1 <- df_long %>%
        filter(Biomarker == marker1) %>%
        group_by(.data[[time_var]]) %>%
        summarise(val = median(Value, na.rm = TRUE)) %>%
        pull(val)

      curve2 <- df_long %>%
        filter(Biomarker == marker2) %>%
        group_by(.data[[time_var]]) %>%
        summarise(val = median(Value, na.rm = TRUE)) %>%
        pull(val)

      if (length(curve1) != length(curve2)) return(NULL)

      curve1 <- scale(curve1)[, 1]
      curve2 <- scale(curve2)[, 1]

      sim_obs <- cor(curve1, curve2, method = "pearson", use = "pairwise.complete.obs")
      sim_perm <- replicate(n_perm, cor(curve1, sample(curve2), method = "pearson"))

      p_val <- if (trend == "similar") {
        mean(sim_perm >= sim_obs)
      } else {
        mean(abs(sim_perm) >= abs(sim_obs))
      }

      if (p_val == 0) p_val <- 1 / n_perm

      return(data.frame(
        Marker1 = marker1,
        Marker2 = marker2,
        similarity = sim_obs,
        p_value = p_val
      ))
    }

    combinazioni <- combn(variables, 2, simplify = FALSE)
    risultati <- vector("list", length(combinazioni))

    cat("🔁 Start permutation test on", length(combinazioni), "pairs...\n")
    start_time <- Sys.time()

    for (i in seq_along(combinazioni)) {
      m1 <- combinazioni[[i]][1]
      m2 <- combinazioni[[i]][2]

      res <- test_similarity_perm(m1, m2)
      if (!is.null(res)) risultati[[i]] <- res

      LandS::Progress_bar(current = i, total = length(combinazioni), start_time = start_time)
    }

    tab_sim <- do.call(rbind, risultati)
    tab_sim$p_adj <- p.adjust(tab_sim$p_value, method = "fdr")
    tab_sim$significativo <- tab_sim$p_adj < 0.05

    tab_sim_dup <- tab_sim %>%
      rename(MarkerA = Marker1, MarkerB = Marker2) %>%
      bind_rows(tab_sim %>% rename(MarkerA = Marker2, MarkerB = Marker1)) %>%
      arrange(p_adj) %>%
      rename(Marker1 = MarkerA, Marker2 = MarkerB)

    writexl::write_xlsx(tab_sim_dup, path = excel)
    cat("\n✅ Completed: saved in", excel, "\n")
    return(tab_sim_dup)
  }

  # === Modalità STRATIFICATA ===
  else {
    group_levels <- unique(df_long[[grouping_var]])
    if (length(group_levels) != 2) stop("Stratification variable need to have exactly 2 levels.")

    groupA <- group_levels[1]
    groupB <- group_levels[2]

    risultati <- list()
    cat("🔁 Start comparison among groups on", length(variables), "biomarkers...\n")
    start_time <- Sys.time()

    for (i in seq_along(variables)) {
      marker <- variables[i]
      df_sub <- df_long %>% filter(Biomarker == marker)

      curveA <- df_sub %>%
        filter(.data[[grouping_var]] == groupA) %>%
        group_by(.data[[time_var]]) %>%
        summarise(med = median(Value, na.rm = TRUE)) %>%
        pull(med)

      curveB <- df_sub %>%
        filter(.data[[grouping_var]] == groupB) %>%
        group_by(.data[[time_var]]) %>%
        summarise(med = median(Value, na.rm = TRUE)) %>%
        pull(med)

      if (length(curveA) != length(curveB)) next

      curveA <- scale(curveA)[, 1]
      curveB <- scale(curveB)[, 1]
      sim_obs <- cor(curveA, curveB, method = "pearson")

      sim_perm <- numeric(n_perm)
      for (k in 1:n_perm) {
        df_perm <- df_sub %>%
          mutate(gruppo_perm = sample(.data[[grouping_var]]))

        curveA_perm <- df_perm %>%
          filter(gruppo_perm == groupA) %>%
          group_by(.data[[time_var]]) %>%
          summarise(med = median(Value, na.rm = TRUE)) %>%
          pull(med)

        curveB_perm <- df_perm %>%
          filter(gruppo_perm == groupB) %>%
          group_by(.data[[time_var]]) %>%
          summarise(med = median(Value, na.rm = TRUE)) %>%
          pull(med)

        if (length(curveA_perm) == length(curveB_perm)) {
          curveA_perm <- scale(curveA_perm)[, 1]
          curveB_perm <- scale(curveB_perm)[, 1]
          sim_perm[k] <- cor(curveA_perm, curveB_perm, method = "pearson")
        } else {
          sim_perm[k] <- NA
        }
      }

      sim_perm <- sim_perm[!is.na(sim_perm)]
      p_val <- if (trend == "similar") {
        mean(sim_perm >= sim_obs)
      } else {
        mean(abs(sim_perm) >= abs(sim_obs))
      }
      if (p_val == 0) p_val <- 1 / n_perm

      risultati[[i]] <- data.frame(
        Biomarker = marker,
        similarity_observed = sim_obs,
        p_value = p_val
      )

      LandS::Progress_bar(current = i, total = length(variables), start_time = start_time)
    }

    tab_strata <- do.call(rbind, risultati)
    tab_strata$p_adj <- p.adjust(tab_strata$p_value, method = "fdr")
    tab_strata$significativo <- tab_strata$p_adj < 0.05
    tab_strata <- tab_strata %>% arrange(p_adj)

    writexl::write_xlsx(tab_strata, path = excel)
    cat("\n✅ Completed: saved in", excel, "\n")
    return(tab_strata)
  }
}

