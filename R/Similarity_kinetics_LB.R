#' Function to compare similarity between two kinetics
#'
#' @param df_long Dataframe in full long format
#' @param variables Vector of variables to compare
#' @param grouping_var If NULL consider the global trend and compare the variables, if a two-level variable is indicated compare all the stratified variables for this
#' @param trend If “similar” evaluates the similar kinetics of the curves, if “correlated” evaluates the correlation of the kinetics
#' @param n_perm Number of permutations
#' @param seed Seed to replicate permutations
#' @param excel Path to export excel
#' @param time_var Time variable name in the dataset
#'
#' @returns Results and file .xlsx
#' @export
#'
#' @examples
Similarity_Kinetic_LB <- function(df_long, variables,
                                  grouping_var = NULL,
                                  time_var = "Time",
                                  trend = "similar",
                                  n_perm = 10000,
                                  seed = 123,
                                  excel = "similarity_output.xlsx") {

  set.seed(seed)

  if (!trend %in% c("similar", "correlated")) {
    stop('trend deve essere "similar" oppure "correlated".')
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

    cat("🔁 Avvio permutational test su", length(combinazioni), "coppie...\n")
    start_time <- Sys.time()

    for (i in seq_along(combinazioni)) {
      m1 <- combinazioni[[i]][1]
      m2 <- combinazioni[[i]][2]

      res <- test_similarity_perm(m1, m2)
      if (!is.null(res)) risultati[[i]] <- res

      LandS::Progress_bar_LB(current = i, total = length(combinazioni), start_time = start_time)
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
    cat("\n✅ Completato: salvato in", excel, "\n")
    return(tab_sim_dup)
  }

  # === Modalità STRATIFICATA ===
  else {
    group_levels <- unique(df_long[[grouping_var]])
    if (length(group_levels) != 2) stop("La variabile di stratificazione deve avere esattamente 2 livelli.")

    groupA <- group_levels[1]
    groupB <- group_levels[2]

    risultati <- list()
    cat("🔁 Avvio confronto tra gruppi su", length(variables), "biomarcatori...\n")
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

      LandS::Progress_bar_LB(current = i, total = length(variables), start_time = start_time)
    }

    tab_strata <- do.call(rbind, risultati)
    tab_strata$p_adj <- p.adjust(tab_strata$p_value, method = "fdr")
    tab_strata$significativo <- tab_strata$p_adj < 0.05
    tab_strata <- tab_strata %>% arrange(p_adj)

    writexl::write_xlsx(tab_strata, path = excel)
    cat("\n✅ Completato: salvato in", excel, "\n")
    return(tab_strata)
  }
}





