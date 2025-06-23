#' Test for continuous variables splitted by categories
#'
#' The most powerful function ever created. You can perform the 4 major tests and the posthoc tests for Friedman and Kruskal-Wallis.
#' If you are dumb (option dumb = T) you can also perform posthoc tests without correcting for test multiplicity.
#' Please do not try this at home/work and consider asking a statistician before performing any test.
#' Stored functions for statistic option are (median), (mean), (sd), (min), (max), (q1), (q3), (n), and (range).
#'
#' @param data dataframe
#' @param variables vector containing all variables of interest
#' @param paired FALSE/TRUE
#' @param group factor variable splitting the data
#' @param dumb FALSE are you dumb? Hope not
#' @param statistic Specifies summary statistics to display for each variable. Default = "(mean) (sd)".
#' @param ID ID variabl (Default = "ID")
#' @param num_dec Decimal number for mean and SD (Default = 2)
#' @param excel export fuction results as multiple Excel sheets
#' @param excel_path path where you want your Excel
#' @param telegram send a telegram message
#' @param p.adjust.method correction method, a character string. Can be abbreviated.

#'
#' @return Una lista con dataset
#' @export
#'
#' @examples cont_var_test_LB(data = iris, variables = c("Sepal.Length", "Sepal.Width"), group = "Species", paired = FALSE)
cont_var_test_LB <- function (data,
                              variables,
                              paired = FALSE,
                              group,
                              dumb = FALSE,
                              statistic = "{mean} ({sd})",
                              ID = "ID",
                              num_dec = 2,
                              p.adjust.method = NULL,
                              excel = F,
                              excel_path = paste0(path_out, "/Results.xlsx"),
                              telegram = "none")
{
  options(warn=-1)
  require(progress)
  require(PMCMRplus)
  require(rlang)
  require(gtsummary)
  require(dplyr)

  if(is.null(p.adjust.method)){
    p.adjust.method = "bonferroni"
  }

  if(telegram != "none"){
    start_time <<- Sys.time()
  }

  # Progress bar
  pb <- progress_bar$new(format = "[:bar] :current/:total (:percent)", total = length(variables))
  pb$tick(0)

  # Check if data is a data frame
  if("tbl_df" %in% base::class(data)){
    stop(paste0(base::substitute(data), " is not a data frame, please make it a data frame!"))
  }

  # Error check: split variable numeric
  if(!is.factor(data[, group])){
    stop(paste0("Variable ", group, " is not factor, please make it factor"))
  }

  # Error check: T/F assigned to something else
  if(T != TRUE | F != FALSE){
    stop("T or F are not assigned as TRUE or FALSE. Check your .GlobalEnv")
  }

  # Error check: split variable with just one level
  if(nlevels(data[,group]) == 1){
    stop(paste0("Variable ", group, " has just 1 level"))
  }

  statistic <- stringr::str_to_lower(statistic)

  # Split variable dicotomica
  if(nlevels(data[, group]) == 2){

    # Mann-Whitney Test
    if (paired == FALSE){
      tabella <- data.frame(matrix(nrow = length(variables), ncol = 4))
      levels <- levels(data[,group])
      colnames(tabella) <- c("Variable",
                             paste0(stringr::str_to_sentence(gsub("[{}]", "", statistic)), ": ", levels[1]),
                             paste0(stringr::str_to_sentence(gsub("[{}]", "", statistic)), ": ", levels[2]),
                             "pvalue")
      tabella[,1] <- variables

      for (i in 1:length(variables)){
        name <- variables[i]

        stats <<- sapply(levels, function(lvl) {
          x <- data[data[, group] == lvl, name]
          vals <- list(
            mean   = round(mean(x, na.rm = TRUE), num_dec),
            sd     = round(sd(x, na.rm = TRUE), num_dec),
            median = round(median(x, na.rm = TRUE), num_dec),
            iqr    = round(IQR(x, na.rm = TRUE), num_dec),
            min    = round(min(x, na.rm = TRUE), num_dec),
            max    = round(max(x, na.rm = TRUE), num_dec),
            q1     = round(quantile(x, 0.25, na.rm = TRUE), num_dec),
            q3     = round(quantile(x, 0.75, na.rm = TRUE), num_dec),
            n      = sum(!is.na(x)),
            range  = paste0(round(min(x, na.rm = TRUE), num_dec), "-", round(max(x, na.rm = TRUE), num_dec))
          )
          glue::glue_data(vals, statistic)
        })

        tabella[tabella$Variable == name, 2] <- stats[1]
        tabella[tabella$Variable == name, 3] <- stats[2]
        tabella[tabella$Variable == name, 4] <- as.numeric(wilcox.test(data[, name] ~ data[, group])$p.value, 4)
        Sys.sleep(0.005)
        pb$tick(1)
        pb
      }

      res <- list()
      tabella_formatted <- dplyr::arrange(tabella, pvalue)
      for(z in colnames(tabella_formatted)[ncol(tabella_formatted)]){
        tabella_formatted[,z] <- LandS::formatz_p(tabella_formatted[,z])
      }
      tabella_p <- tabella[, c(1,4)]

      res[[1]] <- tabella
      res[[2]] <- tabella_formatted
      res[[3]] <- tabella_p
      names(res) <- c("Raw_tests", "Form_tests", "Raw_pval")

      message("Mann-Whitney Rank Sum test used")
    }

    if (paired == TRUE){
      tabella <- data.frame(matrix(nrow = length(variables), ncol = 4))
      levels <- levels(data[, group])
      colnames(tabella) <- c("Variable",
                             paste0(stringr::str_to_sentence(gsub("[{}]", "", statistic)), ": ", levels[1]),
                             paste0(stringr::str_to_sentence(gsub("[{}]", "", statistic)), ": ", levels[2]),
                             "pvalue")
      tabella[,1] <- variables

      for (i in 1:length(variables)){
        name <- variables[i]

        if (sum(is.na(data[, name])) >0){
          vett_excl <- data[is.na(data[, name]), ID]
          tmp <- data[!data[, ID] %in% vett_excl, ]
        }else{
          tmp <- data
        }

        stats <<- sapply(levels, function(lvl) {
          x <- tmp[tmp[, group] == lvl, name]
          vals <- list(
            mean   = round(mean(x, na.rm = TRUE), num_dec),
            sd     = round(sd(x, na.rm = TRUE), num_dec),
            median = round(median(x, na.rm = TRUE), num_dec),
            iqr    = round(IQR(x, na.rm = TRUE), num_dec),
            min    = round(min(x, na.rm = TRUE), num_dec),
            max    = round(max(x, na.rm = TRUE), num_dec),
            q1     = round(quantile(x, 0.25, na.rm = TRUE), num_dec),
            q3     = round(quantile(x, 0.75, na.rm = TRUE), num_dec),
            n      = sum(!is.na(x)),
            range  = paste0(round(min(x, na.rm = TRUE), num_dec), "-", round(max(x, na.rm = TRUE), num_dec))
          )
          glue::glue_data(vals, statistic)
        })

        tabella[tabella$Variable == name, 2] <- stats[1]
        tabella[tabella$Variable == name, 3] <- stats[2]
        tabella[tabella$Variable == name, 4] <- as.numeric(wilcox.test(Pair(tmp[tmp[, group] == levels[1], name],
                                                                            tmp[tmp[, group] == levels[2], name]) ~ 1, data = tmp)$p.value, 4)
        Sys.sleep(0.005)
        pb$tick(1)
        pb
      }

      res <- list()
      tabella_formatted <- dplyr::arrange(tabella, pvalue)
      for(z in colnames(tabella_formatted)[ncol(tabella_formatted)]){
        tabella_formatted[,z] <- LandS::formatz_p(tabella_formatted[,z])
      }

      tabella_p <- tabella[, c(1,4)]

      res[[1]] <- tabella
      res[[2]] <- tabella_formatted
      res[[3]] <- tabella_p
      names(res) <- c("Raw_tests", "Form_tests", "Raw_pval")

      message("Wilcoxon Rank Sum test used")
    }

  }else{

    n_lev_posthoc <- length(combn(levels(data[, group]), 2))/2
    levels_groups <- levels(data[,group])
    n_lev_group <- nlevels(data[,group])

    if(paired == TRUE){

      # Friedman test and post hoc tests

      if(sum(is.na(data[, variables])) > 0){
        message("IDs with NA have been removed")
      }
      # Comparisons
      matrix_comparison <- t(combn(levels_groups, 2))
      vett_comparison <- as.vector(NA)
      for (x in 1:nrow(matrix_comparison)){
        vett_comparison[x] <- paste0(matrix_comparison[x,1], " vs ", matrix_comparison[x,2])
      }

      Friedman_test_df <- as.data.frame(matrix(nrow = length(variables),
                                               ncol = n_lev_group + n_lev_posthoc + 2))
      colnames(Friedman_test_df) <- c("Var",
                                      paste0(stringr::str_to_sentence(gsub("[{}]", "", statistic)), ": ", levels_groups),
                                      "Friedman", vett_comparison)
      Friedman_test_df$Var <- variables

      for (i in variables){

        matt_combn <- as.data.frame(t(combn(levels_groups, 2)))
        colnames(matt_combn) <- c("Var1", "Var2")
        matt_combn$Versus <- paste0(matt_combn$Var1, " vs ", matt_combn$Var2)
        matt_combn$pval <- NA

        if (sum(is.na(data[, i])) > 0){

          vett_excl <- data[is.na(data[,i]), ID]
          tmp <- data[!data[, ID] %in% vett_excl, ]

        }else{

          tmp <- data

        }

        if(nrow(tmp) > 0 ) {

          stats <<- sapply(levels_groups, function(lvl) {
            x <- data[data[, group] == lvl, i]
            vals <- list(
              mean   = round(mean(x, na.rm = TRUE), num_dec),
              sd     = round(sd(x, na.rm = TRUE), num_dec),
              median = round(median(x, na.rm = TRUE), num_dec),
              iqr    = round(IQR(x, na.rm = TRUE), num_dec),
              min    = round(min(x, na.rm = TRUE), num_dec),
              max    = round(max(x, na.rm = TRUE), num_dec),
              q1     = round(quantile(x, 0.25, na.rm = TRUE), num_dec),
              q3     = round(quantile(x, 0.75, na.rm = TRUE), num_dec),
              n      = sum(!is.na(x)),
              range  = paste0(round(min(x, na.rm = TRUE), num_dec), "-", round(max(x, na.rm = TRUE), num_dec))
            )
            glue::glue_data(vals, statistic)
          })

          for (o in 1:n_lev_group){
            Friedman_test_df[Friedman_test_df$Var == i, 1+o] <- stats[o]
          }
          # Friedman test
          Friedman_test_df[Friedman_test_df$Var == i, "Friedman"] <- as.numeric(friedman.test(y = tmp[, i], groups = tmp[, group], blocks = tmp[, ID])$p.val)

          # Post hoc test
          df_posthoc <- as.data.frame(PMCMRplus::frdAllPairsExactTest(y = tmp[, i], groups = tmp[, group], blocks = tmp[, ID], p.adjust.method = p.adjust.method)$p.value)
          df_posthoc_pairs <- as.data.frame(matrix(NA, nrow = nlevels(data[, group]), ncol = nlevels(data[, group])+1))
          colnames(df_posthoc_pairs) <- c("Var", levels(data[, group]))
          df_posthoc_pairs$Var <- levels(data[, group])

        }else{

          Friedman_test_df[Friedman_test_df$Var == i, "Friedman"] <- NA
          df_posthoc <- as.data.frame(matrix(NA, nrow = nlevels(data[, group])-1, ncol = nlevels(data[, group])-1))
          df_posthoc_pairs <- as.data.frame(matrix(NA, nrow = nlevels(data[, group]), ncol = nlevels(data[, group])+1))
          colnames(df_posthoc_pairs) <- c("Var", levels(data[, group]))
          df_posthoc_pairs$Var <- levels(data[, group])

        }


        for (p in df_posthoc_pairs$Var) {

          for(k in df_posthoc_pairs$Var){

            if(p == k){df_posthoc_pairs[df_posthoc_pairs$Var == p, colnames(df_posthoc_pairs) == k] <- 1}else{

              vett_comp_a <- as.numeric(df_posthoc[rownames(df_posthoc) == p, colnames(df_posthoc) == k])
              vett_comp_b <- as.numeric(df_posthoc[rownames(df_posthoc) == k, colnames(df_posthoc) == p])

              if(!rlang::is_empty(vett_comp_a)){

                df_posthoc_pairs[df_posthoc_pairs$Var == p, colnames(df_posthoc_pairs) == k] <- vett_comp_a
              }
            }
          }
        }

        for (j in colnames(df_posthoc_pairs)[3:ncol(df_posthoc_pairs)]){

          for(n in df_posthoc_pairs$Var){

            if(!is.na(df_posthoc_pairs[df_posthoc_pairs$Var == n, j])){

            }else{

              df_posthoc_pairs[df_posthoc_pairs$Var== n,j] <- df_posthoc_pairs[df_posthoc_pairs$Var== j,n]

            }

          }
        }

        for(q in colnames(df_posthoc_pairs)[2:ncol(df_posthoc_pairs)]) {
          df_posthoc_pairs[, q] <- as.numeric(df_posthoc_pairs[, q])
        }

        for (l in 1: nrow(matt_combn)) {

          matt_combn[l, "pval"] <- df_posthoc_pairs[df_posthoc_pairs$Var == matt_combn$Var1[l], colnames(df_posthoc_pairs) == matt_combn$Var2[l]]
        }

        for(m in colnames(Friedman_test_df)[(3+n_lev_group):ncol(Friedman_test_df)]){

          Friedman_test_df[Friedman_test_df$Var == i, m] <- as.numeric(matt_combn$pval[matt_combn$Versus == m])

        }
        pb$tick(1)
        pb
      }

      res <- list()
      Friedman_test_df_ordered <- dplyr::arrange(Friedman_test_df, Friedman)
      Friedman_test_df_form <- Friedman_test_df_ordered

      for(z in colnames(Friedman_test_df_form)[(2+n_lev_group):ncol(Friedman_test_df_form)]){
        Friedman_test_df_form[,z] <- LandS::formatz_p(Friedman_test_df_form[,z])
      }

      Friedman_ph_p <- Friedman_test_df[, c(1, (2+n_lev_group):ncol(Friedman_test_df))]

      if (dumb == FALSE){
        res[[1]] <- Friedman_test_df
        res[[2]] <- Friedman_test_df_form
        res[[3]] <- Friedman_ph_p
        names(res) <- c("Raw_tests", "Form_tests", "Friedman_ph_pval")
      }else{

        n_lev_posthoc <- length(combn(levels(data[, group]), 2))/2
        levels_groups <- levels(data[,group])
        n_lev_group <- nlevels(data[,group])

        matrix_comparison <- t(combn(levels(data[, group]), 2))
        vett_comparison <- as.vector(NA)

        for (x in 1:nrow(matrix_comparison)){
          vett_comparison[x] <- paste0(matrix_comparison[x,1], " vs ", matrix_comparison[x,2])
        }

        Dumb_test_df <- as.data.frame(matrix(nrow = length(variables), ncol = 1 + n_lev_posthoc))
        colnames(Dumb_test_df) <- c("Var", vett_comparison)
        Dumb_test_df$Var <- variables


        for (i in variables){

          matt_combn <- as.data.frame(t(combn(levels(data[, group]), 2)))
          colnames(matt_combn) <- c("Var1", "Var2")
          matt_combn$Versus <- paste0(matt_combn$Var1, " vs ", matt_combn$Var2)
          matt_combn$pval <- NA

          tmp <- data


          # Post hoc test
          df_posthoc_pairs <- as.data.frame(matrix(NA, nrow = nlevels(tmp[, group]), ncol = nlevels(tmp[, group])+1))
          colnames(df_posthoc_pairs) <- c("Var", levels(tmp[, group]))
          df_posthoc_pairs$Var <- levels(tmp[, group])


          for (p in df_posthoc_pairs$Var) {

            for(k in df_posthoc_pairs$Var){

              if(p == k){df_posthoc_pairs[df_posthoc_pairs$Var == p, colnames(df_posthoc_pairs) == k] <- 1}else{

                data1 <- tmp[, i][tmp[, group] %in% p ]
                data2 <- tmp[, i][tmp[, group] %in% k ]

                if (all(is.na(data1)) == TRUE | all(is.na(data2)) == TRUE){

                  df_posthoc_pairs[df_posthoc_pairs$Var == p, colnames(df_posthoc_pairs) == k] <- NA

                } else {

                  df_posthoc_pairs[df_posthoc_pairs$Var == p, colnames(df_posthoc_pairs) == k] <-
                    wilcox.test(Pair(data1, data2) ~ 1, data = tmp)$p.value
                }
              }
            }
          }


          for(q in colnames(df_posthoc_pairs)[2:ncol(df_posthoc_pairs)]) {
            df_posthoc_pairs[, q] <- as.numeric(df_posthoc_pairs[, q])
          }

          for (l in 1: nrow(matt_combn)) {

            matt_combn[l, "pval"] <- df_posthoc_pairs[df_posthoc_pairs$Var == matt_combn$Var1[l], colnames(df_posthoc_pairs) == matt_combn$Var2[l]]
          }

          for(m in colnames(Dumb_test_df)[2:ncol(Dumb_test_df)]){

            Dumb_test_df[Dumb_test_df$Var == i, m] <- as.numeric(matt_combn$pval[matt_combn$Versus == m])

          }
        }

        Dumb_test_df <-  dplyr::inner_join(Dumb_test_df, Friedman_test_df[, c("Var", "Friedman")], by = "Var") %>% dplyr::select("Var", "Friedman", dplyr::all_of(vett_comparison))

        Dumb_test_df_form <- Dumb_test_df

        for(z in colnames(Dumb_test_df_form)[2:ncol(Dumb_test_df_form)]){
          Dumb_test_df_form[,z] <- LandS::formatz_p(Dumb_test_df_form[,z])
        }

        res[[1]] <- Friedman_test_df
        res[[2]] <- Friedman_test_df_form
        res[[3]] <- Friedman_ph_p
        res[[4]] <- Dumb_test_df
        res[[5]] <- Dumb_test_df_form
        names(res) <- c("Raw_tests", "Form_tests", "Friedman_ph_pval", "no_corrected_ph", "Form_no_corrected_ph")
        message("It is reccomended to adjust for multiple testing; please ignore the results of no_corrected_ph")
      }

      message("Friedman rank sum test used")
    }

    if(paired == FALSE){
      # Kruskal Wallis and post hoc tests

      # Comparazioni
      matrix_comparison <- t(combn(levels_groups, 2))
      vett_comparison <- as.vector(NA)
      for (x in 1:nrow(matrix_comparison)){
        vett_comparison[x] <- paste0(matrix_comparison[x,1], " vs ", matrix_comparison[x,2])
      }

      # Dataframe results
      KW_test_df <- as.data.frame(matrix(nrow = length(variables),
                                         ncol = n_lev_group + n_lev_posthoc + 2))
      colnames(KW_test_df) <- c("Var",
                                paste0(stringr::str_to_sentence(gsub("[{}]", "", statistic)), ": ", levels_groups),
                                "Kruskal_Wallis", vett_comparison)
      KW_test_df$Var <- variables

      for (i in variables){

        stats <<- sapply(levels_groups, function(lvl) {
          x <- data[data[, group] == lvl, i]
          vals <- list(
            mean   = round(mean(x, na.rm = TRUE), num_dec),
            sd     = round(sd(x, na.rm = TRUE), num_dec),
            median = round(median(x, na.rm = TRUE), num_dec),
            iqr    = round(IQR(x, na.rm = TRUE), num_dec),
            min    = round(min(x, na.rm = TRUE), num_dec),
            max    = round(max(x, na.rm = TRUE), num_dec),
            q1     = round(quantile(x, 0.25, na.rm = TRUE), num_dec),
            q3     = round(quantile(x, 0.75, na.rm = TRUE), num_dec),
            n      = sum(!is.na(x)),
            range  = paste0(round(min(x, na.rm = TRUE), num_dec), "-", round(max(x, na.rm = TRUE), num_dec))
          )
          glue::glue_data(vals, statistic)
        })

        # Mean and SD in group variable
        for (o in 1:n_lev_group){
          KW_test_df[KW_test_df$Var == i, 1+o] <- stats[o]
        }

        matt_combn <- as.data.frame(t(combn(levels_groups, 2)))
        colnames(matt_combn) <- c("Var1", "Var2")
        matt_combn$Versus <- paste0(matt_combn$Var1, " vs ", matt_combn$Var2)
        matt_combn$pval <- NA

        # Kruskal-Wallis test
        KW_test_df[KW_test_df$Var == i, "Kruskal_Wallis"] <- as.numeric(kruskal.test(x = data[, i], g = data[, group])$p.val)

        # Post hoc test
        df_posthoc <- as.data.frame(PMCMRplus::kwAllPairsDunnTest(x = data[, i], g = data[, group], p.adjust.method = p.adjust.method)$p.value)
        df_posthoc_pairs <- as.data.frame(matrix(NA, nrow = nlevels(data[, group]), ncol = nlevels(data[, group])+1))
        colnames(df_posthoc_pairs) <- c("Var", levels(data[, group]))
        df_posthoc_pairs$Var <- levels(data[, group])


        for (p in df_posthoc_pairs$Var) {

          for(k in df_posthoc_pairs$Var){

            if(p == k){df_posthoc_pairs[df_posthoc_pairs$Var == p, colnames(df_posthoc_pairs) == k] <- 1}else{

              vett_comp_a <- as.numeric(df_posthoc[rownames(df_posthoc) == p, colnames(df_posthoc) == k])
              vett_comp_b <- as.numeric(df_posthoc[rownames(df_posthoc) == k, colnames(df_posthoc) == p])

              if(!rlang::is_empty(vett_comp_a)){

                df_posthoc_pairs[df_posthoc_pairs$Var == p, colnames(df_posthoc_pairs) == k] <- vett_comp_a
              }
            }
          }
        }



        for (j in colnames(df_posthoc_pairs)[3:ncol(df_posthoc_pairs)]){

          for(n in df_posthoc_pairs$Var){

            if(!is.na(df_posthoc_pairs[df_posthoc_pairs$Var == n, j])){

            }else{

              df_posthoc_pairs[df_posthoc_pairs$Var== n,j] <- df_posthoc_pairs[df_posthoc_pairs$Var== j,n]

            }

          }
        }

        for(q in colnames(df_posthoc_pairs)[2:ncol(df_posthoc_pairs)]) {
          df_posthoc_pairs[, q] <- as.numeric(df_posthoc_pairs[, q])
        }

        for (l in 1: nrow(matt_combn)) {

          matt_combn[l, "pval"] <- df_posthoc_pairs[df_posthoc_pairs$Var == matt_combn$Var1[l], colnames(df_posthoc_pairs) == matt_combn$Var2[l]]
        }

        for(m in colnames(KW_test_df)[(3+n_lev_group):ncol(KW_test_df)]){

          KW_test_df[KW_test_df$Var == i, m] <- as.numeric(matt_combn$pval[matt_combn$Versus == m])

        }
        Sys.sleep(0.05)
        pb$tick(1)
        pb

      }

      res <- list()
      KW_test_df_ordered <- arrange(KW_test_df, Kruskal_Wallis)
      KW_test_df_form <- KW_test_df_ordered
      for(z in colnames(KW_test_df_form)[(2+n_lev_group):ncol(KW_test_df_form)]){
        KW_test_df_form[,z] <- LandS::formatz_p(KW_test_df_form[,z])
      }

      KW_ph_pval <- KW_test_df[, c(1, (2+n_lev_group):ncol(KW_test_df))]

      if (dumb == F){
        res[[1]] <- KW_test_df
        res[[2]] <- KW_test_df_form
        res[[3]] <- KW_ph_pval
        names(res) <- c("Raw_tests", "Form_tests", "KW_ph_pval")
      }else{

        n_lev_posthoc <- length(combn(levels(data[, group]), 2))/2
        levels_groups <- levels(data[,group])
        n_lev_group <- nlevels(data[,group])

        matrix_comparison <- t(combn(levels(data[, group]), 2))
        vett_comparison <- as.vector(NA)
        for (x in 1:nrow(matrix_comparison)){
          vett_comparison[x] <- paste0(matrix_comparison[x,1], " vs ", matrix_comparison[x,2])
        }

        Dumb_test_df <- as.data.frame(matrix(nrow = length(variables),
                                             ncol = 1 + n_lev_posthoc))
        colnames(Dumb_test_df) <- c("Var", vett_comparison)
        Dumb_test_df$Var <- variables

        for (i in variables){

          matt_combn <- as.data.frame(t(combn(levels(data[, group]), 2)))
          colnames(matt_combn) <- c("Var1", "Var2")
          matt_combn$Versus <- paste0(matt_combn$Var1, " vs ", matt_combn$Var2)
          matt_combn$pval <- NA

          if (sum(is.na(data[, i])) >0){

            vett_excl <- data[is.na(data[,i]), ID]
            tmp <- data[!data[, ID] %in% vett_excl, ]
          }else{
            tmp <- data
          }


          # Post hoc test
          df_posthoc_pairs <- as.data.frame(matrix(NA, nrow = nlevels(data[, group]), ncol = nlevels(data[, group])+1))
          colnames(df_posthoc_pairs) <- c("Var", levels(data[, group]))
          df_posthoc_pairs$Var <- levels(data[, group])


          for (p in df_posthoc_pairs$Var) {

            for(k in df_posthoc_pairs$Var){

              if(p == k){df_posthoc_pairs[df_posthoc_pairs$Var == p, colnames(df_posthoc_pairs) == k] <- 1}else{

                data1 <- data[, i][data[, group] %in% p ]
                data2 <- data[, i][data[, group] %in% k ]

                df_posthoc_pairs[df_posthoc_pairs$Var == p, colnames(df_posthoc_pairs) == k] <-
                  wilcox.test(data1, data2)$p.val
              }
            }
          }


          for(q in colnames(df_posthoc_pairs)[2:ncol(df_posthoc_pairs)]) {
            df_posthoc_pairs[, q] <- as.numeric(df_posthoc_pairs[, q])
          }

          for (l in 1: nrow(matt_combn)) {

            matt_combn[l, "pval"] <- df_posthoc_pairs[df_posthoc_pairs$Var == matt_combn$Var1[l], colnames(df_posthoc_pairs) == matt_combn$Var2[l]]
          }

          for(m in colnames(Dumb_test_df)[2:ncol(Dumb_test_df)]){

            Dumb_test_df[Dumb_test_df$Var == i, m] <- as.numeric(matt_combn$pval[matt_combn$Versus == m])

          }
        }
        res[[1]] <- KW_test_df
        res[[2]] <- KW_test_df_form
        res[[3]] <- KW_ph_pval
        res[[4]] <- Dumb_test_df
        names(res) <- c("Raw_tests", "Form_tests", "KW_ph_pval", "no_corrected_ph")
        message("It is reccomended to adjust for multiple testing; please ignore the results of no_corrected_ph")
      }

      message("Kruskal-Wallis rank sum test used")
    }

  }
  if (excel == FALSE){
    if(telegram != "none"){
      LandS::telegram_mess_LB(dest = telegram, script = "Boxplot")
    }
    return(res)}else{
      writexl::write_xlsx(res, path = excel_path)
      if(telegram != "none"){
        LandS::telegram_mess_LB(dest = telegram, script = "Boxplot")
      }
      return(res)
    }
  invisible(gc())
}
