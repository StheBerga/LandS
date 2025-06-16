#' Function for outliers report in excel
#'
#' @param data a dataframe
#' @param variables vector of variables to check
#' @param ID ID variable. Default = "ID"
#' @param Group Grouping variable. Default = "Time"
#' @param excel Whether to create an excel file of the report
#' @param excel_path Path for the excel file
#' @param k Parameter between Q1-Q3 and IQR. Default = 1.5
#'
#' @returns a list of dataframe one of the global variable, the other for the variable stratified for the grouping variable
#' @export
#'
#' @examples
Outlier_Report_LB <- function (data, variables, ID = "ID", Group = "Time",
                               k = 1.5, excel = FALSE, excel_path = "Output/Outlier Report.xlsx") {
  require(progress)
  require(dplyr)

  # Progress bar
  pb <- progress_bar$new(format = "[:bar] :current/:total (:percent)", total = length(variables))
  pb$tick(0)

  df <- data; df2 <- data
  df[, variables] <- NA; df2[, variables] <- NA

  for (p in variables){

    tmp <- cbind(tapply(data[, p], data[, Group], FUN = function (x) quantile(x, probs = .25, na.rm = T)) -
                   (k * tapply(data[, p], data[, Group], FUN = function (x) IQR(x, na.rm = T))),
                 tapply(data[, p], data[, Group], FUN = function (x) quantile(x, probs = .75, na.rm = T)) +
                   (k * tapply(data[, p], data[, Group], FUN = function (x) IQR(x, na.rm = T)))) %>%  as.data.frame() %>%
      mutate(Var = rownames(.)) %>%
      `rownames<-`(NULL) %>%
      `colnames<-`(c("Min", "Max", "Group"))

    for (i in unique(df[, Group])){
      for (j in unique(df[, ID])){

        df[df[, ID] == j & df[, Group] == i, p] <-
          ifelse(data[data[, Group] == i & data[, ID] == j, p] < tmp[tmp$Group == i, "Min"] |
                   data[data[, Group] == i & data[, ID] == j, p] > tmp[tmp$Group == i, "Max"], TRUE, NA)

        df2[df2[, ID] == j & df2[, Group] == i, p] <- ifelse(
          data[data[, Group] == i & data[, ID] == j, p] > quantile(data[, p], probs = 0.75, na.rm = T) + (k * IQR(data[, p], na.rm = T)) |
          data[data[, Group] == i & data[, ID] == j, p] < quantile(data[, p], probs = 0.25, na.rm = T) - (k * IQR(data[, p], na.rm = T)),
          TRUE, NA)

      }
    }
    Sys.sleep(0.005)
    pb$tick(1)
    pb
  }
  list <- list("Global" = df2, "Stratified" = df)
  if (excel){writexl::write_xlsx(list, path = excel_path)}
  return(list)
}

