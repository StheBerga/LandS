#' Function to create a multivariate cph model with a vector of variables
#'
#' @param db A dataframe
#' @param vars Vector of variables to be included in the multivariate model
#' @param ptime Survival Time variable
#' @param pevent Event variable
#' @param dec_HR digits of HR (Default = 4)
#'
#' @return the multivariate model
#' @export
#'
#' @examples
multivariate_LL <- function(db, vars, ptime, pevent, dec_HR = 4){
  workdata <- db
  options(warn=-1)   # suppress global warnings
  assign("dist",
         datadist(workdata, adjto.cat = "first"),
         envir = .GlobalEnv)
  options(datadist = "dist")
  options(contrasts=c("contr.treatment", "contr.treatment"))

  # Funzioni fomattazione
  formatz <- function(value){
    new_value <- value
    for(i in 1:length(value)){
      if(value[i] > 10^10){
        new_value[i] <- " >10^10"
      }else{
        new_value[i] <- format(round(value[i], dec_HR), digits = dec_HR, nsmall = dec_HR, width = 6, scientific=F, justify = "centre")
      }
    }
    return(new_value)
  }

  formatz_p <- function(value){
    if(is.data.frame(value)==T){
      new_frame <- value
      for(i in 1:dim(value)[1]){
        if(value[i,1]> 0.0001){
          new_frame[i,1] <- format(round(value[i,1], 4), digits = 4, nsmall = 4, width = 6, scientific=F, justify = "centre")
        }else{
          new_frame[i,1] <- "<0.0001"
        }
      }
      return(new_frame)
    }else{
      if(value > 0.0001){
        value <- format(round(value, 4), digits = 4, nsmall = 4, width = 6, scientific=F, justify = "centre")
      }else{
        value <- "<0.0001"}
      return(value)
    }
  }

  # Funzione stringa additiva
  Stringa_LL <- function(data, vet, sep= "+"){
    p <- list()
    k <- 1
    for (i in vet) {
      p[k] <- colnames(data)[i]
      k <- k+1
    }
    p <- unlist(p)
    p <- toString(p)
    if (sep!= ",") {
      p <- gsub(",", sep, p)
    }else {}
    return(p)
  }

  # Prendo la posizione dei nomi
  pos <- c()
  for (i in 1:length(vars)) {
    pos[i] <- which(colnames(workdata)==vars[i] )
  }

  string <- Stringa_LL(workdata, pos)
  frm <- formula(paste0("Surv(", ptime, ",", pevent, ")~", string))

  mod.cph <-   cph(frm, data = workdata, iter.max = 100000)
  mod.coxph <- coxph(frm, data = workdata, iter.max = 100000)

  summ.cph <- summary(mod.cph)
  summ.coxph <- summary(mod.coxph)

  var_multi <- mod.cph$Design$name

  var_cat     <- vector()
  var_quan    <- vector()

  var_cat     <- vector()
  var_quan    <- vector()
  var_diverse <- vector()

  for(i in pos){
    if(is.factor(workdata[[i]])==TRUE|
       is.character(workdata[[i]])==TRUE|
       is.logical(workdata[[i]])==TRUE){
      workdata[,i]<-factor(workdata[[i]])
      var_cat[i]<-i}
    else if(is.numeric(workdata[[i]])==TRUE){
      var_quan[i]<-i}
    else{
      var_diverse[i]<-i}
  }

  if(length(var_cat)==0){
  }else{
    var_cat <- var_cat[is.na(var_cat)==FALSE]}

  if(length(var_quan)==0){
  }else{
    var_quan <- var_quan[is.na(var_quan)==FALSE]}

  if(length(var_diverse)==0){
  }else{
    var_diverse <- var_diverse[is.na(var_diverse)==FALSE]}

  nlev <- c()
  k <- 1
  for(i in var_multi){
    nlev[k] <- nlevels(workdata[,i])
    k <- k+1
  }
  nlev <- sum(nlev) + length(var_quan)

  rows_mod <- nrow(summ.cph)

  mod.df <- data.frame(
    "Var" = rownames(summ.cph)[seq(1,rows_mod,2)],
    "HR" = summ.cph[,"Effect"][seq(2,rows_mod,2)],
    "Lower" = summ.cph[,"Lower 0.95"][seq(2,rows_mod,2)],
    "Upper" = summ.cph[,"Upper 0.95"][seq(2,rows_mod,2)],
    "Pvalue" = NA)

  pvalue.coxph <- as.data.frame(coefficients(summ.coxph))
  pvalue.coxph$Var <- rownames(pvalue.coxph)
  rownames(pvalue.coxph) <- NULL
  pvalue.coxph <- pvalue.coxph[,c(6,5)]
  colnames(pvalue.coxph)[2] <- "pvalue"

  nomi_quan <- colnames(workdata)[var_quan]

  nomi_cat <- NULL
  for(i in var_cat){
    nomi_cat <- c(nomi_cat, colnames(workdata)[i], levels(workdata[,i]))
  }

  nomi_tot  <- c(nomi_quan, nomi_cat)

  # Df finale totale
  tot.df <- data.frame(
    "Var" = nomi_tot,
    "HR" = NA,
    "Lower" = NA,
    "Upper" = NA,
    "Pvalue" = NA)

  lev_ref <- NULL
  for(i in var_cat){
    lev_ref <- c(lev_ref, levels(workdata[,i])[1])
  }

  # Inserisco reference per le categoriche e - nei macronomi categoriche
  for (i in lev_ref) {
    tot.df$HR[tot.df$Var == i] <- "ref"
    tot.df$Lower[tot.df$Var == i] <- "ref"
    tot.df$Upper[tot.df$Var == i] <- "ref"
    tot.df$Pvalue[tot.df$Var == i] <- "ref"
  }

  for (i in colnames(workdata)[var_cat]) {
    tot.df$HR[tot.df$Var == i]   <- "-"
    tot.df$Lower[tot.df$Var == i] <- "-"
    tot.df$Upper[tot.df$Var == i] <- "-"
  }

  # Sistemo pvalue anova nel tot.df
  anova_df <- as.data.frame(anova(mod.cph))
  anova_df$Var <- rownames(anova_df)
  colnames(anova_df)[3] <- "pvalue"
  anova_df <- anova_df[,c("Var", "pvalue")]
  rownames(anova_df) <- NULL

  for (i in c(var_quan, var_cat)) {
    tot.df$Pvalue[tot.df$Var == colnames(workdata)[i]] <- anova_df$pvalue[anova_df$Var == colnames(workdata)[i]]
  }

  # Creo minidataset per il merge pvalue
  pvalue_df <- as.data.frame(summary(mod.coxph)$coefficients)
  pvalue_df$name_paste <- rownames(pvalue_df)
  colnames(pvalue_df)[5] <- "pvalue"
  pvalue_df <- pvalue_df[,c("name_paste", "pvalue")]
  rownames(pvalue_df) <- NULL


  # Dataset design var complete #
  df.design <- data.frame(Var = as.character(),
                          level = as.character())
  for (i in var_cat) {
    nlev_cat <- nlevels(workdata[,i])
    lev_i <- levels(workdata[,i])
    for (k in lev_i) {
      row_ins <- c(colnames(workdata)[i], k)
      df.design <- rbind(df.design, row_ins)
    }
  }

  for (i in var_quan) {
    row_ins <- c(colnames(workdata)[i], colnames(workdata)[i])
    df.design <- rbind(df.design, row_ins)
  }
  colnames(df.design) <- c("Var", "level")

  df.design$name_paste <- NA
  for (i in 1:dim(df.design)[1]) {
    if(df.design$Var[i] == df.design$level[i]){
      df.design$name_paste[i] <- df.design$Var[i]
    } else {
      df.design$name_paste[i] <- paste0(df.design$Var[i], df.design$level[i])
    }
  }

  # Schema categoriche-Ref
  lev_ref.df <- data.frame(Var = as.character(),
                           Ref = as.character())
  for (i in var_cat) {
    row_ins <- c(colnames(workdata)[i], levels(workdata[,i])[1])
    lev_ref.df <- rbind(lev_ref.df, row_ins)
  }
  colnames(lev_ref.df) <- c("Var", "Ref")

  df.design$name_mod <- NA
  for (i in 1:dim(df.design)[1]) {
    if(df.design$Var[i] == df.design$level[i]){
      df.design$name_mod[i] <- df.design$Var[i]
    } else {
      df.design$name_mod[i] <-
        paste0(df.design$Var[i], " ", "-", " ", df.design$level[i],":", lev_ref.df$Ref[lev_ref.df$Var == df.design$Var[i]])
    }
  }

  # Aggiungo pvalue
  tmp1 <- pvalue_df
  df.design <- left_join(df.design, tmp1)
  df.design
  rm(tmp1)

  # Unisco tutto per costruire tot.df bene
  colnames(mod.df)
  tmp1 <- mod.df[,1:4]
  colnames(tmp1)[1] <- "name_mod"

  df.merge <- inner_join(df.design, tmp1)
  rm(tmp1)

  # Sistemazione importante df.tot da merge #
  tot.df.merge <- tot.df
  for (i in df.merge$level) {
    tot.df.merge$HR[tot.df.merge$Var == i] <- df.merge$HR[df.merge$level == i]
    tot.df.merge$Lower[tot.df.merge$Var == i] <- df.merge$Lower[df.merge$level == i]
    tot.df.merge$Upper[tot.df.merge$Var == i] <- df.merge$Upper[df.merge$level == i]
    tot.df.merge$Pvalue[tot.df.merge$Var == i] <- df.merge$pvalue[df.merge$level == i]
  }

  for (i in 1:dim(tot.df.merge)[1]) {
    if (is.na(as.numeric(tot.df.merge$HR[i])) == T) {
    }else{
      num <- tot.df.merge$HR[i]
      num <- as.numeric(num)
      num <- formatz(num)
      tot.df.merge$HR[i] <- num
    }
  }
  for (i in 1:dim(tot.df.merge)[1]) {
    if (is.na(as.numeric(tot.df.merge$Lower[i])) == T) {
    }else{
      num <- tot.df.merge$Lower[i]
      num <- as.numeric(num)
      num <- formatz(num)
      tot.df.merge$Lower[i] <- num
    }
  }
  for (i in 1:dim(tot.df.merge)[1]) {
    if (is.na(as.numeric(tot.df.merge$Upper[i])) == T) {
    }else{
      num <- tot.df.merge$Upper[i]
      num <- as.numeric(num)
      num <- formatz(num)
      tot.df.merge$Upper[i] <- num
    }
  }
  for (i in 1:dim(tot.df.merge)[1]) {
    if (is.na(as.numeric(tot.df.merge$Pvalue[i])) == T) {
    }else{
      num <- tot.df.merge$Pvalue[i]
      num <- as.numeric(num)
      num <- formatz_p(num)
      tot.df.merge$Pvalue[i] <- num
    }
  }
  #colnames(tot.df.merge)[2:5] <- c("       HR ",     "    Lower",  "    Upper",  "    Pvalue")
  return(tot.df.merge)
}
