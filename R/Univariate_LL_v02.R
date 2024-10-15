#' This function allows you to create the univariate regression model for a vector of variables
#'
#' @param db dataframe
#' @param vars vector with variables name
#' @param ptime Survival Time variable
#' @param pevent Event variable
#' @param dec_HR digits of HR (Default = 4)
#'
#' @return a dataframe with all univariate models
#' @export
#'
#' @examples
univariate_LL <- function(db, vars, ptime, pevent, dec_HR = 4){
  workdata <- db

  assign("dist",
         rms::datadist(db[, vars], adjto.cat = "first"),
         envir = .GlobalEnv)
  options(datadist = "dist")
  options(contrasts=c("contr.treatment", "contr.treatment"))

  # Funzioni formattazione
  formatz_p <- function(value){
    if(is.data.frame(value)==T){
      new_frame <- value

      for(i in 1:dim(value)[1]){

        if(is.na(value[i, 1]) == T) {

          new_frame[i, 1] = NA
        }

        else if(value[i, 1] >= 0.0001){
          new_frame[i, 1] <- format(round(value[i,1], 4), digits = 4, nsmall = 4, width = 6, scientific=F, justify = "centre")
        }else if (value[i, 1] < 0.0001){
          new_frame[i, 1] <- "<0.0001"
        }
      }
      return(new_frame)
    }else if (is.vector(value) == TRUE){
      new_vett <- c()

      for (i in 1:length(value)) {


        if (is.na(value[i])){

          p <- NA

        }else if (value[i] > 0.0001){

          p <- format(round(value[i], 4), digits = 4, nsmall = 4, width = 6, scientific=F, justify = "centre")

        }else if (value[i] < 0.0001){

          p <- "<0.0001"
        }
        new_vett <- c(new_vett,p)
      }
      return(new_vett)
    }else{

      if(is.na(value) == T) {

        value = NA
      }
      else if(value >= 0.0001){
        value <- format(round(value, 4), digits = 4, nsmall = 4, width = 6, scientific=F, justify = "centre")
      }else if(value < 0.0001){
        value <- " <0.0001"}
      return(value)
    }
  }

  # Inizio funzione
  var_uni <- vars

  pos <- c()
  for (i in 1:length(var_uni)) {
    pos[i] <- which(colnames(workdata)==var_uni[i] )
  }

  var_cat     <- vector()
  var_quan    <- vector()
  var_diverse <- vector()

  for(i in pos){
    if(is.factor(workdata[[i]])==TRUE|
       is.character(workdata[[i]])==TRUE|
       is.logical(workdata[[i]])==TRUE){
      workdata[,i] <- factor(workdata[[i]])
      var_cat[i] <- i}
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

  # Calcolo dei livelli globali (serve per sapere le righe dell'output)
  nlev <- c()

  k <- 1
  for(i in var_cat){
    if(is.ordered(workdata[, i])){
      workdata[, i] <- factor(workdata[, i], levels = levels(workdata[, i]), ordered = F)

      message("La variabile ", colnames(workdata)[i], " è factor ordinata. rimuovo ordinamento", "\n")
      # stop(paste0("La variabile ", colnames(workdata)[i], " è factor ordinata. Stop funzione"))
    }

    nlev[k] <- nlevels(workdata[,i])
    k <- k+1
  }
  nlev <- sum(nlev) + length(var_quan)

  # Variabili quantitative in univariata
  cox_model_q <- data.frame(Var = as.character(NA),
                            HR = as.character(NA),
                            "95%CI"= as.character(NA),
                            pvalue= as.character(NA),
                            pval_raw = as.numeric(NA),
                            check.names = F)
  k <- 1
  for(i in var_quan){
    frm <- as.formula(paste0("Surv(", ptime, ",", pevent, ")~", colnames(workdata)[i]))
    mod  <- rms::cph(frm, data=workdata)
    summ <- summary(mod)

    HR <- formatz_p(summ[2,4])
    CI.l <- formatz_p(summ[2,6])
    CI.u <- formatz_p(summ[2,7])
    pg <- stats::anova(mod)[1,3]
    p <- formatz_p(stats::anova(mod)[1,3])
    CI <- paste0(CI.l, "-", CI.u)
    CI <- gsub(" ", "", CI)

    cox_model_q[k, "Var"] <- colnames(workdata)[i]
    cox_model_q[k, "HR"] <- HR
    cox_model_q[k, "95%CI"] <- CI
    cox_model_q[k, "pval_raw"] <- pg
    cox_model_q[k, "pvalue"] <- p
    k <- k+1
  }

  # Variabili categoriche in univariata

  cox_model_cat <- data.frame(Var = as.character(NA),
                              HR = as.character(NA),
                              "95%CI"= as.character(NA),
                              pvalue= as.character(NA),
                              pval_raw = as.numeric(NA),
                              check.names = F)

  for(i in var_cat){

    frm <- as.formula(paste0("Surv(", ptime, ",", pevent, ")~", colnames(workdata)[i]))
    mod  <- survival::coxph(frm, data= workdata)
    summ <- summary(mod)
    lev <- nlevels(workdata[,i])
    name_lev <- levels(workdata[,i])
    ref <- name_lev[1]

    if(lev == 2){
      p <- summ$coefficients[,5]
      row_cat <- c(colnames(workdata)[i], "-", "-", formatz_p(p), p)
    } else {
      row_cat <- c(colnames(workdata)[i], "-", "-", formatz_p(summ$logtest["pvalue"]), summ$logtest["pvalue"])
    }

    row_ref <- c(ref, "ref", "ref", "ref", "ref")

    HR <- formatz_p(exp(mod$coefficients))
    CI.l <- formatz_p(summ$conf.int[, "lower .95"])
    CI.u <- formatz_p(summ$conf.int[, "upper .95"])
    CI <- paste0(CI.l, "-", CI.u)
    CI <- gsub(" ", "", CI)
    p <- data.frame(value = summ$coefficients[, "Pr(>|z|)"])
    pg <- p
    p <- formatz_p(p)

    cox_model_cat <- rbind(cox_model_cat, row_cat)
    cox_model_cat <- rbind(cox_model_cat, row_ref)

    for (i in 1:(lev-1)) {
      row_lev <- c(name_lev[i+1], HR[i], CI[i], p[i, 1], pg[i, 1])
      cox_model_cat <- rbind(cox_model_cat, row_lev)
    }
  }
  cox_model_cat <- cox_model_cat[-1, ]

  if(length(var_quan)==0){
    cox_model <- cox_model_cat
  } else if(length(var_cat)==0){
    cox_model <- cox_model_q
  }else{
    cox_model <- rbind(cox_model_q, cox_model_cat)
  }

  return(cox_model)
}
