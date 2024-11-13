#' This function allows to create a KM survival curve overall or splitted by a categorical variable
#'
#' @param Event Event variable
#' @param tEvent Survival Time Variable
#' @param strata Variable to stratify (Default = 1)
#' @param data dataframe
#' @param title Graph title (Default = "Prova")
#' @param xlab x-axis title (Default = "Time in months")
#' @param ylab y-axis title (Default = "Probaility of Surv")
#' @param xlim limits of x-axis (Default is from 0 to maximum observed time)
#' @param breaks_by breaks of risk table(Default = 3)
#'
#' @return a KM graph
#' @export
#'
#' @examples
KM_LB <- function(Event = "OS_EVENT",
                  tEvent = "OS",
                  strata = 1,
                  data = data,
                  title = "Prova",
                  xlab = "Time in months",
                  ylab = "Probaility of Surv",
                  xlim = c(0, max(data[, tEvent], na.rm = T)),
                  breaks_by = 3){

  require(survival)
  require(survminer)
  require(ggplot2)

  frm <- formula(paste0("Surv(", tEvent, ",", Event, ")~", strata))
  fit = surv_fit(frm, data = data)

  if (strata != 1){pval = T}else{pval = F}

  KM.fit <- ggsurvplot(fit = fit,
                       title = title,
                       data = data,
                       risk.table = T,
                       tables.height=0.25,
                       palette="jco",
                       pval = pval,
                       pval.size=5,
                       pval.coord=c(2, 0.1),
                       conf.int = F,
                       xlim = xlim,
                       xlab = xlab,
                       ylab = ylab,
                       legend.title="",
                       break.time.by = breaks_by,
                       surv.scale = "percent",
                       risk.table.title="",
                       risk.table.y.text.col = T,
                       risk.table.y.text = F,
                       risk.table.fontsize= 4,
                       censor=F,
                       size=1.0)

  KM.fit$plot <- KM.fit$plot+
    coord_cartesian(expand = F)+
    theme(plot.title = element_text(size=12, hjust = 0.5, face = "bold", vjust = -3),
          plot.subtitle = element_text(size=6, color = "black"),
          legend.key.width = unit(0.7,"cm"),
          legend.key.height = unit(0.1,"cm"),
          axis.title.y = element_text(size = 11,  face = "plain", color = "black"),
          axis.title.x = element_blank(),
          legend.text = element_text(face = "bold")
    )

  KM.fit$table <- KM.fit$table +
    theme(axis.text.x = element_text(size = 6,  face = "bold", color = "black"),
          axis.title.y = element_text(size = 6,  face = "plain"),
          axis.title.x = element_text(size = 11,  face = "plain"),
          plot.margin = margin(0, 1.1, 0, 0, "cm")
    )
  KM.fit$table$theme$axis.text.x.bottom <- element_text(size = 11, face = "plain")
  return(KM.fit)
}


