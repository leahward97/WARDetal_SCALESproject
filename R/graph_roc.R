#' Graph ROC - for age-matched groups
#'
#' Takes the data frame of ROC comparisons, from 'DiagComp.table' function and
#' plots each ROC for each method of scoring and for the covariate (age or distance)


graph_roc <- function(DATA, MATCHING){
  if (MATCHING == "age"){
    plot(roc(controls=DATA$sentrepsentencetotal[DATA$diagnosis=="TD"],
             cases = DATA$sentrepsentencetotal[DATA$diagnosis=="DLD"], direction = ">", ci=TRUE), print.auc = TRUE,
         print.ci = TRUE, col = palette[1])
    plot(roc(controls=DATA$sentrepcontenttotal[DATA$diagnosis=="TD"],
             cases = DATA$sentrepcontenttotal[DATA$diagnosis=="DLD"], direction = ">", ci=TRUE), print.auc = TRUE,
         print.ci = TRUE, col = palette[2], print.auc.y = .4, add = TRUE)
    plot(roc(controls=DATA$sentrepfunctiontotal[DATA$diagnosis=="TD"],
             cases = DATA$sentrepfunctiontotal[DATA$diagnosis=="DLD"], direction = ">", ci=TRUE), print.auc = TRUE,
         print.ci = TRUE, col = palette[3], print.auc.y = .3, add = TRUE)
    plot(roc(controls=DATA$sentrepverbtotal[DATA$diagnosis=="TD"],
             cases = DATA$sentrepverbtotal[DATA$diagnosis=="DLD"], direction = ">", ci=TRUE), print.auc = TRUE,
         print.ci = TRUE, col = palette[4], print.auc.y = .2, add = TRUE)
    plot(roc(controls=DATA$scale_agemths[DATA$diagnosis=="TD"],
             cases = DATA$scale_agemths[DATA$diagnosis=="DLD"], direction = ">", ci=TRUE), print.auc = TRUE,
         print.ci = TRUE, col = "black", print.auc.y = .1, add = TRUE)
    legend(0, 1, legend=c("Binary", "Content", "Function", "Verb Inflection", "age"),
           fill = c(palette[1], palette[2],palette[3],palette[4], "black")
    )
  } else if (MATCHING == "LANGUAGE"){
    plot(roc(controls=DATA$sentrepsentencetotal[DATA$diagnosis=="TD"],
             cases = DATA$sentrepsentencetotal[DATA$diagnosis=="DLD"], direction = ">", ci=TRUE), print.auc = TRUE,
         print.ci = TRUE, col = palette[1])
    plot(roc(controls=DATA$sentrepcontenttotal[DATA$diagnosis=="TD"],
             cases = DATA$sentrepcontenttotal[DATA$diagnosis=="DLD"], direction = ">", ci=TRUE), print.auc = TRUE,
         print.ci = TRUE, col = palette[2], print.auc.y = .4, add = TRUE)
    plot(roc(controls=DATA$sentrepfunctiontotal[DATA$diagnosis=="TD"],
             cases = DATA$sentrepfunctiontotal[DATA$diagnosis=="DLD"], direction = ">", ci=TRUE), print.auc = TRUE,
         print.ci = TRUE, col = palette[3], print.auc.y = .3, add = TRUE)
    plot(roc(controls=DATA$sentrepverbtotal[DATA$diagnosis=="TD"],
             cases = DATA$sentrepverbtotal[DATA$diagnosis=="DLD"], direction = ">", ci=TRUE), print.auc = TRUE,
         print.ci = TRUE, col = palette[4], print.auc.y = .2, add = TRUE)
    plot(roc(controls=DATA$distance[DATA$diagnosis=="TD"],
             cases = DATA$distance[DATA$diagnosis=="DLD"], direction = ">", ci=TRUE), print.auc = TRUE,
         print.ci = TRUE, col = "black", print.auc.y = .1, add = TRUE)
    legend(0, 1, legend=c("Binary", "Content", "Function", "Verb Inflection", "Propensity Score"),
           fill = c(palette[1], palette[2],palette[3],palette[4], "black")
    )
  }
}
