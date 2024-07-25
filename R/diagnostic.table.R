#' Diagnostic Table
#'
#' Takes the output of the 'diagnostic' function and puts them in a dataframe


diagnostic.table <- function(DIAG){
  Scoring <- c("Binary (/32)", "Content Word (/121)", "Function Word (/176)", "Verb Inflection (/44)")
  Optimal <- c()
  AUC <- c()
  sensitivity <- c()
  specificity <- c()
  for (i in DIAG){
    Optimal <- c(Optimal, as.numeric(i$"optimal_cutpoint"))
    AUC <- c(AUC, as.numeric(i$"AUC"))
    sensitivity <- c(sensitivity, as.numeric(i$"sensitivity"))
    specificity <- c(specificity, as.numeric(i$"specificity"))
  }
  data.frame("Type of Scoring" = Scoring, "Optimal Cut Off Point" = Optimal,
             AUC, sensitivity, specificity)
}
