#' Diagnostic Comparison
#'
#' For each matched dataset, calculates the roc curve for each method of scoring
#'
#' These are then compared with roc.test with comparisons between each scoring method

DiagComp <- function(DATA){
  roc_binary <- roc(controls=DATA$sentrepsentencetotal[DATA$diagnosis=="TD"],
                    cases = DATA$sentrepsentencetotal[DATA$diagnosis=="DLD"], direction = ">", ci = TRUE)
  roc_content <- roc(controls=DATA$sentrepcontenttotal[DATA$diagnosis=="TD"],
                     cases = DATA$sentrepcontenttotal[DATA$diagnosis=="DLD"], direction = ">", ci = TRUE)
  roc_function <- roc(controls=DATA$sentrepfunctiontotal[DATA$diagnosis=="TD"],
                      cases = DATA$sentrepfunctiontotal[DATA$diagnosis=="DLD"], direction = ">", ci = TRUE)
  roc_inflection <- roc(controls=DATA$sentrepverbtotal[DATA$diagnosis=="TD"],
                        cases = DATA$sentrepverbtotal[DATA$diagnosis=="DLD"], direction = ">", ci = TRUE)
  combn(
    list("Binary" = roc_binary, "Content" = roc_content, "Function" = roc_function, "Verb" = roc_inflection),
    FUN = function(x, ...) roc.test(x[[1]], x[[2]], reuse.auc = TRUE, method = "bootstrap", boot.n = 10000),
    m = 2, simplify = FALSE) %>%
    setNames(., tests_names)
}
