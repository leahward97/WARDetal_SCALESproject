#' Diagnostic
#'
#' Calculates the diagnostic values - Sensitivity, Specificity, AUC for the optimal cut
#' off point calculated by ROC curves

diagnostic <- function(DATA){
  binary_opt <- cutpointr(data = DATA, x = sentrepsentencetotal, class = diagnosis,
                          direction = "<=", pos_class = "DLD", neg_class = "TD",
                          method = maximize_metric, metric = youden, boot_runs = 2000)
  content_opt <- cutpointr(data = DATA, x = sentrepcontenttotal, class = diagnosis,
                           direction = "<=", pos_class = "DLD", neg_class = "TD",
                           method = maximize_metric, metric = youden,boot_runs = 2000)
  function_opt <- cutpointr(data = DATA, x = sentrepfunctiontotal, class = diagnosis,
                            direction = "<=", pos_class = "DLD", neg_class = "TD",
                            method = maximize_metric, metric = youden,boot_runs = 2000)
  inflection_opt <- cutpointr(data = DATA, x = sentrepverbtotal, class = diagnosis,
                              direction = "<=", pos_class = "DLD", neg_class = "TD",
                              method = maximize_metric, metric = youden,boot_runs = 2000)
  list("Binary" = binary_opt, "content" = content_opt,
       "function" = function_opt, "inflection" = inflection_opt)
}
