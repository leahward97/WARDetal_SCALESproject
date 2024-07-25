#' BF10
#'
#' Calculates evidence in support of the experimental hypothesis
#'
#' Compares Beta Regression model to a null model
#'

BF10 <- function(MODEL, NULLMODEL){
  margLik_beta <- bridge_sampler(MODEL, silent = TRUE)
  margLik_null <- bridge_sampler(NULLMODEL, silent = TRUE)
  BF10 <- exp(margLik_beta$logml -  margLik_null$logml)
  print(BF10)
}
