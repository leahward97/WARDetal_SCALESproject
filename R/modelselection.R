#' Model Selection
#'
#' Function that identifies what variation of Beta model is needed for each analysis
#'
#' Run for each level of scoring for each level of matching
#'
#' Identifies whether 0s or 1s are present in the data and if a zero-, one- or zero-one-
#' inflated beta regression is needed

modelselection <- function(DV, DATA){
  if ((length(which(DV == 0))) > 0 & (length(which(DV == 1))) > 0){
    print("Zero One Inflation")
  } else if ((length(which(DV == 0))) > 0){
    print("Zero Inflation")
  } else if ((length(which(DV == 1))) > 0){
    print("One Inflation")
  } else
    print("Beta")
}
