#' Proportions
#'
#' Calculating the proportion correct for each of the sentence repetition measures of accuracy
#' and adding these as a column to the dataframe
#'
#' Measures -
#' Sentence Binary scoring /32
#' Content words correct /121
#' Function words correct /176
#' Verbs inflected correctly /44

proportions <- function(DATA){
  DATA <- DATA %>% mutate(p_Binary = sentrepsentencetotal/32)
  DATA <- DATA %>% mutate(p_Content = sentrepcontenttotal/121)
  DATA <- DATA %>% mutate(p_Function = sentrepfunctiontotal/176)
  DATA <- DATA %>% mutate(p_Inflection = sentrepverbtotal/44)
  DATA <- return(DATA)
}
