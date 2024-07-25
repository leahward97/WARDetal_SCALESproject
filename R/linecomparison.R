#' Line Comparison
#'
#' Creates 4 graphs visualising sentence repetition performance between TD and DLD groups
#'
#' Across each of the 4 measures of scoring
#'
#' To be used on age-matched and language-matched data

linecomparison <- function(DATA){
  Binary_plot <- DATA %>%
    ggplot(aes(x = p_Binary, color = diagnosis)) + geom_density() + theme_apa()
  Content_plot <- DATA %>%
    ggplot(aes(x = p_Content, color = diagnosis)) + geom_density() + theme_apa()
  Function_plot <- DATA %>%
    ggplot(aes(x = p_Function, color = diagnosis)) + geom_density() + theme_apa()
  Inflection_plot <- DATA %>%
    ggplot(aes(x = p_Inflection, color = diagnosis)) + geom_density() + theme_apa()
  grid.arrange(Binary_plot, Content_plot,Function_plot, Inflection_plot, ncol=2, nrow=2)
}
