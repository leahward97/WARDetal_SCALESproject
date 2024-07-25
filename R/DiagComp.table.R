#' Diagnostic Comparisons Table
#'
#' Takes the output of 'DiagComp' function and forms a dataframe



DiagComp.table <- function(CompSum){
  Value = as.numeric(c(CompSum$Binary_Content["statistic"], CompSum$Binary_Function["statistic"],
                       CompSum$Binary_Verb["statistic"], CompSum$Content_Function["statistic"],
                       CompSum$Content_Verb["statistic"], CompSum$Function_Verb["statistic"]))
  p.value = as.numeric(c(CompSum$Binary_Content["p.value"], CompSum$Binary_Function["p.value"],
                         CompSum$Binary_Verb["p.value"], CompSum$Content_Function["p.value"],
                         CompSum$Content_Verb["p.value"], CompSum$Function_Verb["p.value"]))
  data.frame("Comparison" = tests_names, Value, p.value)
}
