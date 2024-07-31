#' Descriptive Table
#'
#'Generating a descriptive table in APA style for the SCALES data
#'
#'To be used per each time point individually

descriptivetable <- function(TDdata, DLDdata){
  age_mean <- c(round(mean(TDdata$agemths),2), round(mean(DLDdata$agemths),2))
  age_sd <- c(round(sd(TDdata$agemths),2), round(sd(DLDdata$agemths),2))
  age <- str_glue("{age_mean} ({age_sd})")
  income_mean <- c(round(mean(TDdata$idaci_quintilet1),2), round(mean(DLDdata$idaci_quintilet1),2))
  income_sd <- c(round(sd(TDdata$idaci_quintilet1),2), round(sd(DLDdata$idaci_quintilet1),2))
  income <- str_glue("{income_mean} ({income_sd})")
  SummaryTable <- tibble("Does child meet criteria of DLD?" = c("no", "yes"), "n" = c(nrow(TDdata), nrow(DLDdata)),
                         "Male" = c(sum(TDdata$q5gender == "Male"), sum(DLDdata$q5gender == "Male")), 
                         "Female" = c(sum(TDdata$q5gender == "Female"), sum(DLDdata$q5gender == "Female")),
                         "Age in months" = age, "White ethnic origin" = c(sum(TDdata$ethnicity == "White"), sum(DLDdata$ethnicity == "White")),
                         "Other ethnic origin" = c(sum(TDdata$ethnicity == "Other"), sum(DLDdata$ethnicity == "Other")),
                         "IDACI" = income)
  nice_table(SummaryTable)
}
