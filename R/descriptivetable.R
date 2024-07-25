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
                         "Male" = c(sum(TDdata$q5gender == 1), sum(DLDdata$q5gender == 1)), "Female" = c(sum(TDdata$q5gender == 0), sum(DLDdata$q5gender == 0)),
                         "Age in months" = age, "White ethnic origin" = c(sum(TDdata$ethnicity == 1), sum(DLDdata$ethnicity == 1)),
                         "Other ethnic origin" = c(sum(TDdata$ethnicity == 2), sum(DLDdata$ethnicity == 2)),
                         "IDACI" = income)
  nice_table(SummaryTable)
}
