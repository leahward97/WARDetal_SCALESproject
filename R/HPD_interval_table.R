#' HPD Interval Table
#'
#' Creates a table of the 90% HPD intervals for parameters
#'
#' These parameters are based upon the variation of beta regression model used

HPD_interval_table <- function(MODEL, DV, DATA){
  if (modelselection(DV, DATA) == "Beta"){
    posterior_samples(MODEL, pars = "b_")[,1:5] %>%
      mutate_at(c("b_phi_Intercept"), exp) %>%
      mutate_at(vars(-"b_phi_Intercept"), plogis) %>%
      posterior_summary() %>%
      as.data.frame() %>%
      rownames_to_column("Parameter") %>%
      nice_table()
  } else if (modelselection(DV, DATA) == "Zero Inflation"){
    posterior_samples(MODEL, pars = "b_")[,1:7] %>%
      mutate_at(c("b_phi_Intercept"), exp) %>%
      mutate_at(vars(-"b_phi_Intercept"), plogis) %>%
      posterior_summary() %>%
      as.data.frame() %>%
      rownames_to_column("Parameter") %>%
      nice_table()
  } else if (modelselection(DV, DATA) == "Zero One Inflation"){
    posterior_samples(MODEL, pars = "b_")[,1:9] %>%
      mutate_at(c("b_phi_Intercept"), exp) %>%
      mutate_at(vars(-"b_phi_Intercept"), plogis) %>%
      posterior_summary() %>%
      as.data.frame() %>%
      rownames_to_column("Parameter") %>%
      nice_table()
  } else
    posterior_samples(MODEL, pars = "b_")[,1:7] %>%
    mutate_at(c("b_phi_Intercept"), exp) %>%
    mutate_at(vars(-"b_phi_Intercept"), plogis) %>%
    posterior_summary() %>%
    as.data.frame() %>%
    rownames_to_column("Parameter") %>%
    nice_table()
}

