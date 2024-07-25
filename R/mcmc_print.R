#' MCMC Print
#'
#' Creates plots using MCMC_trace for the parameters of interest
#'
#' The parameters plotted are based upon the variation of beta regression model used

mcmc_print <- function(POSTERIOR, DV, DATA, MATCHING){
  if (MATCHING == "AGE"){
    if (modelselection(DV, DATA) == "Beta"){
      mcmc_trace(POSTERIOR, pars = c("b_Intercept", "b_phi_Intercept", "b_diagnosisTD",
                                     "b_scale_agemths", "b_phi_diagnosisTD"),
                 facet_args = list(ncol = 3),
                 size = .15) +
        theme_clean() +
        theme(legend.position = c(.95, .2))
    } else if (modelselection(DV, DATA) == "Zero Inflation"){
      mcmc_trace(POSTERIOR, pars = c("b_Intercept", "b_phi_Intercept", "b_diagnosisTD",
                                     "b_zi_Intercept", "b_zi_diagnosisTD",
                                     "b_scale_agemths", "b_phi_diagnosisTD"),
                 facet_args = list(ncol = 3),
                 size = .15) +
        theme_clean() +
        theme(legend.position = c(.95, .2))
    } else if (modelselection(DV, DATA) == "Zero One Inflation"){
      mcmc_trace(POSTERIOR, pars = c("b_Intercept", "b_phi_Intercept", "b_diagnosisTD",
                                     "b_zoi_Intercept", "b_zoi_diagnosisTD",
                                     "b_coi_Intercept", "b_coi_diagnosisTD",
                                     "b_scale_agemths", "b_phi_diagnosisTD"),
                 facet_args = list(ncol = 3),
                 size = .15) +
        theme_clean() +
        theme(legend.position = c(.95, .2))
    } else
      mcmc_trace(POSTERIOR, pars = c("b_Intercept", "b_phi_Intercept", "b_diagnosisTD",
                                     "b_zoi_Intercept", "b_zoi_diagnosisTD",
                                     "b_scale_agemths", "b_phi_diagnosisTD"),
                 facet_args = list(ncol = 3),
                 size = .15) +
      theme_clean() +
      theme(legend.position = c(.95, .2))
  } else if (MATCHING == "LANGUAGE"){
    if (modelselection(DV, DATA) == "Beta"){
      mcmc_trace(POSTERIOR, pars = c("b_Intercept", "b_phi_Intercept", "b_diagnosisTD",
                                     "b_distance", "b_phi_diagnosisTD"),
                 facet_args = list(ncol = 3),
                 size = .15) +
        theme_clean() +
        theme(legend.position = c(.95, .2))
    } else if (modelselection(DV, DATA) == "Zero Inflation"){
      mcmc_trace(POSTERIOR, pars = c("b_Intercept", "b_phi_Intercept", "b_diagnosisTD",
                                     "b_zi_Intercept", "b_zi_diagnosisTD",
                                     "b_distance", "b_phi_diagnosisTD"),
                 facet_args = list(ncol = 3),
                 size = .15) +
        theme_clean() +
        theme(legend.position = c(.95, .2))
    } else if (modelselection(DV, DATA) == "Zero One Inflation"){
      mcmc_trace(POSTERIOR, pars = c("b_Intercept", "b_phi_Intercept", "b_diagnosisTD",
                                     "b_zoi_Intercept", "b_zoi_diagnosisTD",
                                     "b_coi_Intercept", "b_coi_diagnosisTD",
                                     "b_distance", "b_phi_diagnosisTD"),
                 facet_args = list(ncol = 3),
                 size = .15) +
        theme_clean() +
        theme(legend.position = c(.95, .2))
    } else
      mcmc_trace(POSTERIOR, pars = c("b_Intercept", "b_phi_Intercept", "b_diagnosisTD",
                                     "b_zoi_Intercept", "b_zoi_diagnosisTD",
                                     "b_distance", "b_phi_diagnosisTD"),
                 facet_args = list(ncol = 3),
                 size = .15) +
      theme_clean() +
      theme(legend.position = c(.95, .2))
  }
}

