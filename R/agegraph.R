#' Age Graph
#'
#' Plots sentence repetition performance by diagnosis and age to investigate any
#' confounding influence

agegraph <- function(MODEL){
  age_factor <- MODEL %>%
    predictions(newdata = datagrid(diagnosis = unique,
                                   scale_agemths = seq(-2, 2, by = 0.1))) %>%
    posterior_draws()
  ggplot(age_factor,
         aes(x = scale_agemths, y = draw, color = diagnosis, fill = diagnosis)) +
    stat_lineribbon(aes(fill_ramp = stat(level))) +
    scale_y_continuous(labels = label_percent()) +
    scale_fill_viridis_d(option = "plasma", end = 0.8) +
    scale_color_viridis_d(option = "plasma", end = 0.8) +
    scale_fill_ramp_discrete(range = c(0.2, 0.7)) +
    facet_wrap(vars(diagnosis), ncol = 2,
               labeller = labeller(diagnosis = c(`TD` = "TD",
                                                 `DLD` = "DLD"))) +
    labs(x = "agemths",
         y = "Score",
         fill = "diagnosis", color = "diagnosis",
         fill_ramp = "Credible interval") +
    theme_clean() +
    theme(legend.position = "bottom")
}
