#' Distance Graph
#'
#' Plots sentence repetition performance by diagnosis and distance (propensity score) to investigate any
#' confounding influence of matching

distancegraph <- function(MODEL){
  distance_factor <- MODEL %>%
    predictions(newdata = datagrid(diagnosis = unique,
                                   distance = seq(-1, 1, by = 0.05))) %>%
    posterior_draws()
  ggplot(distance_factor,
         aes(x = distance, y = draw, color = diagnosis, fill = diagnosis)) +
    stat_lineribbon(aes(fill_ramp = stat(level))) +
    scale_y_continuous(labels = label_percent()) +
    scale_fill_viridis_d(option = "plasma", end = 0.8) +
    scale_color_viridis_d(option = "plasma", end = 0.8) +
    scale_fill_ramp_discrete(range = c(0.2, 0.7)) +
    facet_wrap(vars(diagnosis), ncol = 2,
               labeller = labeller(diagnosis = c(`TD` = "TD",
                                                 `DLD` = "DLD"))) +
    labs(x = "Propensity Score for Matching",
         y = "Score",
         fill = "diagnosis", color = "diagnosis",
         fill_ramp = "Credible interval") +
    theme_clean() +
    theme(legend.position = "bottom")
}
