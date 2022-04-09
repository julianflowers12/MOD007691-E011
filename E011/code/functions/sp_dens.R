## species density plot for bbs_density data

species_density_plot <- function(species) {

  bbs_dens %>%
    filter(str_detect(species, !!species)) %>%
    ggplot(aes(year, dens)) +
    geom_point() +
    geom_smooth(method = "gam") +
    labs(title = paste(species, "density from BBS: count per square km"),
         y = "Count per km2",
         x = "Year",
         caption ="Source: https://app.bto.org/bbs-results/results/reg_lists/bbsdenslist-20.html") +
    theme_minimal() +
    scale_y_continuous(position = "right")
}
