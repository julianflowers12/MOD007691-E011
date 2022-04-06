library(rvest)


bbsdens <- read_html("https://app.bto.org/bbs-results/results/reg_lists/bbsdenslist-20.html") %>%
  html_table() %>%
  .[[1]]

colnames(bbsdens) <- bbsdens[1,]

bbsdens

bbsdens <- bbsdens %>%
  slice(-1) %>%
  janitor::clean_names() %>%
  pivot_longer(names_to = "year", values_to = "vals", cols = 2:ncol(.))

sq <- bbsdens %>%
  filter(str_detect(species, "square")) %>%
  pluck("vals") %>%
  map(., parse_number) %>%
  map_dbl(., 1)

8046/27

bbs_dens <- bbsdens %>%
  mutate(sq = rep(sq, 298)) %>%
  filter(!str_detect(species, "sq")) %>%
  mutate_at(.vars = 3, parse_number) %>%
  mutate(dens = vals / sq,
         year = parse_number(year))

View(bbs_dens)

species_denisty_plot <- function(species){

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

species_denisty_plot("Greenfinch")

species_list <- unique(bbs_dens$species)

sl <- species_list[c(71, 73, 198, 200, 202, 208, 212, 213, 239, 244, 246, 249, 262, 264,
                     273, 278, 287, 184)]

plots <- map(sl, species_denisty_plot)

library(geomtextpath)

bbs_dens %>%
  group_by(species) %>%
  filter(species %in% sl) %>%
  mutate(sc = ifelse(species %in% c("Greenfinch", "Starling", "House Sparrow"), "Red",
                     ifelse(species %in% c("Song Thrush",
                                          "Woodpigeon", "Dunnock",
                                          "Wren"), "Amber", "Green" )),
         direction = (dens[25] / dens[1]),
         dir = ifelse(direction > 1.2, "up",
                      ifelse(direction < 0.8, "down", "stable"))) %>%
  mutate(sc = fct_relevel(sc, "Amber", after = 1)) %>%
  ggplot(aes(year, dens, group = species)) +
  stat_smooth(se = FALSE,
              method = "gam",
              aes(label = species, hjust = factor(sc), colour = sc),
              size = 3,
              lty = 1,
              geom = "textpath",
              show.legend = F
              ) +
  scale_hjust_discrete() +
  scale_y_log10() +
  scale_colour_manual(values = c('darkgreen', "goldenrod", "red")) +
  theme_minimal() +
  facet_wrap(~sc) +
  labs(y = "Birds / km2 (log scale)",
       x = "Year",
       caption = "Source: https://app.bto.org/bbs-results/results/reg_lists/bbsdenslist-20.html",
       title = "Density (observations per km2) of 20 commonest garden birds",
       subtitle = "Smoothed data from Breeding Bird Survey 1995-2020") +
  theme(plot.title.position = "plot",
        plot.title = element_text(size = 14, face = "bold"))

bbs_dens %>%
  group_by(species) %>%
  filter(species %in% sl, year %in% c(1994, 2020)) %>%
  mutate(sc = ifelse(species %in% c("Greenfinch", "Starling", "House Sparrow"), "Red",
                     ifelse(species %in% c("Song Thrush",
                                           "Woodpigeon", "Dunnock",
                                           "Wren"), "Amber", "Green" )),
         direction = (dens[2] / dens[1]),
         dir = ifelse(direction > 1.2, "up",
                      ifelse(direction < 0.6, "down", "stable"))) %>%
  filter(dir %in% c("up", "down")) %>%
  ggplot(aes(year, dens, group = species)) +
  geom_textline(aes(colour = sc, label = species, hjust = sc), show.legend = F) +
  scale_y_continuous(sec.axis = sec_axis(~.)) +
  scale_hjust_discrete() +
  scale_color_manual(values = c("goldenrod", "darkgreen", "red")) +
  theme(axis.text.x = element_blank(),
        panel.background = element_blank()) +
  annotate("text", x = 1994, y = 24, label = "1994") +
  annotate("text", x = 2020, y = 24, label = "2020") +
  labs(x = "",
       y = "",
       title = "Winners and losers") +
  theme(plot.title.position = "plot",
        plot.title = element_text(size = 14, face = "bold"))


  mutate(sc = fct_relevel(sc, "Amber", after = 1))

### occupancy

bbsdens
