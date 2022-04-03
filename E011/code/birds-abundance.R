## breeding pairs esimates from RSPB

library(rvest)

url <- "https://www.rspb.org.uk/birds-and-wildlife/wildlife-guides/bird-a-z/"
species_list <- list("wren", "house-sparrow", "blackbird", "song-thrush", "blue-tit",
                     "great-tit", 'goldfinch', "greenfinch", "coal-tit", "great-spotted-woodpecker",
                     "chaffinch", "jackdaw", "long-tailed-tit", "carrion-crow", "woodpigeon", "collared-dove", "magpie",
                     "starling", "robin", "dunnock", "hawfinch", "corn-bunting", "yellowhammer", "tree-sparrow",
                     "mistle-thrush", "turtle-dove", "linnet", "redstart")

get_details <- function(url = "https://www.rspb.org.uk/birds-and-wildlife/wildlife-guides/bird-a-z/", species){

  paste0(url, species) %>%
  read_html() %>%
  html_nodes(".species-measurements-population__details-content") %>%
  html_text() %>%
  str_squish() %>%
  str_split(., "        ") %>%
  enframe() %>%
  unnest("value") %>%
  mutate(species = species)

}

get_details(species = species_list[12])

details <- map_dfr(species_list, ~get_details(species = .x))

details %>%
  filter(species == "redstart")

options(scipen = 999)

details %>%
  #filter(species == "starling")
  filter(str_detect(value, "pairs|terr|million")) %>%
  slice(-c(4, 7:8)) %>%
  mutate(count = parse_number(value),
    count = ifelse(count < 100, count *10^6, count),
    status = ifelse(species %in% c("starling", "greenfinch", "house-sparrow", "hawfinch", "corn-bunting", "yellowhammer",
                                   "tree-sparrow", "mistle-thrush", "turtle-dove", "linnet"), "red",
                    ifelse(species %in% c("dunnock", "song-thrush", "redstart", "woodp", "wren"), "amber", "green")),
    species = str_to_title(species),
    species = str_replace_all(species, "-", " "),
    species = case_when(str_detect(species, "Long Tailed Tit") ~ "Long-tailed Tit",
                        TRUE ~ species)) %>%
  arrange(species) %>%
  ggplot(aes(reorder(species, count), count)) +
  geom_col(aes(fill = status)) +
  coord_flip() +
  labs(x = "",
       y = "Pairs / territories",
       caption = "Sources: RSPB (breeding population estimates);\nBTO Birds of Conservation Concern 5th Review (red-list status)",
       title = "Estimated breeding pairs/ territories for common British birds",
       subtitle = "Bars coloured by current red-list status") +
  scale_y_continuous() +
  scale_y_log10(labels = scales::comma, n.breaks = 5) +
  scale_fill_manual(values = c("goldenrod", "darkgreen", "red"), name = "Red-list\nstatus") +
  theme_minimal() +
  theme(plot.title.position = "plot",
        plot.title = element_text(face = "bold", size = 32,),
        plot.subtitle = element_text(size = 20),
        axis.text = element_text(size = 20),
        axis.title = element_text(size = 24),
        legend.text = element_text(size = 16),
        legend.title = element_text(size = 20),
        plot.caption = element_text(size = 16),
        plot.caption.position = "panel")

