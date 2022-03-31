library(sf); library(tidyverse); library(gt); library(ggrepel); library(geomtextpath)
# if needed install.packages("remotes")
remotes::install_github("jthomasmock/gtExtras")

library(gtExtras)

path <- here::here("E011/data")

list_files <- list.files(path, "csv", full.names = T)

bto_data <- map_dfr(list_files[c(1:4, 5:8, 12:24, 26:34)], read_csv)

bto_data %>%
  count(spcode) %>%
  View()


g <- bto_data %>%
  filter(year > 1994) %>%
  group_by(spcode) %>%
  select(year, sm, spcode) %>%
  fill(sm, .direction = "up") %>%
  arrange(spcode, desc(year)) %>%
  mutate(trend = sm/sm[25] * 100) %>%
  mutate(sc = ifelse(spcode %in% c("grefi", "starl", "housp"), "red",
                     ifelse(spcode %in% c("swift",
                                          "houma", "dunno",
                                          "sonth"), "amber", "green" )))

end <- g %>%
  group_by(spcode) %>%
  filter(year == max(year),
         sc !=  "neutral")

g %>%
  filter(spcode %in% c("bluti", "chaff", "goldf", "grefi", "grswo", "housp", "sonth", "starl", "dunno")) %>%
  mutate(spcode = case_when(spcode == "bluti" ~ "Blue Tit",
                            spcode == "chaff" ~ "Chaffinch",
                            spcode == "housp" ~ "House Sparrow",
                            spcode == "grefi" ~ "Greenfinch",
                            spcode == "sonth" ~ "Song Thrush",
                            spcode == "goldf" ~ "Goldfinch",
                            spcode == "grswo" ~ "Gt Sp Woodpecker",
                            spcode == "starl" ~ "Starling",
                            spcode == "dunno" ~ "Dunnock"

                            )) %>%
  ggplot(aes(year, trend, group = spcode, colour = sc)) +
  stat_smooth(aes(label = spcode), show.legend = FALSE,
            geom = "textpath",
            hjust = 1.05,
            vjust = 0.5) +
  #geom_text_repel(aes(label = spcode, year, trend), data = end, nudge_x = 2, show.legend = FALSE) +
  geom_hline(yintercept = 100) +
  theme(panel.background = element_rect(fill = "#EDF2BD")) +
        #panel.grid = element_blank()) +
  scale_color_manual(values  = c("goldenrod", "darkgreen", "red")) +
  labs(title = "Relative change in selected species abundance since 1995",
       subtitle = "Smoothed data",
       y = "Relative change",
       x = "Year",
       caption = "Baseline year 1995: 100\nSource: BTO Trends") +
  theme(plot.title.position = "plot",
        plot.title = element_text(size = 16, face = "bold"),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14))


pluck(bto_data, "spcode") %>% unique()

redlist <- readRDS("~/Dropbox/Mac (2)/Desktop/MOD007691-E011_1/redlist1.rds")

get_image <- function(search, size){
  require(magick)
  require(rphylopic)

  ## get uid

  im <- name_search(search, options = "namebankID")[1,]
  id <- name_images(uuid = im$uid[1])
  uid <- id$same[[1]]$uid

  img <- paste0("http://phylopic.org/assets/images/submissions/", uid, ".", size, ".png") %>%
    image_read()

  img

}

common <- pluck(redlist, "species") %>%
  unique() %>%
  .[c(59, 136, 164, 166, 174, 195, 196, 199, 209, 216, 226, 235, 218, 234, 176, 160, 144, 28, 178, 201)]

rl_common <- redlist %>%
  filter(species %in% common) %>%
  mutate(spcode = case_when(str_detect(species, "Blue") ~ "bluti",
                            str_detect(species, "Carrion") ~ "carcr",
                            str_detect(species, "Coal") ~ "coati",
                            str_detect(species, "Collar") ~ "coldo",
                            str_detect(species, "Dunnoc") ~ "dunno",
                            str_detect(species, "Gold") ~ "goldf",
                            str_detect(species, "Great") ~ "greti",
                            str_detect(species, "Spotted") ~ "grswo",
                            str_detect(species, "Sparrow") ~ "housp",
                            str_detect(species, "Jack") ~ "jackd",
                            str_detect(species, "Magpi") ~ "magpi",
                            str_detect(species, "Robin") ~ "robin",
                            str_detect(species, "Song") ~ "sonth",
                            str_detect(species, "Starl") ~ "starl",
                            str_detect(species, "Pigeon") ~ "woodp",
                            str_detect(species, "Wren") ~ "wren.",
                            str_detect(species, "Green") ~ "grefi",
                            str_detect(species, "Long-tailed") ~ "lotti",
                            str_detect(species, "Blackbird") ~ "blabi",
                            str_detect(species, "Chaffinch") ~ "chaff")
         )


rl_common %>%
  pluck("species") %>%
  unique()


common_birds_data <- bto_data %>%
  left_join(rl_common) %>%
  #left_join(rl, by = "species") %>%
  dplyr::select(year, sm, sm_ll85, sm_ul85, species, spcode, iucn, redlist, vals, sp2, mean_2021, rank_2021, mean_2020)

pluck(common_birds_data, "spcode") %>%
  unique()

bto_data_1 <- common_birds_data %>%
  distinct() %>%
  pivot_wider(names_from = "redlist.x", values_from = "vals.x") %>%
  janitor::remove_empty()

 bto_data_2 <- bto_data_1 %>%
  filter(year > 1994) %>%
  group_by(spcode) %>%
  add_count() %>%
  ungroup() %>%
  group_by(sp2) %>%
  arrange(sp2, desc(year)) %>%
  mutate(reverse = 100 * sm/sm[25],
         rev_lci = 100 * sm_ll85/sm_ll85[25],
         rev_uci = 100 * sm_ul85/sm_ul85[25],
         change = reverse[1] - 100)

 bto_data_2 %>%
   filter(year == max(year)) %>%
   select(year, reverse, species, spcode, sp2)

 library(ggrepel)

 sp <- bto_data_2 %>%
   group_by(sp2) %>%
   filter(year == max(year))

bto_data_2 %>%
   drop_na() %>%
   ggplot() +
   geom_line(aes(year, reverse, colour = sp2)) +
   geom_text_repel(aes(label = sp2, year, sm), data = sp, max.overlaps = 20,
                    size = 3)
   theme_light()

 bto_data_2 %>%
   select(change) %>%
   distinct()

bto_data_2 %>%
  drop_na() %>%
  ggplot(aes(year, reverse)) +
  geom_line() +
  geom_ribbon(aes(ymin = rev_lci,
                  ymax = rev_uci),
              fill = "grey",
              alpha = 0.6) +
  geom_hline(yintercept = 100, colour= "red", lty = "dotted") +
  theme(panel.grid = element_blank(),
        panel.background = element_rect(fill = NULL)) +
  facet_wrap(~sp2)



bto_data_2 %>%
  drop_na() %>%
  #slice(1:20) %>%
  group_by(Species = sp2) %>%
  arrange(rank_2021, year) %>%
  mutate(Species = str_replace_all(Species, "_", " ")) %>%
  summarise(`Trend \n1995 - 2019` = list(reverse), .groups = "drop",
            `% change in population \n1995 to 2021` = mean(change),
            `1996` = unique(`1996`),
            `2002` = unique(`2002`),
            `2009` = unique(`2009`),
            `2015` = unique(`2015`),
            `2021` = unique(`2021`),
            IUCN = unique(iucn.x),
            rank = mean(rank_2021)
            ) |>
 # unnest(cols = "Trend \n1995 - 2019")
  drop_na() %>%
  arrange(rank) %>%
  select(-c(rank, IUCN)) %>%
  gt() %>%
  gt_theme_nytimes() %>%
  gt_plt_bar(`% change in population \n1995 to 2021`, scale_type = "number", text_color = "white", color = "blue") %>%
  tab_style(
    style = cell_text(color = "red", weight = "bold"),
    locations = cells_body(
      columns = vars(`1996`),
      rows = `1996` == "R")) %>%

  tab_style(
    style = cell_text(color = "darkgreen", weight = "bold"),
    locations = cells_body(
      columns = vars(`1996`),
      rows = `1996` == "G")) %>%
  tab_style(
    style = cell_text(color = "goldenrod", weight = "bold"),
    locations = cells_body(
      columns = vars(`1996`),
      rows = `1996` == "A")) %>%
  tab_style(
    style = cell_text(color = "red", weight = "bold"),
    locations = cells_body(
      columns = vars(`2002`),
      rows = `2002` == "R")) %>%

  tab_style(
    style = cell_text(color = "darkgreen", weight = "bold"),
    locations = cells_body(
      columns = vars(`2002`),
      rows = `2002` == "G")) %>%
  tab_style(
    style = cell_text(color = "goldenrod", weight = "bold"),
    locations = cells_body(
      columns = vars(`2002`),
      rows = `2002` == "A")) %>%
  tab_style(
    style = cell_text(color = "red", weight = "bold"),
    locations = cells_body(
      columns = vars(`2009`),
      rows = `2009` == "R")) %>%

  tab_style(
    style = cell_text(color = "darkgreen", weight = "bold"),
    locations = cells_body(
      columns = vars(`2009`),
      rows = `2009` == "G")) %>%
  tab_style(
    style = cell_text(color = "goldenrod", weight = "bold"),
    locations = cells_body(
      columns = vars(`2009`),
      rows = `2009` == "A")) %>%
  tab_style(
    style = cell_text(color = "red", weight = "bold"),
    locations = cells_body(
      columns = vars(`2015`),
      rows = `2015` == "R")) %>%

  tab_style(
    style = cell_text(color = "darkgreen", weight = "bold"),
    locations = cells_body(
      columns = vars(`2015`),
      rows = `2015` == "G")) %>%
  tab_style(
    style = cell_text(color = "goldenrod", weight = "bold"),
    locations = cells_body(
      columns = vars(`2015`),
      rows = `2015` == "A")) %>%
  tab_style(
    style = cell_text(color = "red", weight = "bold"),
    locations = cells_body(
      columns = vars(`2021`),
      rows = `2021` == "R")) %>%

  tab_style(
    style = cell_text(color = "darkgreen", weight = "bold"),
    locations = cells_body(
      columns = vars(`2021`),
      rows = `2021` == "G")) %>%
  tab_style(
    style = cell_text(color = "goldenrod", weight = "bold"),
    locations = cells_body(
      columns = vars(`2021`),
      rows = `2021` == "A")) %>%
  tab_spanner(
    label = "Assessment year",
    columns = vars(`1996`, `2002`, `2009`, `2015`, `2021`)
  ) %>%
  gt_sparkline(`Trend \n1995 - 2019`, type = "sparkline", same_limit = FALSE, label = TRUE) %>%
  gt::tab_source_note("Sources: https://app.bto.org/birdtrends/species.jsp?year=2020, \nStanbury, A., Eaton, M., Aebischer, N., Balmer, D., Brown, A., Douse, A., Lindley, P., McCulloch, N., Noble, D. and Ilka, W., 2021. Data from: The fifth review of Birds of Conservation Concern in the United Kingdom, Channel Islands and Isle of Man and second IUCN Red List assessment of extinction risk of birds for Great Britain. Available at: <http://datadryad.org/stash/dataset/doi:10.5061/dryad.cc2fqz672> [Accessed 6 Jan. 2022]. S")

bf <- get_image(search = "Dunnock", 64)
bf
