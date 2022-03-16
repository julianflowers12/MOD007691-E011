library(sf); library(tidyverse); library(gt)
# if needed install.packages("remotes")
remotes::install_github("jthomasmock/gtExtras")

library(gtExtras)

path <- here::here("E011/data")

list_files <- list.files(path, "csv", full.names = T)

bto_data <- map_dfr(list_files[c(1:5, 9:17, 19:24)], read_csv)

redlist <- readRDS("~/Dropbox/Mac (2)/Desktop/MOD007691-E011_1/redlist1.rds")

common <- pluck(redlist, "species") %>%
  unique() %>%
  .[c(32, 59, 136, 164, 166, 174, 195, 199, 209, 216, 226, 235, 230, 218, 234, 175, 176, 160, 144, 28)]

rl_common <- redlist %>%
  filter(species %in% common) %>%
  mutate(spcode = case_when(str_detect(species, "Blue") ~ "bluti",
                            str_detect(species, "Bull") ~ "bullf",
                            str_detect(species, "Carrion") ~ "carcr",
                            str_detect(species, "Coal") ~ "coati",
                            str_detect(species, "Collar") ~ "coldo",
                            str_detect(species, "Dunnoc") ~ "dunno",
                            str_detect(species, "Gold") ~ "goldf",
                            str_detect(species, "Great") ~ "greti",
                            str_detect(species, "Spotted") ~ "grswo",
                            str_detect(species, "Sparown") ~ "housp",
                            str_detect(species, "Martin") ~ "houma",
                            str_detect(species, "Jack") ~ "jackd",
                            str_detect(species, "Magpi") ~ "magpi",
                            str_detect(species, "Robin") ~ "robin",
                            str_detect(species, "sSong") ~ "sonth",
                            str_detect(species, "Starl") ~ "starl",
                            str_detect(species, "Swift") ~ "swift",
                            str_detect(species, "Pigeon") ~ "woodp",
                            str_detect(species, "Wren") ~ "wren.",
                            str_detect(species, "Green") ~ "grefi"))

pluck(bto_data, "spcode") %>%
  unique()

bto_data_1 <- bto_data %>%
  group_by(year) %>%
  add_count() %>%
  filter(year > 1994) %>%
  ungroup() %>%
  group_by(spcode) %>%
  arrange(spcode, desc(year)) %>%
  mutate(reverse = 100 * sm/sm[25],
         rev_lci = 100 * sm_ll85/sm_ll85[25],
         rev_uci = 100 * sm_ul85/sm_ul85[25])

bto_data_1 <- bto_data_1 %>%
  left_join(rl_common) %>%
  pivot_wider(names_from = "redlist", values_from = "vals") %>%
  janitor::remove_empty()


%>%
  ggplot(aes(year, reverse)) +
  geom_line() +
  geom_ribbon(aes(ymin = rev_lci,
                  ymax = rev_uci),
                  fill = "grey",
                  alpha = 0.6) +
  facet_wrap(~spcode, scales = "free")


bto_data_1 %>%
  group_by(year) %>%
  add_count() %>%
  filter(year > 1994) %>%
  ungroup() %>%
  group_by(spcode) %>%
  fill(sm, .direction = "down") %>%
  arrange(spcode, desc(year)) %>%
  mutate(reverse = 100 * sm/sm[25],
         rev_lci = 100 * sm_ll85/sm_ll85[25],
         rev_uci = 100 * sm_ul85/sm_ul85[25]) %>%
  arrange(year) %>%
  summarise(data = list(reverse), .groups = "drop") %>%
  gt() %>%
  gt_theme_guardian() %>%
  gt_sparkline(data, type = "sparkline", same_limit = FALSE, label = TRUE)
