## garden birdwatch
library(readxl)

tmp <- tempfile()

curl::curl_download("https://www.rspb.org.uk/globalassets/downloads/biggardenbirdwatch/2021/results/full-2021-results-for-web.xlsx", tmp)

bbs <- tempfile()

curl::curl_download("https://www.bto.org/sites/default/files/bbs_report_2020_-_england_population_trends_1995-2019_final.xlsx", bbs)

bbs <- readxl::read_excel(bbs, sheet = 2)

bbs %>%
  View()

gbb <- read_excel(tmp)

gbb_top_20 <- top_n(gbb, 21, wt = `Mean 2021`) %>%
  janitor::clean_names()

gbb_top_20 %>%
  pluck("species")

gbb_top_20 <- gbb_top_20 %>%
  mutate(species = case_when(species == "Woodpigeon" ~ "Wood_pigeon",
                             species == "Starling" ~ "Common_starling",
                             species == "Chaffinch" ~ "Common_chaffinch",
                             #species == "Carrion_crow" ~ "Carrion/Hooded_crow",
                             species == "Great_spotted_woodpecker" ~ "Great_spotted Woodpecker",
                             TRUE ~ species))

gbb_top_20 %>%
  pluck("species")

redlist <- readRDS("~/Dropbox/Mac (2)/Desktop/MOD007691-E011_1/redlist1.rds")

redlist <- redlist %>%
  mutate(species = case_when(str_detect(species, "Carrion") ~ "Carrion crow",
                             TRUE ~ species))

rl <- redlist %>%
  mutate(sp1 = str_remove(species, word(species, -1)),
         sp2 = str_remove(sp1, word(species, -2)),
         sp2 = str_squish(sp2),
         sp2 = str_to_lower(sp2),
         sp2 = str_replace(sp2, " ", "_"),
         sp2 = str_replace(sp2, "-", "_"),
         sp2 = str_to_title(sp2),
         sp2 = case_when(str_detect(sp2, "Carrion") ~ "Carrion_crow",
                         TRUE ~ sp2)) %>%
  left_join(gbb_top_20, by = c("sp2" = "species")) %>%
  filter(!is.na(rank_2021))




rl %>%
  pluck("species") %>%
  unique()
