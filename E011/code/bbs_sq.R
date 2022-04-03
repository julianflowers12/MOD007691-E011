##########################
#
# Script for charting E011
#
# Draws charts used in MOD007691
#
###########################


############################
# Uses a range of BTO data
# plus data from https://datadryad.org/stash/dataset/doi:10.5061/dryad.hdr7sqvfh
# Supplementary data for
# https://besjournals.onlinelibrary.wiley.com/doi/10.1111/1365-2664.13703
############################

############################
#
# libraries
#
############################

library(pacman)
p_load(tidyverse, readxl, curl, janitor, sf, ggspatial)

############################
#
# Get BTO species codes/ dominant habitats
#
############################

bto_sp <-read_csv("https://datadryad.org/stash/downloads/file_stream/353903", show_col_types = F)


### birds pers bbs square - wide table

birds_per_site <- read_csv("https://datadryad.org/stash/downloads/file_stream/353904" show_col_types = F)

bps_l <- birds_per_site %>%
  pivot_longer(names_to = "species",
               values_to = "count",
               cols = 2:ncol(.)) %>%
  mutate(bbs = str_remove(sid, "BBS_"))

gf <- bps_l %>%
  filter(species == "GREFI")

### urban metrics for each bbs square

urban_metrics <- read_csv("https://datadryad.org/stash/downloads/file_stream/353906", show_col_types = F)

### code book for urban metrics

code_book <- read_delim("https://datadryad.org/stash/downloads/file_stream/353905", "\t")

code_book %>%
  View()

### bbs squares

bbs_squares <- urban_metrics %>%
  select(sid:northing) |>
  mutate(bbs = str_remove(sid, "BBS_"))


### model params

tmp <- tempfile()
curl::curl_download("https://datadryad.org/stash/downloads/file_stream/353908", tmp)
mod <- readxl::read_excel(tmp, skip = 2)

### unoccupied square by species

bps_l %>%
  group_by(species) %>%
  mutate(zero_count = ifelse(count == 0, 1, 0)) %>%
  count(zero_count) %>%
  summarise(percent_zero = round(1 - n[1]/482, 2)) %>%
  arrange(percent_zero) %>%
  left_join(bto_sp) %>%
  gt::gt()




### map bbs squares

 %>%
  st_as_sf(coords = c(x = "northing", y = "easting"), crs = 27700) %>%
  ggplot() +
  geom_sf() +
  coord_sf() +
  theme_void()








