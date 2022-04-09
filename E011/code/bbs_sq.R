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
p_load(tidyverse, readxl, curl, janitor, sf, ggspatial, corrplot, ggbeeswarm)

############################
#
# Get BTO species codes/ dominant habitats
#
############################

bto_sp <-read_csv("https://datadryad.org/stash/downloads/file_stream/353903", show_col_types = F)


### birds pers bbs square - wide table

birds_per_site <- read_csv("https://datadryad.org/stash/downloads/file_stream/353904", show_col_types = F)

bps_l <- birds_per_site %>%
  pivot_longer(names_to = "species",
               values_to = "count",
               cols = 2:ncol(.)) %>%
  mutate(bbs = str_remove(sid, "BBS_"))

range <- bps_l %>%
  complete(bbs, species) %>%
  mutate(occ = ifelse(count > 0, 1, 0)) %>%
  group_by(species) %>%
  summarise(occ_count = sum(occ))

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

d <- here::here("E011/data")

f <- list.files(d, "shp", full.names = T)

onek <- st_read(f[[1]]) %>%
  right_join(bps_l, by = c("PLAN_NO" = "bbs"))

bound <- st_read(f[[3]])

sp_list <- pluck(onek, "species") %>%
  unique()

onek %>%
  #filter(species == "GREFI") %>%
  filter(species %in% c("STARL", "HOUSP", "CHAFF", "GREFI", "WOODP", "GOLDF",
                     "BLUTI", "GRETI", "SONTH", "JACKD", "WREN.", "BLABI")) %>%
  ggplot() +
  geom_sf(data = bound, fill = "white") +
  geom_sf(aes(colour = count), size = 1 ) +
  coord_sf() +
  viridis::scale_colour_viridis(option = "rocket", direction = -1) +
  theme_void() +
  annotation_north_arrow(location = "tr") +
  annotation_scale(location = "br") +
  facet_wrap(~species)



### urban predictors

bps_l %>%
  group_by(species) %>%
  summarise(index = mean(count))

birds_per_site %>%
  .[, -1] %>%
  cor() %>%
  corrplot(method = "square", tl.cex = .5, tl.col = "black", order = "hclust", outline = FALSE)

urban_predictors <- birds_per_site %>%
  left_join(urban_metrics)

pairs(urban_predictors[c(2:)])

up1 <- urban_predictors %>%
  pivot_longer(names_to = "species", values_to = "counts", cols = BLABI:SPARR)

up1 %>%
  glimpse()

  pivot_longer(names_to = "metrics", values_to = "values", cols = c(areakm2, pc_building:climate)) %>%
  select(-starts_with("xval"))

urban_metrics %>%
  .[, -c(1:7)] %>%
  cor() %>%
  corrplot(method = "square", tl.cex = .5, tl.col = "black", order = "hclust", outline = FALSE)

urban_predictors %>%
  select(ROBIN, WREN., BLUTI, GRETI, SONTH, BLABI, GRSWO, DUNNO, GREFI,
         WOODP, CHAFF, COATI, GOLDF, contains("garden")|contains("road")) %>%
  cor() %>%
  corrplot(method = "square", tl.cex = .5, tl.col = "black", order = "hclust", outline = FALSE)


up_summary <- up1 %>%
  group_by(species, metrics) %>%
  summarise(mean_count = mean(counts, na.rm = TRUE),
            mean_metrics = mean(values))


up_summary %>%
  filter(str_detect(metrics, "den_")) %>%
  ggplot(aes(mean_count, mean_metrics)) +
  geom_point() +
  facet_wrap(~metrics, scales = )

umap <- select_if(urban_predictors, is.numeric) %>%
  umap::umap(.)

library(dbscan)

clus <- umap$layout %>%
  data.frame() %>%
  #ggplot(aes(X1, X2)) +
  #geom_point()
  dbscan::hdbscan(., minPts = 25)

clustering_mean_values <- urban_predictors %>%
  bind_cols(cluster = clus$cluster) %>%
  group_by(cluster) %>%
  summarise(across(where(is.double), mean, na.rm = TRUE)) %>%
  mutate(across(2:ncol(.), scale)) %>%
  pivot_longer(names_to = "metrics", values_to = "values", cols = 2:ncol(.))


clustering_mean_values %>%
  mutate(cluster = factor(cluster),
    across(where(is.double), scale)) %>%
  ggplot(aes(factor(cluster), metrics, fill = values)) +
  geom_tile() +
  coord_fixed() +
  viridis::scale_fill_viridis(option = "turbo") +
  coord_flip() +
  scale_colour_manual(position = "top") +
  theme(axis.text.x  = element_text(angle = 45, hjust = 1, size = 6))

clustering_mean_values %>%
  filter(nchar(metrics) == 5) %>%
  ggplot(aes(values[,1], factor(cluster))) +
  geom_boxplot( aes(fill = factor(cluster)))

  tail()
