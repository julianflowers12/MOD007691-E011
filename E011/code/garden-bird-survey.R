library(tidyverse)
library(lubridate)

s_thrush %>%
  ggplot(aes(date ,-rate)) +
  geom_line()

minst <- 3.2
range_old <- 53.3 - 3.2
range_new <- max(st$rate) - min(st$rate)

scale = range_new / range_old

(max(st$rate) - min(st$rate))/scale +3.2

min(st$rate) / scale

scale <- r_st/ra

new_min = min(st$rate)/scale

min_date <- as.Date("1995-01-01")
d2 = min_date +days(7)

st %>%
  slice(-c(1)) %>%
  # ggplot() +
  # geom_histogram(aes(rate))
  mutate(r = (max(rate) -rate)/scale +  minst,
         series = row_number(),
         days = (series - 1) * 30,
         date1 = min_date + days(days)) %>%
  ggplot(aes(date1, r)) +
  geom_point(pch = 1) +
  geom_line(colour = "grey") +
  geom_smooth(method = "gam") +
  labs(title = "Average weekly recording rate",
       subtitle = "Song thrush",
       y = "Recording rate") +
  scale_x_date(breaks = "year") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


### gf

gfmax<- 82.7
gfmin <- 23.3

range_old_gf <- gfmax - gfmin
range_new_gf <- max(gf$rate) - min(gf$rate)

scale_gf = range_new_gf / range_old_gf

gf %>%
  slice(-c(1)) %>%
  # ggplot() +
  # geom_histogram(aes(rate))
  mutate(r = (max(rate) -rate)/scale +  gfmin,
         series = row_number(),
         days = (series - 1) * 30,
         date1 = min_date + days(days)) %>%
  ggplot(aes(date1, r)) +
  geom_point(pch = 1) +
  geom_line(colour = "grey") +
  geom_smooth(method = "gam") +
  labs(title = "Average weekly recording rate",
       subtitle = "Greenfinch",
       y = "Recording rate") +
  scale_x_date(breaks = "year") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#### chaffinch

cfmin <- 35.8
cfmax <- 85.8

cf %>%
  slice(-c(1)) %>%
  # ggplot() +
  # geom_histogram(aes(rate))
  mutate(r = (max(rate) -rate)/scale +  cfmin,
         series = row_number(),
         days = (series - 1) * 30,
         date1 = min_date + days(days)) %>%
  ggplot(aes(date1, r)) +
  geom_point(pch = 1) +
  geom_line(colour = "grey") +
  geom_smooth(method = "gam") +
  labs(title = "Average weekly recording rate",
       subtitle = "Chaffinch",
       y = "Recording rate") +
  scale_x_date(breaks = "year") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

### goldfinch

gdfmin <- 6.9
gdfmax <- 71.2

gdf %>%
  slice(-c(1)) %>%
  # ggplot() +
  # geom_histogram(aes(rate))
  mutate(r = (max(rate) -rate)/scale +  gdfmin,
         series = row_number(),
         days = (series - 1) * 30,
         date1 = min_date + days(days)) %>%
  ggplot(aes(date1, r)) +
  geom_point(pch = 1) +
  geom_line(colour = "grey") +
  geom_smooth(method = "gam") +
  labs(title = "Average weekly recording rate",
       subtitle = "Goldfinch",
       y = "Recording rate") +
  scale_x_date(breaks = "year") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
