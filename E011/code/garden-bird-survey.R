library(tidyverse)
library(lubridate)
library(geomtextpath)

## source: https://www.bto.org/our-science/projects/gbw/results/long-term-patterns
## process
## go to web page above and choose species
## drag slider under chart to show full range of data
## charting tech is highcharts
## data is stored in web page so we need to extract it
## right click on chart and choose inspect
## find <g class = highcharts-series highcharts-series-0
## click black arrow to expand
## this should show data
## right click and select copy element
## past into a text-editor
## select just the data (starts with 0 and ends with 215)
## open a spreadsheet, copy data and paste into first cell
## click data> text to columns > delimimted and choose other L as separator = press OK
## select data and paste special > transpose into a new sheet
## run data > text to columns again. this time choose space as separator
## tidy the data into 2 columns and label columns as `date` and `rate`
## save sheet as a csv (e.g. greenfinch.csv)
## this script takes the data and rescales it  - you need to go back to the chart on the web
## and identify max and min values
## add dates

s_thrush %>%
  ggplot(aes(date ,-rate)) +
  geom_line()

minst <- 3.2
range_old <- 53.3 - 3.2
range_new <- max(st$rate) - min(st$rate)

scale = range_new / range_old

(max(st$rate) - min(st$rate))/scale +3.2

min(st$rate) / scale


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


### nuthatch

nhmin <- 8.4
nhmax <- 26.1

nuthatch %>%
  slice(-c(1)) %>%
  # ggplot() +
  # geom_histogram(aes(rate))
  mutate(r = (max(rate) -rate)/scale +  nhmin,
         series = row_number(),
         days = (series - 1) * 30,
         date1 = min_date + days(days)) %>%
  ggplot(aes(date1, r)) +
  geom_point(pch = 1) +
  geom_line(colour = "grey") +
  geom_smooth(method = "gam") +
  labs(title = "Average weekly recording rate",
       subtitle = "Nuthatch",
       y = "Recording rate") +
  scale_x_date(breaks = "year") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


## grt spotted woodpecker

gswmin <- 6.6
gswmax <- 33.4
tmin
gbfs_gsw %>%
  rename(date = data) %>%
  slice(-c(1)) %>%
  # ggplot() +
  # geom_histogram(aes(rate))
  mutate(r = (max(rate) -rate)/scale +  nhmin,
         series = row_number(),
         days = (series - 1) * 30,
         date1 = min_date + days(days)) %>%
  ggplot(aes(date1, r)) +
  geom_point(pch = 1) +
  geom_line(colour = "grey") +
  geom_smooth(method = "gam") +
  labs(title = "Average weekly recording rate",
       subtitle = "Great spotted woodpecker",
       y = "Recording rate") +
  scale_x_date(breaks = "year") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

### blue tit

btmin <- 83.3
btmax <- 97

gbfs_blue_tit %>%
  #rename(date = data) %>%
  #slice(-c(1)) %>%
  # ggplot() +
  # geom_histogram(aes(rate))
  mutate(r = (max(rate) -rate)/scale +  btmin,
         series = row_number(),
         days = (series - 1) * 30,
         date1 = min_date + days(days))

%>%
  ggplot(aes(date1, r)) +
  geom_point(pch = 1) +
  geom_line(colour = "grey") +
  geom_smooth(method = "gam") +
  labs(title = "Average weekly recording rate",
       subtitle = "Blue Tit",
       y = "Recording rate") +
  scale_x_date(breaks = "year") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


### house sparrow

hsmin <- 56.9
hsmax <- 88.6

gbfs_house_sparrow %>%
  #rename(date = data) %>%
  slice(-c(1)) %>%
  # ggplot() +
  # geom_histogram(aes(rate))
  mutate(r = (max(rate) -rate)/scale +  nhmin,
         series = row_number(),
         days = (series - 1) * 30,
         date1 = min_date + days(days)) %>%
  ggplot(aes(date1, r)) +
  geom_point(pch = 1) +
  geom_line(colour = "grey") +
  geom_smooth(method = "gam") +
  labs(title = "Average weekly recording rate",
       subtitle = "House sparrow",
       y = "Recording rate") +
  scale_x_date(breaks = "year") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#### all

h <- here::here("E011/data")

f <- list.files(h, "csv", full.names = TRUE)

gb <-  f[c(5, 13, 14, 15, 16, 17, 32)]

import_fn <- function(x){
  require(tidyverse)
  x <- read_csv(x, show_col_types = F) %>%
    mutate(sp = basename(x),
           sp = str_remove(sp, ".csv"))

  x
}

cfmin <- 35.8
cfmax <- 85.8

min_max <- data.frame(

  sp = c("cf", "gbfs_blue_tit", "gbfs_gsw", "gdf", "gf", "st", "gbfs_house_sparrow"),
  min = c(31.5, 83.3, 6.6, 6.9, 23.3, 3.2, 56.9),
  max = c(85.8, 97, 33.4, 71.2, 82.7, 53.3, 88.6)


)

map_dfr(gb, import_fn) %>%
  left_join(min_max) %>%
  group_by(sp) %>%

  mutate(maxr = max(rate),
         z = maxr - rate,
         a = z/scale,
         b = a + min,
         sp = case_when(str_detect(sp, "blue") ~ "Blue tit",
                        str_detect(sp, "house") ~ "House sparrow",
                        str_detect(sp, "cf") ~ "Chaffinch",
                        str_detect(sp, "gf") ~ "Greenfinch",
                        str_detect(sp, "gsw") ~ "Great spotted woodpecker",
                        str_detect(sp, "st") ~ "Song thrush",
                        str_detect(sp, "gdf") ~ "Goldfinch")) %>%
  mutate(date1 = rep(c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep","Oct", "Nov", "Dec"),
                       length.out = 326),
         year = rep(1995:2022, each = 12, length.out = 326),
         date2 = paste(year = year, month = date1, day = "01", sep = "-"),
         date2 = lubridate::ymd(date2)) %>%

  ggplot(aes(date2, b, group = sp)) +
  stat_smooth(
    aes(label = sp),
    geom = "textpath",
    color = "red",
    linecolor = "navyblue"
  ) +
  labs(
    title = "Smoothed trend in proportion of birdfeeders visited",
    y = "% birdfeeders",
    x = "Year",
    caption = "Source: BTO Garden Bird Feeding Survey"

  ) +
  theme(
        panel.background = element_rect("#EDF2BD"),
        legend.position = "",
        panel.grid.minor = element_blank()
    ) +

  scale_x_date(breaks = "2 years") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title.position = "plot")
