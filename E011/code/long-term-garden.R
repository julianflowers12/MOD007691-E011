library(tidyverse)

h <- here::here("E011/data")
txt <- list.files(h, "txt", full.names = T)
txt

parse_df <- function(txt){

  ltt_df <- read_delim(txt, delim = "L") %>%
  as.vector()

t(ltt_df) %>%
  enframe() %>%
  mutate(name = str_squish(name)) %>%
  separate(name, c("date", "rate"), "\\s") %>%
  mutate( date = parse_number(date),
          rate = parse_number(rate)) %>%
  select(-value) %>%
  mutate(sp = str_remove(basename(txt), ".txt"))

}

df2 <- map_df(txt[c(1:7, 9, 11:16)], parse_df)

##########################################################
h <- here::here("E011/data")

f <- list.files(h, "csv", full.names = TRUE)

gb <-  f[c(5, 13, 14, 15, 16, 17, 18, 33)]

import_fn <- function(x){
  require(tidyverse)
  x <- read_csv(x, show_col_types = F) %>%
    mutate(sp = basename(x),
           sp = str_remove(sp, ".csv"))

  x
}

min_max <- data.frame(

  sp = c("cf", "gbfs_blue_tit", "gbfs_gsw", "gdf", "gf", "sonth", "gbfs_house_sparrow", "grtti",
         "coati", "coldo", "jackd", "lotti", "magpi", "wren.", "starl", "rob1n", "blabi", "woodp", "dunno", "cacro"),
  min = c(31.5, 83.3, 6.6, 6.9, 23.3, 3.2, 56.9, 62.3, 17.5, 41.6, 15, 4.5, 43.4, 17, 31.1, 60.6, 64, 27, 58.2, 18.3),
  max = c(85.8, 97, 33.4, 71.2, 82.7, 55.3, 88.6, 86.1,  52.7, 81.2, 44.2, 51.5, 66.5, 52.7, 84, 93.1, 96.9, 93.1, 86.2, 36.5)


)

df1 <- map_dfr(gb, import_fn)

combined_df <- df1 %>%
  bind_rows(df2) %>%
  #filter(sp == "grtti")
  #count(sp)
  filter(!sp %in% c("gbfs_starling", "robin")) %>%
  select(date:sp) %>%
  left_join(min_max) %>%
  drop_na() %>%
  group_by(sp) %>%
  mutate(scale = (max(rate) - min(rate))/(max - min),
         maxr = max(rate),
         z = maxr - rate,
         a = z/scale,
         b = a + min,
         # sp = case_when(str_detect(sp, "blue") ~ "Blue tit",
         #                str_detect(sp, "house") ~ "House sparrow",
         #                str_detect(sp, "cf") ~ "Chaffinch",
         #                str_detect(sp, "gf") ~ "Greenfinch",
         #                str_detect(sp, "gsw") ~ "Great spotted woodpecker",
         #                str_detect(sp, "st") ~ "Song thrush",
         #                str_detect(sp, "gt") ~ "Great tit",
         #                str_detect(sp, "gdf") ~ "Goldfinch"),
  ) %>%
  add_count() %>%
  mutate(date1 = rep(c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep","Oct", "Nov", "Dec"),
                     length.out = 326),
         year = rep(1995:2022, each = 12, length.out = 326),
         date2 = paste(year = year, month = date1, day = "01", sep = "-"),
         date2 = lubridate::ymd(date2))

########################################################################

combined_df %>%
  ggplot(aes(date2, b)) +
  geom_point(pch = 1, size = .2) +
  geom_smooth(method = "gam") +
  facet_wrap(~sp)

combined_df %>%
  mutate(min_date = min(date2),
         max_date = max(date2)) %>%
  select(min_date, max_date)


birds_data <- combined_df %>%
  mutate(spcode = case_when(sp == "cf" ~ "chaff",
                            sp == "gbfs_blue_tit" ~ "bluti",
                            sp == "gbfs_gsw" ~ "grswo",
                            sp == "gbfs_house_sparrow" ~ "housp",
                            sp == "grtti" ~ "greti",
                            sp == "gdf" ~ "goldf",
                            sp == "gf" ~ "grefi",
                            sp == "rob1n" ~ "robin",
                            sp == "cacro" ~ "carcr",
                            TRUE ~ sp)) %>%
  left_join(common_birds)

birds_data %>%
  group_by(year, spcode) %>%
  summarise(mean_g_rate = mean(b),
            sm = mean(sm)) %>%
  filter(spcode != "sp") %>%
  ggplot(aes(group = spcode, x = year)) +
  geom_line(aes(y = mean_g_rate)) +
  geom_line(aes(y = sm / 3), colour = "red") +
  scale_y_continuous(name = "N",
                     sec.axis = sec_axis(~., "garden")) +
  facet_wrap(~spcode)

birds_data %>%
  select(spcode)
