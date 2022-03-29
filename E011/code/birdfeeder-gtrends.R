### google trends

library(pacman)
p_load(gtrendsR, zoo)

bw <- gtrendsR::gtrends(c("bird feeder"), geo = "GB", time = "2015-01-01 2022-03-01")

g <- bw$interest_over_time %>%
  mutate(rm = zoo::rollmean(hits, k = 3, align = "center", na.pad = TRUE)) %>%
  ggplot(aes(date, rm)) +
  geom_line(lty = "dotted") +
  geom_point(pch = 1) +
  geom_smooth(method = "gam", se = FALSE) +
  geom_vline(xintercept = as.POSIXct("2020-03-16"), colour = "red") +
  theme_minimal() +
  theme(panel.background = element_rect(fill = "palegreen"),
        plot.title = element_text(face = "bold", size = 16)) +
  annotate("text", label = "Start of 1st COVID19 lockdown", x = as.POSIXct("2018-06-01"), y = 100) +
  labs(title = "Google searches for 'bird feeder' in UK since 2015",
       x = "Date",
       y = "Searches",
       caption = "Source: https://trends.google.com/trends/?geo=GB\nGAM smooth fitted") +
  scale_y_continuous(position = "right")

ggsave(g, paste0(here::here(), "/E011/images/bird-feeder.png"), device = "png")

bw$interest_by_city

gtrendsR::categories

here::here()
