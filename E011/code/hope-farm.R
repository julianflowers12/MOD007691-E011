## hope farm
library(rvest)
library(tidyverse)

hf <- "https://www.rspb.org.uk/about-the-rspb/about-us/media-centre/press-releases/hope-farm/"

t2 <- read_html(hf) %>%
  html_table() %>%
  .[2]

colnames(t2[[1]]) <- t2[[1]][1,]

t2 %>%
  data.frame() %>%
  slice(-1) %>%
  mutate_at(.vars = 2:7, as.numeric) %>%
  janitor::clean_names() %>%
  group_by(species = var_1) %>%
  summarise(before = mean(dec_2000:feb_2001),
            post = mean(dec_2019:feb_2020, na.rm = TRUE),
            change = post - before) %>%
  mutate(sign = ifelse(change > 0, 1, 0)) %>%
  filter(species != "Woodpigeon") %>%
 # pivot_longer(names_to = "period",  values_to = "values", cols = 2:3) %>%
  ggplot(aes(reorder(species, change), change, fill = factor(sign))) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  labs(x = "",
       y = "Change in counts between winter 1999:2000 and winter 2019:2020",
       title = "Success of Hope Farm project for may species...",
       caption = "Source: https://www.rspb.org.uk/globalassets/downloads/documents/conservation--sustainability/hope-farm/hope-farm-annual-review-2019.pdf",
       subtitle = "but greenfinch numbers continue to decline") +
  theme(plot.title.position = "plot") +
  scale_fill_manual(values = c("red", "darkgreen"))

