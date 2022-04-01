h <- here::here("E011/data")

f <- list.files(h, "csv", full.names = TRUE)
gb <-  f[c(5, 13:15, 17:18, 22, 36, 38)]
gb


import_fn <- function(x){
  require(tidyverse)
  x <- read_csv(x, show_col_types = F) %>%
    mutate(sp = basename(x),
           sp = str_remove(sp, ".csv"))

  x
}


df1 <- purrr::map_df(gb, import_fn)

df1 %>%
  write_rds("E011/data/gb.rds")

