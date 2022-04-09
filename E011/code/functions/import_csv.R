## imports csv and adds species column


import_fn <- function(x){
  require(tidyverse)
  x <- read_csv(x, show_col_types = F) %>%
    mutate(sp = basename(x),
           sp = str_remove(sp, ".csv"))

  x
}
