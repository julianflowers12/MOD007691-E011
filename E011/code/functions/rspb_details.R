## get species details from RSPB web pages


get_details <- function(url = "https://www.rspb.org.uk/birds-and-wildlife/wildlife-guides/bird-a-z/", species){
  require(rvest); required(tidyverse)
  paste0(url, species) %>%
    read_html() %>%
    html_nodes(".species-measurements-population__details-content") %>%
    html_text() %>%
    str_squish() %>%
    str_split(., "        ") %>%
    enframe() %>%
    unnest("value") %>%
    mutate(species = species)

}
