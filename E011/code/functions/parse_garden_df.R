## parse delimited files from highchart data extract from long term garden bird trends


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
