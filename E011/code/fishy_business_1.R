### fishy business

path <- "C:\\Users\\Julian\\OneDrive\\Desktop\\fishy_data"


files <- list.files(path, "csv", full.names = TRUE)


flow <- read_csv(files[3]) %>%
  janitor::clean_names() %>%
  slice(-c(6627:6626)) %>%
  select(-s)

tail(flow)

mutate(flow, species = str_extract(commodity_name, "^[:upper:].+"))

flow <- data.table::setDT(flow)

flow[, tot := sum(x2019), by = .(commodity_name, trade_flow_name)][order(-tot, -x2019)][tot > 10000]
