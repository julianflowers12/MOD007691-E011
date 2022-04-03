## jncc conservation designations https://hub.jncc.gov.uk/assets/478f7160-967b-4366-acdf-8941fd33850b

library(readxl); library(skimr)

jncc_file <- tempfile()

curl::curl_download("https://data.jncc.gov.uk/data/478f7160-967b-4366-acdf-8941fd33850b/conservation-designations-20220202.zip", jncc_file)

unzip(jncc_file, exdir = "jncc")

path <- here::here("jncc/conservation-designations-20220202")

xlsx <- list.files(path, "xlsx", full.names = T)

x <- excel_sheets(xlsx[1])

master <- read_excel(xlsx[1], sheet = 2, skip = 1)

glimpse(master)

taxon_summary <- read_excel(xlsx[1], sheet = 3, skip = 4)

glimpse(taxon_summary)

taxon <- read_excel(xlsx[1], sheet = 4, skip = 2)

skimr::skim(master)

skimr::skim(taxon_summary)

master %>%
  select(-c(`Source description`, `designation description`)) %>%
  #rename(Class = 1) %>%
  DT::datatable(filter = "top",
                editable = TRUE,
                extensions = c('Buttons', 'Select'),
                options = list(pageLength = 25,
                  select = list(style = 'os', items = 'row'),
                  dom = 'Bfrtip',
                  buttons = c('copy', 'csv', 'excel', 'pdf', 'print', 'selectRows')
                  ))
