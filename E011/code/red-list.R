## bocc 5

library(readtext); library(pdftools);library(tabulizer); library(rJava); library(tidytext);library(tidyverse)
library(readxl)

jncc <- "https://hub.jncc.gov.uk/assets/478f7160-967b-4366-acdf-8941fd33850b"

red_list <- tempfile()
red_list_jncc <- curl::curl_download("https://data.jncc.gov.uk/data/478f7160-967b-4366-acdf-8941fd33850b/consolidated-red-list-extract-20220202.xlsx", red_list)

red_list_jncc <- red_list_jncc %>%
  read_excel(sheet = 2) %>%
  janitor::clean_names()


red_list_jncc %>%
  #glimpse()
  filter(!is.na(bto_red_data_categories), str_detect(common_name, "^St"))
%>%
  select(recommended_taxon_name, common_name, england_red_list) %>%
  arrange(common_name)




library(tabulizer);library(rJava)
pdf <- pdftools::pdf_text("~/Downloads/bocc5.pdf")

table <- pdf[10:17] %>%
  str_split(., "\\n")

t <- table[1] %>%
  tibble() %>%
  unnest(".") %>%
  slice(-c(1:34)) %>%
  slice(-c(13:17)) %>%
  mutate(string = str_squish(.)) %>%
  select(string) %>%
  mutate(species = word(string, start = 1, end = 5),
         species = str_split(species, " ")) %>%
  unnest("species") %>%
  filter(nchar(species) > 1)


t <- t %>%
  group_by(string) %>%
  summarise(species = paste(species, collapse = " ")) %>%
  mutate(s1 = str_remove(string, species),
         iucn = str_extract(string, "EN|VU|LC|NT|CR|NE"),
         cat = word(s1, start = 1, end = 6)) %>%
  separate(cat, c("A", "B", "C", "D", "E", "F"), " ") %>%
  select(species, B:F, iucn) %>%
  mutate(F = str_extract(F, "[A-Z]"))


t1 <- table[2] %>%
  tibble() %>%
  unnest(".") %>%
  slice(-c(1:6)) %>%
  slice(-c(9:13, 40:41)) %>%
  mutate(string = str_squish(.)) %>%
  select(string) %>%
  mutate(species = word(string, start = 1, end = 5),
         species = str_split(species, " ")) %>%
  unnest("species") %>%
  filter(nchar(species) > 1)

t1 <- t1 %>%
  group_by(string) %>%
  summarise(species = paste(species, collapse = " ")) %>%
  mutate(s1 = str_remove(string, species),
         iucn = str_extract(string, "EN|VU|LC|NT|CR|NE"),
         cat = word(s1, start = 1, end = 6)) %>%
  separate(cat, c("A", "B", "C", "D", "E", "F"), " ") %>%
  select(species, B:F, iucn) %>%
  mutate(F = str_extract(F, "[A-Z]"))

t2 <- table[3] %>%
  tibble() %>%
  unnest(".") %>%
  slice(-c(1:6)) %>%
  slice(-c(1, 23:27, 29, 41)) %>%
  mutate(string = str_squish(.)) %>%
  select(string) %>%
  mutate(species = word(string, start = 1, end = 5),
         species = str_split(species, " ")) %>%
  unnest("species") %>%
  filter(nchar(species) > 1)

t2 <- t2 %>%
  group_by(string) %>%
  summarise(species = paste(species, collapse = " ")) %>%
  mutate(s1 = str_remove(string, species),
         iucn = str_extract(string, "EN|VU|LC|NT|CR|NE"),
         cat = word(s1, start = 1, end = 6)) %>%
  separate(cat, c("A", "B", "C", "D", "E", "F"), " ") %>%
  select(species, B:F, iucn) %>%
  mutate(F = str_extract(F, "[A-Z]"))

t3 <- table[4] %>%
  tibble() %>%
  unnest(".") %>%
  slice(-c(1:6)) %>%
  slice(-c(3, 9:13, 41)) %>%
  mutate(string = str_squish(.)) %>%
  select(string) %>%
  mutate(species = word(string, start = 1, end = 5),
         species = str_split(species, " ")) %>%
  unnest("species") %>%
  filter(nchar(species) > 1)

t3 <- t3 %>%
  group_by(string) %>%
  mutate(species = str_remove(species, "\\("),
         species = str_remove(species, "\\)")) %>%
  summarise(species = paste(species, collapse = " ")) %>%
  mutate(s1 = str_remove(string, species),
         iucn = str_extract(string, "EN|VU|LC|NT|CR|NE"),
         cat = word(s1, start = 1, end = 6)) %>%
  separate(cat, c("A", "B", "C", "D", "E", "F"), " ") %>%
  select(species, B:F, iucn) %>%
  mutate(F = str_extract(F, "[A-Z]"))



t4 <- table[5] %>%
  tibble() %>%
  unnest(".") %>%
  slice(-c(1:6)) %>%
  slice(-c(1, 23:27, 41)) %>%
  mutate(string = str_squish(.)) %>%
  select(string) %>%
  mutate(species = word(string, start = 1, end = 5),
         species = str_split(species, " ")) %>%
  unnest("species") %>%
  filter(nchar(species) > 1)

t4 <- t4 %>%
  group_by(string) %>%
  mutate(species = str_remove(species, "\\("),
         species = str_remove(species, "\\)")) %>%
  summarise(species = paste(species, collapse = " ")) %>%
  mutate(s1 = str_remove(string, species),
         iucn = str_extract(string, "EN|VU|LC|NT|CR|NE"),

         cat = word(s1, start = 1, end = 6)) %>%
  separate(cat, c("A", "B", "C", "D", "E", "F"), " ") %>%
  select(species, B:F, iucn) %>%
  mutate(F = str_extract(F, "[A-Z]"))


t5 <- table[6] %>%
  tibble() %>%
  unnest(".") %>%
  slice(-c(1:6)) %>%
  slice(-c(9:13, 40:41)) %>%
  mutate(string = str_squish(.)) %>%
  select(string) %>%
  mutate(species = word(string, start = 1, end = 5),
         species = str_split(species, " ")) %>%
  unnest("species") %>%
  filter(nchar(species) > 1)

t5 <- t5 %>%
  group_by(string) %>%
  mutate(species = str_remove(species, "\\("),
         species = str_remove(species, "\\)")) %>%
  summarise(species = paste(species, collapse = " ")) %>%
  mutate(s1 = str_remove(string, species),
         iucn = str_extract(string, "EN|VU|LC|NT|CR|NE"),

         cat = word(s1, start = 1, end = 6)) %>%
  separate(cat, c("A", "B", "C", "D", "E", "F"), " ") %>%
  select(species, B:F, iucn) %>%
  mutate(F = str_extract(F, "[A-Z]"))

t6 <- table[7] %>%
  tibble() %>%
  unnest(".") %>%
  slice(-c(1:7)) %>%
  slice(-c(22:26, 40)) %>%
  mutate(string = str_squish(.)) %>%
  select(string) %>%
  mutate(species = word(string, start = 1, end = 5),
         species = str_split(species, " ")) %>%
  unnest("species") %>%
  filter(nchar(species) > 1)

t6 <- t6 %>%
  group_by(string) %>%
  mutate(species = str_remove(species, "\\("),
         species = str_remove(species, "\\)")) %>%
  summarise(species = paste(species, collapse = " ")) %>%
  mutate(s1 = str_remove(string, species),
         iucn = str_extract(string, "EN|VU|LC|NT|CR|NE"),

         cat = word(s1, start = 1, end = 6)) %>%
  separate(cat, c("A", "B", "C", "D", "E", "F"), " ") %>%
  select(species, B:F, iucn) %>%
  mutate(F = str_extract(F, "[A-Z]"))

t7 <- table[8] %>%
  tibble() %>%
  unnest(".") %>%
  slice(-c(1:6)) %>%
  slice(c(1:8, 14:23)) %>%
  mutate(string = str_squish(.)) %>%
  select(string) %>%
  mutate(species = word(string, start = 1, end = 5),
         species = str_split(species, " ")) %>%
  unnest("species") %>%
  filter(nchar(species) > 1)

t7 <- t7 %>%
  group_by(string) %>%
  mutate(species = str_remove(species, "\\("),
         species = str_remove(species, "\\)")) %>%
  summarise(species = paste(species, collapse = " ")) %>%
  mutate(s1 = str_remove(string, species),
         iucn = str_extract(string, "EN|VU|LC|NT|CR|NE"),

         cat = word(s1, start = 1, end = 6)) %>%
  separate(cat, c("A", "B", "C", "D", "E", "F"), " ") %>%
  select(species, B:F, iucn) %>%
  mutate(F = str_extract(F, "[A-Z]"))


combined <- bind_rows(t, t1, t2, t3, t4, t5, t6, t7 )

combined %>%
  View()

table1 <- table %>%
  flatten() %>%
  tibble() %>%
  unnest(".") %>%
  filter(nchar(.) >1) %>%
  slice(-c(1:8)) %>%
  mutate(species = str_extract(., "\\w{3,5}"))

table1 %>%
  filter(!str_detect(., "British")) %>%
  janitor::remove_empty() %>%
  mutate(species = str_extract(., "\\w{3,5}"),
         cat = str_extract(., "^A|R|G\\s.+A|R|G\\s.+A|R|G\\s.+A|R|G$")
         ) %>%
  unnest("cat") %>%
  View()
%>%
  enframe() %>%
  unnest("value")

%>%
  str_squish()


survey_data <- read_csv(paste0(here("MOD007045"), "/data/Cherry Hinton CP/records-2022-02-17.csv"), show_col_types = FALSE) %>%
  janitor::clean_names()

survey_full <- survey_data %>%
  left_join(red_list_jncc, by = c("species_id_tvk" = "recommended_taxon_version_key"))

pdf <- "https://britishbirds.co.uk/sites/default/files/BB_Dec21-BoCC5-IUCN2.pdf"

pdftools::pdf_text(pdf) %>%
  map(., str_squish)

pdftools::pdf_data(pdf)

rt <- readtext(pdf)

bocc_list <- rt$text %>%
  str_split(., "\n")

bocc_words <- bocc_list[[1]][560:903] %>%
  str_squish() %>%
  data.frame() %>%
  janitor::clean_names() %>%
  janitor::remove_empty("rows") %>%
  unnest_tokens(word, x, "words")

bocc_words %>%
  #count(word, sort = TRUE) %>%
  filter(!str_detect(word, "\\d.*")) %>%
  #anti_join(stop_words) %>%
  #head(40)
  filter(!word %in% c("hdrec", "bdmp1", "br", "lc", "n", "vu", "en", "nt", "a2b", "cr", "d", "a4b", "bdp1", "ii",
                      "erlob", "bdp2", "wr", "bl", "wi", "wl", "ne", "3c", "w", "e", "hd", "vuo", "cont", "iucn", "british",
                      "birds", "december", "eno", "populations", "our", "bird", "the", "status", "of", "bi", "and",
                      "stanbury", "table", "et", "al", "mto", "nto", "iv")) %>%
  mutate(len = ifelse(nchar(word) == 1, "iucn", "sp")) %>%
  group_by(len) %>%
  mutate(id1 = ifelse(len == "iucn", seq(from = 1, to = 4, by = 1),  0),
         id3 = ifelse(len == "sp", seq(from = 1, to = 4, by = 1),  0)) %>%
  ungroup() %>%
  mutate(id2 = lag(id1)) %>%
  mutate(len = ifelse(id2 == 4, "iucn", len),
         w = ifelse(id1 > 0 | id2 > 0, word, "") ) %>%
  drop_na() %>%
  select(!contains("id2")) %>%
  mutate(id1 = ifelse(nchar(w) > 1, 5, id1),
         word = ifelse(word == w, NA, word)) %>%
  View()




  %>%
  fill(word) %>%
  filter(id1 > 3) %>%
  mutate(w1 = lag(w)) %>%
  filter(w == "g", w1 == "red") %>%
  left_join(bocc_words) %>%
  View()

%>%
  filter(id1 == 4 & w == "g", id1 == 5 & w == "red")
  head(20)



%>%



  pivot_wider(names_from = "len", values_from =  "w", ) %>%
  unnest_auto("word")




%>%
)
  View()
  mutate(l = ifelse(len == max(id), 0, 1))

%>%
  group_by(len) %>%
  ungroup() %>%
  mutate(id1 = lag(word)) %>%
  mutate(l = paste(len, word))

  separate(x, remove = T, c("common", "scientific", "1", "2", "3", "4", "5", "a", "b", "c", "d", "e"), sep = " ")


%>%
  str_squish()
  head() %>%
  dim()
  enframe() %>%
  mutate(value = str_squish(value))
