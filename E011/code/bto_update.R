## bocc 5 results from https://zenodo.org/record/5739368#.Yk71CxPMI6E

library(readxl)


t1 <- tempfile()

curl::curl_download("https://zenodo.org/record/5739368/files/SOM_Table_1_UK_BoCC5_species_results.xlsx?download=1", t1)

bocc5 <- read_excel(t1) %>%
  janitor::clean_names()

t2 <- tempfile()

curl::curl_download("https://zenodo.org/record/5739368/files/SOM_Table_3_GB_IUCN2_species_results.xlsx?download=1", t2)

bocc5_1 <- read_excel(t2) %>%
  janitor::clean_names()


skimr::skim(bocc5)

skimr::skim(bocc5_1)



### habitat
bocc5 %>%
  select(habitat = breeding_habitat_based_on_gibbons_et_al_1993_eaton_et_al_2015,
         bocc5 = bo_cc5_assessment_breeding_seabird_assessment_were_taken_from_eaton_et_al_2015) %>%
  filter(habitat != "not assessed") %>%
  filter(bocc5 != "Former breeder") %>%
  mutate(bocc = case_when(str_detect(bocc5, "Amber") ~ "amber",
                           str_detect(bocc5, "Green") ~ "green",
                           str_detect(bocc5, "Red") ~ "red"
                           )) %>%
    group_by(habitat, bocc) %>%
    summarise(nn = n(),
              sum = sum(nn)) %>%
  mutate(bocc = fct_relevel(bocc, c("red", "amber", "green"))) %>%
  ggplot(aes(reorder(habitat, sum), nn, fill = bocc, text = nn)) +
  geom_col(position = "fill", show.legend = FALSE) +
  geom_text(aes(label = nn), position = "fill", hjust = 2, colour = "white") +
  coord_flip() +
  scale_fill_manual(values = c("red", "goldenrod", "darkgreen"), name = "Assessment") +
  labs(x = "",
       y = "Number of species",
       title = "Farmland and upland birds are more likely to be red-listed",
       subtitle = "Number of species by habitat and listing in \n5th review of Birds of Conservation Concern",
       caption = "Source: https://zenodo.org/record/5739368#.Yk71CxPMI6E") +
  theme(panel.background = element_blank(),
        plot.title = element_text(size = 14, face = "bold"),
        plot.title.position = "plot")

## what bird where

bocc_combined <- bocc5 %>%
  left_join(bocc5_1) %>%
  select(species = publication_species_name,
         scientific_name = hbw_bird_life_international_species_scientific_name,
         abundance = number_of_mature_individuals_based_on_woodward_et_al_2020,
         habitat = breeding_habitat_based_on_gibbons_et_al_1993_eaton_et_al_2015,
         season = season_assessed_b_breeding_nb_non_breeding,
         #bocc5 = bo_cc5_assessment_breeding_seabird_assessment_were_taken_from_eaton_et_al_2015,
         iucn = global_iucn_01_01_2020_except_breeding_seabird_assessments_taken_from_bo_cc4,
         europe = european_status_bird_life_international_2021_except_breeding_seabird_assessments_which_were_taken_from_eaton_et_al_2015)


rl_common <- rl_common %>%
  mutate(ln = case_when(str_detect(ln, "coron") ~ "Corvus corone",
                        str_detect(ln, "monedula") ~ "Corvus monedula",
                        TRUE ~ ln))

common_birds <-bocc_combined %>%
  inner_join(rl_common, by = c("scientific_name" = "ln"))

b <- common_birds$scientific_name %>%
  unique()

a <- rl_common$ln %>%
  unique()

union(a, b)

cb <- common_birds %>%
  select(species = species.x, scientific_name, spcode, vals, season, habitat, abundance, iucn.y, redlist) %>%
  pivot_wider(names_from = "redlist", values_from = "vals") %>%
  arrange(-abundance) %>%
  left_join(bto_data, by = "spcode")

library(geomtextpath)

cb %>%
  filter(year > 1994, season == "B") %>%
  group_by(species) %>%
  mutate(delta = sm/sm[1]) %>%
  select(species, year, delta) %>%
  ggplot(aes(year, delta, group = species )) +
  stat_smooth(
    method = "gam",
    geom = "textpath",
    aes(label = species),
    hjust = 1



  ) +
scale_y_log10() +
scale_hjust_discrete()
  count(species)



write_csv(cb, "common_birds.csv")
