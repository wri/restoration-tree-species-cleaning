
library(dplyr)
library(tidyverse)
library(here)

# Load project data ---------------------------------------------------------------

clean_project_data <-
  read_csv(
    file = here(
      "Tree Species",
      "Data",
      "Processed",
      "Matched Data",
      "All Match",
      "CSV",
      "final_cleaned_terrafund_project_tree_species_2024-11-08_15-07-24.csv"
    )
  )

cleaned_establishment_data <- 
  read_csv(
    file = here(
      "Tree Species",
      "Data",
      "Processed",
      "TerraFund Tree Species",
      "02_matched_terrafund_tree_species_project_establishment_2024-12-12_12-08-33.csv"
    )
  )

raw_project_data <- read.csv(
  here(
    "Tree Species",
    "Data",
    "Raw",
    "TerraFund Tree Species",
    "terrafund_tree_species_202410301559.csv"
  ),
  encoding = "UTF-8"
)

# terrafund project establishment

raw_establishment_data <- read.csv(
  here(
    "Tree Species",
    "Data",
    "Raw",
    "TerraFund Tree Species",
    "terrafund_tree_species_project_establisment_202412121125.csv"
  ),
  encoding = "UTF-8"
)

names(clean_project_data)
names(cleaned_establishment_data)

# subset columns ----------------------------------------------------------

cleaned_establishment_data_sub <- cleaned_establishment_data %>%
  rename(original_tree_species_name = tree_species_name) %>%
  select(project_uuid, tree_species_uuid, taxon_id, scientific_name, original_tree_species_name, family, genus, specific_epithet, old_name)

clean_project_data_sub <- clean_project_data %>%
  select(project_uuid, tree_species_uuid, taxon_id, scientific_name, original_tree_species_name, family, genus, specific_epithet, old_name)

all_clean_data <- 
  rbind(clean_project_data_sub,
        cleaned_establishment_data_sub)

# counts ------------------------------------------------------------------

all_clean_data %>%
  distinct(project_uuid) %>%
  count()

all_clean_data %>%
  mutate(original_tree_species_name = tolower(original_tree_species_name)) %>%
  distinct(original_tree_species_name) %>%
  count()

all_clean_data %>%
  filter(!is.na(taxon_id)) %>%
  distinct(scientific_name) %>%
  count()

clean_project_data_sub %>%
  distinct(specific_epithet) %>%
  count()

clean_project_data_sub %>%
  filter(is.na(specific_epithet))

distinct_scientific_names <- all_clean_data %>%
  distinct(scientific_name, original_tree_species_name) %>%
  view()

all_clean_data %>%
  distinct(genus, specific_epithet) %>%
  arrange(genus) %>%
  view()

clean_project_data %>%
  filter(site_report_due_date == "2024-07-30",
         framework == "terrafund",
         country_code == "KE") %>%
  view()


# arcos -------------------------------------------------------------------

arcos <- cleaned_establishment_data %>%
  filter(project_uuid == "3a2099dd-9a85-4091-b0c7-5edd11acbcd6") %>%
  select(organisation_name, country_code, tree_species_name, scientific_name, taxon_id)

# remaining IDs to check on -----------------------------------------------

# d878b7f8-75a1-467d-bd3c-6c86582cce3f
# 84ec4a75-fdce-47c8-9429-592c8f07de37
# d30ddfde-47d1-4287-aa33-5a71b1de582c (change to gossypium hirsitum)

# list of species to further classify:
# gossypium = gossypium hirsitum
# Gliricidia = Gliricidia sepium
# Leucaena = Leucaena leucocephala
# Mammea = mammea bongo
# balanites?
# beguea = beguea apetela
# cocos = cocos nucifera?
