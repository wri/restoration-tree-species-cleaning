# Description -------------------------------------------------------------

# Author: Ben Steiger
# Date Created: 11/01/2024
# Last Updated: 11/01/2024
# Description: Joining Matched TerraFund Project Report Tree Species Data to Invasive Species Datasets & Binding All Data

# Load libraries ----------------------------------------------------------

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
      "final_cleaned_terrafund_project_tree_species_10_30.csv"
    )
  )

# subset columns ----------------------------------------------------------

tree_species_import <- clean_project_data %>%
  select(organisation_uuid:specific_epithet)


# export data -------------------------------------------------------------

write_excel_csv(
  tree_species_import,
  file = here(
    "Tree Species",
    "Data",
    "Processed",
    "Final Cleaned Tree Species to Import",
    "final_terrafund_tree_species_to_import_11_01.csv"
  )
)


clean_project_data %>%
  filter(is.na(af),
         str_detect(scientific_name, " ")) %>%
  distinct(scientific_name, old_name) %>%
  arrange(scientific_name) %>%
  view()
