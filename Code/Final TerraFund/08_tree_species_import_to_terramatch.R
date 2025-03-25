# Description -------------------------------------------------------------

# Author: Ben Steiger
# Date Created: 11/01/2024
# Last Updated: 11/08/2024
# Description: Subsetting Cleaned Tree Species Data for Import to TM

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
      "final_cleaned_terrafund_project_tree_species_2024-11-08_15-07-24.csv"
    )
  )

# subset columns ----------------------------------------------------------

tree_species_import <- clean_project_data %>%
  select(organisation_uuid:specific_epithet)

clean_project_data %>%
  filter(framework == "terrafund-landscapes",
         country_code != "GH",
         !is.na(specific_epithet)) %>%
  group_by(nativity) %>%
  distinct(specific_epithet) %>%
  count()

tree_species_import %>%
  distinct(framework)

# export data -------------------------------------------------------------

write_excel_csv(
  tree_species_import,
  file = here(
    "Tree Species",
    "Data",
    "Processed",
    "Final Cleaned Tree Species to Import",
    "final_terrafund_tree_species_to_import_11_04.csv"
  )
)

