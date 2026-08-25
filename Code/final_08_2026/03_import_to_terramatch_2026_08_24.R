# Description -------------------------------------------------------------

# Author: Ben Steiger
# Date Created: 11/01/2024
# Last Updated: 08/15/2025
# Description: Subsetting Cleaned Tree Species Data for Import to TM

# Load libraries ----------------------------------------------------------

library(dplyr)
library(tidyverse)
library(here)

# Load data ---------------------------------------------------------------

# Terrafund site report data

data_cleaned <-
  read_csv(
    file = here(
      "Tree Species",
      "Data",
      "Processed",
      "Projects",
      "02_matched_terrafund_tree_species_site_reports_2025-09-26_09-42-26.csv"
    )
  ) 

# subset data -------------------------------------------------------------

tree_species_import <- data_cleaned %>%
  filter(!is.na(tree_species_uuid)) %>%
  select(tree_species_uuid, taxon_id) %>%
  rename(uuid = tree_species_uuid) 

# drop data ----------------------------------------------------------

tree_species_import <- tree_species_import %>%
  filter(uuid != "2c3749c2-301e-427c-942b-0502f6921a6a")


# export data -------------------------------------------------------------

timestamp <- format(Sys.time(), "%Y-%m-%d_%H-%M-%S")

# Cleaned TerraFund site report data
write_excel_csv(tree_species_import,
                file = here(
                  "Tree Species",
                  "Data",
                  "Processed",
                  "Projects",
                  "Final",
                  paste0(
                    "cleaned_data_for_import_",
                    timestamp,
                    ".csv"
                  )
                ))