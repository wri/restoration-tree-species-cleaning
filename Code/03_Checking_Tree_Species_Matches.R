# Description -------------------------------------------------------------

# Author: Ben Steiger
# Date Created: 06/10/2024
# Last Updated: 07/30/2024
# Description: Verifying Matched TerraFund Project Report Tree Species Data

# Load libraries ----------------------------------------------------------

library(dplyr)
library(tidyverse)
library(stringr)
library(here)
library(snakecase)
library(readxl)
library(stringdist)
library(fuzzyjoin)
library(stringi)
library(writexl)

# Load data ---------------------------------------------------------------

# Remaining non-matches
unmatched_data <- read_csv(
  file = here(
    "Tree Species",
    "Data",
    "Processed",
    "Unmatched Data",
    "CSV",
    "all_unmatched_project_data_8_21.csv"
  )
)

# matched data for comparison
matched_data <- read_csv(
  file = here(
    "Tree Species",
    "Data",
    "Processed",
    "Matched Data",
    "All Match",
    "CSV",
    "all_matched_project_data_8_21.csv"
  )
)

project_data_07_22_raw <- read_excel(
  here(
    "Tree Species",
    "Data",
    "Raw",
    "TerraFund Tree Species",
    "TerraFund Tree Species Export 2024-07-22.xlsx"
  )
)

# convert project_data_07_22 columns to snake_case ------------------------------

names(project_data_07_22_raw) <-
  to_snake_case(names(project_data_07_22_raw))

# Correct accents ---------------------------------------------------------

# Define a function to correct accents
correct_accents <- function(text) {
  text %>%
    str_replace_all("Ô", "ï") %>%
    str_replace_all("È", "é") %>%
    str_replace_all("Ó", "î") %>%
    str_replace_all("Ë", "è") %>%
    str_replace_all("í", "'") %>%
    str_replace_all("ñ", "–") %>%
    str_replace_all("‡", "à") %>%
    str_replace_all("Í", "ê") %>%
    str_replace_all("Ò", "ñ") %>%
    str_replace_all("Ù", "ô")
}

# Apply the function to all character columns in the dataframe
project_data_07_22 <- project_data_07_22_raw %>%
  mutate(across(where(is.character), correct_accents))

# subset columns to tree_speices_uuid and species name --------------------

project_data_07_22 <- project_data_07_22 %>%
  select(tree_species_uuid, country_code, name) %>%
  rename(original_name = name) 

# convert to dataframe ----------------------------------------------------

project_data_07_22 <- as.data.frame(project_data_07_22)

# join matched and unmatched data to country data -------------------------

matched_data <-
  left_join(matched_data,
            project_data_07_22,
            by = "tree_species_uuid")

unmatched_data <-
  left_join(unmatched_data,
            project_data_07_22,
            by = "tree_species_uuid")

# group_by ----------------------------------------------------------------

check_matches <- matched_data %>%
  mutate(original_name = tolower(original_name)) %>%
  group_by(original_name, country_code) %>%
  slice(1) %>%
  ungroup() %>%
  select(
    tree_species_uuid,
    country_code,
    original_name,
    name,
    scientific_name,
    scientific_name_id,
    taxon_id,
    unique,
    fuzzy,
    fuzzy_dist,
    old_status,
    old_name
  ) %>%
  rename(clean_name = name) %>%
  mutate(notes = "") %>%
  arrange(scientific_name)

# now start sifting through the matches, filtering fuzzy joins

# explore fuzzy matches for accuracy -------------------------------------

fuzzy_matched_project_data <- matched_data %>%
  filter(fuzzy == TRUE) %>%
  group_by(name_orig) %>%
  slice(1) %>%
  ungroup() %>%
  select(
    tree_species_uuid,
    country_code,
    name_orig,
    name,
    scientific_name,
    scientific_name_id,
    taxon_id,
    unique,
    fuzzy,
    fuzzy_dist
  ) %>%
  arrange(scientific_name)

project_data_07_22 %>%
  group_by(`Project Name`) %>%
  count() %>%
  view()

# save data ---------------------------------------------------------------

write_xlsx(
  check_matches,
  path = here(
    "Tree Species",
    "Data",
    "Processed",
    "Matched Data",
    "All Match",
    "xlsx",
    "tree_species_matching_verification_08_21.xlsx"
  )
)

