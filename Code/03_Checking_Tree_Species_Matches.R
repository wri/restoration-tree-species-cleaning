# Description -------------------------------------------------------------

# Author: Ben Steiger
# Date Created: 06/10/2024
# Last Updated: 06/10/2024
# Description: Cleaning Unmatched TerraFund Project Report Tree Species Data

# Load libraries ----------------------------------------------------------

library(dplyr)
library(tidyverse)
library(stringr)
library(here)
library(snakecase)
library(WorldFlora)
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
    "all_unmatched_project_data_7_30.csv"
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
    "all_matched_project_data_7_30.csv"
  )
)

project_data_07_22 <- read_excel(
  here(
    "Tree Species",
    "Data",
    "Raw",
    "TerraFund Tree Species",
    "TerraFund Tree Species Export 2024-07-22.xlsx"
  )
)

# convert project_data_07_22 columns to snake_case ------------------------------

names(project_data_07_22) <-
  to_snake_case(names(project_data_07_22))

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
  group_by(name_orig, country_code) %>%
  slice(1) %>%
  ungroup() %>%
  select(
    tree_species_uuid,
    country_code,
    original_name,
    name,
    scientific_name,
    scientific_name_id,
    unique,
    fuzzy,
    fuzzy_dist,
    old_status,
    old_name
  ) %>%
  rename(clean_name = name) %>%
  arrange(scientific_name)

# now start sifting through the matches, filtering fuzzy joins

# species I am unsure of:

# tsimitetra
# le ma
# mol
# matembela
# molucata
# mil
# wild crippers
# gombo
# miombo
# tavia
# le ha
# gbaji
# local grasses
# mwanga
# sarimanga
# tavia
# umbrella invoresia
# bean

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
    unique,
    fuzzy,
    fuzzy_dist
  ) %>%
  arrange(scientific_name)


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
    "tree_species_matching_verification_07_30.xlsx"
  )
)
