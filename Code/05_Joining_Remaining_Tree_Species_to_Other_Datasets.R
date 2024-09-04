# Description -------------------------------------------------------------

# Author: Ben Steiger
# Date Created: 07/30/2024
# Last Updated: 08/28/2024
# Description: Joining Matched TerraFund Project Report Tree Species Data to GlobUNT

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
library(janitor)
library(sf)

# Load data ---------------------------------------------------------------

# gts

gts <-
  read_csv(file = here(
    "Tree Species",
    "Data",
    "Raw",
    "GTS",
    "global_tree_search_trees_1_8.csv"
  ))

# wcvp

wcvp_names <-
  read.csv(
    file = here("Tree Species", "Data", "Raw", "wcvp", "wcvp_names.csv"),
    sep = "|"
  )

# remaining data

project_data <-
  read_csv(
    file = here(
      "Tree Species",
      "Data",
      "Processed",
      "Unmatched Data",
      "CSV",
      "cleaned_project_tree_species_no_globunt.csv"
    )
  )

# manual WCUPS entry

wcups <-
  read_csv(here(
    "Tree Species",
    "Data",
    "Raw",
    "WCUPS",
    "manual_wcups_species_data.csv"
  ))

iucn <-
  read_csv(
    here(
      "Tree Species",
      "Data",
      "Raw",
      "IUCN",
      "redlist_species_data_7d081f5f-1377-4d24-858a-8bf3f6ffbad0",
      "assessments.csv"
    )
  )

# convert columns to snakecase -----------------------------------------------

names(project_data) <-
  to_snake_case(names(project_data))

names(gts) <- to_snake_case(names(gts))

names(wcvp_names) <- to_snake_case(names(wcvp_names))

names(iucn) <- to_snake_case(names(iucn))

# correct accents ---------------------------------------------------------

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
project_data <- project_data %>%
  mutate(across(where(is.character), correct_accents))

# filter project data to data in gts --------------------------------------

project_data_gts <- project_data %>%
  filter(scientific_name %in% gts$taxon_name |
           old_name %in% gts$taxon_name) %>%
  distinct(scientific_name)

# this tells us the species we can download for country distribution

# load gts country distribution data --------------------------------------

# list files
csv_files <- list.files(path = "Tree Species/Data/Raw/GTS/species_distributions",
                        pattern = "*.csv",
                        full.names = TRUE)

# read all files
csv_list <- lapply(csv_files, read.csv)

# bind files
gts_country_distributions <- do.call(rbind, csv_list)

# subset data ---------------------------------------------------------

gts <- gts %>%
  select(taxon_name)

gts_country_distributions <- gts_country_distributions %>%
  select(taxon, native_distribution)

wcvp_names <- wcvp_names %>%
  select(
    plant_name_id,
    taxon_name,
    geographic_area,
    climate_description,
    lifeform_description
  )

iucn <- iucn %>%
  select(scientific_name, redlist_category)

project_data <- project_data %>%
  select(
    project_name,
    site_name,
    project_country,
    amount,
    site_report_due_date,
    tree_species_uuid,
    taxon_id,
    scientific_name_id,
    scientific_name,
    family,
    genus,
    specific_epithet,
    old_name
  )

# change terminalia glaucescens to Terminalia schimperiana ----------------

# gts
gts_country_distributions <- gts_country_distributions %>%
  mutate(taxon = case_when(
    taxon == "Terminalia glaucescens" ~ "Terminalia schimperiana",
    TRUE ~ taxon
  ))

# iucn
iucn <- iucn %>%
  mutate(
    scientific_name = case_when(
      scientific_name == "Terminalia glaucescens" ~ "Terminalia schimperiana",
      TRUE ~ scientific_name
    )
  )

# edit wcups scientific names ---------------------------------------------

# Replace all non-standard spaces with regular spaces
wcups <- wcups %>%
  mutate(scientific_name = str_replace_all(scientific_name, "\\s+", " "))

# change all data to character

wcups <- wcups %>%
  mutate_all(as.character)

# join datasets -----------------------------------------------------------

join_project_data <- project_data %>%
  left_join(gts_country_distributions, by = c("scientific_name" = "taxon")) %>%
  left_join(wcvp_names,
            by = c("scientific_name" = "taxon_name"),
            keep = TRUE) %>%
  left_join(wcups, by = c("taxon_name" = "scientific_name")) %>%
  left_join(iucn, by = c("scientific_name" = "scientific_name"))

# duplicates --------------------------------------------------------------

# get duplicates
dupes <- join_project_data %>%
  get_dupes(tree_species_uuid)

# duplicate data from wcvp
dupes_to_drop <- dupes %>%
  filter(lifeform_description == "" &
           climate_description == "" & geographic_area == "")

# drop using plant_name_id from wcvp

join_project_data <- join_project_data %>%
  filter(!plant_name_id %in% dupes_to_drop$plant_name_id)

# edit countries to match project country ---------------------------------

join_project_data <- join_project_data %>%
  mutate(
    native_distribution = str_replace_all(
      native_distribution,
      "\\bThe Democratic Republic of the\\b",
      "Democratic Republic of the"
    )
  )

# case_when statement for nativity ----------------------------------------

join_project_data <- join_project_data %>%
  mutate(nativity =
           case_when(
             str_detect(native_distribution, fixed(project_country)) ~ "Native",!str_detect(native_distribution, fixed(project_country)) ~ "Non-Native",
             TRUE ~ NA_character_
           ))

# replace all "" with NA --------------------------------------------------

join_project_data <- join_project_data %>%
  mutate(across(everything(), ~ na_if(., "")))

# save data ---------------------------------------------------------------

write_excel_csv(
  join_project_data,
  file = here(
    "Tree Species",
    "Data",
    "Processed",
    "Matched Data",
    "All Match",
    "CSV",
    "new_project_data_match.csv"
  )
)
