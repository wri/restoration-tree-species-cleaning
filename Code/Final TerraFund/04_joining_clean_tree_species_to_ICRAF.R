# Description -------------------------------------------------------------

# Author: Ben Steiger
# Date Created: 10/28/2024
# Last Updated: 11/08/2024
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

# Load data ---------------------------------------------------------------

# GlobUNT data - all countries in TerraMatch
globunt_5_june <- read.table(
  file =  here(
    "Tree Species",
    "Data",
    "Raw",
    "Globunt",
    "GlobUNT species information 5 June 2024.txt"
  ),
  header = TRUE,
  sep = "|",
  fill = TRUE,
  quote = "",
  na.strings = '""'
)

globunt_14_may <-
  read.table(
    file =  here(
      "Tree Species",
      "Data",
      "Raw",
      "Globunt",
      "GlobUNT species information 14 May 2024.txt"
    ),
    header = TRUE,
    sep = "|",
    fill = TRUE,
    quote = "",
    na.strings = '""'
  )

# Matched project data
matched_data <- read_csv(
  file = here(
    "Tree Species",
    "Data",
    "Processed",
    "TerraFund Tree Species",
    "02_matched_terrafund_tree_species.csv"
  )
)

# countries
countries <- read_csv(
  file = here(
    "Unified Database",
    "Data",
    "All",
    "Raw",
    "Country Cleaning",
    "TerraMatch - Country Codes.csv"
  )
)

# Drop the NA data --------------------------------------------------------

matched_data <- matched_data %>%
  filter(!is.na(tree_species_uuid))

# convert columns to snakecase -----------------------------------------------

names(globunt_14_may) <-
  to_snake_case(names(globunt_14_may))

names(globunt_5_june) <-
  to_snake_case(names(globunt_5_june))

# rbind globunt exports ---------------------------------------------------

# filter out duplicates in globunt_5_june - Madagascar and India

globunt_5_june <- globunt_5_june %>%
  filter(!country %in% globunt_14_may$country)

# rbind

globunt_all <-
  rbind(globunt_14_may,
        globunt_5_june) %>%
  arrange(country,
          species)


# edit country names in GlobUNT -------------------------------------------

globunt_all <- globunt_all %>%
  mutate(country = 
           case_when(
             country == "Congo, The Democratic Republic of the" ~ "Congo, Democratic Republic of the",
             country == "Cape Verde" ~ "Cabo Verde",
             TRUE ~ country
           ))

# add countries -----------------------------------------------------------

matched_data <-
  left_join(
    matched_data,
    countries,
    by = "country_code"
  )

# group globunt by country ------------------------------------------------

globunt_grouped <- globunt_all %>%
  group_by(sid_x) %>%
  mutate(globunt_country_list = paste(country, collapse = ";")) %>%
  slice(1) %>%
  ungroup() %>%
  select(-country, -family)

# edits for matching ------------------------------------------------------

globunt_grouped <- globunt_grouped %>%
  mutate(
    species =
      case_when(
        species == "Olea europaea" ~ "Olea europaea subsp. cuspidata",
        species == "Manihot carthagenensis" ~ "Manihot carthaginensis subsp. glaziovii",
        species == "Faidherbia albida" ~ "Acacia albida",
        species == "Cupressus lusitanica" ~ "Hesperocyparis lusitanica",
        species == "Citrus medica" ~ "Citrus aurantiifolia",
        species == "Eucalyptus globulus" ~ "Eucalyptus maidenii",
        species == "Actinidia chinensis" ~ "Actinidia chinensis var. deliciosa",
        species == "Archidendron clypearia" ~ "Archidendron clypearia subsp. clypearia",
        species == "Cinnamomum iners" ~ "Cinnamomum verum",
        species == "Terminalia glaucescens" ~ "Terminalia schimperiana",
        species == "Toona ciliata" ~ "Toona hexandra",
        species == "Vitex fischeri" ~ "Vitex fischeri var. keniensis",
        TRUE ~ species
      )
  )

# join taxon id to SID.x -------------------------------------------------

# successful join
matched_data_globunt <-
  inner_join(matched_data,
            globunt_grouped,
            by = c("taxon_id" = "sid_x"),
            #unmatched = "drop",
            keep = TRUE) %>%
  arrange(project_name, scientific_name) 

# join remaining by species ---------------------------------------

matched_data_globunt_2 <-
  inner_join(matched_data,
            globunt_grouped,
            by = c("scientific_name" = "species"),
            keep = TRUE) %>%
  filter(!is.na(af),!taxon_id %in% matched_data_globunt$taxon_id)

# join by scientific_final -----------------------------------------------------------

#matched_data_globunt_3 <-
#  left_join(matched_data,
#            globunt_grouped,
#            by = c("scientific_name" = "scientific_final")) %>%
#  filter(!is.na(af),!taxon_id %in% matched_data_globunt$taxon_id,
#         !taxon_id %in% matched_data_globunt_2$taxon_id) 

# bind results ------------------------------------------------------------

matched_data_globunt_all <-
  rbind(matched_data_globunt,
        matched_data_globunt_2)

# remaining data ----------------------------------------------------------

matched_data_remain <- matched_data %>%
  filter(!taxon_id %in% matched_data_globunt_all$taxon_id)

matched_data_remain %>%
  distinct(scientific_name) %>%
  arrange(scientific_name) %>%
  view()

# select columns for saving matched data ----------------------------------

matched_data_globunt_all <- matched_data_globunt_all %>%
  select(
    framework,
    tree_species_uuid,
    project_uuid,
    project_name,
    organisation_name,
    country_name,
    tree_species_name,
    amount,
    tree_species_name_clean,
    taxon_id,
    scientific_name_id,
    scientific_name,
    family,
    genus,
    specific_epithet,
    scientific_final,
    sid_x,
    old_name,
    geographic_area,
    lifeform_description,
    climate_description,
    red_list,
    af:globunt_country_list
  ) %>%
  arrange(project_name, scientific_name)

# case_when statement for nativity ----------------------------------------

matched_data_globunt_all <- matched_data_globunt_all %>%
  mutate(nativity = 
           case_when(
             str_detect(globunt_country_list, fixed(country_name)) ~ "native",
             !str_detect(globunt_country_list, fixed(country_name)) ~ "non-native",
             TRUE ~ NA_character_
           ))

# complete projects -------------------------------------------------------

complete_projects <- matched_data_globunt_all %>%
  filter(!project_name %in% matched_data_remain$project_name) %>%
  arrange(project_name, scientific_name)

# Completeness statistics -------------------------------------------------

species_info_counts <- matched_data %>%
  summarize(sub_family_only = sum(is.na(family) & is.na(genus) & is.na(specific_epithet)),
            family_only = sum(!is.na(family) & is.na(genus) & is.na(specific_epithet)),
            genus_only = sum(is.na(family) & !is.na(genus) & is.na(specific_epithet)),
            family_genus_only = sum(!is.na(family) & !is.na(genus) & is.na(specific_epithet)),
            family_genus_species = sum(!is.na(family) & !is.na(genus) & !is.na(specific_epithet)),
            total = nrow(matched_data)) 

#matched_data %>%
#  filter(is.na(family) & !is.na(genus) & is.na(specific_epithet)) %>%
#  distinct(genus)

# export data -------------------------------------------------------

timestamp <- format(Sys.time(), "%Y-%m-%d_%H-%M-%S")

write_excel_csv(matched_data_globunt_all,
                file = here(
                  "Tree Species",
                  "Data",
                  "Processed",
                  "Matched Data",
                  "All Match",
                  "CSV",
                  paste0("terrafund_matched_data_globunt_all_final_", timestamp, ".csv")
                ))

write_excel_csv(
  complete_projects,
  file = here(
    "Tree Species",
    "Data",
    "Processed",
    "Matched Data",
    "All Match",
    "CSV",
    paste0("terrafund_complete_project_data_globunt_", timestamp, ".csv")
  )
)

write_excel_csv(
  matched_data_remain,
  file = here(
    "Tree Species",
    "Data",
    "Processed",
    "Unmatched Data",
    "CSV",
    paste0("terrafund_cleaned_project_tree_species_no_globunt_", timestamp, ".csv")
  )
)

