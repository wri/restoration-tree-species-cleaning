# Description -------------------------------------------------------------

# Author: Ben Steiger
# Date Created: 07/30/2024
# Last Updated: 08/23/2024
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
    "Matched Data",
    "All Match",
    "CSV",
    "all_matched_project_data_8_21.csv"
  )
)

# Raw project data export 7/22
project_data_07_22 <- read_excel(
  here(
    "Tree Species",
    "Data",
    "Raw",
    "TerraFund Tree Species",
    "TerraFund Tree Species Export 2024-07-22.xlsx"
  )
)

# TM Country codes
country_codes <- read.csv(
  here(
    "Unified Database",
    "Data",
    "All",
    "Raw",
    "Country Cleaning",
    "TerraMatch - Country Codes.csv"
  ),
  encoding = "UTF-8"
)

# convert columns to snakecase -----------------------------------------------

names(globunt_14_may) <-
  to_snake_case(names(globunt_14_may))

names(globunt_5_june) <-
  to_snake_case(names(globunt_5_june))

names(project_data_07_22) <-
  to_snake_case(names(project_data_07_22))


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

# TM Country Codes --------------------------------------------------------

project_data_07_22 <-
  left_join(project_data_07_22,
            country_codes,
            by = "country_code")

# group globunt by country ------------------------------------------------

globunt_grouped <- globunt_all %>%
  group_by(sid_x) %>%
  mutate(globunt_country_list = paste(country, collapse = "|")) %>%
  slice(1) %>%
  ungroup() %>%
  select(-country, -family)

# join matched data to project data --------------------------------------

project_data_07_22_sub <- project_data_07_22 %>%
  select(
    tree_species_uuid,
    project_name,
    site_name,
    country_name,
    amount,
    site_report_due_date
  ) %>%
  rename(project_country = country_name)

matched_data <-
  left_join(matched_data,
            project_data_07_22_sub,
            by = "tree_species_uuid")

# edits for matching ------------------------------------------------------

matched_data <- matched_data %>%
  mutate(
    scientific_name =
      case_when(
        #        scientific_name == "Olea europaea subsp. cuspidata" ~ "Olea europaea",
        scientific_name == "Manihot carthaginensis subsp. glaziovii" ~ "Manihot carthagenensis",
        scientific_name == "Acacia albida" ~ "Faidherbia albida",
        scientific_name == "Hesperocyparis lusitanica" ~ "Cupressus lusitanica",
        TRUE ~ scientific_name
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


# select columns for saving matched data ----------------------------------

matched_data_globunt_all_sub <- matched_data_globunt_all %>%
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
    scientific_final,
    sid_x,
    geographic_area,
    lifeform_description,
    climate_description,
    red_list,
    af:globunt_country_list
  ) %>%
  arrange(project_name, site_report_due_date, scientific_name)

# complete projects -------------------------------------------------------

complete_projects <- matched_data_globunt_all %>%
  filter(!project_name %in% matched_data_remain$project_name) %>%
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
    scientific_final,
    sid_x,
    geographic_area,
    lifeform_description,
    climate_description,
    red_list,
    af:globunt_country_list
  ) %>%
  arrange(project_name, site_report_due_date, scientific_name)


# Completeness statistics -------------------------------------------------

species_info_counts <- matched_data %>%
  summarize(sub_family_only = sum(is.na(family) & is.na(genus) & is.na(specific_epithet)),
            family_only = sum(!is.na(family) & is.na(genus) & is.na(specific_epithet)),
            genus_only = sum(is.na(family) & !is.na(genus) & is.na(specific_epithet)),
            family_genus_only = sum(!is.na(family) & !is.na(genus) & is.na(specific_epithet)),
            family_genus_species = sum(!is.na(family) & !is.na(genus) & !is.na(specific_epithet)),
            total = nrow(matched_data)) 

# case_when statement for nativity ----------------------------------------

complete_projects <- complete_projects %>%
  mutate(nativity = 
           case_when(
             str_detect(globunt_country_list, fixed(project_country)) ~ "Native",
             !str_detect(globunt_country_list, fixed(project_country)) ~ "Non-Native",
             TRUE ~ NA_character_
  ))

complete_projects %>%
  distinct(project_name) %>%
  count()

matched_data %>%
  nrow()

6268/7775

# KAMRI data to display ----------------------------------------------------

kamri <- complete_projects %>%
  mutate(site_report_due_date = as.Date(site_report_due_date),
         amount = as.numeric(amount)) %>%
  filter(project_name == "KAMRI - FoE Ghana",
         site_report_due_date == "2024-02-01") %>%
  group_by(taxon_id) %>%
  mutate(amount = sum(amount, na.rm = TRUE)) %>%
  slice(1) %>%
  ungroup() %>%
  select(-site_name, -scientific_name_id)

# export data -------------------------------------------------------

write_excel_csv(matched_data_globunt_all_sub,
                file = here(
                  "Tree Species",
                  "Data",
                  "Processed",
                  "Matched Data",
                  "All Match",
                  "CSV",
                  "matched_data_globunt_all_final.csv"
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
    "complete_project_data_globunt.csv"
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
    "cleaned_project_tree_species_no_globunt.csv"
  )
)

write_xlsx(kamri,
           path = here(
             "Tree Species",
             "Data",
             "Processed",
             "Matched Data",
             "All Match",
             "xlsx",
             "kamri_2024_02_01_cleaned_report_data.xlsx"
           ))
