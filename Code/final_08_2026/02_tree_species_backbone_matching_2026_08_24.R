# Description -------------------------------------------------------------

# Author: Ben Steiger
# Date Created: 07/03/2024
# Last Updated: 10/22/2025
# Description: Matching Cleaned Tree Species Data to Backbones

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

# Load data ---------------------------------------------------------------

# Clean site report data

data <- read.csv(
  here(
    "Tree Species",
    "Data",
    "Processed",
    "Projects",
    "01_project_tree_species_normalized_2026-08-24_21-40-21.csv"
  ),
  encoding = "UTF-8"
)

# World Flora Online Backbone - use WFO.remember to load data
#WFO.remember(WFO.file = here("Tree Species", "Data", "Raw", "WFO_Backbone_20240622", "classification.csv"), WFO.data = "WFO.data", WFO.pos = 1)

# check data source with WFO.remember once loaded
WFO.remember()

# World Checklist of Vascular Plants (WCVP) - names

wcvp_names <- read.csv(
  file = here("Tree Species",
              "Data",
              "Raw",
              "wcvp",
              "wcvp_names.csv"),
  sep = "|"
)

# Convert all "×" to "X" in data ------------------------------------------

# replace in WFO.data and wcvp data

# WFO data
WFO.data <- WFO.data %>%
  mutate(scientificName = str_replace_all(scientificName, "×", "x"))

# WCVP name
wcvp_names <- wcvp_names %>%
  mutate(taxon_name = str_replace_all(taxon_name, "×", "x"))

# create new backbone for wcvp data ---------------------------------------

WCVP.data <- new.backbone(
  wcvp_names,
  taxonID = "plant_name_id",
  scientificName = "taxon_name",
  scientificNameAuthorship = "taxon_authors",
  acceptedNameUsageID = "accepted_plant_name_id",
  taxonomicStatus = "taxon_status"
)

# convert to dataframe ----------------------------------------------------

data <- as.data.frame(data)

data <- data %>%
  select(tree_species_uuid, species_normalized, tree_species_name)

# Worldflora script -------------------------------------------------------

cuts <- cut(c(1:nrow(data)), breaks = 20, labels = FALSE)
cut.i <- sort(unique(cuts))

start.time <- Sys.time()

for (i in 1:length(cut.i)) {
  cat(paste("Cut: ", i, "\n"))
  
  data.i <-
    WFO.one(
      WFO.match.fuzzyjoin(
        spec.data = data[cuts == cut.i[i],],
        WFO.data = WFO.data,
        spec.name = "species_normalized",
        fuzzydist.max = 3
      ),
      verbose = FALSE
    )
  
  if (i == 1) {
    data.WFO <- data.i
  } else{
    data.WFO <- rbind(data.WFO, data.i)
  }
  
}

# filter to successful and unsuccessful matches ---------------------------

unmatched_data_wfo <- data.WFO %>%
  filter(Matched == "FALSE")

matched_data_wfo <- data.WFO %>%
  filter(Matched == "TRUE")

# match unsuccessful matches with WCVP backbone ---------------------------

# subset to variables needed

data_remain <- unmatched_data_wfo 

# convert to data frame

data_remain <- as.data.frame(data_remain)

# run script --------------------------------------------------------------

cuts <- cut(c(1:nrow(data_remain)), breaks = 20, labels = FALSE)
cut.i <- sort(unique(cuts))

start.time <- Sys.time()

for (i in 1:length(cut.i)) {
  cat(paste("Cut: ", i, "\n"))
  
  data_remain.i <-
    WFO.one(
      WFO.match.fuzzyjoin(
        spec.data = data_remain[cuts == cut.i[i],],
        WFO.data = WCVP.data,
        spec.name = "species_normalized",
        fuzzydist.max = 3
      ),
      verbose = FALSE
    )
  
  if (i == 1) {
    data_remain.WCVP <- data_remain.i
  } else{
    data_remain.WCVP <-
      rbind(data_remain.WCVP, data_remain.i)
  }
  
}

# filter to matched and unmatched data ------------------------------------

# matched WCVP data
matched_data_remain.WCVP <- data_remain.WCVP %>%
  filter(Matched == "TRUE")

# unmatched WCVP data
unmatched_data_remain.WCVP <- data_remain.WCVP %>%
  filter(Matched == "FALSE")

# subset columns before bind ----------------------------------------------

# matched WCVP data
matched_data_remain.WCVP_sub <-
  matched_data_remain.WCVP %>%
  select(
    tree_species_uuid,
    tree_species_name,
    species_normalized,
    Squished,
    Brackets.detected,
    Number.detected,
    Unique,
    Matched,
    Fuzzy,
    Fuzzy.dist,
    taxonID,
    scientificName,
    specificEpithet,
    family,
    genus,
    Old.status,
    Old.ID,
    Old.name
  )

# matched WFO data
matched_data_wfo_sub <- matched_data_wfo %>%
  select(
    tree_species_uuid,
    tree_species_name,
    species_normalized,
    Squished,
    Brackets.detected,
    Number.detected,
    Unique,
    Matched,
    Fuzzy,
    Fuzzy.dist,
    scientificNameID,
    taxonID,
    scientificName,
    family,
    genus,
    specificEpithet,
    Old.status,
    Old.ID,
    Old.name
  )

# unmatched WCVP data
unmatched_data_remain.WCVP_sub <-
  unmatched_data_remain.WCVP %>%
  select(
    tree_species_uuid,
    tree_species_name,
    species_normalized,
    Squished,
    Brackets.detected,
    Number.detected,
    Unique,
    Matched,
    Fuzzy,
    Fuzzy.dist,
    taxonID,
    scientificName,
    specificEpithet,
    family,
    genus,
    Old.status,
    Old.ID,
    Old.name
  )

# bind matched and unmatched dataframes -----------------------------------

# matched data
all_matched_data <-
  bind_rows(matched_data_remain.WCVP_sub,
            matched_data_wfo_sub)

# convert to snakecase ----------------------------------------------------

# matched data
names(all_matched_data) <-
  to_snake_case(names(all_matched_data))

# unmatched data
names(unmatched_data_remain.WCVP_sub) <-
  to_snake_case(names(unmatched_data_remain.WCVP_sub))

# rename unmatched data ---------------------------------------------------

all_unmatched_data <- unmatched_data_remain.WCVP_sub

# check matched data ------------------------------------------------------

all_matched_data %>%
  select(tree_species_uuid, taxon_id, scientific_name, species_normalized, tree_species_name, fuzzy_dist, old_name) %>%
  view()

# save .csv files ---------------------------------------------------------

timestamp <- format(Sys.time(), "%Y-%m-%d_%H-%M-%S")

# Bound matches - site reports
write_excel_csv(all_matched_data,
                file = here(
                  "Tree Species",
                  "Data",
                  "Processed",
                  "Projects",
                  paste0(
                    "02_matched_site_report_tree_species_",
                    timestamp,
                    ".csv"
                  )
                ))


# Remaining non-matches - site reports
write_excel_csv(all_unmatched_data,
                file = here(
                  "Tree Species",
                  "Data",
                  "Processed",
                  "Projects",
                  "Review",
                  paste0(
                    "02_unmatched_site_report_tree_species_",
                    timestamp,
                    ".csv"
                  )
                ))


