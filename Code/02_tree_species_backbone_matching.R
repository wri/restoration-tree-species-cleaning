# Description -------------------------------------------------------------

# Author: Ben Steiger
# Date Created: 07/03/2024
# Last Updated: 10/30/2024
# Description: Matching Cleaned TerraFund Landscapes Project Establishment Tree Species Data to Backbones

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

# TerraFund Tree Species Data

project_data <- read.csv(
  here(
    "Tree Species",
    "Data",
    "Processed",
    "TerraFund Tree Species",
    "01_clean_terrafund_tree_species.csv"
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

project_data <- as.data.frame(project_data)

# Worldflora script -------------------------------------------------------

cuts <- cut(c(1:nrow(project_data)), breaks = 20, labels = FALSE)
cut.i <- sort(unique(cuts))

start.time <- Sys.time()

for (i in 1:length(cut.i)) {
  cat(paste("Cut: ", i, "\n"))
  
  project_data.i <-
    WFO.one(
      WFO.match.fuzzyjoin(
        spec.data = project_data[cuts == cut.i[i],],
        WFO.data = WFO.data,
        spec.name = "tree_species_name_clean",
        fuzzydist.max = 3
      ),
      verbose = FALSE
    )
  
  if (i == 1) {
    project_data.WFO <- project_data.i
  } else{
    project_data.WFO <- rbind(project_data.WFO, project_data.i)
  }
  
}

# filter to successful and unsuccessful matches ---------------------------

unmatched_project_data_wfo <- project_data.WFO %>%
  filter(Matched == "FALSE")

matched_project_data_wfo <- project_data.WFO %>%
  filter(Matched == "TRUE")

# match unsuccessful matches with WCVP backbone ---------------------------

# subset to variables needed

project_data_remain <- unmatched_project_data_wfo 

# convert to data frame

project_data_remain <- as.data.frame(project_data_remain)

# run script --------------------------------------------------------------

cuts <- cut(c(1:nrow(project_data_remain)), breaks = 20, labels = FALSE)
cut.i <- sort(unique(cuts))

start.time <- Sys.time()

for (i in 1:length(cut.i)) {
  cat(paste("Cut: ", i, "\n"))
  
  project_data_remain.i <-
    WFO.one(
      WFO.match.fuzzyjoin(
        spec.data = project_data_remain[cuts == cut.i[i],],
        WFO.data = WCVP.data,
        spec.name = "tree_species_name_clean",
        fuzzydist.max = 3
      ),
      verbose = FALSE
    )
  
  if (i == 1) {
    project_data_remain.WCVP <- project_data_remain.i
  } else{
    project_data_remain.WCVP <-
      rbind(project_data_remain.WCVP, project_data_remain.i)
  }
  
}

# filter to matched and unmatched data ------------------------------------

# matched WCVP data
matched_project_data_remain.WCVP <- project_data_remain.WCVP %>%
  filter(Matched == "TRUE")

# unmatched WCVP data
unmatched_project_data_remain.WCVP <- project_data_remain.WCVP %>%
  filter(Matched == "FALSE")

# subset columns before bind ----------------------------------------------

names(matched_project_data_remain.WCVP)
# matched WCVP data
matched_project_data_remain.WCVP_sub <-
  matched_project_data_remain.WCVP %>%
  select(
    framework,
    tree_species_uuid,
    project_uuid,
    project_name,
    organisation_name,
    country_code,
    tree_species_name,
    amount,
    tree_species_name_clean,
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
matched_project_data_wfo_sub <- matched_project_data_wfo %>%
  select(
    framework,
    tree_species_uuid,
    project_uuid,
    project_name,
    organisation_name,
    country_code,
    tree_species_name,
    amount,
    tree_species_name_clean,
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
unmatched_project_data_remain.WCVP_sub <-
  unmatched_project_data_remain.WCVP %>%
  select(
    framework,
    tree_species_uuid,
    project_uuid,
    project_name,
    organisation_name,
    country_code,
    tree_species_name,
    amount,
    tree_species_name_clean,
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
all_matched_project_data <-
  bind_rows(matched_project_data_remain.WCVP_sub,
            matched_project_data_wfo_sub)

# convert to snakecase ----------------------------------------------------

# matched data
names(all_matched_project_data) <-
  to_snake_case(names(all_matched_project_data))

# unmatched data
names(unmatched_project_data_remain.WCVP_sub) <-
  to_snake_case(names(unmatched_project_data_remain.WCVP_sub))

# rename unmatched data ---------------------------------------------------

all_unmatched_project_data <- unmatched_project_data_remain.WCVP_sub

# save .csv files ---------------------------------------------------------

# Bound matches
write_excel_csv(
  all_matched_project_data,
  file = here(
    "Tree Species",
    "Data",
    "Processed",
    "TerraFund Tree Species",
    "02_matched_terrafund_tree_species.csv"
  )
)

# Remaining non-matches
write_excel_csv(
  all_unmatched_project_data,
  file = here(
    "Tree Species",
    "Data",
    "Processed",
    "TerraFund Tree Species",
    "03_unmatched_terrafund_tree_species.csv"
  )
)
