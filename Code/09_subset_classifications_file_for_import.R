# Description -------------------------------------------------------------

# Author: Ben Steiger
# Date Created: 12/05/2024
# Last Updated: 12/06/2024
# Description: Removing duplicates from WFO taxonomic backbone

# load packages -----------------------------------------------------------

library(dplyr)
library(tidyverse)
library(stringr)
library(here)
library(snakecase)
library(WorldFlora)
library(janitor)

# load data ---------------------------------------------------------------

WFO.remember()


# filter to duplicates ----------------------------------------------------

WFO.data_dupes <- WFO.data %>%
  group_by(scientificName) %>%
  filter(n() > 1) %>%
  ungroup()

# filter to where taxonomic status == accepeted
# group by scientific name 
# keep shortest taxonID

WFO.data_dupes_1 <- WFO.data_dupes %>%
  filter(taxonomicStatus == "Accepted") %>%
  group_by(scientificName) %>%
  mutate(numericID = as.numeric(str_remove(taxonID, "wfo-"))) %>%
  slice_min(numericID) %>%
  ungroup() %>%
  select(-numericID)

# for remaining duplicates
# group by scientific name 
# keep shortest taxonID

WFO.data_dupes_2 <- WFO.data_dupes %>%
  filter(!taxonID %in% WFO.data_dupes_1$taxonID) %>%
  group_by(scientificName) %>%
  mutate(numericID = as.numeric(str_remove(taxonID, "wfo-"))) %>%
  slice_min(numericID) %>%
  ungroup() %>%
  select(-numericID)

# bind rows
WFO.data_dupes_all <- rbind(WFO.data_dupes_1, WFO.data_dupes_2)

# for remaining duplicates, keep 1 scientific name by taxonomic status
# which will keep the "accepted" name when duplicated
WFO.data_dupes_all_sub <- WFO.data_dupes_all %>%
  group_by(scientificName) %>%
  arrange(taxonomicStatus) %>%
  slice(1) %>%
  ungroup()

# drop all duplictes that aren't in the _sub dataframe
WFO.data_dupes_drops <- WFO.data_dupes %>%
  filter(!taxonID %in% WFO.data_dupes_all_sub$taxonID)

# drop all taxonIDs in drops 
# select columns needed
WFO.data_final <- WFO.data %>%
  filter(!taxonID %in% WFO.data_dupes_drops$taxonID) %>%
  select(taxonID, scientificName, family, genus, specificEpithet, infraspecificEpithet)


# export ------------------------------------------------------------------

write_excel_csv(
  WFO.data_final,
  file = here(
    "Tree Species",
    "Data",
    "Processed",
    "WFO_Backbone",
    "classifications_no_dupes.csv"
  )
)



