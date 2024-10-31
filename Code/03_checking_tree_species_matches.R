# Description -------------------------------------------------------------

# Author: Ben Steiger
# Date Created: 06/10/2024
# Last Updated: 10/30/2024
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
    "TerraFund Tree Species",
    "03_unmatched_terrafund_tree_species.csv"
  )
)

# matched data for comparison
matched_data <- read_csv(
  file = here(
    "Tree Species",
    "Data",
    "Processed",
    "TerraFund Tree Species",
    "02_matched_terrafund_tree_species.csv"
  )
)

tree_species_data <- read.csv(
  here(
    "Tree Species",
    "Data",
    "Raw",
    "TerraFund Tree Species",
    "terrafund_tree_species_202410301559.csv"
  ),
  encoding = "UTF-8"
)

# group_by ----------------------------------------------------------------

check_matches <- matched_data %>%
  distinct(scientific_name, tree_species_name_clean, old_name) %>%
  arrange(scientific_name) %>%
  view()

unmatched_data %>%
  distinct(scientific_name, tree_species_name_clean) %>%
  arrange(scientific_name)
