# Description -------------------------------------------------------------

# Author: Ben Steiger
# Date Created: 06/10/2024
# Last Updated: 01/06/2025
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

#unmatched_data <- read_csv(
#  file = here(
#    "Tree Species",
#    "Data",
#    "Processed",
#    "TerraFund Tree Species",
#    "03_unmatched_terrafund_tree_species_project_establishment_2024-12-12_12-08-33.csv"
#  )
#)

#unmatched_data <- read_csv(
#  file = here(
#    "Tree Species",
#    "Data",
#    "Processed",
#    "TerraFund Tree Species",
#    "03_unmatched_terrafund_tree_species.csv"
#  )
#)

# Unmatched PPC Project Report Data

unmatched_data <- read_csv(
  file = here(
    "Tree Species",
    "Data",
    "Processed",
    "PPC Tree Species",
    "03_unmatched_ppc_tree_species_project_reports_2025-01-06_01-57-01.csv"
  )
)

# Unmatched PPC Project Establishment Data

unmatched_data <- read_csv(
  file = here(
    "Tree Species",
    "Data",
    "Processed",
    "PPC Tree Species",
    "03_unmatched_ppc_tree_species_project_establishment_2024-12-14_15-09-08.csv"
  )
)

# matched data for comparison

#matched_data <- read_csv(
#  file = here(
#    "Tree Species",
#    "Data",
#    "Processed",
#    "TerraFund Tree Species",
#    "02_matched_terrafund_tree_species_project_establishment_2024-12-12_12-08-33.csv"
#  )
#)

# matched_data <- read_csv(
#  file = here(
#    "Tree Species",
#    "Data",
#    "Processed",
#    "TerraFund Tree Species",
#    "02_matched_terrafund_tree_species.csv"
#  )
#)


# Matched PPC Project Report Species Data
matched_data <- read_csv(
  file = here(
    "Tree Species",
    "Data",
    "Processed",
    "PPC Tree Species",
    "02_matched_ppc_tree_species_project_reports_2025-01-06_01-57-01.csv"
  )
)

# Matched PPC Project Establishment Species Data
#matched_data <- read_csv(
#  file = here(
#    "Tree Species",
#    "Data",
#    "Processed",
#    "PPC Tree Species",
#    "02_matched_ppc_tree_species_project_establishment_2024-12-14_15-09-08.csv"
#  )
#)

#tree_species_data <- read.csv(
#  here(
#    "Tree Species",
#    "Data",
#    "Raw",
#    "TerraFund Tree Species",
#    "terrafund_tree_species_202410301559.csv"
#  ),
#  encoding = "UTF-8"
#)

#tree_species_data <- read.csv(
#  here(
#    "Tree Species",
#    "Data",
#    "Raw",
#    "TerraFund Tree Species",
#    "terrafund_tree_species_project_establisment_202412121125.csv"
#  ),
#  encoding = "UTF-8"
#)

# PPC Raw Project Report Data

tree_species_data <- read.csv(
  here(
    "Tree Species",
    "Data",
    "Raw",
    "PPC Tree Species",
    "ppc_project_tree_species_202411261132.csv"
  ),
  encoding = "UTF-8"
)

# PPC Raw Project Establishment Data

#tree_species_data <- read.csv(
#  here(
#    "Tree Species",
#    "Data",
#    "Raw",
#    "PPC Tree Species",
#    "ppc_tree_species_project_establishment_202412131257.csv"
#  ),
#  encoding = "UTF-8"
#)

# group_by ----------------------------------------------------------------

check_matches <- matched_data %>%
  distinct(scientific_name, tree_species_name_clean, old_name) %>%
  arrange(scientific_name) %>%
  view()

unmatched_data %>%
  distinct(tree_species_name, tree_species_name_clean) %>%
  arrange(tree_species_name_clean) %>%
  view()

