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

# Load data ---------------------------------------------------------------

# Remaining non-matches
load(
  file = here(
    "Tree Species",
    "Data",
    "Processed",
    "Unmatched Data",
    "Rdata",
    "all_unmatched_project_data_6_07.rdata"
  )
)

# matched data for comparison
load(
  file = here(
    "Tree Species",
    "Data",
    "Processed",
    "Matched Data",
    "All Match",
    "Rdata",
    "all_matched_project_data_6_07.rdata"
  )
)

project_data_raw <- read_excel(
  here(
    "Tree Species",
    "Data",
    "Raw",
    "TerraFund Tree Species",
    "Tree Species Export 2024-06-03.xlsx"
  )
)

# convert project_data_raw columns to snake_case ------------------------------

names(project_data_raw) <- to_snake_case(names(project_data_raw))

# subset columns to tree_speices_uuid and species name --------------------

project_data_clean <- project_data_raw %>%
  select(tree_species_uuid, name)

# convert to dataframe ----------------------------------------------------

project_data_clean <- as.data.frame(project_data_clean)


# create new species name column ------------------------------------------

project_data_clean <- project_data_clean %>%
  mutate(
    name_clean = tolower(name))

# str_replace_all typos ---------------------------------------------------

project_data_clean <- project_data_clean %>%
  mutate(
    name_clean = str_remove_all(name_clean,
                                "\t"),
    name_clean = str_remove_all(name_clean,
                                "\\?"),
    name_clean = str_replace_all(name_clean,
                                "\\(\\(", "\\("),
    name_clean = str_replace_all(name_clean,
                                 "\\)\\)", "\\)"),
  )

# edit names using str_replace_all ----------------------------------------

project_data_clean <- project_data_clean %>%
  mutate(
    name_clean = str_replace_all(
      name_clean,
      "avocatier|avocat|ovacado|avacado|aavocado|avocadoes|avocados|ovacodos",
      "avocado"
    ),
    name_clean = str_replace_all(name_clean,
                                 "percea|persa|persia|persis",
                                 "persea"),
    
  )


# case_when str_replace ---------------------------------------------------

project_data_clean <- project_data_clean %>%
  mutate(name_clean =
           case_when(
             str_detect(name_clean, "avocado|persea") ~ "persea americana",
             str_detect(name_clean, "cashew|anacarde anacardium occidentale") ~ "anacardium occidentale",
             str_detect(name_clean, "andasonia digitata") ~ "andasonia digitata",
             str_detect(name_clean, "acacia angustissima") ~ "acacia angustissima",
             str_detect(name_clean, "bamboo bamboosodea|bamboo bambusodea") ~ "bambusoideae",
             
             TRUE ~ name_clean
           ))

# extract data from within parentheses ------------------------------------

project_data_clean <- project_data_clean %>%
  mutate(extracted_name = str_extract(name_clean, "(?<=\\().+?(?=\\))"))
