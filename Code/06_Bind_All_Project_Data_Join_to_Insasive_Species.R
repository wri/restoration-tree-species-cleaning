# Description -------------------------------------------------------------

# Author: Ben Steiger
# Date Created: 08/28/2024
# Last Updated: 08/28/2024
# Description: Joining Matched TerraFund Project Report Tree Species Data to GlobUNT

# Load libraries ----------------------------------------------------------

library(dplyr)
library(tidyverse)
library(here)
library(snakecase)
library(fuzzyjoin)
library(stringdist)

# Download and install Java first (https://www.oracle.com/java/technologies/downloads/)
# Install the package

#if (!requireNamespace("remotes", quietly = TRUE)) {
#  install.packages("remotes")
#}
#remotes::install_github("ropensci/tabulizer")
library(tabulapdf)
library(pdftools)

# Load project data ---------------------------------------------------------------

project_data_new <-
  read_csv(
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

project_data_globunt <-
  read_csv(
    file = here(
      "Tree Species",
      "Data",
      "Processed",
      "Matched Data",
      "All Match",
      "CSV",
      "matched_data_globunt_all_final.csv"
    )
  )

# GISD

gisd <-
  read.csv(
    file = here("Tree Species", "Data", "Raw", "GISD", "gisd_all.csv"),
    sep = ";"
  )


# extract pdf -------------------------------------------------------------

# Extract text from the PDF
pdf_text <- pdf_text("Tree Species/Data/Raw/WCUPS/World_Checklist_of_Useful_Plant_Species_2020.pdf")

# View the text from page 
page_text <- pdf_text[4]
cat(page_text)

# load griis data --------------------------------------

# Get a list of all CSV files in the folder
csv_files <- list.files(path = "Tree Species/Data/Raw/GRIIS",
                        pattern = "*.csv",
                        full.names = TRUE)

# Load each CSV file and name it after the file (without the .csv extension)
csv_data <- csv_files %>%
  set_names(nm = basename(csv_files) %>% tools::file_path_sans_ext()) %>%
  map(read_csv)

# This will drop the "accepted_name" column from each data frame in the list
# which is the extra column in 4 .csv files
csv_data_cleaned <- csv_data %>%
  map( ~ .x %>% select(-matches("^accepted_name$")))

# bind files
# The 'source' column will contain the name of the original file each row came from
griis <- bind_rows(csv_data_cleaned)

# convert columns to snakecase -----------------------------------------------

names(griis) <- to_snake_case(names(griis))

names(gisd) <- to_snake_case(names(gisd))

# change column names -----------------------------------------------------

# see where columns are different
setdiff(names(project_data_new), names(project_data_globunt))

# globunt joined data
project_data_globunt <- project_data_globunt %>%
  rename(native_distribution = globunt_country_list,
         red_list_category = red_list) %>%
  select(-sid_x, -scientific_final)

# joined other data
project_data_new <- project_data_new %>%
  rename(top_830 = top830, red_list_category = redlist_category) %>%
  select(-taxon_name, -plant_name_id)

# bind project data -------------------------------------------------------

project_data_all <-
  rbind(project_data_globunt, project_data_new)

project_data_all %>%
  distinct(project_country) %>%
  arrange(project_country) %>%
  print(n = Inf)

# select columns from gisd and griis --------------------------------------

griis <- griis %>%
  select(accepted_name_species, is_invasive, establishment_means, checklist_name, scientific_name) %>%
  rename(scientific_name_griis = scientific_name)

gisd <- gisd %>%
  select(species, eicat)

# drop NAs in GRIIS accepted_name_species ---------------------------------

griis <- griis %>%
  filter(!is.na(accepted_name_species))

# join gisd and griis data ------------------------------------------------

join_griis_gisd <-
  left_join(griis,
             gisd,
             by = c("accepted_name_species" = "species")) 


# edit country names in griis ---------------------------------------------

join_griis_gisd <- join_griis_gisd %>%
  mutate(checklist_name = 
    case_when(checklist_name == "Democratic Republic of the Congo" ~ "Congo, Democratic Republic of the",
              checklist_name == "United Republic of Tanzania" ~ "Tanzania, United Republic of",
              TRUE ~ checklist_name
  ))

# join griis data to project data -----------------------------------------

project_data_griis_gisd <-
  left_join(project_data_all,
            join_griis_gisd,
            by = c("scientific_name" = "accepted_name_species",
                   "project_country" = "checklist_name")) 

# duplicates --------------------------------------------------------------

# get duplicates
dupes <- project_data_griis_gisd %>%
  get_dupes(tree_species_uuid)

# duplicate data from griis dataset - don't need the null invasiveness
dupes_to_drop <- dupes %>%
  filter(is_invasive == "null")

# drop using scientific name

project_data_griis_gisd <- project_data_griis_gisd %>%
  filter(!scientific_name_griis %in% dupes_to_drop$scientific_name_griis)


# change blank EICAT to NA ------------------------------------------------

# strictly for data viz

project_data_griis_gisd <- project_data_griis_gisd %>%
  mutate(eicat = case_when(
    eicat == "" ~ NA_character_,
    TRUE ~ eicat
  ))


# Correct accents ---------------------------------------------------------

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
project_data_griis_gisd <- project_data_griis_gisd %>%
  mutate(across(where(is.character), correct_accents))

# save data ---------------------------------------------------------------

write_excel_csv(
  project_data_griis_gisd,
  file = here(
    "Tree Species",
    "Data",
    "Processed",
    "Matched Data",
    "All Match",
    "CSV",
    "project_data_griis_gisd.csv"
  )
)
