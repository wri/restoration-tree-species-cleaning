# Description -------------------------------------------------------------

# Author: Ben Steiger
# Date Created: 10/29/2024
# Last Updated: 11/08/2024
# Description: Joining Matched TerraFund Project Report Tree Species Data to Invasive Species Datasets & Binding All Data

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
library(janitor)

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
      "terrafund_new_project_data_match_2024-11-08_14-44-46.csv"
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
      "terrafund_matched_data_globunt_all_final_2024-11-08_14-11-08.csv"
    )
  )

# GISD

gisd <-
  read.csv(
    file = here("Tree Species", "Data", "Raw", "GISD", "gisd_all.csv"),
    sep = ";"
  )

# raw project data
raw_project_data <- 
  read.csv(
    here(
      "Tree Species",
      "Data",
      "Raw",
      "TerraFund Tree Species",
      "terrafund_tree_species_202410301559.csv"
    ),
    encoding = "UTF-8"
  )

# extract pdf -------------------------------------------------------------

# Extract text from the PDF
#pdf_text <- pdf_text("Tree Species/Data/Raw/WCUPS/World_Checklist_of_Useful_Plant_Species_2020.pdf")
#
## View the text from page
#page_text <- pdf_text[4]
#cat(page_text)

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
  select(
    -taxon_name,-plant_name_id,-squished,-brackets_detected,-number_detected,-unique,-matched,-fuzzy,-fuzzy_dist,
    -old_status,-old_id,-country_code
  )

# bind project data -------------------------------------------------------

project_data_all <-
  rbind(project_data_globunt, project_data_new)

# select columns from gisd and griis --------------------------------------

griis <- griis %>%
  select(
    accepted_name_species,
    is_invasive,
    establishment_means,
    checklist_name,
    scientific_name
  ) %>%
  rename(scientific_name_griis = scientific_name)

gisd <- gisd %>%
  select(species, eicat)

# drop NAs in GRIIS accepted_name_species ---------------------------------

griis <- griis %>%
  filter(!is.na(accepted_name_species))

# edit names in griis dataset ---------------------------------------------

griis <- griis %>%
  mutate(accepted_name_species =
           case_when(
             accepted_name_species == "Citrus aurantium" ~ "Citrus x aurantium",
             accepted_name_species == "Musa paradisiaca" ~ "Musa x paradisiaca",
             accepted_name_species == "Cupressus lusitanica" ~ "Hesperocyparis lusitanica",
             accepted_name_species == "Actinidia chinensis" ~ "Actinidia chinensis var. deliciosa",
             accepted_name_species == "Toona ciliata" ~ "Toona hexandra",
             TRUE ~ accepted_name_species
           ))

# join gisd and griis data ------------------------------------------------

join_griis_gisd <-
  left_join(griis, gisd, by = c("accepted_name_species" = "species"))

# edit country names in griis ---------------------------------------------

join_griis_gisd <- join_griis_gisd %>%
  mutate(
    checklist_name =
      case_when(
        checklist_name == "Democratic Republic of the Congo" ~ "Congo, Democratic Republic of the",
        checklist_name == "United Republic of Tanzania" ~ "Tanzania, United Republic of",
        TRUE ~ checklist_name
      )
  )

# join griis data to project data -----------------------------------------

# join by name and if project country is in the GRIIS country distribution

project_data_griis_gisd <-
  left_join(
    project_data_all,
    join_griis_gisd,
    by = c("scientific_name" = "accepted_name_species", "country_name" = "checklist_name")
  )

# duplicates --------------------------------------------------------------

# get duplicates
dupes <- project_data_griis_gisd %>%
  get_dupes(tree_species_uuid)

# duplicate data from griis dataset - don't need the null invasiveness
dupes_to_drop <- dupes %>%
  filter(is_invasive == "null")

# drop using anti_join

project_data_griis_gisd <- anti_join(project_data_griis_gisd, dupes_to_drop)

# rename and remove columns
project_data_griis_gisd <- project_data_griis_gisd %>%
  select(-scientific_name_griis) 

# change blank EICAT to NA ------------------------------------------------

# strictly for data viz

project_data_griis_gisd <- project_data_griis_gisd %>%
  mutate(eicat = case_when(eicat == "" ~ NA_character_, TRUE ~ eicat))


# get all rows from original export that were removed ---------------------

raw_project_data_sub <- raw_project_data %>%
  filter(!tree_species_uuid %in% project_data_griis_gisd$tree_species_uuid)

setdiff(names(raw_project_data_sub), names(project_data_griis_gisd))

# drop columns already in data

raw_project_data <- raw_project_data %>%
  select(-one_of(setdiff(intersect(names(raw_project_data), names(project_data_griis_gisd)), "tree_species_uuid")))

# inner join to get original data

project_data_griis_gisd <- 
  inner_join(project_data_griis_gisd, raw_project_data,
            by = "tree_species_uuid")

# bind_rows to get final data

final_cleaned_project_tree_species <-
  bind_rows(project_data_griis_gisd,
            raw_project_data_sub)


# edit redlist categories -------------------------------------------------

final_cleaned_project_tree_species <- final_cleaned_project_tree_species %>%
  mutate(red_list_category =
           case_when(red_list_category == "Least Concern" ~ "LC",
                     red_list_category == "Data Deficient" ~ "DD",
                     red_list_category == "Endangered" ~ "EN",
                     red_list_category == "Critically Endangered" ~ "CR",
                     red_list_category == "Near Threatened" ~ "NT",
                     TRUE ~ red_list_category
                     )
         )


# edit use categories -----------------------------------------------------

final_cleaned_project_tree_species <- final_cleaned_project_tree_species %>%
  rename(animal_food = af,
         environmental_uses = eu,
         fuels = fu,
         gene_sources = gs,
         human_food = hf,
         invertebrate_food = 'if',
         materials = ma,
         medicines = me,
         poisons = po,
         social_uses = su
         )

# clean up final_cleaned_project_tree_species -----------------------------

# reorder variables
# change tree_species_name to original_tree_species_name to remove confusion
# remove temporary tree_species_name_clean

final_cleaned_project_tree_species <- final_cleaned_project_tree_species %>%
  rename(original_tree_species_name = tree_species_name) %>%
  select(organisation_uuid, organisation_name, project_uuid, project_name, site_name, framework, country_code, country_name, site_report_due_date, tree_species_uuid,
         original_tree_species_name, amount, collection, everything(), -tree_species_name_clean, -top_830, -scientific_name_id)

# save data ---------------------------------------------------------------
timestamp <- format(Sys.time(), "%Y-%m-%d_%H-%M-%S")

write_excel_csv(
  final_cleaned_project_tree_species,
  file = here(
    "Tree Species",
    "Data",
    "Processed",
    "Matched Data",
    "All Match",
    "CSV",
    paste0("final_cleaned_terrafund_project_tree_species_", timestamp, ".csv")
  )
)
names(final_cleaned_project_tree_species)


