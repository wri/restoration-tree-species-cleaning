# Description -------------------------------------------------------------

# Author: Ben Steiger
# Date Created: 10/29/2024
# Last Updated: 10/29/2024
# Description: Create Summary Tables and Visualizations

# Load libraries ----------------------------------------------------------

library(dplyr)
library(tidyverse)
library(here)
library(writexl)
library(openxlsx)

# Load project data ---------------------------------------------------------------

project_data <-
  read_csv(
    file = here(
      "Tree Species",
      "Data",
      "Processed",
      "Matched Data",
      "All Match",
      "CSV",
      "terrafund_project_data_griis_gisd_10_30.csv"
    )
  )

project_profiles <-
  read.csv(
    here(
      "Tree Species",
      "Data",
      "Raw",
      "TerraFund Project Profiles",
      "landscapes_project_profiles.csv"
    ),
    encoding = "UTF-8"
  )


# convert data to snakecase -----------------------------------------------

names(project_profiles) <- to_snake_case(names(project_profiles))

# convert project profile data to lower -----------------------------------

project_profiles <- project_profiles %>%
  mutate(across(everything(), tolower))

# subset columns ----------------------------------------------------------

project_profiles <- project_profiles %>%
  select(
    uuid,
    description,
    objectives,
    proj_partner_info,
    siting_strategy_description,
    environmental_goals,
    socioeconomic_goals,
    community_incentives,
    land_use_types,
    restoration_strategy
  )

# join data together ------------------------------------------------------

join_project_data <-
  left_join(project_data,
            project_profiles,
            by = c("project_uuid" = "uuid"))

# task --------------------------------------------------------------------

#What outcomes are in the landscape already? -
# Commodities engagement on these key value chains per landscape could answer TerraFund's contribution to part of that question.
# How many business or projects are already producing key value chains?
#GCB: cocao, timber, carbon, or plantains/food crops
#LKR key value chains: Coffee, avocado, macadamia nuts, mango, honey
#GRV key value chains: Avocado, macadamia nuts, mango, livestock, honey, carbon

# create landscapes variable ----------------------------------------------

join_project_data <- join_project_data %>%
  mutate(
    landscape =
      case_when(
        country_name %in% c("Congo, Democratic Republic of the", "Burundi", "Rwanda") ~ "LKR",
        country_name %in% c("Ghana") ~ "GCB",
        country_name %in% c("Kenya") ~ "GRV"
      )
  )

# vector of columns to search keywords in ---------------------------------

columns_to_search <- c(
  "description",
  "objectives",
  "proj_partner_info",
  "siting_strategy_description",
  "environmental_goals",
  "socioeconomic_goals",
  "community_incentives"
)


# land use types and restoration strategy ---------------------------------

land_use_strategy <- c("land_use_types", "restoration_strategy")


# carbon projects ---------------------------------------------------------

# champions that have have actively issued carbon credits: 
# ARCOS, Solidaridad, WWF, Hen Mpoano, Birdlife International, Eden People + Planet, Green Belt Movement

carbon_projects <- c("3a2099dd-9a85-4091-b0c7-5edd11acbcd6", "562fa859-5124-49a5-947f-e1ddb7680e07", "9db2495f-bee1-4323-97ac-c86a3a4e0283",
                     "7a4f2aa6-e916-45bd-ba27-c3fa3d4ea938", "cb8c8dad-43a4-42fa-a262-b9244d23cca8", "cdbf9bf1-d088-4664-a98f-8c684b36e5d3",
                     "d537cd2c-dfb2-4a36-a8ef-1f2e7bc705b3")

# Value chains ------------------------------------------------------------

join_project_data <- join_project_data %>%
  mutate(
    cocoa = case_when(
      scientific_name == "Theobroma cacao" |
        rowSums(across(
          all_of(columns_to_search), ~ str_detect(., "cocoa")
        )) > 0 ~ 1,
      TRUE ~ 0
    ),
    coffee = case_when(
      scientific_name == "Coffea arabica" |
        rowSums(across(
          all_of(columns_to_search), ~ str_detect(., "coffee|café")
        )) > 0 ~ 1,
      TRUE ~ 0
    ),
    macadamia = case_when(
      scientific_name == "Macadamia integrifolia" |
        rowSums(across(
          all_of(columns_to_search), ~ str_detect(., "macadamia")
        )) > 0 ~ 1,
      TRUE ~ 0
    ),
    avocado = case_when(
      scientific_name == "Persea americana" |
        rowSums(across(
          all_of(columns_to_search), ~ str_detect(., "avocado|avocat")
        )) > 0 ~ 1,
      TRUE ~ 0
    ),
    mango = case_when(
      scientific_name == "Mangifera indica" |
        rowSums(across(
          all_of(columns_to_search),
          ~ str_detect(., "mango|mangue|manguier")
        )) > 0 ~  1,
      TRUE ~ 0
    ),
    honey = case_when(rowSums(across(
      all_of(columns_to_search),
      ~ str_detect(
        .,
        "honey|\\bmiel\\b|\\bbeekeeping\\b|\\bbee\\b|beeswax|wax|\\babeille\\b"
      )
    )) > 0 ~  1, TRUE ~ 0),
    livestock = case_when(rowSums(across(
      all_of(land_use_strategy), ~ str_detect(., "silvopasture")
    )) > 0 |
      rowSums(across(
        all_of(columns_to_search),
        ~ str_detect(., "livestock|cattle|bétail|silvopasture")
      )) > 0 ~  1, TRUE ~ 0),
    carbon = case_when(project_uuid %in% carbon_projects ~ 1,
                       TRUE ~ 0),
    timber = case_when(fu == 1 | ma == 1 ~ 1, TRUE ~ 0),
    food_crops = case_when(hf == 1 ~ 1, TRUE ~ 0),
  )


# timber data for Will to review ------------------------------------------

#timber <- join_project_data %>%
#  filter(timber == 1) %>%
#  group_by(project_uuid) %>%
#  mutate(tree_species = paste(scientific_name, collapse = ", ")) %>%
#  ungroup() %>%
#  distinct(project_uuid, .keep_all = TRUE) %>%
#  select(
#    project_uuid,
#    organization_name,
#    project_name,
#    project_country,
#    description,
#    objectives,
#    tree_species
#  )

# load Will's review ------------------------------------------------------

# timber projects reviewed by Will
timber_projects <-
  read_xlsx(
    path = here(
      "Tree Species",
      "Data",
      "Processed",
      "Review",
      "timber_data_to_review_wa.xlsx"
    )
  )

# convert to snakecase

names(timber_projects) <- to_snake_case(names(timber_projects))


# drop timber data from join and reimport ---------------------------------

timber_projects <- timber_projects %>%
  select(uuid_of_project, timber_0_or_1) %>%
  rename(timber = timber_0_or_1)

join_project_data <- join_project_data %>%
  select(-timber)

names(join_project_data)
join_project_data <-
  left_join(
    join_project_data,
    timber_projects,
    by = c("project_uuid" = "uuid_of_project") 
  )

# summary data ------------------------------------------------------------

summary_data_by_landscape <- join_project_data %>%
  group_by(landscape) %>%
  summarise(
    cocoa = n_distinct(project_uuid[cocoa == 1]),
    coffee = n_distinct(project_uuid[coffee == 1]),
    macadamia = n_distinct(project_uuid[macadamia == 1]),
    avocado = n_distinct(project_uuid[avocado == 1]),
    mango = n_distinct(project_uuid[mango == 1]),
    honey = n_distinct(project_uuid[honey == 1]),
    livestock = n_distinct(project_uuid[livestock == 1]),
    carbon = n_distinct(project_uuid[carbon == 1]),
    timber = n_distinct(project_uuid[timber == 1]),
    food_crops = n_distinct(project_uuid[food_crops == 1]),
  )

# birdlife overall --------------------------------------------------------

birdlife <- join_project_data %>%
  filter(organization_name == "BirdLife International - Kenya") %>%
  select(
    organization_name,
    project_name,
    project_country,
    landscape,
    scientific_name,
    species_count,
    native_distribution,
    nativity,
    cocoa:food_crops
  )

## birdlife_summary --------------------------------------------------------
#
#birdlife_summary <- join_project_data %>%
#  filter(organization_name == "BirdLife International - Kenya") %>%
#  group_by(organization_name) %>%
#  summarize(
#    tree_species = paste(scientific_name, collapse = ", "),
#    native = sum(nativity == "Native"),
#    non_native = sum(nativity == "Non-Native"),
#    avocado = sum(avocado),
#    coffee = sum(coffee),
#    macadamia = sum(macadamia),
#    mango = sum(mango),
#    honey = sum(honey),
#    fruit = sum(avocado + coffee + macadamia + mango)
#  )
#
## export birdlife data ----------------------------------------------------
#
## Create a new workbook
#wb <- createWorkbook()
#
## Add sheets and data
#addWorksheet(wb, "BirdLife Data")
#writeData(wb, "BirdLife Data", birdlife)
#
#addWorksheet(wb, "BirdLife Summary Statistics")
#writeData(wb, "BirdLife Summary Statistics", birdlife_summary)
#
## Save the workbook to an .xlsx file
#saveWorkbook(
#  wb,
#  here(
#    "Tree Species",
#    "Data",
#    "Processed",
#    "Summary Data",
#    "birdlife_value_chains.xlsx"
#  ),
#  overwrite = TRUE
#)

# export data -------------------------------------------------------------

write_xlsx(
  summary_data_by_landscape,
  path = here(
    "Tree Species",
    "Data",
    "Processed",
    "Summary Data",
    "terrafund_value_chains_data.xlsx"
  )
)

#write_xlsx(
#  timber,
#  path = here(
#    "Tree Species",
#    "Data",
#    "Processed",
#    "Review",
#    "timber_data_to_review.xlsx"
#  )
#)
