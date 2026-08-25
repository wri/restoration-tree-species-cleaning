# Description -------------------------------------------------------------

# Author: Ben Steiger
# Date Created: 07/03/2024
# Last Updated: 08/24/2026
# Description: Cleaning Tree Species Data before Matching
#                 to backbone

# Load libraries ----------------------------------------------------------

library(dplyr)
library(tidyverse)
library(stringr)
library(here)
library(readxl)
library(snakecase)
library(stringdist)
library(fuzzyjoin)
library(stringi)

# Load data ---------------------------------------------------------------

# raw species data - replace as needed

data <- read.csv(
  here(
    "Tree Species",
    "Data",
    "Raw",
    "Projects",
    "project_tree_species_null_taxon_id_202608241805.csv"
  ),
  encoding = "UTF-8"
)

# species exclusions - update this file with any new exclusions

species_exclusions <- read.csv(
  here(
    "Tree Species",
    "Data",
    "species_mapping_conversion_fixed",
    "species_mapping_conversion_fixed",
    "species_conversion",
    "species_exclusions.csv"
  ),
  encoding = "UTF-8"
)

# species replacements - update with any new string replacements

species_replacements <- read.csv(
  here(
    "Tree Species",
    "Data",
    "species_mapping_conversion_fixed",
    "species_mapping_conversion_fixed",
    "species_conversion",
    "species_replacements.csv"
  ),
  encoding = "UTF-8"
)

# species mapping - update with any new new species mapping

species_dictionary <- read.csv(
  here(
    "Tree Species",
    "Data",
    "species_mapping_conversion_fixed",
    "species_mapping_conversion_fixed",
    "species_conversion",
    "species_dictionary.csv"
  ),
  encoding = "UTF-8"
)


# Normalize species names -------------------------------------------------

normalize_species <- function(x) {
  
  x <- x %>%
    stringi::stri_trans_general("Latin-ASCII") %>%
    str_to_lower() %>%
    str_squish()
  
  # Missing values
  x[x %in% c("", "n/a")] <- NA_character_
  x[str_length(x) == 1] <- NA_character_
  
  x <- x %>%
    # Remove botanical author citations appearing after the species name
    str_remove(
      "\\s+\\b(l|gaertn|benth|hook|dc|lam|willd|roxb)\\.?\\b.*$"
    ) %>%
    
    # Remove numbers
    str_remove_all("[0-9]") %>%
    
    # Remove punctuation
    str_remove_all("[[:punct:]]") %>%
    
    # Remove taxonomic qualifiers
    str_replace_all(
      "\\b(spp|ssp|sp|species|specie)\\b",
      ""
    ) %>%
    
    str_squish()
  
  x[x == ""] <- NA_character_
  x[str_length(x) == 1] <- NA_character_
  
  x
}
data <- data %>%
  mutate(
    species_normalized = normalize_species(tree_species_name)
  )


# Species exclusions ------------------------------------------------------

# removes species that won't match to backbone 
data <- data %>%
  mutate(
    species_normalized = if_else(
      species_normalized %in% species_exclusions$value,
      NA_character_,
      species_normalized
    )
  )


# Species Dictionary Mapping ----------------------------------------------

# add in mapping R script

source(here(
  "Tree Species",
  "Data",
  "species_mapping_conversion_fixed",
  "species_mapping_conversion_fixed",
  "species_conversion",
  "apply_species_dictionary_mappings.R"
))

data <- clean_species_with_lookup(
  data,
  here(  "Tree Species",
         "Data",
         "species_mapping_conversion_fixed",
         "species_mapping_conversion_fixed",
         "species_conversion")
)

# drop NAs ----------------------------------------------------------------

na_sub <- data %>%
  filter(is.na(species_normalized))

data <- data %>%
  filter(!is.na(species_normalized))

# check matches -----------------------------------------------------------

data %>%
  distinct(species_normalized, tree_species_name) %>%
  arrange(species_normalized) %>%
  view()

# for review --------------------------------------------------------------

#review <- data %>%
#  filter(
#    (species_normalized %in% c("ako",
#                                    "adida",
#                                    "bois vert",
#                                   "chindora",
#                                   "fanamb",
#                                   "farehy",
#                                   "hazo",
#                                   "hazond",
#                                   "hazondrano",
#                                   "ilbamba",
#                                   "imwemwe",
#                                   "kwaker",
#                                   "lomotr",
#                                   "mbimbel",
#                                   "mentso",
#                                   "molomp",
#                                   "mpoule",
#                                   "olong",
#                                   "openg",
#                                   "plume de madagascar",
#                                   "randrompody",
#                                   "ringen",
#                                   "green harsh",
#                                   "tropophylla perr",
#                                   "antidisma altissima",
#                                   "con ella",
#                                   "arbol bejuco",
#                                   "diamante azul",
#                                   "aryton senna",
#                                   "barbasco blanco",
#                                   "bridalia celicina",
#                                   "crecropia pentandra",
#                                   "erythrinia hockii",
#                                   "folha composta",
#                                   "fraises exelsion",
#                                   "majahua de tuza",
#                                   "majauhua de pinolillo",
#                                   "geronima alcornoide",
#                                   "grateus hidridos",
#                                   "juerana irwinii",
#                                   "kaziaria javitenses",
#                                   "mahuagua cotzocon",
#                                   "malacate blanco",
#                                   "onichopetala periani",
#                                   "palo blanco",
#                                   "palo colorado",
#                                   "pau diamante",
#                                   "pau osso bogonota",
#                                   "pinha da mata",
#                                   "pittospermum pertandum",
#                                   "portoricensis pittier",
#                                   "propithecus coquereli",
#                                   "quiebra acha",
#                                   "serriyo blanco",
#                                   "tachiadenius gerrardianum",
#                                   "trachynodium verrococum",
#                                   "vismia renatiana",
#                                   "winmania runtenbergi",
#                                   "fig carcia",
#                                   "gorgojo",
#                                   "ficus spuria",
#                                   "lombricera",
#                                   "pau brasil",
#                                   "psydio",
#                                   "quebra acha",
#                                   "douglas",
#                                   "fucus luteus",
#                                   "palo pintacaja"
#                                   ) | 
#       str_detect(species_normalized, "mfsp|unha d cabra"))
#  )

# drop review species

#data <- data %>%
#  filter(!species_normalized %in% review$tree_species_name_clean)

# combine review species -----------------------------------------------------

#review_all <- rbind(na_sub, 
#                review) %>%
#  select(-species_normalized)

# Make first letter capitalized -------------------------------------------

data_normalized <- data %>%
  mutate(species_normalized = str_to_sentence(species_normalized))

# save data ---------------------------------------------------------------

timestamp <- format(Sys.time(), "%Y-%m-%d_%H-%M-%S")

write_excel_csv(data_normalized,
                file = here(
                  "Tree Species",
                  "Data",
                  "Processed",
                  "Projects",
                  paste0(
                    "01_project_tree_species_normalized_",
                    timestamp,
                    ".csv"
                  )
                ))

