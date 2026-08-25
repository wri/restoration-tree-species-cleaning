# Apply species replacement and dictionary mappings ------------------------
#
# Workflow:
#   1. Apply ordered regex replacements.
#   2. Apply exact species dictionary mappings.
#   3. Apply regex species dictionary mappings.
#   4. Leave unmatched values unchanged.
#
# Reference files:
#   - species_replacements.csv
#   - species_dictionary.csv
#
# species_replacements.csv columns:
#   priority, regex_pattern, replacement
#
# species_dictionary.csv columns:
#   pattern, taxonomic_name, match_type

library(dplyr)
library(readr)
library(stringr)


# Apply species replacement rules -------------------------------------------

apply_species_replacements <- function(x, lookup) {
  
  required_columns <- c(
    "priority",
    "regex_pattern",
    "replacement"
  )
  
  if (!all(required_columns %in% names(lookup))) {
    stop(
      "Replacement file must contain: ",
      paste(required_columns, collapse = ", ")
    )
  }
  
  lookup <- lookup %>%
    arrange(priority)
  
  for (i in seq_len(nrow(lookup))) {
    
    pattern <- lookup$regex_pattern[[i]]
    replacement <- lookup$replacement[[i]]
    
    x <- str_replace_all(
      x,
      pattern,
      replacement
    )
  }
  
  x
}


# Apply species dictionary --------------------------------------------------

apply_species_dictionary <- function(x, dictionary) {
  
  required_columns <- c(
    "pattern",
    "taxonomic_name",
    "match_type"
  )
  
  if (!all(required_columns %in% names(dictionary))) {
    stop(
      "Dictionary must contain: ",
      paste(required_columns, collapse = ", ")
    )
  }
  
  invalid_match_types <- setdiff(
    unique(dictionary$match_type),
    c("exact", "regex")
  )
  
  invalid_match_types <- invalid_match_types[
    !is.na(invalid_match_types)
  ]
  
  if (length(invalid_match_types) > 0) {
    stop(
      "match_type must be either 'exact' or 'regex'. ",
      "Invalid value(s): ",
      paste(invalid_match_types, collapse = ", ")
    )
  }
  
  # Remove incomplete rows
  dictionary <- dictionary %>%
    filter(
      !is.na(pattern),
      pattern != "",
      !is.na(taxonomic_name),
      taxonomic_name != "",
      !is.na(match_type)
    )
  
  
  # Exact matches -----------------------------------------------------------
  
  exact_map <- dictionary %>%
    filter(match_type == "exact")
  
  if (nrow(exact_map) > 0) {
    
    for (i in seq_len(nrow(exact_map))) {
      
      matches <- !is.na(x) &
        x == exact_map$pattern[[i]]
      
      x[matches] <- exact_map$taxonomic_name[[i]]
    }
  }
  
  
  # Regex / str_detect matches ---------------------------------------------
  
  regex_map <- dictionary %>%
    filter(match_type == "regex")
  
  if (nrow(regex_map) > 0) {
    
    for (i in seq_len(nrow(regex_map))) {
      
      matches <- !is.na(x) &
        str_detect(
          x,
          regex_map$pattern[[i]]
        )
      
      x[matches] <- regex_map$taxonomic_name[[i]]
    }
  }
  
  x
}


# Run complete species cleaning stage --------------------------------------

clean_species_with_lookup <- function(
    data,
    lookup_dir
) {
  
  # Check input column
  if (!"species_normalized" %in% names(data)) {
    stop(
      "data must contain a 'species_normalized' column."
    )
  }
  
  
  # Read replacement rules
  replacements <- read_csv(
    file.path(
      lookup_dir,
      "species_replacements.csv"
    ),
    show_col_types = FALSE
  )
  
  
  # Read species dictionary
  dictionary <- read_csv(
    file.path(
      lookup_dir,
      "species_dictionary.csv"
    ),
    show_col_types = FALSE
  )
  
  
  # 1. Apply replacement rules
  data <- data %>%
    mutate(
      species_normalized = apply_species_replacements(
        species_normalized,
        replacements
      )
    )
  
  
  # 2–3. Apply species dictionary
  data <- data %>%
    mutate(
      species_normalized = apply_species_dictionary(
        species_normalized,
        dictionary
      )
    )
  
  
  data
}


# Example -------------------------------------------------------------------

# data <- clean_species_with_lookup(
#   data,
#   here(
#     "TerraMatch",
#     "reference",
#     "tree_species"
#   )
# )

# At this point, continue with your existing
# review/fuzzy-matching workflow.