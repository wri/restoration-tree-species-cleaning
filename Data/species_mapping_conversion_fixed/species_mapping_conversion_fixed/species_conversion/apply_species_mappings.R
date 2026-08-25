# Apply species replacement and canonicalization mappings -------------------
# Generated from 01_tree_species_cleaning_2025_11_03.R
# The mapping tables preserve the original rule order.
#
# Workflow:
#   1. Apply ordered regex replacements.
#   2. Apply ordered canonical species mappings.
#   3. Leave unmatched values unchanged.
#
# IMPORTANT:
# The original script contains some broad/semantic rules (for example,
# mapping a common name or genus-level value to a species). Those rules are
# intentionally preserved rather than reinterpreted.

library(dplyr)
library(readr)
library(stringr)

#' Apply the old str_replace_all() replacement rules from the reference table.
#'
#' @param x Character vector of normalized species names.
#' @param lookup Data frame with priority, regex_pattern, replacement.
#' @return Character vector after all ordered replacements.
apply_species_replacements <- function(x, lookup) {
  lookup <- lookup %>% arrange(priority)

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

#' Apply ordered canonical species mappings.
#'
#' Each row represents one original case_when() rule. Regex rules and exact
#' value rules are evaluated against their original source columns.
#'
#' @param data Data frame containing tree_species_name and
#'   species_normalized.
#' @param lookup Mapping table generated from the original case_when().
#' @return Data frame with species_normalized updated.
apply_species_mappings <- function(data, lookup) {
  lookup <- lookup %>% arrange(priority)

  for (i in seq_len(nrow(lookup))) {
    row <- lookup[i, ]

    regex_source <- row$regex_source_column[[1]]
    exact_source <- row$exact_source_column[[1]]
    pattern <- row$regex_pattern[[1]]
    exact_values <- row$exact_values[[1]]
    canonical <- row$canonical_name[[1]]

    # Start with no matches for this rule.
    matched <- rep(FALSE, nrow(data))

    # Regex portion of the original condition.
    if (!is.na(pattern) && nzchar(pattern) &&
        !is.na(regex_source) && nzchar(regex_source)) {
      matched <- matched | str_detect(
        data[[regex_source]],
        pattern
      )
    }

    # Exact-value portion of the original condition.
    if (!is.na(exact_values) && nzchar(exact_values) &&
        !is.na(exact_source) && nzchar(exact_source)) {
      values <- str_split(exact_values, fixed(" || "), simplify = FALSE)[[1]]
      values <- values[nzchar(values)]

      if (length(values) > 0) {
        matched <- matched | data[[exact_source]] %in% values
      }
    }

    # case_when() does not replace NA values unless the condition matches.
    matched[is.na(matched)] <- FALSE

    data$species_normalized[matched] <- canonical
  }

  data
}

#' Run the complete replacement/canonicalization stage.
#'
#' @param data Data frame with species_normalized already normalized.
#' @param lookup_dir Directory containing species_replacements.csv and
#'   species_mappings.csv.
#' @return Data frame with species_normalized updated.
clean_species_with_lookup <- function(data, lookup_dir) {
  replacements <- read_csv(
    file.path(lookup_dir, "species_replacements.csv"),
    show_col_types = FALSE
  )

  mappings <- read_csv(
    file.path(lookup_dir, "species_mappings.csv"),
    show_col_types = FALSE
  )

  data <- data %>%
    mutate(
      species_normalized = apply_species_replacements(
        species_normalized,
        replacements
      )
    )

  apply_species_mappings(data, mappings)
}

# Example -------------------------------------------------------------------
#
# data <- clean_species_with_lookup(
#   data,
#   here("TerraMatch", "reference", "tree_species")
# )
#
# At this point, continue with your existing review/fuzzy-matching workflow.
