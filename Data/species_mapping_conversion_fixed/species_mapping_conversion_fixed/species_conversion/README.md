# Species mapping conversion

These files were generated from `01_tree_species_cleaning_2026_08_24.R`.

## Files

- `species_replacements.csv` — the 14 ordered `str_replace_all()` rules from the species-editing section.
- `species_dictionary.csv` — the 762 non-default rules extracted from the large `case_when()` block. 
- `apply_species_mappings.R` — functions that apply the two tables in the same order as the original script.

## Recommended project layout

```text
TerraMatch/
  reference/
    tree_species/
      species_replacements.csv
      species_dictionary.csv
      apply_species_mappings.R
```

## Use

Source the helper and run:

```r
source(here(
  "TerraMatch", "reference", "tree_species", "apply_species_mappings.R"
))

data <- clean_species_with_lookup(
  data,
  here("TerraMatch", "reference", "tree_species")
)
```

## Important

The conversion preserves the original rule order and conditions as closely as possible. The mapping source columns have been adapted to the new pipeline and now use `species_normalized` (the original `tree_species_name_clean` references are retained only inside `original_condition` for audit purposes). It does **not** reinterpret broad semantic mappings. Those mappings remain in the lookup table so they can be reviewed and eventually assigned metadata such as `rule_type`, `source`, `approved`, and `notes`.

The `original_condition` column in `species_mappings.csv` is retained as an audit trail back to the original R rule.
