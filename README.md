This repository contains R scripts and data for cleaning tree species data for tree species in TerraMatch.

One script is cleaning the tree species data entered by Champions so it can match to the tree species backbone provided by the World Flora Organization (WFO) and the World Checklist of Vascular Plants (WCVP)

The other script follows the RPub series (https://rpubs.com/Roeland-KINDT/1134151) published by Roeland Kindt at ICRAF to match the raw tree species data to the two backbones, using the WorldFlora R Package.

For more information on how the WFO.match function and the WorldFlora package work, please see this paper (https://bsapubs.onlinelibrary.wiley.com/doi/full/10.1002/aps3.11388).

For additional context on this exercise, please see the concept note.

The data for the backbones are too large to post on GitHub, so please download them here:
  - WFO backbone: https://zenodo.org/records/12171908 - as of August 2026, we have used the June 2024 version (Version 2024-06).
  - WCVP backbone: https://www.checklistbank.org/dataset/gbif-f382f0ce-323a-4091-bb9f-add557f3a9a2/download (account setup required). The 'names' file is what is needed
