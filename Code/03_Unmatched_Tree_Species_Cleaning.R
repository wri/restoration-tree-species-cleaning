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
library(stringi)

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
  mutate(name_clean = tolower(name)) %>%
  arrange(name_clean)

# convert species to Latin-ASCII ------------------------------------------

project_data_clean <- project_data_clean %>%
  mutate(name_clean = stri_trans_general(name_clean, "Latin-ASCII"))

# str_replace_all typos ---------------------------------------------------

# already replaced all "\t" and extra parentheses in Excel

# remove question marks
# remove all numbers and "."
# remove "spp"
# remove whitespace
project_data_clean <- project_data_clean %>%
  mutate(
    name_clean = str_remove_all(name_clean,
                                "\\?"),
    name_clean = str_remove_all(name_clean, "[0-9]"),
    name_clean = str_remove_all(name_clean, "[.]"),
    name_clean = str_remove_all(
      name_clean,
      "spp |\\bspp\\b|\\bsp\\b|\\bspecies\\b|species feb|\\bspecie\\b"
    ),
    name_clean = str_trim(name_clean),
    name_clean = str_squish(name_clean)
  )


# convert NAs -------------------------------------------------------------

project_data_clean <- project_data_clean %>%
  mutate(
    name_clean = case_when(
      name_clean == "" ~ NA_character_,
      name_clean == "n/a" ~ NA_character_,
      str_length(name_clean) == 1 ~ NA_character_,
      name_clean %in% c(
        "wri production",
        "claire shapiro",
        "test",
        "testtt",
        "elizabeth lepage",
        "geospatial workflow update",
        "ignames",
        "species",
        "liza lepage",
        "no non tree",
        "no tree planted",
        "no trees planted",
        "not yet",
        "a",
        "b",
        "c",
        "gl",
        "j",
        "m",
        "p",
        "t",
        "aucune",
        "none",
        "nontree feb",
        "null",
        "feb",
        "umbrellaovacaddo jack fruitmangoes,albizia",
        "maoes"
      ) ~ NA_character_,
      TRUE ~ name_clean
    )
  )

# edit names using str_replace_all ----------------------------------------

project_data_clean <- project_data_clean %>%
  mutate(
    name_clean = str_replace_all(
      name_clean,
      "avocatier|avocat|ovacado|avacado|aavocado|avocadoes|avocados|ovacodos|ovocado|avovado",
      "avocado"
    ),
    name_clean = str_replace_all(name_clean,
                                 "percea|persa|persia|persis",
                                 "persea"),
    name_clean = str_replace_all(name_clean, "acasia|accasia", "acacia"),
    name_clean = str_replace_all(name_clean, "zuzufus|\\bziziph\\b|zizphus", "ziziphus"),
    name_clean = str_replace_all(name_clean, "africam", "african"),
    name_clean = str_replace_all(
      name_clean,
      "gravellia|gravilia|grevelia|grevelea|grebillea|grevellea|grevillia|gravellea|gravillea",
      "grevillea"
    ),
    name_clean = str_replace_all(name_clean, "upaca", "uapaca"),
    name_clean = str_replace_all(name_clean, "ctrucs|ctirus|ctirus|cirtrus", "citrus"),
    name_clean = str_replace_all(name_clean, "polycantha|polycatha", "polyacantha"),
    name_clean = str_replace_all(name_clean, "anthoteca|anthotheka", "anthotheca"),
    name_clean = str_replace_all(
      name_clean,
      "lucena|luceana|lucaena|leauceana|leucena",
      "leucaena"
    )
  )


# case_when str_replace ---------------------------------------------------

project_data_clean <- project_data_clean %>%
  mutate(
    name_clean =
      case_when(
        str_detect(name_clean, "avocado|persea") ~ "persea americana",
        str_detect(
          name_clean,
          "cashew|anacarde anacardium occidentale|anacardium occidentale"
        ) ~ "anacardium occidentale",
        str_detect(
          name_clean,
          "andasonia digitata|adansonia digitata|andansonia digitata|adasonia digitata|anansonia digitata"
        ) ~ "adansonia digitata",
        str_detect(name_clean, "acacia angustissima|acasia angustissima") ~ "acacia angustissima",
        str_detect(name_clean, "bamboo bamboosodea|bamboo bambusodea") ~ "bambusoideae",
        str_detect(
          name_clean,
          "citrus limon|citronier|citronnier|lemon|citroniers|citrus limom|\\blimon\\b"
        ) ~ "citrus limon",
        str_detect(
          name_clean,
          "mangifera indica|mangifera endica|\\bmango\\b|\\bmangoes\\b|\\bmangos\\b"
        ) ~ "mangifera indica",
        str_detect(name_clean, "cassava|casaava") ~ "manihot glaziovii",
        str_detect(name_clean, "orita multicola|achuechue") ~ "orita multicola",
        str_detect(name_clean, "\\bmaos\\b|\\bmaize\\b|\\bmao\\b|\\bmais\\b") ~ "zea mays",
        str_detect(name_clean, "uapaca spp|milanga") ~ "uapaca",
        str_detect(name_clean, "uapaca guineensis") ~ "uapaca guineensis",
        str_detect(name_clean, "soja") ~ "glycine max",
        str_detect(name_clean, "balanities eygptica|balanites aegyptiaca") ~ "balanites aegyptiaca",
        str_detect(
          name_clean,
          "robusta coffee|cafe robuster|cafe robusta|cofea robusta|caffea robusta"
        ) ~ "coffea robusta",
        str_detect(name_clean, "coffea arabica|cofea arabica|cafe arabica") ~ "coffea arabica",
        str_detect(name_clean, "african tulip tree") ~ "spathodea campanulata",
        str_detect(
          name_clean,
          "acajou de bassam|accajou de bassam|khaya ivorensis|khama ivorensis|khya ivorensis|ivorensis"
        ) ~ "khaya ivorensis",
        str_detect(name_clean, "khaya senegalensis") ~ "khaya senegalensis",
        str_detect(name_clean, "swietenia macrophylla") ~ "swietenia macrophylla",
        str_detect(name_clean, "\\bmahogany\\b") ~ "khaya",
        str_detect(
          name_clean,
          "terminalis ivonronsis|terminalia ivorensis|ivory coast almond|tamalia ivorensis"
        ) ~ "terminalia ivorensis",
        str_detect(
          name_clean,
          "terminalia superba|terminalia super|terminalia of superba"
        ) ~ "terminalia superba",
        str_detect(name_clean, "\\btree tomatoes\\b|tamarillo") ~ "solanum betaceum",
        str_detect(
          name_clean,
          "\\btomato\\b|\\btomate\\b|\\btomates\\b|\\btomatoes\\b"
        ) ~ "solanum lycopersicum",
        str_detect(name_clean, "trema orientalis") ~ "trema orientalis",
        str_detect(name_clean, "papaya|pawpaw|paw paw|papayer") ~ "carica papaya",
        str_detect(name_clean, "crossolier|corossolier") ~ "annona muricata",
        str_detect(name_clean, "heritaria utilis") ~ "heritiera utilis",
        str_detect(
          name_clean,
          "melia azedach|melia azerach|china berry|chinaberry"
        ) ~ "melia azedarach",
        str_detect(
          name_clean,
          "isobelina duka|isobelinia duka|isoberlinia duka|isoberlina doka|etenghi"
        ) ~ "isoberlinia doka",
        str_detect(name_clean, "senna siamea") ~ "senna siamea",
        str_detect(name_clean, "bauhinia petersianasian") ~ "bauhinia petersiana",
        str_detect(name_clean, "bamboo") ~ "bamboo",
        str_detect(name_clean, "plantin|plantain") ~ "plantago",
        str_detect(name_clean, "musaceae banana|\\bbananas\\b|\\bbanana\\b") ~ "musa acuminata",
        str_detect(name_clean, "faidherbia albida|faideiherbia albida|musangu") ~ "faidherbia albida",
        str_detect(name_clean, "apples") ~ "malus domestica",
        str_detect(name_clean, "syzgium guineense") ~ "syzgium guineense",
        str_detect(name_clean, "black plum") ~ "syzygium cumini",
        str_detect(name_clean, "swiss chard|beetroot") ~ "beta vulgaris",
        str_detect(name_clean, "\\bdecurrens\\b") ~ "acacia decurrens",
        str_detect(name_clean, "\\bneem\\b") ~ "azadirachita indica",
        str_detect(name_clean, "swatziz madagacahensis|swartzia madagacahensis") ~ "swartzia mangabalensis",
        str_detect(name_clean, "beans|bean - planted in ha") ~ "vicia",
        str_detect(name_clean, "brastchegia spiciformis") ~ "brachystegia spiciformis",
        str_detect(name_clean, "cedrella ordorata") ~ "cedrela odorata",
        str_detect(name_clean, "ciba petando") ~ "ceiba pentandra",
        str_detect(name_clean, "cola acuminata") ~ "cola acuminata",
        str_detect(name_clean, "cola lepidota") ~ "cola lepidota",
        str_detect(name_clean, "petit cola") ~ "garcinia kola",
        str_detect(name_clean, "cola laleritia") ~ "cola laleritia",
        str_detect(name_clean, "cola lepidota|monkey kola") ~ "cola lepidota",
        str_detect(name_clean, "combtryum zeyheri") ~ "combretum zeyheri",
        str_detect(name_clean, "\\bcoton\\b") ~ "croton",
        str_detect(name_clean, "cupressaceae lustanica|cypress lusitanica") ~ "cupressus lusitanica",
        str_detect(name_clean, "cypriot cedar") ~ "cedrus brevifolia",
        str_detect(name_clean, "douglas fir") ~ "pseudotsuga menziesii",
        str_detect(name_clean, "\\beriobotria\\b") ~ "eriobotrya",
        str_detect(name_clean, "erithrina lystermon") ~ "erythrina lysistemon",
        str_detect(name_clean, "eucalyptus camalnd") ~ "eucalyptus camaldulensis",
        str_detect(name_clean, "eucalnus saligna") ~ "eucalyptus saligna",
        str_detect(name_clean, "eucalyptus globulus|teucalyptus globules") ~ "eucalyptus globulus",
        str_detect(name_clean, "ficus vallis choudea") ~ "ficus vallis-choudae",
        str_detect(name_clean, "gliricirdia sepuim") ~ "gliricidia sepium",
        str_detect(
          name_clean,
          "arachis hypogaea|ground nuts|groundnuts|gnuts|arachide|arachides|arachys hypogea"
        ) ~ "arachis hypogaea",
        str_detect(name_clean, "jackfruit|jack fruit") ~ "artocarpus heterophyllus",
        str_detect(name_clean, "vitellaria paradoxa|vitellariat paradoxa") ~ "vitellaria paradoxa",
        str_detect(
          name_clean,
          "leucaena leucocephala|luceana leucocephala|leuceana leucocephala"
        ) ~ "leucaena leucocephala",
        str_detect(name_clean, "mandariniers|mandarins|citrus reticula") ~ "citrus reticulata",
        str_detect(
          name_clean,
          "citrus sinensis|citrus x sinensis|citrus ◊ sinensis|\\borange\\b|\\boranges\\b|\\boranger\\b|\\borangers\\b"
        ) ~ "citrus sinensis",
        str_detect(name_clean, "mansonia altissima") ~ "mansonia altissima",
        str_detect(
          name_clean,
          "measopsis eminni|maesopsis e|maesopses eminii|maesospsis eminii|musizi"
        ) ~ "maesopsis eminii",
        str_detect(name_clean, "millitia ferruginol") ~ "millettia ferruginea",
        str_detect(name_clean, "mitragyna librostipulosa") ~ "mitragyna stipulosa",
        str_detect(
          name_clean,
          "moringa oleifera|moringa oleifer|moringa oleifere"
        ) ~ "moringa oleifera",
        str_detect(name_clean, "morus alba") ~ "morus alba",
        str_detect(name_clean, "parkia biglobosa") ~ "parkia biglobosa",
        str_detect(name_clean, "parkia bicolor|parquia biclor") ~ "parkia bicolor",
        str_detect(
          name_clean,
          "olea europaea sub cuspidate|olea europaea subsp cuspidata"
        ) ~ "olea africana",
        str_detect(name_clean, "elaeis guineensis") ~ "elaeis guineensis",
        str_detect(name_clean, "passion fruits") ~ "passiflora edulis",
        str_detect(name_clean, "philosyigma thonnongi") ~ "piliostigma thonningii",
        str_detect(name_clean, "pidgeon pea|pigeon pea") ~ "cajanus cajan",
        str_detect(name_clean, "mkpen|pterocarpus erinaceus") ~ "pterocarpus erinaceus",
        str_detect(name_clean, "mimuspos kummel") ~ "mimuspos kummel",
        str_detect(name_clean, "pear packham's triumph") ~ "pyrus communis",
        str_detect(name_clean, "\\bpumpkin\\b|\\bpumpkins\\b|\\bcourge\\b") ~ "cucurbita",
        str_detect(name_clean, "rahminus prinoides") ~ "rhamnus prinoides",
        str_detect(name_clean, "ricinodondron heudeleuti") ~ "ricinodendron heudelotii",
        str_detect(name_clean, "sausage tree") ~ "kigelia africana",
        str_detect(name_clean, "senna siamea") ~ "semia siamea",
        str_detect(name_clean, "sorghum moench|sorghum mohench") ~ "sorghum bicolor",
        str_detect(name_clean, "le sorg|sorgho") ~ "sorghum",
        str_detect(name_clean, "sunflowers") ~ "helianthus annuus",
        str_detect(name_clean, "sweet potato") ~ "ipomoea batatas",
        str_detect(name_clean, "\\bpotato\\b") ~ "solanum tuberosum",
        str_detect(name_clean, "mbogabuchungu|mbogabughungu|mbogabushungu") ~ "solanum nigrum",
        str_detect(name_clean, "vetiver grass") ~ "chrysopogon zizanioides",
        str_detect(name_clean, "triplochiton scleroxylon|ayous") ~ "triplochiton scleroxylon",
        str_detect(name_clean, "dioscorea") ~ "dioscorea",
        str_detect(name_clean, "lucern") ~ "medicago sativa",
        str_detect(name_clean, "khaya senegalensis") ~ "khaya senegalensis",
        str_detect(name_clean, "khaya anthotheca") ~ "khaya anthotheca",
        str_detect(name_clean, "irvingia gabonensis") ~ "irvingia gabonensis",
        str_detect(name_clean, "ricinodendron heudelotii") ~ "ricinodendron heudelotii",
        str_detect(name_clean, "tieghemella heckelii") ~ "tieghemella heckelii",
        str_detect(
          name_clean,
          "cocoa|theobroma cacao|cacao|theobroma caoa|theobroma cocoa"
        ) ~ "theobroma cocao",
        str_detect(name_clean, "coconuts|coccos nucifera|cocos nucifera") ~ "cocos nucifera",
        str_detect(name_clean, "pomagranate|pomegranate") ~ "punica granatum",
        str_detect(name_clean, "busala tuber") ~ "dioscorea hirtiflora",
        str_detect(name_clean, "cabbage|kale|brocoli") ~ "brassica oleracea",
        str_detect(name_clean, "grevillea robusts|grevillea robusta") ~ "grevillea robusta",
        str_detect(name_clean, "theka|teak") ~ "tectona grandis",
        str_detect(name_clean, "cola boxiana") ~ "cola boxiana",
        str_detect(name_clean, "\\bpepper\\b") ~ "capsicum annum",
        str_detect(name_clean, "\\bbirch\\b") ~ "betula",
        str_detect(name_clean, "ficus sycamora") ~ "ficus Sycomorus",
        str_detect(name_clean, "celery") ~ "apium graveolens",
        str_detect(name_clean, "grewia bicolar") ~ "grewia bicolor",
        str_detect(name_clean, "maple") ~ "acer",
        str_detect(name_clean, "haricot") ~ "phaseolus vulgaris",
        str_detect(
          name_clean,
          "leucaena leucocephala|leucaena lecocephala|leucaena leucecephala|leucaena leucocephala|leucaena leucocephiala|leucaena leucpcephala"
        ) ~ "leucaena leucocephala",
        str_detect(name_clean, "manioc|manihot esculenta") ~ "manihot esculenta",
        str_detect(name_clean, "markhamia ob") ~ "markhamia obtusifolia",
        str_detect(name_clean, "vigna unguiculata|niebe") ~ "vigna unguiculata",
        str_detect(name_clean, "rosemary") ~ "salvia rosmarinus",
        str_detect(
          name_clean,
          "prekese|tetrapleura tetraptera|tetrapleura tetrapetra"
        ) ~ "tetrapleura tetraptera",
        str_detect(name_clean, "spinach") ~ "spinacia oleracea",
        str_detect(name_clean, "colocasia esculenta") ~ "colocasia esculenta",
        str_detect(name_clean, "ricinodendron heudoletii") ~ "ricinodendron heudoletii",
        str_detect(name_clean, "\\bananas\\b") ~ "ananas comusus",
        str_detect(name_clean, "calotropis procera") ~ "calotropis procera",
        str_detect(name_clean, "eucalyptus globules seedlings|eucalyptus glaulus|eucalyptus globlus|eucalyptus globules") ~ "eucalyptus globulus",
        str_detect(name_clean, "guajava|psidium guajava|guava") ~ "psidium guajava",
        str_detect(name_clean, "eucalyptus mannifera") ~ "eucalyptus mannifera",
        str_detect(name_clean, "amaranth|lengalenga") ~ "amaranth",
        str_detect(name_clean, "\\bwillow\\b") ~ "salix",
        str_detect(name_clean, "casimiroa edulis") ~ "casimiroa edulis",
        TRUE ~ name_clean
      )
  )

# now start sifting through the matches, filtering fuzzy joins

# species I am unsure of:

# grave?
# riz
# Tsimitetra
# le ma
# mol
# totame (this might be a mispelling of tomate)
# hazombato
# matembela
# mbogabuchungu
# molucata
# mil
# wild crippers
# gombo
# miombo
# tavia
# le ha
# gbaji
# local grasses
# maoes
# mwanga
# poivrier

# explore fuzzy matches for accuracy -------------------------------------

fuzzy_matched_project_data <- all_matched_project_data %>%
  filter(fuzzy == TRUE) %>%
  group_by(name_orig) %>%
  slice(1) %>%
  ungroup() %>%
  arrange(scientific_name)

# extract data from within parentheses ------------------------------------

project_data_clean <- project_data_clean %>%
  mutate(
    extracted_name = str_extract(name_clean, "(?<=\\().*?(?=\\))"),
    extracted_name = str_remove_all(extracted_name, "\\("),
    extracted_name = str_trim(extracted_name),
    extracted_name = str_squish(extracted_name)
  ) %>%
  arrange(name_clean)


# grouped data ------------------------------------------------------------

grouped_project_data_clean <- project_data_clean %>%
  group_by(name_clean) %>%
  slice(1) %>%
  ungroup() %>%
  select(name_clean)

grouped_unmatched_data <- all_unmatched_project_data %>%
  mutate(name_orig = tolower(name_orig)) %>%
  group_by(name_orig) %>%
  slice(1) %>%
  ungroup() %>%
  select(name_orig)
