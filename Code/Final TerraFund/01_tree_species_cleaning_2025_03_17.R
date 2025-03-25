# Description -------------------------------------------------------------

# Author: Ben Steiger
# Date Created: 07/03/2024
# Last Updated: 03/14/2025
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
# terrafund project report

#data <- read.csv(
#  here(
#    "Tree Species",
#    "Data",
#    "Raw",
#    "TerraFund Tree Species",
#    "terrafund_tree_species_202410301559.csv"
#  ),
#  encoding = "UTF-8"
#)

# terrafund project establishment

#data <- read.csv(
#  here(
#    "Tree Species",
#    "Data",
#    "Raw",
#    "TerraFund Tree Species",
#    "terrafund_tree_species_project_establisment_202412121125.csv"
#  ),
#  encoding = "UTF-8"
#)

#ppc project report

#data <- read.csv(
#  here(
#    "Tree Species",
#    "Data",
#    "Raw",
#    "PPC Tree Species",
#    "ppc_project_tree_species_202412231755.csv"
#  ),
#  encoding = "UTF-8"
#)

#ppc project establishment

#data <- read.csv(
#  here(
#    "Tree Species",
#    "Data",
#    "Raw",
#    "PPC Tree Species",
#    "ppc_tree_species_project_establishment_202412131257.csv"
#  ),
#  encoding = "UTF-8"
#)

# all site report data, NULL taxon ID

site_report_data <- read.csv(
  here(
    "Tree Species",
    "Data",
    "Raw",
    "Sites",
    "site_reports_tree_species_null_taxon_id_202503171255.csv"
  ),
  encoding = "UTF-8"
) 

# all site data, NULL taxon ID

site_data <- read.csv(
  here(
    "Tree Species",
    "Data",
    "Raw",
    "Sites",
    "sites_tree_species_null_taxon_id_202503171254.csv"
  ),
  encoding = "UTF-8"
)

# all nursery data, NULL taxon ID

nursery_data <- read.csv(
  here(
    "Tree Species",
    "Data",
    "Raw",
    "Nurseries",
    "nurseries_tree_species_null_taxon_id_202503171254.csv"
  ),
  encoding = "UTF-8"
)

# all nursery report data, NULL taxon ID

nursery_report_data <- read.csv(
  here(
    "Tree Species",
    "Data",
    "Raw",
    "Nurseries",
    "nursery_reports_tree_species_null_taxon_id_202503171254.csv"
  ),
  encoding = "UTF-8"
) 

# all project data, NULL taxon ID

project_data <- read.csv(
  here(
    "Tree Species",
    "Data",
    "Raw",
    "Projects",
    "projects_tree_species_null_taxon_id_202503171235.csv"
  ),
  encoding = "UTF-8"
) 

# all project report data, NULL taxon ID

project_report_data <- read.csv(
  here(
    "Tree Species",
    "Data",
    "Raw",
    "Projects",
    "project_reports_tree_species_null_taxon_id_202503171235.csv"
  ),
  encoding = "UTF-8"
) 

# bind data ---------------------------------------------------------------

data <-
  bind_rows(site_report_data,
        site_data,
        nursery_data,
        nursery_report_data,
        project_data,
        project_report_data) 

# convert data columns to snake_case ------------------------------

names(data) <-
  to_snake_case(names(data))

# convert to dataframe ----------------------------------------------------

data <- as.data.frame(data)

# Correct accents ---------------------------------------------------------

# Define a function to correct accents
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
data <- data %>%
  mutate(across(where(is.character), correct_accents))

# convert species to Latin-ASCII, create new column ------------------------------------------

data <- data %>%
  mutate(
    tree_species_name_clean = stri_trans_general(tree_species_name, "Latin-ASCII"),
    tree_species_name_clean = tolower(tree_species_name_clean)
  ) %>%
  arrange(tree_species_name_clean)

# drop test projects ------------------------------------------------------

#data <- data %>%
#  filter(
#    !project_name %in% c(
#      "3SC Production 2.3",
#      "Thef",
#      "1",
#      "Landscapes test Sept2024",
#      "TerraFund Landscapes test project",
#      "GG Smoke Test edit",
#      ""
#    )
#  )

data <- data %>%
  filter(
    !project_name %in% c(
      "3SC Production 2.3 - PPC PROJECT",
      "Claire Test 72324",
      "DUMMY PROJECT PPC",
      "Acacia PPC test",
      "ppc test 0709",
      "test 0726"
    )
  )

# str_replace_all typos ---------------------------------------------------

# already replaced all "\t" and extra parentheses in Excel

# remove question marks
# remove all numbers and "."
# remove "spp"
# remove whitespace
data <- data %>%
  mutate(
    tree_species_name_clean = str_remove_all(tree_species_name_clean, "\\\\t"),
    tree_species_name_clean = str_remove_all(tree_species_name_clean, "\\\\"),
    tree_species_name_clean = str_remove_all(tree_species_name_clean, "\\.\\\\"),
    tree_species_name_clean = str_remove_all(tree_species_name_clean, "\\?\\t"),
    tree_species_name_clean = str_remove_all(tree_species_name_clean, "\\?"),
    tree_species_name_clean = str_remove_all(tree_species_name_clean, "[.]"),
    tree_species_name_clean = str_remove_all(tree_species_name_clean, "[0-9]"),
    tree_species_name_clean = str_remove_all(tree_species_name_clean, "º"),
    tree_species_name_clean = str_remove_all(
      tree_species_name_clean,
      "spp |\\bspp\\b|\\bsp\\b|\\bspecies\\b|species feb|\\bspecie\\b|\\bssp\\b"
    ),
    tree_species_name_clean = str_remove_all(tree_species_name_clean, ""),
    tree_species_name_clean = str_replace_all(tree_species_name_clean, "’", "'"),
    tree_species_name_clean = str_remove_all(tree_species_name_clean, "''"),
    tree_species_name_clean = str_remove_all(tree_species_name_clean, "'"),
    tree_species_name_clean = str_remove_all(tree_species_name_clean, '"'),
    tree_species_name_clean = str_remove_all(tree_species_name_clean, ":"),
    tree_species_name_clean = str_trim(tree_species_name_clean),
    tree_species_name_clean = str_squish(tree_species_name_clean)
  )

# convert NAs -------------------------------------------------------------

# some of the below names were removed already from dropping test projects, but keeping in

data <- data %>%
  mutate(
    tree_species_name_clean = case_when(
      tree_species_name_clean == "" ~ NA_character_,
      tree_species_name_clean == "n/a" ~ NA_character_,
      str_length(tree_species_name_clean) == 1 ~ NA_character_,
      tree_species_name_clean %in% c(
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
        "no plating took place",
        "plantation",
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
        "vegetables",
        "local grasses",
        "le ha",
        "le ma",
        "mil",
        "miombo",
        "assorted fruits",
        "dombea has been removed due to their seeds availability in project area (this was discussed with project manager",
        "mixed indigenous",
        "other fruit and timber trees",
        "unidentified trees and shrubs",
        "tukumombo",
        "trees",
        "quirisi-a",
        "planting data for november 2022",
        "other sp",
        "xxx",
        "other",
        "add species name"
      ) |
        str_detect(tree_species_name_clean, "wild crippers") ~ NA_character_,
      TRUE ~ tree_species_name_clean
    )
  )

# edit names using str_replace_all ----------------------------------------

data <- data %>%
  mutate(
    tree_species_name_clean = str_replace_all(
      tree_species_name_clean,
      "avocatier|avocat|ovacado|avacado|aavocado|avocadoes|avocados|ovacodos|ovocado|avovado|avcatiers",
      "avocado"
    ),
    tree_species_name_clean = str_replace_all(
      tree_species_name_clean,
      "percea|persa|persia|persis|\\bperisa\\b|\\bparcea\\b|\\bpercia\\b",
      "persea"
    ),
    tree_species_name_clean = str_replace_all(tree_species_name_clean, "acasia|accasia", "acacia"),
    tree_species_name_clean = str_replace_all(
      tree_species_name_clean,
      "zuzufus|\\bziziph\\b|zizphus",
      "ziziphus"
    ),
    tree_species_name_clean = str_replace_all(tree_species_name_clean, "africam", "african"),
    tree_species_name_clean = str_replace_all(
      tree_species_name_clean,
      "gravellia|gravilia|grevelia|grevelea|grebillea|grevellea|grevillia|gravellea|gravillea|\\bgrave\\b|gravellae|greveillea|grevellia|greveria|grevilia|grivellea|grivelea",
      "grevillea"
    ),
    tree_species_name_clean = str_replace_all(tree_species_name_clean, "upaca", "uapaca"),
    tree_species_name_clean = str_replace_all(
      tree_species_name_clean,
      "ctrucs|ctirus|ctirus|cirtrus|citrus variaties",
      "citrus"
    ),
    tree_species_name_clean = str_replace_all(
      tree_species_name_clean,
      "polycantha|polycatha",
      "polyacantha"
    ),
    tree_species_name_clean = str_replace_all(
      tree_species_name_clean,
      "anthoteca|anthotheka",
      "anthotheca"
    ),
    tree_species_name_clean = str_replace_all(tree_species_name_clean, "eriobotria", "eriobotrya"),
    tree_species_name_clean = str_replace_all(tree_species_name_clean, "warbugia", "warburgia"),
    tree_species_name_clean = str_replace_all(
      tree_species_name_clean,
      "lucena|luceana|lucaena|leauceana|leucena|leucaaena",
      "leucaena"
    ),
    tree_species_name_clean = str_replace_all(
      tree_species_name_clean,
      "\\babizia\\b|albizzia|albisia",
      "albizia"
    )
  )

# case_when str_replace ---------------------------------------------------

data <- data %>%
  mutate(
    tree_species_name_clean =
      case_when(
        str_detect(
          tree_species_name_clean,
          "elaeis guineensis|\\bpalmia\\b|palm tree"
        ) ~ "elaeis guineensis",
        str_detect(tree_species_name_clean, "avocado|persea") ~ "persea americana",
        str_detect(
          tree_species_name_clean,
          "cashew|anacarde anacardium occidentale|anacardium occidentale|\\banacardium\\b"
        ) ~ "anacardium occidentale",
        str_detect(
          tree_species_name_clean,
          "andasonia digitata|adansonia digitata|andansonia digitata|adasonia digitata|anansonia digitata|andosonia digitata"
        )
        | tree_species_name_clean %in% c("baobab")
        ~ "adansonia digitata",
        str_detect(tree_species_name_clean, "adansonia grandidieri") ~ "adansonia grandidieri",
        str_detect(
          tree_species_name_clean,
          "acacia angustissima|acasia angustissima|acacia anguistissima|acacia angustima"
        ) ~ "acaciella angustissima",
        tree_species_name_clean %in% c("a thomsonii") ~ "acacia thomsonii",
        tree_species_name_clean %in% c("abrevispica") ~ "acacia brevispica",
        str_detect(tree_species_name_clean, "\\bdecurrens\\b") ~ "acacia decurrens",
        tree_species_name_clean %in% c("acacia aborea") ~ "acacia",
        str_detect(tree_species_name_clean, "acacia saligina|akesia saligina") ~ "acacia saligna",
        str_detect(
          tree_species_name_clean,
          "molucata|acacia mangium|acacia magnum"
        ) |
          tree_species_name_clean %in% c("mol") ~ "acacia mangium",
        str_detect(
          tree_species_name_clean,
          "acacia auriculoformis|ear leaf acacia"
        ) ~ "acacia auriculoformis",
        str_detect(tree_species_name_clean, "acacia geradii|acacia gerrardii") ~ "acacia gerrardii",
        tree_species_name_clean %in% c("anilotica") |
          str_detect(tree_species_name_clean, "acacia nilotica") ~ "acacia nilotica",
        tree_species_name_clean %in% c("apolyacantha") ~ "acacia polyacantha",
        tree_species_name_clean %in% c("asenegal") ~ "senegalia senegal",
        str_detect(
          tree_species_name_clean,
          "umbrella thorn|umbrella tree|umbrella invoresia|acacia tortilis|vachellia tortilis|acacia tortalis|acacia tortillis|acaciia tortilis|vachelia tortilis"
        ) |
          tree_species_name_clean %in% c("umbrella", "a tortilis") ~ "vachellia tortilis",
        tree_species_name_clean %in% c(
          "acacia xanthophloea",
          "acascia xanthropholea",
          "axanthophloea"
        ) |
          str_detect(tree_species_name_clean, "acacia xanthophloea") ~ "vachellia xanthophloea",
        str_detect(tree_species_name_clean, "acacia galpinii") ~ "senegalia galpinii",
        str_detect(
          tree_species_name_clean,
          "black thorn|honey acacias|acacia mellifera"
        ) ~ "senegalia mellifera",
        str_detect(
          tree_species_name_clean,
          "albizia gumifera|albizia gummifera|albizia gummifere"
        ) ~ "albizia gummifera",
        str_detect(
          tree_species_name_clean,
          "arundinaria alpine|arundinaria alpina"
        ) ~ "arundinaria alpina",
        str_detect(tree_species_name_clean, "bamboo yushania|yushania alpina") ~ "yushania alpina",
        str_detect(
          tree_species_name_clean,
          "bambousa vulgaris|bambusa vulgaris|bambous vulgaris|bambusa vugaris|bambusa vurlgaris"
        ) ~ "bambusa vulgaris",
        str_detect(tree_species_name_clean, "dendrocalamus asper") ~ "dendrocalamus asper",
        str_detect(
          tree_species_name_clean,
          "bamboo bamboosodea|bamboo bambusodea|bambusa cuttings|bambusoideae"
        ) |
          tree_species_name_clean %in% c("bamboussa", "bambous \\( \\)") ~ "bambusa",
        tree_species_name_clean %in% c("bamboo", "bamboos", "indigenous bamboo", "bamboo trees") ~ "bamboo",
        str_detect(
          tree_species_name_clean,
          "citrus limon|citronier|citronnier|lemon|citroniers|citrus limom|\\blimon\\b"
        ) ~ "citrus limon",
        str_detect(tree_species_name_clean, "citrus aurantifolia") ~ "citrus aurantifolia",
        str_detect(
          tree_species_name_clean,
          "mandariniers|mandarins|citrus reticula|mandalin"
        ) ~ "citrus reticulata",
        str_detect(
          tree_species_name_clean,
          "citrus sinensis|citrus x sinensis|citrus ◊ sinensis|\\borange\\b|\\boranges\\b|\\boranger\\b|\\borangers\\b|citrus cinensis|orangiers"
        ) ~ "citrus sinensis",
        tree_species_name_clean %in% c("grape fruit") ~ "citrus paridisi",
        str_detect(
          tree_species_name_clean,
          "mangifera indica|mangifera endica|mangifer endica|\\bmango\\b|\\bmangoes\\b|\\bmangos\\b|manjifera indidca|manguier|manguifera indica"
        ) |
          tree_species_name_clean %in% c("mangifera") ~ "mangifera indica",
        str_detect(tree_species_name_clean, "cassava|casaava") ~ "manihot glaziovii",
        str_detect(tree_species_name_clean, "orita multicola|achuechue") ~ "lannea welwitschii",
        str_detect(
          tree_species_name_clean,
          "\\bmaos\\b|\\bmaize\\b|\\bmao\\b|\\bmais\\b|\\bmaoes\\b|\\bzea\\b|\\bmai\\b"
        ) ~ "zea mays",
        str_detect(tree_species_name_clean, "uapaca spp|milanga") ~ "uapaca",
        str_detect(tree_species_name_clean, "uapaca guineensis|misela") ~ "uapaca guineensis",
        str_detect(tree_species_name_clean, "uapaca kirkiana") ~ "uapaca kirkiana",
        str_detect(tree_species_name_clean, "soja") ~ "glycine max",
        str_detect(
          tree_species_name_clean,
          "balanities eygptica|balanites aegyptiaca|balanites egyptica|balanites egyptiaca"
        ) ~ "balanites aegyptiaca",
        tree_species_name_clean %in% c("ballantines") ~ "balanites",
        str_detect(
          tree_species_name_clean,
          "robusta coffee|cafe robuster|cafe robusta|cofea robusta|caffea robusta"
        ) ~ "coffea robusta",
        str_detect(
          tree_species_name_clean,
          "coffea arabica|cofea arabica|cafe arabica|coffeia arabica|coffe arabiga|coffe arabica|coffea arab|coffea arabic|coffea arabiga|coffea arabira|coffee arabica|coffefa arabica|coffie arabica"
        ) ~ "coffea arabica",
        tree_species_name_clean %in% c("cafe", "coffee", "coffea") ~ "coffea",
        str_detect(
          tree_species_name_clean,
          "african tulip tree|spathodea campanulata|african tulip|spatodea campanulata"
        ) ~ "spathodea campanulata",
        str_detect(
          tree_species_name_clean,
          "terminalis ivonronsis|terminalia ivorensis|ivory coast almond|tamalia ivorensis|gbaji|\\bemire\\b|\\bframiré\\b|\\bframire\\b"
        ) ~ "terminalia ivorensis",
        str_detect(
          tree_species_name_clean,
          "acajou de bassam|accajou de bassam|khaya ivorensis|khama ivorensis|khya ivorensis|ivorensis"
        ) ~ "khaya ivorensis",
        str_detect(tree_species_name_clean, "khaya senegalensis") ~ "khaya senegalensis",
        str_detect(tree_species_name_clean, "swietenia macrophylla") |
          tree_species_name_clean %in% c("caoba") ~ "swietenia macrophylla",
        str_detect(tree_species_name_clean, "swietenia mahogani") ~
          "swietenia mahogani",
        str_detect(tree_species_name_clean, "khaya anthotheca|khaya anthethe") ~ "khaya anthotheca",
        str_detect(tree_species_name_clean, "khaya grandifolia") ~ "khaya grandifolia",
        str_detect(tree_species_name_clean, "khaya senegalensis") ~ "khaya senegalensis",
        str_detect(tree_species_name_clean, "\\bmahogany\\b|\\bmahagany\\b") ~ "khaya",
        str_detect(
          tree_species_name_clean,
          "terminalia superba|terminalia super|terminalia of superba"
        ) |
          tree_species_name_clean %in% c("frake", "ofram") ~ "terminalia superba",
        str_detect(tree_species_name_clean, "\\btree tomatoes\\b|tamarillo") ~ "solanum betaceum",
        str_detect(
          tree_species_name_clean,
          "\\btomato\\b|\\btomate\\b|\\btomates\\b|\\btomatoes\\b|\\btotame\\b|\\btomaotoes\\b"
        ) ~ "solanum lycopersicum",
        str_detect(tree_species_name_clean, "trema orientalis") ~ "trema orientalis",
        str_detect(tree_species_name_clean, "papaya|pawpaw|paw paw|papayer") ~ "carica papaya",
        str_detect(tree_species_name_clean, "crossolier|corossolier|guanabana") ~ "annona muricata",
        str_detect(tree_species_name_clean, "annona cherimola") ~ "annona cherimola",
        str_detect(tree_species_name_clean, "custard apple|annona squamosa") ~ "annona squamosa",
        str_detect(
          tree_species_name_clean,
          "heritaria utilis|haritaria utilis|heritaria utlis"
        ) ~ "heritiera utilis",
        str_detect(
          tree_species_name_clean,
          "melia azedach|melia azerach|china berry|chinaberry"
        ) ~ "melia azedarach",
        str_detect(tree_species_name_clean, "melia volkensii|melia volkensi")
        |
          tree_species_name_clean %in% c("melia") ~ "melia volkensii",
        str_detect(
          tree_species_name_clean,
          "isobelina duka|isobelinia duka|isoberlinia duka|isoberlina doka|etenghi"
        ) ~ "isoberlinia doka",
        str_detect(
          tree_species_name_clean,
          "senna siamea|senna enamea|senna senamia|cacia siamea"
        ) ~ "senna siamea",
        str_detect(tree_species_name_clean, "bauhinia petersianasian") ~ "bauhinia petersiana",
        str_detect(tree_species_name_clean, "plantin|plantain|plaintain") ~ "musa x paradisiaca",
        str_detect(
          tree_species_name_clean,
          "musaceae banana|\\bbananas\\b|\\bbanana\\b"
        ) ~ "musa acuminata",
        str_detect(
          tree_species_name_clean,
          "faidherbia albida|faideiherbia albida|musangu|faiderbia albida"
        ) ~ "faidherbia albida",
        tree_species_name_clean %in% c("apple", "apples") |
          str_detect(tree_species_name_clean, "malus domestica")  ~ "malus domestica",
        str_detect(
          tree_species_name_clean,
          "syzgium guineense|syzygium guineese|syzygium quinensi|syzigium guineines|syzygium guineense"
        ) ~ "syzygium guineense",
        str_detect(tree_species_name_clean, "syzygium cordatum|syzgium cordatum") |
          tree_species_name_clean %in% c("water berry") ~ "syzygium cordatum",
        str_detect(tree_species_name_clean, "syzygium zeyheri") ~ "eugenia zeyheri",
        str_detect(
          tree_species_name_clean,
          "syzygium cumini|sygizium cumini|zysygium cuminii"
        ) ~ "syzygium cumini",
        str_detect(tree_species_name_clean, "vitex payos") ~ "vitex payos",
        str_detect(tree_species_name_clean, "swiss chard|beetroot") ~ "beta vulgaris",
        str_detect(
          tree_species_name_clean,
          "\\bneem\\b|axadirachta|azadirochata indica|azadirachta indica"
        ) |
          tree_species_name_clean %in% c("azadirachita", "azadirachta") ~ "azadirachita indica",
        str_detect(
          tree_species_name_clean,
          "swatziz madagacahensis|swartzia madagacahensis"
        ) ~ "swartzia mangabalensis",
        str_detect(tree_species_name_clean, "beans|bean - planted in ha") ~ "vicia",
        str_detect(
          tree_species_name_clean,
          "brastchegia spiciformis|brachystegia spiciformis"
        ) ~ "brachystegia spiciformis",
        str_detect(
          tree_species_name_clean,
          "cedrella ordorata|cedrella odorata|cedrela red cedar|cedrela odorata"
        ) |
          tree_species_name_clean %in% c("cedro") ~ "cedrela odorata",
        str_detect(tree_species_name_clean, "cedrela angusifolia") ~ "cedrela angustifolia",
        tree_species_name_clean %in% c(
          "cedrela sililata",
          "cedrella serrata",
          "cedrela serrata",
          "cedrela cellulafera"
        ) ~ "cedrela serrata",
        str_detect(tree_species_name_clean, "cedrela fissilis") ~ "cedrela fissilis",
        tree_species_name_clean %in% c("cedrella siensis") ~ "toona sinensis",
        str_detect(
          tree_species_name_clean,
          "ciba patendo|ceiba pentandra|ceiba pendratra|ceiba petandra|cieba pentandra"
        ) ~ "ceiba pentandra",
        str_detect(tree_species_name_clean, "ceiba glaziovii") ~ "ceiba glaziovii",
        str_detect(tree_species_name_clean, "ceiba speciosa") ~ "ceiba speciosa",
        str_detect(tree_species_name_clean, "cola boxiana") ~ "cola boxiana",
        str_detect(tree_species_name_clean, "cola acuminata") ~ "cola acuminata",
        str_detect(tree_species_name_clean, "petit cola|small cola") ~ "garcinia kola",
        str_detect(tree_species_name_clean, "cola laleritia") ~ "cola laleritia",
        str_detect(tree_species_name_clean, "cola lepidota|monkey kola") ~ "cola lepidota",
        str_detect(tree_species_name_clean, "cola coche|cola de coche") ~ "cola",
        str_detect(tree_species_name_clean, "combtryum zeyheri") ~ "combretum zeyheri",
        str_detect(tree_species_name_clean, "\\bcoton\\b") | tree_species_name_clean %in% "croton spp" ~ "croton",
        str_detect(
          tree_species_name_clean,
          "croton megalocarpus|croton wood|croton megalorcarpus|croton mergalorcarpus|croton megalocapus|croton megalocurpus"
        ) ~ "croton megalocarpus",
        str_detect(tree_species_name_clean, "crtoton mycrostachus") ~ "croton macrostachyus",
        str_detect(
          tree_species_name_clean,
          "cupressaceae lustanica|cypress lusitanica|cupressus lusitanica|cupressaceae lusitanica|cupresus lustanica|cypressus lustanica|cyprus lusitanica"
        ) ~ "cupressus lusitanica",
        tree_species_name_clean %in% c("cipres") ~ "cupressus",
        str_detect(tree_species_name_clean, "cypriot cedar") ~ "cedrus brevifolia",
        str_detect(tree_species_name_clean, "cedrus cedar") |
          tree_species_name_clean %in% c("cedar") ~ "cedrus",
        str_detect(tree_species_name_clean, "douglas fir|douglas-fir") ~ "pseudotsuga menziesii",
        str_detect(tree_species_name_clean, "erithrina lystermon") ~ "erythrina lysistemon",
        str_detect(
          tree_species_name_clean,
          "eucalyptus camalnd|eucalyptus camalandis|eucalyptus camandulensis"
        ) ~ "eucalyptus camaldulensis",
        str_detect(tree_species_name_clean, "eucalnus saligna") ~ "eucalyptus saligna",
        str_detect(tree_species_name_clean, "eucalyptus/gum trees") |
          tree_species_name_clean %in% c("eaucalyptus", "eucaryptus", "eucalyptus sp") ~ "eucalyptus",
        str_detect(
          tree_species_name_clean,
          "eucalyptus globules seedlings|eucalyptus glaulus|eucalyptus globlus|eucalyptus globules|eucalyptus globulus|teucalyptus globules|eucalyptus commun"
        ) ~ "eucalyptus globulus",
        str_detect(tree_species_name_clean, "eucalyptus mannifera") ~ "eucalyptus mannifera",
        str_detect(tree_species_name_clean, "ficus vallis choudea") ~ "ficus vallis-choudae",
        str_detect(tree_species_name_clean, "ficus sycamora|ficus sycomorus") ~ "ficus sycomorus",
        str_detect(tree_species_name_clean, "ficus thonningii|ficus thon") ~ "ficus thonningii",
        str_detect(tree_species_name_clean, "ficus enormis") ~ "ficus enormis",
        tree_species_name_clean %in% c("ficus tree", "ficus") ~ "ficus",
        str_detect(
          tree_species_name_clean,
          "gliricirdia sepuim|\\bgliricidia\\b"
        ) ~ "gliricidia sepium",
        str_detect(
          tree_species_name_clean,
          "arachis hypogaea|ground nuts|groundnuts|gnuts|arachide|arrachide|arachides|arachys hypogea"
        ) ~ "arachis hypogaea",
        str_detect(tree_species_name_clean, "jackfruit|jack fruit") ~ "artocarpus heterophyllus",
        str_detect(
          tree_species_name_clean,
          "vitellaria paradoxa|vitellariat paradoxa"
        ) ~ "vitellaria paradoxa",
        str_detect(
          tree_species_name_clean,
          "mansonia altissima|mansonia ultisima|mansonia altisima"
        ) ~ "mansonia altissima",
        str_detect(tree_species_name_clean, "musanga cecropioides") ~ "musanga cecropioides",
        str_detect(
          tree_species_name_clean,
          "measopsis eminni|maesopsis e|maesopses eminii|maesospsis eminii|musizi|\\bmaesopsis\\b|moesopsis"
        ) ~ "maesopsis eminii",
        str_detect(
          tree_species_name_clean,
          "millitia ferruginol|milletia ferruginea|milletia ferruginea|millitia ferrugenia"
        ) ~ "millettia ferruginea",
        str_detect(tree_species_name_clean, "millettia pinnata|pongamia pinnata") ~ "millettia pinnata",
        str_detect(tree_species_name_clean, "mitragyna librostipulosa") ~ "mitragyna stipulosa",
        str_detect(
          tree_species_name_clean,
          "moringa oleifera|moringa oleifer|moringa oleifere|moringa tree"
        ) |
          tree_species_name_clean %in% c("moringa", "mulinga") ~ "moringa oleifera",
        str_detect(tree_species_name_clean, "morus alba") ~ "morus alba",
        str_detect(tree_species_name_clean, "morus mulberry tree") ~ "morus",
        str_detect(tree_species_name_clean, "parkia biglobosa") ~ "parkia biglobosa",
        str_detect(tree_species_name_clean, "parkia bicolor|parquia biclor") ~ "parkia bicolor",
        str_detect(
          tree_species_name_clean,
          "olea europaea sub cuspidate|olea europaea subsp cuspidata|african olive|olea africana|olea europaea subsp africana|olea europea cuspidata"
        ) ~ "olea africana",
        str_detect(tree_species_name_clean, "olea capensis") ~ "olea capensis",
        str_detect(
          tree_species_name_clean,
          "olea welwichii|olea wewyshii|olea welwischi|olea welwitchschii"
        ) ~ "olea welwitschii",
        tree_species_name_clean %in% c("olea europea", "olea europae", "olea europaea (weira)") ~ "olea europaea",
        str_detect(tree_species_name_clean, "passion fruits") ~ "passiflora edulis",
        str_detect(
          tree_species_name_clean,
          "philosyigma thonnongi|piliostigma thonningi|piliostigma thonning"
        ) ~ "piliostigma thonningii",
        str_detect(
          tree_species_name_clean,
          "pidgeon pea|pigeon pea|cajanus cajan"
        ) ~ "cajanus cajan",
        str_detect(tree_species_name_clean, "mkpen|pterocarpus erinaceus") ~ "pterocarpus erinaceus",
        str_detect(tree_species_name_clean, "mimuspos kummel") ~ "mimuspos kummel",
        str_detect(
          tree_species_name_clean,
          "pear packham's triumph|pyrus communis"
        ) ~ "pyrus communis",
        str_detect(
          tree_species_name_clean,
          "\\bpumpkin\\b|\\bpumpkins\\b|\\bcourge\\b"
        ) ~ "cucurbita pepo",
        str_detect(tree_species_name_clean, "rahminus prinoides") ~ "rhamnus prinoides",
        str_detect(
          tree_species_name_clean,
          "ricinodondron heudeleuti|ricinodondron heudelotii|ricinodendron heudelotii|ricinodendron heudoletii|ricinodendrom heudelotti|ricinodendron heudelott|ricinodendron heudelotti"
        ) |
          tree_species_name_clean %in% c("akpi") ~ "ricinodendron heudelotii",
        str_detect(
          tree_species_name_clean,
          "sausage tree|\\bkigelia\\b|kigelia africana|kigeria africana"
        ) ~ "kigelia africana",
        str_detect(tree_species_name_clean, "sorghum moench|sorghum mohench") ~ "sorghum bicolor",
        str_detect(tree_species_name_clean, "le sorg|sorgho") ~ "sorghum",
        str_detect(tree_species_name_clean, "sunflowers") ~ "helianthus annuus",
        str_detect(
          tree_species_name_clean,
          "sweet potato|patate douce|\\bmatembela\\b|\\bmatembele\\b"
        ) ~ "ipomoea batatas",
        str_detect(tree_species_name_clean, "\\bpotato\\b|irish potatoes") ~ "solanum tuberosum",
        str_detect(
          tree_species_name_clean,
          "mbogabuchungu|mbogabughungu|mbogabushungu"
        ) ~ "solanum macrocarpon",
        str_detect(tree_species_name_clean, "vetiver grass") ~ "chrysopogon zizanioides",
        str_detect(tree_species_name_clean, "triplochiton scleroxylon|ayous") ~ "triplochiton scleroxylon",
        str_detect(tree_species_name_clean, "dioscorea hirtiflora|busala tuber") ~ "dioscorea hirtiflora",
        str_detect(tree_species_name_clean, "dioscorea") ~ "dioscorea",
        str_detect(tree_species_name_clean, "lucern") ~ "medicago sativa",
        str_detect(tree_species_name_clean, "irvingia gabonensis") ~ "irvingia gabonensis",
        str_detect(tree_species_name_clean, "tieghemella heckelii") ~ "tieghemella heckelii",
        str_detect(
          tree_species_name_clean,
          "cocoa|theobroma cacao|cacao|theobroma caoa|theobroma cocoa|theobroma ca|theobroma caacao|theobroma caca") |
          tree_species_name_clean %in% c("theo") ~ "theobroma cacao",
        str_detect(
          tree_species_name_clean,
          "coconut|coconuts|coccos nucifera|cocos nucifera|cocus nucifera"
        ) ~ "cocos nucifera",
        str_detect(tree_species_name_clean, "pomagranate|pomegranate") ~ "punica granatum",
        str_detect(tree_species_name_clean, "cabbage|kale|brocoli") ~ "brassica oleracea",
        str_detect(
          tree_species_name_clean,
          "grevillea robusts|grevillea robusta|\\bgrevillea\\b|southern silky oak|grevilea robusta|grevilea robista"
        ) ~ "grevillea robusta",
        str_detect(tree_species_name_clean, "tectona grandis") ~ "tectona grandis",
        str_detect(
          tree_species_name_clean,
          "theka|teak|mivule|milicia excelsa|milecia excelsa|millecia excelsa"
        ) ~ "milicia excelsa",
        str_detect(tree_species_name_clean, "capsicum frutescens") ~ "capsicum frutescens",
        str_detect(tree_species_name_clean, "\\bpepper\\b|\\bpoivrier\\b") ~ "piper nigrum",
        str_detect(tree_species_name_clean, "\\bbirch\\b") ~ "betula",
        str_detect(tree_species_name_clean, "celery") ~ "apium graveolens",
        str_detect(tree_species_name_clean, "grewia bicolar") ~ "grewia bicolor",
        tree_species_name_clean %in% c("maple") ~ "acer",
        str_detect(tree_species_name_clean, "acer saccharum") ~ "acer saccharum",
        str_detect(tree_species_name_clean, "haricot") ~ "phaseolus vulgaris",
        str_detect(
          tree_species_name_clean,
          "leucaena leucocephala|leuceana lycocefala|luceana leucocephala|leuceana leucocephala|leuceana lycocefala|leucaena lecocephala|leucaena leucecephala|leucaena leucocephiala|leucaena leucpcephala|luceane lococifra|leucaena lecena|leucaena cena"
        ) ~ "leucaena leucocephala",
        str_detect(tree_species_name_clean, "manioc|manihot esculenta") ~ "manihot esculenta",
        str_detect(tree_species_name_clean, "markhamia ob") ~ "markhamia obtusifolia",
        str_detect(tree_species_name_clean, "vigna unguiculata|niebe") ~ "vigna unguiculata",
        str_detect(tree_species_name_clean, "rosemary") ~ "salvia rosmarinus",
        str_detect(
          tree_species_name_clean,
          "prekese|tetrapleura tetraptera|tetrapleura tetrapetra"
        ) ~ "tetrapleura tetraptera",
        str_detect(tree_species_name_clean, "spinach") ~ "spinacia oleracea",
        str_detect(tree_species_name_clean, "colocasia esculenta") ~ "colocasia esculenta",
        str_detect(tree_species_name_clean, "\\bananas\\b") ~ "ananas comusus",
        str_detect(
          tree_species_name_clean,
          "calotropis procera|calostropis procera"
        ) ~ "calotropis procera",
        str_detect(
          tree_species_name_clean,
          "guajava|psidium guajava|guava|pisdium goyava|psidium guayava|psidium guayana"
        ) ~ "psidium guajava",
        str_detect(tree_species_name_clean, "amaranth|lengalenga") ~ "amaranth",
        str_detect(
          tree_species_name_clean,
          "\\bwillow\\b|salix \\(various native vars\\)"
        ) ~ "salix",
        str_detect(tree_species_name_clean, "casimiroa edulis|white sapote") ~ "casimiroa edulis",
        str_detect(tree_species_name_clean, "semia chlorophora") ~ "chlorophora excelsa",
        str_detect(tree_species_name_clean, "paradise apple") ~ "malus dasyphylla",
        str_detect(
          tree_species_name_clean,
          "ziziphus morticians|ziziphus morticians|ziziphusmoriciana|ziziphus moriciana"
        ) ~ "ziziphus mauritiana",
        str_detect(tree_species_name_clean, "\\briz\\b") ~ "rhizophora",
        str_detect(tree_species_name_clean, "\\bhazombato\\b") ~ "kalanchoe orgyalis",
        str_detect(tree_species_name_clean, "mitrigyna librostipulosa") ~ "mitragyna stipulosa",
        str_detect(tree_species_name_clean, "picralima nitida") ~ "picralima nitida",
        str_detect(tree_species_name_clean, "capsicum annuum") ~ "capsicum annuum",
        str_detect(tree_species_name_clean, "eriobotrya") ~ "eriobotrya japonica",
        tree_species_name_clean %in% c("gombo", "okro", "okra") ~ "abelmoschus esculentus",
        str_detect(tree_species_name_clean, "\\btavia\\b") ~ "rhopalocarpus coriaceus",
        str_detect(tree_species_name_clean, "\\bsarimanga\\b") ~ "noronhia",
        str_detect(tree_species_name_clean, "\\bwarburgia\\b") ~ "warburgia ugandensis",
        str_detect(tree_species_name_clean, "\\bphilenoptera\\b") ~ "philenoptera violacea",
        str_detect(tree_species_name_clean, "\\boryza\\b") ~ "oryza sativa",
        str_detect(tree_species_name_clean, "\\bgmelina\\b") ~ "gmelina arborea",
        str_detect(tree_species_name_clean, "\\bceriops\\b") ~ "ceriops tagal",
        tree_species_name_clean %in% c("prunus", "prunas") |
          str_detect(tree_species_name_clean, "prunus africana") ~ "prunus africana",
        str_detect(tree_species_name_clean, "prunus persic") ~ "prunus persica",
        str_detect(tree_species_name_clean, "prunus domestica") ~ "prunus domestica",
        str_detect(tree_species_name_clean, "prunus serotina") ~ "prunus serotina",
        tree_species_name_clean %in% c("mwanga") ~ "pericopsis angolensis",
        str_detect(tree_species_name_clean, "pericopsis elata") ~ "pericopsis elata",
        str_detect(tree_species_name_clean, "\\btsimitetra\\b") ~ "ilex mitis",
        str_detect(tree_species_name_clean, "leucaena dephasfolia") ~ "leucaena diversifolia",
        str_detect(tree_species_name_clean, "cocoyam") ~ "colocasia esculenta",
        str_detect(tree_species_name_clean, "entandrophragma angolense") ~ "entandrophragma angolense",
        str_detect(
          tree_species_name_clean,
          "entandrophragma cylindricum|entandrophragma cilindricum"
        ) ~ "entandrophragma cylindricum",
        str_detect(
          tree_species_name_clean,
          "entandrophragma candelei|entandrophragma candollei|entendrophragma candellei|entendrophragma candolei"
        ) ~ "entandrophragma candollei",
        str_detect(tree_species_name_clean, "cedar \\(entandrophragma\\)") ~ "entandrophragma",
        str_detect(tree_species_name_clean, "allinum cepa|allunum cepa") ~ "allium cepa",
        str_detect(tree_species_name_clean, "aloe africana|aloe europea") ~ "aloe africana",
        str_detect(tree_species_name_clean, "aloe var barbadensis") ~ "aloe barbadensis",
        str_detect(tree_species_name_clean, "aloe var chinensis") ~ "aloe chinensis",
        str_detect(tree_species_name_clean, "aloe var secundiflora") ~ "aloe secundiflora",
        str_detect(tree_species_name_clean, "avicenia germinans") |
          tree_species_name_clean %in% c("avicennia") ~ "avicenia germinans",
        str_detect(tree_species_name_clean, "carapa procera") ~ "carapa procera",
        str_detect(tree_species_name_clean, "carapa grandiflora") ~ "carapa grandiflora",
        str_detect(tree_species_name_clean, "carapa guianensis") ~ "carapa guianensis",
        str_detect(tree_species_name_clean, "cassia spectabilis") ~ "cassia spectabilis",
        str_detect(tree_species_name_clean, "cassia leiandra") ~ "cassia leiandra",
        str_detect(
          tree_species_name_clean,
          "cordia africana|cordial africana|cordyla africana|wanza"
        ) ~ "cordia africana",
        str_detect(tree_species_name_clean, "cordia sebestena") ~ "cordia sebestena",
        tree_species_name_clean %in% c("cordia milleni & c africana") ~ "cordia milleni",
        str_detect(tree_species_name_clean, "cordia goeldiana") ~ "cordia goeldiana",
        str_detect(tree_species_name_clean, "cucumber") ~ "cucumis sativus",
        str_detect(tree_species_name_clean, "eucalyptus gunnii") ~ "eucalyptus gunnii",
        str_detect(
          tree_species_name_clean,
          "hagenia abyssinica|hagenia abysinicca|hagenia habhysynica"
        ) ~ "hagenia abyssinica",
        tree_species_name_clean %in% c("indigofera so") ~ "indigofera",
        str_detect(
          tree_species_name_clean,
          "juniperus procera|african pencil-cedar|janiperus procera|juniperas procera|juniperous procera|juniperus procurus"
        ) ~ "juniperus procera",
        str_detect(tree_species_name_clean, "juniperus virginiana") ~ "juniperus virginiana",
        tree_species_name_clean %in% c("macadamia") ~ "macadamia integrifolia",
        str_detect(tree_species_name_clean, "colophospermum mopane") ~ "colophospermum mopane",
        str_detect(tree_species_name_clean, "myrsine africana") ~ "myrsine africana",
        str_detect(tree_species_name_clean, "nuxia congesta") ~ "nuxia congesta",
        str_detect(
          tree_species_name_clean,
          "pseudolachnostylis|pseuudolachnostylis"
        ) ~ "pseudolachnostylis maprouneifolia",
        str_detect(tree_species_name_clean, "tropical almond|terminalia catappa") ~ "terminalia catappa",
        str_detect(tree_species_name_clean, "albizia lebbeck") ~ "albizia lebbeck",
        str_detect(tree_species_name_clean, "albizia \\(coriaria \\)") ~ "albizia coriara",
        str_detect(tree_species_name_clean, "dombeya burgessiae") ~ "dombeya burgessiae",
        str_detect(
          tree_species_name_clean,
          "dombeya torrida|dombeya goetzenii|dombeya goetzi|dombea torida|dombea toriida|dombeya \\(mukeu\\)|dombeya tarida|ndombeya"
        ) ~ "dombeya torrida",
        str_detect(tree_species_name_clean, "monotes") ~ "monotes africanus",
        str_detect(tree_species_name_clean, "pennisetum purpureum") ~ "pennisetum purpureum",
        tree_species_name_clean %in% c("podo carpus", "podocupus", "podocurpus mix", "pordocapus") ~ "podocarpus",
        str_detect(
          tree_species_name_clean,
          "podo latifolia|podocarpus latifolia|podorcarpus latifolia"
        ) ~ "podocarpus latifolius",
        str_detect(
          tree_species_name_clean,
          "podocapus fulcatus|podocarpus falcatus|podorcarpus falcatus"
        ) ~ "afrocarpus falcatus",
        str_detect(tree_species_name_clean, "podocapus elatus|podocarpus elatus") ~ "podocarpus elatus",
        str_detect(tree_species_name_clean, "podocarpus macrophyllus") ~ "podocarpus macrophyllus",
        str_detect(tree_species_name_clean, "schizolobium parahyba")
        |
          tree_species_name_clean %in% c("schizolobium") ~ "schizolobium parahyba",
        str_detect(tree_species_name_clean, "zingiber|zingliber") |
          tree_species_name_clean %in% c("zing", "zi") ~ "zingiber officinale",
        str_detect(
          tree_species_name_clean,
          "royal pointiciana|flambodea|delonix regia"
        ) |
          tree_species_name_clean %in% c("flamboyant") ~ "delonix regia",
        str_detect(tree_species_name_clean, "cornius volkensi|cornus volkensii") ~ "cornus volkensii",
        str_detect(tree_species_name_clean, "allophyllus abysssinica") ~ "allophylus abyssinicus",
        str_detect(tree_species_name_clean, "manihot scleroxylon") ~ "triplochiton scleroxylon",
        # might be incorrect - check with PM
        str_detect(tree_species_name_clean, "actinidia deliciosa") ~ "actinidia deliciosa",
        str_detect(tree_species_name_clean, "anthocleista shweinfurtii") ~ "anthocleista schweinfurthii",
        str_detect(tree_species_name_clean, "apodytis dimiata") ~ "apodytes dimidiata",
        str_detect(
          tree_species_name_clean,
          "jacaranda mimosifolia|jacaranda mimosaefolia"
        ) ~ "jacaranda mimosifolia",
        str_detect(tree_species_name_clean, "jacarandar copaia") ~ "jacaranda copaia",
        tree_species_name_clean %in% c("boskia") ~ "boscia",
        str_detect(tree_species_name_clean, "breonadia salicina") ~ "breonadia salicina",
        str_detect(tree_species_name_clean, "casuarina equisetifolia") ~ "casuarina equisetifolia",
        str_detect(tree_species_name_clean, "dobergia latifolia") ~ "dalbergia latifolia",
        str_detect(tree_species_name_clean, "aubregrinia taiensis") ~ "aubregrinia taiensis",
        str_detect(tree_species_name_clean, "faurea saligna") ~ "faurea saligna",
        str_detect(tree_species_name_clean, "pitacolumbium") ~ "pithecellobium",
        str_detect(
          tree_species_name_clean,
          "harungana madagascariensis|harungana madagariensis|harongana madagascariensis|harrungana madagariensis|harungana madagarensis|haungana madagarensis"
        ) ~ "harungana madagascariensis",
        str_detect(tree_species_name_clean, "intsia bijuga") ~ "intsia bijuga",
        str_detect(tree_species_name_clean, "maesa lanceolata|maesa laseorata") ~ "maesa lanceolata",
        str_detect(tree_species_name_clean, "pittosporum verticilatum") ~ "pittosporum verticilatum",
        str_detect(tree_species_name_clean, "meru oak") ~ "vitex keniensis",
        str_detect(tree_species_name_clean, "mexican green ash") ~ "fraxinus berlandieriana",
        str_detect(tree_species_name_clean, "pouteria adolfi-friedericii") ~ "pouteria adolfi-friedericii",
        str_detect(tree_species_name_clean, "myrianthus holstii") ~ "myrianthus holstii",
        str_detect(tree_species_name_clean, "mytragyna rubrostipulata") ~ "mytragyna rubrostipulata",
        str_detect(tree_species_name_clean, "alstonia bronei") ~ "alstonia boonei",
        str_detect(tree_species_name_clean, "afzelia africana") ~ "afzelia africana",
        str_detect(tree_species_name_clean, "pinus patula") ~ "pinus patula",
        str_detect(tree_species_name_clean, "pinus kesiya") ~ "pinus kesiya",
        str_detect(
          tree_species_name_clean,
          "polycias fluva|polycias fulva|polyscians fulva|polyscias fulva"
        ) ~ "polyscias fulva",
        str_detect(
          tree_species_name_clean,
          "polyscias kikuyuensis|polycius kikuyuensis"
        ) ~ "polyscias kikuyuensis",
        str_detect(tree_species_name_clean, "porospermum atrorufum") ~ "osteospermum atrorufum",
        str_detect(tree_species_name_clean, "canarium madagascariensis") ~ "canarium madagascariensis",
        tree_species_name_clean %in% c("raspberry") ~ "rubus",
        str_detect(
          tree_species_name_clean,
          "tamarindus indica|tamarindas indica|tamarids|tamarinds"
        ) ~ "tamarindus indica",
        str_detect(tree_species_name_clean, "tarconanthus littiveli") ~ "tarchonanthus littoralis",
        str_detect(tree_species_name_clean, "zanthoxylum gillettii") ~ "zanthoxylum gillettii",
        str_detect(tree_species_name_clean, "tephrosia \\(umuhunwa \\)") | 
          tree_species_name_clean %in% c("tephrosia", "tephrosia sp") ~ "tephrosia",
        str_detect(
          tree_species_name_clean,
          "calliandra callystrusus|calliandra collonthysus|calliandra calothyrsys"
        ) ~ "calliandra calothyrsus",
        str_detect(tree_species_name_clean, "raminhus perdinious") ~ "rhamnus prinoides",
        str_detect(tree_species_name_clean, "brachlaena hutchinsii") ~ "brachylaena hutchinsii",
        str_detect(tree_species_name_clean, "sterculia africana") |
          tree_species_name_clean %in% c("sterculia")  ~ "sterculia africana",
        str_detect(tree_species_name_clean, "xanthorcesis zambeasiaca") ~ "xanthorcesis zambeasiaca",
        str_detect(tree_species_name_clean, "xeroderris stuhlmannii|xeroderis shulmanai") ~ "xeroderris stuhlmannii",
        str_detect(
          tree_species_name_clean,
          "matilisguate|matilis guate|taubebia rosae|tabeiua rosea|tabeuia rosea"
        ) ~ "tabebuia rosea",
        tree_species_name_clean %in% c("cortez") ~ "tabebuia ochracea",
        tree_species_name_clean %in% c("encino") ~ "quercus virginiana",
        tree_species_name_clean %in% c("pino", "puno") ~ "pinus",
        tree_species_name_clean %in% c("rosul") ~ "dalbergia tucarensis",
        str_detect(tree_species_name_clean, "dalbergia sissoo|sheesham") ~ "dalbergia sissoo",
        tree_species_name_clean %in% c("santa maria") ~ "calophyllum antillanum",
        tree_species_name_clean %in% c("calophylon") ~ "calophyllum",
        tree_species_name_clean %in% c("zapote de agua", "pachyra aquatica", "zapote de aqua") ~ "pachira aquatica",
        str_detect(tree_species_name_clean, "madhuca indica|madhuca longifolia") |
          tree_species_name_clean %in% c("mahua") ~ "madhuca longifolia",
        str_detect(tree_species_name_clean, "bombax ceiba") ~ "bombax ceiba",
        str_detect(tree_species_name_clean, "acokanthera oppositifolia") ~ "acokanthera oppositifolia",
        str_detect(
          tree_species_name_clean,
          "afzelia bignioides|afzelia bipidoides"
        ) ~ "afzelia bipindensis",
        str_detect(
          tree_species_name_clean,
          "staudtia camerounensis|staudtia kamerounensis"
        ) ~ "staudtia kamerunensis",
        str_detect(tree_species_name_clean, "nauclia dideritchii") ~ "nauclea diderrichii",
        str_detect(tree_species_name_clean, "aegiceras corniculatum") ~ "aegiceras corniculatum",
        str_detect(tree_species_name_clean, "amburana cearensis") ~ "amburana cearensis",
        str_detect(tree_species_name_clean, "anadenanthera colubrina") ~ "anadenanthera colubrina",
        str_detect(tree_species_name_clean, "aniba firmula") ~ "aniba firmula",
        str_detect(tree_species_name_clean, "annona mucosa") ~ "annona mucosa",
        str_detect(tree_species_name_clean, "apeiba tibourbou") ~ "apeiba tibourbou",
        str_detect(tree_species_name_clean, "apuleia leiocarpa") ~ "apuleia leiocarpa",
        str_detect(tree_species_name_clean, "astrocaryum huaimi") ~ "astrocaryum huaimi",
        str_detect(tree_species_name_clean, "astronium lecointei") ~ "astronium lecointei",
        str_detect(tree_species_name_clean, "astronium urundeuva") ~ "astronium urundeuva",
        str_detect(tree_species_name_clean, "caderno de capoeira") ~ "astronium graveolens",
        str_detect(tree_species_name_clean, "bagassa guianensis") ~ "bagassa guianensis",
        str_detect(tree_species_name_clean, "bauhinia forficata|unha de cabra") ~ "bauhinia forficata",
        str_detect(tree_species_name_clean, "bauhinia variegata") ~ "bauhinia variegata",
        str_detect(tree_species_name_clean, "bertholletia excelsa") ~ "bertholletia excelsa",
        str_detect(tree_species_name_clean, "bixa arborea") ~ "bixa arborea",
        str_detect(tree_species_name_clean, "bixa orellana") ~ "bixa orellana",
        str_detect(tree_species_name_clean, "byrsonima crispa") ~ "byrsonima crispa",
        str_detect(tree_species_name_clean, "byrsonima cydoniifolia") ~ "byrsonima cydoniifolia",
        str_detect(tree_species_name_clean, "byrsonima sericea") ~ "byrsonima sericea",
        str_detect(
          tree_species_name_clean,
          "calophylum brasiliensis|callophylum braisliense|calophiullum brasilienses|calophyllum brasieliense|calophyllum brasilienses"
        ) ~ "calophyllum brasiliense",
        str_detect(tree_species_name_clean, "canavalia ensiformis") ~ "canavalia ensiformis",
        str_detect(tree_species_name_clean, "\\bcarya\\b") ~ "carya",
        str_detect(tree_species_name_clean, "caryocar cf brasiliense") ~ "caryocar brasiliense",
        str_detect(tree_species_name_clean, "cecropia hololeuca") ~ "cecropia hololeuca",
        str_detect(tree_species_name_clean, "cercis canadensis") ~ "cercis canadensis",
        str_detect(tree_species_name_clean, "chloroleucon cf mangense") ~ "chloroleucon mangense",
        str_detect(tree_species_name_clean, "chrysophyllum splendes") ~ "chrysophyllum splendes",
        str_detect(tree_species_name_clean, "clarisia racemosa") ~ "clarisia racemosa",
        str_detect(tree_species_name_clean, "clitoria fairchildian") ~ "clitoria fairchildiana",
        str_detect(tree_species_name_clean, "clusia cf spiritu-sanctensis") ~ "clusia spiritu-sanctensis",
        str_detect(tree_species_name_clean, "copaifera langsdorffii") ~ "copaifera langsdorffii",
        str_detect(tree_species_name_clean, "\\bcornus\\b") ~ "cornus",
        str_detect(tree_species_name_clean, "coumo utilis") ~ "coumo utilis",
        str_detect(tree_species_name_clean, "cupania vernalis") ~ "cupania vernalis",
        str_detect(tree_species_name_clean, "cytharexyllum myrianthum") ~ "cytharexyllum myrianthum",
        str_detect(tree_species_name_clean, "dialium guianense") ~ "dialium guianense",
        str_detect(tree_species_name_clean, "dinizia excelsa|\\bangelim\\b") ~ "dinizia excelsa",
        str_detect(
          tree_species_name_clean,
          "dipteryx odorata|dipteryx guianensis"
        ) ~ "dipteryx odorata",
        str_detect(
          tree_species_name_clean,
          "dodoanea viscosa sub angustissima|dodonaea viscosa ssp angustissima|dodonaea viscosa sub angustissima|dodonaea viscosa angustissima"
        ) ~ "dodonaea viscosa subsp. angustissima",
        str_detect(tree_species_name_clean, "enterolobium contortisiliquum") ~ "enterolobium contortisiliquum",
        str_detect(tree_species_name_clean, "enterolobium maximum") ~ "enterolobium maximum",
        str_detect(tree_species_name_clean, "enterolobium timbouva") ~ "enterolobium timbouva",
        str_detect(tree_species_name_clean, "eriotheca pentaphylla") ~ "eriotheca pentaphylla",
        str_detect(tree_species_name_clean, "eugenia brasiliensis") ~ "eugenia brasiliensis",
        str_detect(tree_species_name_clean, "eugenia florida") ~ "eugenia florida",
        str_detect(tree_species_name_clean, "eugenia pyriformis") ~ "eugenia pyriformis",
        str_detect(tree_species_name_clean, "eugenia stipitata") ~ "eugenia stipitata",
        str_detect(tree_species_name_clean, "eugenia uniflora") ~ "eugenia uniflora",
        str_detect(tree_species_name_clean, "euterpe oleracea") ~ "euterpe oleracea",
        str_detect(tree_species_name_clean, "gallesia integrifolia") ~ "gallesia integrifolia",
        str_detect(tree_species_name_clean, "garcinia gardneriana") ~ "garcinia gardneriana",
        str_detect(tree_species_name_clean, "garcinia macrophylla") ~ "garcinia macrophylla",
        str_detect(tree_species_name_clean, "geissonspermum laeve") ~ "geissonspermum laeve",
        str_detect(tree_species_name_clean, "genipa americana") ~ "genipa americana",
        str_detect(tree_species_name_clean, "genipa infundibuliformis") ~ "genipa infundibuliformis",
        str_detect(tree_species_name_clean, "guatteria australis") ~ "guatteria australis",
        str_detect(tree_species_name_clean, "handroanthus vellosoi") ~ "handroanthus vellosoi",
        str_detect(
          tree_species_name_clean,
          "hevea brasiliensis|hevea paraheba|hevea braseliensis|hevea brasileinsis|hevea brasiliensis"
        ) ~ "hevea brasiliensis",
        str_detect(tree_species_name_clean, "hymenaea courbaril") ~ "hymenaea courbaril",
        str_detect(tree_species_name_clean, "inga cuil") ~ "inga inicuil",
        str_detect(tree_species_name_clean, "inga cylindrica") ~ "inga cylindrica",
        str_detect(tree_species_name_clean, "inga edulis") ~ "inga edulis",
        str_detect(tree_species_name_clean, "inga laurina") ~ "inga laurina",
        str_detect(tree_species_name_clean, "inga sessilis") ~ "inga sessilis",
        str_detect(tree_species_name_clean, "inga marginata") ~ "inga marginata",
        str_detect(tree_species_name_clean, "inga vera spuria") ~ "inga vera subsp. spuria",
        tree_species_name_clean %in% c("ingas") ~ "inga",
        str_detect(tree_species_name_clean, "jacaranda copaia") ~ "jacaranda copaia",
        str_detect(tree_species_name_clean, "johanesia princeps") ~ "johanesia princeps",
        str_detect(tree_species_name_clean, "kandelia obovate") ~ "kandelia obovate",
        str_detect(tree_species_name_clean, "liriodendron tulipifera") ~ "liriodendron tulipifera",
        str_detect(tree_species_name_clean, "lodgepole pine") ~ "pinus contorta",
        str_detect(tree_species_name_clean, "macrolobium acaciifolium") ~ "macrolobium acaciifolium",
        str_detect(tree_species_name_clean, "macrolobium bifolium") ~ "macrolobium bifolium",
        str_detect(tree_species_name_clean, "macrosamanea pedicellaris") ~ "macrosamanea pedicellaris",
        str_detect(tree_species_name_clean, "manilkara salzmanii") ~ "manilkara salzmanii",
        str_detect(tree_species_name_clean, "mauritia flexuosa") ~ "mauritia flexuosa",
        str_detect(tree_species_name_clean, "melocanna bambusoides") ~ "melocanna bambusoides",
        str_detect(tree_species_name_clean, "mezilaurus itauba") ~ "mezilaurus itauba",
        str_detect(tree_species_name_clean, "myrcia rostrata") ~ "myrcia rostrata",
        str_detect(tree_species_name_clean, "myrciaria dubia") ~ "myrciaria dubia",
        str_detect(tree_species_name_clean, "oenocarpus bacaba") ~ "oenocarpus bacaba",
        str_detect(tree_species_name_clean, "oenocarpus bataua") ~ "oenocarpus bataua",
        str_detect(tree_species_name_clean, "ormosia excelsa") ~ "ormosia excelsa",
        str_detect(tree_species_name_clean, "ormosia paraensis") ~ "ormosia paraensis",
        str_detect(tree_species_name_clean, "parkia multijuga") ~ "parkia multijuga",
        str_detect(tree_species_name_clean, "parkia pendula") ~ "parkia pendula",
        str_detect(tree_species_name_clean, "pinus echinata") ~ "pinus echinata",
        str_detect(tree_species_name_clean, "piptadenia gonoacantha") ~ "piptadenia gonoacantha",
        str_detect(tree_species_name_clean, "plinia cauliflora") ~ "plinia cauliflora",
        str_detect(tree_species_name_clean, "ponderosa pine") ~ "pinus ponderosa",
        str_detect(tree_species_name_clean, "pouteria caimito") ~ "pouteria caimito",
        str_detect(tree_species_name_clean, "pouteria macrophylla") ~ "pouteria macrophylla",
        str_detect(tree_species_name_clean, "protium heptaphyllum") ~ "protium heptaphyllum",
        str_detect(tree_species_name_clean, "prunus serontina") ~ "prunus serontina",
        str_detect(tree_species_name_clean, "quercus alba") ~ "quercus alba",
        str_detect(tree_species_name_clean, "quercus prinus") ~ "quercus prinus",
        str_detect(tree_species_name_clean, "robinia pseudoacacia") ~ "robinia pseudoacacia",
        str_detect(tree_species_name_clean, "samanea tubulosa") ~ "samanea tubulosa",
        str_detect(tree_species_name_clean, "sapindus saponaria") ~ "sapindus saponaria",
        str_detect(
          tree_species_name_clean,
          "schinus terebinthifolia|schinus terebinthifolius|schinus tereninthifolius|schinus terebenthifolia"
        ) ~ "schinus terebinthifolia",
        str_detect(tree_species_name_clean, "simarouba amara") ~ "simarouba amara",
        str_detect(tree_species_name_clean, "spondias lutea") ~ "spondias lutea",
        str_detect(tree_species_name_clean, "spondias mombin") ~ "spondias mombin",
        str_detect(tree_species_name_clean, "spruce hybrid") ~ "picea",
        str_detect(tree_species_name_clean, "sterculia chicha") ~ "sterculia chicha",
        str_detect(tree_species_name_clean, "sterculia striata") ~ "sterculia striata",
        str_detect(tree_species_name_clean, "stryphnodendron pulcherrimum") ~ "stryphnodendron pulcherrimum",
        str_detect(tree_species_name_clean, "swartzia euxylophora") ~ "swartzia euxylophora",
        str_detect(tree_species_name_clean, "tabebuia heptaphylla") ~ "tabebuia heptaphylla",
        str_detect(
          tree_species_name_clean,
          "tabebuia impetiginosa|tebeuia inpectinosa"
        ) ~ "tabebuia impetiginosa",
        str_detect(tree_species_name_clean, "tabebuia serratifolia") ~ "tabebuia serratifolia",
        str_detect(tree_species_name_clean, "tachigali guianensis") ~ "tachigali guianensis",
        str_detect(tree_species_name_clean, "talisia esculenta") ~ "talisia esculenta",
        str_detect(tree_species_name_clean, "tapirira guianensis") ~ "tapirira guianensis",
        str_detect(tree_species_name_clean, "terminalia amazonia") ~ "terminalia amazonia",
        str_detect(tree_species_name_clean, "terminalia corrugata") ~ "terminalia corrugata",
        str_detect(tree_species_name_clean, "toulicia guianensis") ~ "toulicia guianensis",
        str_detect(tree_species_name_clean, "trema micrantha") ~ "trema micrantha",
        str_detect(tree_species_name_clean, "trembling aspen") ~ "populus tremuloides",
        str_detect(tree_species_name_clean, "viburnum") ~ "viburnum",
        str_detect(tree_species_name_clean, "leptolobium nitens") ~ "leptolobium nitens",
        str_detect(tree_species_name_clean, "puteria torta") ~ "puteria torta",
        str_detect(tree_species_name_clean, "mimusops coriacea") ~ "mimusops coriacea",
        tree_species_name_clean %in% c("acacia dealbata dealbata") ~ "acacia dealbata subsp. dealbata",
        str_detect(tree_species_name_clean, "aegiphila intergrifolia|aegiphila integrifolia") ~ "aegiphila intergrifolia",
        str_detect(tree_species_name_clean, "albizia niopoides") ~ "albizia niopoides",
        str_detect(tree_species_name_clean, "alchornea triplinervia") ~ "alchornea triplinervia",
        str_detect(tree_species_name_clean, "allophyllus edulis") ~ "allophyllus edulis",
        str_detect(tree_species_name_clean, "amburana acreana") ~ "amburana acreana",
        str_detect(tree_species_name_clean, "protium heptaphilla") ~ "protium heptaphilla",
        str_detect(tree_species_name_clean, "anadenanthera macrocarpa") ~ "anadenanthera macrocarpa",
        str_detect(tree_species_name_clean, "anadenanthera peregrina") ~ "anadenanthera peregrina",
        str_detect(tree_species_name_clean, "andira fraxinifolia") ~ "andira fraxinifolia",
        str_detect(tree_species_name_clean, "anadenanthera eregrina") ~ "anadenanthera eregrina",
        str_detect(tree_species_name_clean, "adenatera pavoronica|adenathera pavaronica|adenanthera pavonia|adenathera pavonica|adenathera pavonina") ~ "adenanthera pavonina",
        str_detect(tree_species_name_clean, "annona coriaceae") ~ "annona coriaceae",
        str_detect(tree_species_name_clean, "\\baraca\\b") ~ "psidium",
        str_detect(
          tree_species_name_clean,
          "arapatiella psilophylla|arapatiella psilophilla"
        ) ~ "arapatiella psilophylla",
        str_detect(tree_species_name_clean, "schinus terebentifolius") ~ "schinus terebentifolius",
        tree_species_name_clean %in% c("brachystegia", "brachystegia spec", "branchystegia") ~ "brachystegia",
        str_detect(tree_species_name_clean, "brosimum alicastrum") ~ "brosimum alicastrum",
        str_detect(tree_species_name_clean, "caesalpinea echinata") ~ "caesalpinea echinata",
        str_detect(tree_species_name_clean, "caesalpinea ferrea") ~ "caesalpinea ferrea",
        str_detect(tree_species_name_clean, "caesalpinia eriostachys") ~ "caesalpinia eriostachys",
        str_detect(tree_species_name_clean, "peltophorum dubium") ~ "peltophorum dubium",
        str_detect(tree_species_name_clean, "canela folha longa") ~ "ocotea puberula",
        str_detect(
          tree_species_name_clean,
          "cariniana estrellensis|cariniana estre"
        ) ~ "cariniana estrellensis",
        str_detect(tree_species_name_clean, "cariniana legalis") ~ "cariniana legalis",
        str_detect(tree_species_name_clean, "cariniana micrantha") ~ "cariniana micrantha",
        str_detect(tree_species_name_clean, "jacaranda micrantha") ~ "jacaranda micrantha",
        str_detect(tree_species_name_clean, "jacaranda brasiliana") ~ "jacaranda brasiliana",
        str_detect(tree_species_name_clean, "castilla ulei") ~ "castilla ulei",
        str_detect(
          tree_species_name_clean,
          "cecropia pachystachya|cecropiapachystachya"
        ) ~ "cecropia pachystachya",
        str_detect(tree_species_name_clean, "ceiba aesculifolia parvifolia") ~ "ceiba aesculifolia subsp. parvifolia",
        str_detect(tree_species_name_clean, "ceiba aesculifolia") ~ "ceiba aesculifolia",
        str_detect(tree_species_name_clean, "ceiba pubiflora") ~ "ceiba pubiflora",
        str_detect(
          tree_species_name_clean,
          "citarexylum myrianthum|citarexilum mirianthum|citharexylum myrianthum"
        ) ~ "citharexylum myrianthum",
        str_detect(tree_species_name_clean, "cochlospermum vitifolium") ~ "cochlospermum vitifolium",
        str_detect(tree_species_name_clean, "colubrina glandulosa") ~ "colubrina glandulosa",
        str_detect(tree_species_name_clean, "combretum leprosum") ~ "combretum leprosum",
        str_detect(tree_species_name_clean, "copafera grycycarpa") ~ "copaifera glycycarpa",
        str_detect(tree_species_name_clean, "copaifera lucens") ~ "copaifera lucens",
        str_detect(tree_species_name_clean, "cordia magnoliifolia") ~ "cordia magnoliifolia",
        str_detect(tree_species_name_clean, "cordia tricothoma|cordia trichotoma") ~ "cordia tricothoma",
        str_detect(tree_species_name_clean, "couratari guianensis") ~ "couratari guianensis",
        str_detect(tree_species_name_clean, "croton floribundus") ~ "croton floribundus",
        str_detect(tree_species_name_clean, "croton lechleri") ~ "croton lechleri",
        str_detect(tree_species_name_clean, "croton urucurana") ~ "croton urucurana",
        str_detect(
          tree_species_name_clean,
          "cyntharexyllum myrianthum|cytharexyllum myrianthum"
        ) ~ "citharexylum myrianthum",
        str_detect(tree_species_name_clean, "dalbergia nigra") ~ "dalbergia nigra",
        str_detect(
          tree_species_name_clean,
          "diphysa americana|diphisa americana|diphisa amerina"
        ) ~ "diphysa americana",
        str_detect(tree_species_name_clean, "diplotropis purpurea") ~ "diplotropis purpurea",
        tree_species_name_clean %in% c("dracaena", "dracenna") ~ "dracaena",
        str_detect(
          tree_species_name_clean,
          "entada strichnostachy|intada strichnostachys"
        ) ~ "entada chrysostachys",
        str_detect(
          tree_species_name_clean,
          "enterolobium ciclocarpum|enterolobium cyclocarp|enterolobium cyclocarpum"
        ) ~ "enterolobium cyclocarpum",
        str_detect(tree_species_name_clean, "enterolobium schomburgkii") ~ "enterolobium schomburgkii",
        str_detect(
          tree_species_name_clean,
          "erythrophleum graeveolens|erythrophleum graveolens|erythropleum graveolo|erythrophleum suaveolensaf"
        ) ~ "erythrophleum suaveolens",
        str_detect(tree_species_name_clean, "eschweilera ovata") ~ "eschweilera ovata",
        str_detect(tree_species_name_clean, "euterpe precatoria") ~ "euterpe precatoria",
        str_detect(
          tree_species_name_clean,
          "gilberiodendron dewevrii|gilbertiodendron dewevrii|gilbertiondendron dewevrii|gilbertodendrion dewevrii|gilbertodendron dewevrii"
        ) ~ "gilbertiodendron dewevrei",
        str_detect(tree_species_name_clean, "astronium fraxinifolium") ~ "astronium fraxinifolium",
        str_detect(tree_species_name_clean, "guapira opposita") ~ "guapira opposita",
        str_detect(tree_species_name_clean, "guazuma ulmifolia") ~ "guazuma ulmifolia",
        str_detect(
          tree_species_name_clean,
          "guibourtia demeusei|guibourtha demeusii|guibourthia demeusii|guibourtia demeusi|guibourtia demeusii"
        ) ~ "guibourtia demeusei",
        str_detect(tree_species_name_clean, "handroanthus impetiginosus") ~ "handroanthus impetiginosus",
        str_detect(tree_species_name_clean, "himatanthus phagedaenicu") ~ "himatanthus phagedaenicus",
        str_detect(tree_species_name_clean, "homalium trichostemon") ~ "homalium trichostemon",
        str_detect(tree_species_name_clean, "humira balsamifera") ~ "humira balsamifera",
        str_detect(tree_species_name_clean, "hymenaea parvifolia") ~ "hymenaea parvifolia",
        str_detect(tree_species_name_clean, "hymenolobium petreum") ~ "hymenolobium petreum",
        str_detect(tree_species_name_clean, "zeyheria tuberculosa") ~ "zeyheria tuberculosa",
        str_detect(tree_species_name_clean, "ipe verde") ~ "cybistax antisyphilitica",
        str_detect(tree_species_name_clean, "jabuti de macaco") ~ "hymenolobium sericeum",
        str_detect(tree_species_name_clean, "jaracatia spinosa") ~ "jaracatia spinosa",
        str_detect(tree_species_name_clean, "joannesia princeps") ~ "joannesia princeps",
        str_detect(tree_species_name_clean, "euterpe edulis") ~ "euterpe edulis",
        str_detect(tree_species_name_clean, "kunzeaspp") ~ "kunzea",
        str_detect(tree_species_name_clean, "lecythis pisonis") ~ "lecythis pisonis",
        str_detect(tree_species_name_clean, "leptospermumspp") ~ "leptospermum",
        tree_species_name_clean %in% c("lichi", "litchi sinensis") ~ "litchi chinensis",
        tree_species_name_clean %in% c("liqui dambar") ~ "liquidambar",
        str_detect(tree_species_name_clean, "machaerium nictitans") ~ "machaerium nictitans",
        str_detect(tree_species_name_clean, "margaritaria nobilis") ~ "margaritaria nobilis",
        str_detect(tree_species_name_clean, "mimosa bimucronata") ~ "mimosa bimucronata",
        str_detect(tree_species_name_clean, "maytenus obtusifolia") ~ "maytenus obtusifolia",
        str_detect(tree_species_name_clean, "micropholis venulosa") ~ "micropholis venulosa",
        str_detect(tree_species_name_clean, "mimosa bimucronata") ~ "mimosa bimucronata",
        str_detect(tree_species_name_clean, "mimosa caesalpinifolia|mimosa caesalpiniifolia") ~ "mimosa caesalpiniifolia",
        str_detect(tree_species_name_clean, "humira balsamifera") ~ "humira balsamifera",
        str_detect(tree_species_name_clean, "nectandra rubra") ~ "nectandra rubra",
        str_detect(tree_species_name_clean, "nuxia capitata") ~ "nuxia capitata",
        str_detect(tree_species_name_clean, "ochroma pyramidale") ~ "ochroma pyramidale",
        str_detect(tree_species_name_clean, "ormosia arborea") ~ "ormosia arborea",
        str_detect(tree_species_name_clean, "parahancornia fasciculata") ~ "parahancornia fasciculata",
        str_detect(
          tree_species_name_clean,
          "paramacrolobium coelucum|paramacrolobium coelureum|paramacrolobium coeru"
        ) ~ "paramacrolobium coeruleum",
        str_detect(tree_species_name_clean, "paubrasilia echinata") ~ "paubrasilia echinata",
        str_detect(tree_species_name_clean, "cyntharexyllum myrianthum") ~ "cyntharexyllum myrianthum",
        str_detect(tree_species_name_clean, "caesalpinia ferrea") ~ "caesalpinia ferrea",
        str_detect(tree_species_name_clean, "triplaris amaericana") ~ "triplaris amaericana",
        str_detect(
          tree_species_name_clean,
          "peltophorum dubium|peltorphoorium dubium"
        ) ~ "peltophorum dubium",
        str_detect(
          tree_species_name_clean,
          "pentaclethra macrophilla|pentaclehtra macrophylla|pentaclehtra macrophilla|pentachlethra macrophylla"
        ) ~ "pentaclethra macrophylla",
        str_detect(tree_species_name_clean, "peschiera fuchsiaefolia") ~ "peschiera fuchsiaefolia",
        str_detect(tree_species_name_clean, "piptadenia paniculata") ~ "piptadenia paniculata",
        str_detect(
          tree_species_name_clean,
          "plathymenia reticulata|platimenia reticulata|platymenia reticulata"
        ) ~ "plathymenia reticulata",
        str_detect(
          tree_species_name_clean,
          "platymenia foliolosa|plathymenia foliolosa"
        ) ~ "plathymenia foliolosa",
        str_detect(
          tree_species_name_clean,
          "platymiscium aff hebestachyum") ~ "platymiscium hebestachyum",
        str_detect(tree_species_name_clean, "poeppgia procera") ~ "poeppgia procera",
        str_detect(tree_species_name_clean, "pourouma guianensis") ~ "pourouma guianensis",
        str_detect(tree_species_name_clean, "pouteria gardneri") ~ "pouteria gardneri",
        str_detect(tree_species_name_clean, "priora balsamifera") ~ "prioria balsamifera",
        str_detect(tree_species_name_clean, "protium aracouchini") ~ "protium aracouchini",
        str_detect(tree_species_name_clean, "pseudobombax grandiflorum") ~ "pseudobombax grandiflorum",
        str_detect(tree_species_name_clean, "pseudobombax munguba") ~ "pseudobombax munguba",
        str_detect(tree_species_name_clean, "pseudolmedia laevis") ~ "pseudolmedia laevis",
        str_detect(tree_species_name_clean, "psidium cattleanum|psidium cattleianum") ~ "psidium cattleianum",
        str_detect(tree_species_name_clean, "psidium myrtoides") ~ "psidium myrtoides",
        str_detect(tree_species_name_clean, "psidium rufum") ~ "psidium rufum",
        str_detect(tree_species_name_clean, "pterigota brasiliensis") ~ "pterigota brasiliensis",
        str_detect(tree_species_name_clean, "centrolobium microcaete") ~ "centrolobium microcaete",
        str_detect(tree_species_name_clean, "pterocarpus rohrii") ~ "pterocarpus rohrii",
        str_detect(tree_species_name_clean, "rapanea ferruginea") ~ "rapanea ferruginea",
        str_detect(tree_species_name_clean, "schefflera morototoni") ~ "schefflera morototoni",
        str_detect(
          tree_species_name_clean,
          "schizolobium parahyba|schysolobium parayba|scizolobium parahyba"
        ) ~ "schizolobium parahyba",
        str_detect(tree_species_name_clean, "senegalia polyphylla") ~ "senegalia polyphylla",
        str_detect(tree_species_name_clean, "senna alata") ~ "senna alata",
        str_detect(tree_species_name_clean, "senna macranthera") ~ "senna macranthera",
        str_detect(tree_species_name_clean, "senna multijuga") ~ "senna multijuga",
        str_detect(tree_species_name_clean, "socratea exorrhiza") ~ "socratea exorrhiza",
        str_detect(tree_species_name_clean, "sparattanthelium botocudorum") ~ "sparattanthelium botocudorum",
        str_detect(tree_species_name_clean, "sparattosperma leucanthum") ~ "sparattosperma leucanthum",
        str_detect(tree_species_name_clean, "spondias macrocarpa") ~ "spondias macrocarpa",
        str_detect(tree_species_name_clean, "spondias venulosa") ~ "spondias venulosa",
        str_detect(tree_species_name_clean, "tabebuia chrysanthav") ~ "tabebuia chrysanthus",
        str_detect(tree_species_name_clean, "terminalia fagifolia") ~ "terminalia fagifolia",
        str_detect(tree_species_name_clean, "terminalia tetraphylla") ~ "terminalia tetraphylla",
        str_detect(tree_species_name_clean, "theobrama speciosum") ~ "theobrama speciosum",
        str_detect(tree_species_name_clean, "thyrsodium schomburgkianum") ~ "thyrsodium schomburgkianum",
        str_detect(tree_species_name_clean, "tibouchina granulosa") ~ "tibouchina granulosa",
        str_detect(tree_species_name_clean, "trattinickia rhoifolia") ~ "trattinickia rhoifolia",
        str_detect(tree_species_name_clean, "vataireopsis araroba") ~ "vataireopsis araroba",
        str_detect(tree_species_name_clean, "virola venosa") ~ "virola venosa",
        str_detect(tree_species_name_clean, "zieria arboresence|ziera arboresence") ~ "zieria arborescens",
        str_detect(tree_species_name_clean, "ziziphus mucronata") ~ "ziziphus mucronata",
        str_detect(tree_species_name_clean, "albizia aldantifolio") ~ "albizia adianthifolia",
        str_detect(tree_species_name_clean, "disthamenanthus bentamianus") ~ "distemonanthus benthamianus",
        str_detect(tree_species_name_clean, "crateva andansonii subsp odorata") ~ "crateva adansonii subsp. odora",
        str_detect(tree_species_name_clean, "peltogyne vogel") ~ "peltogyne vogel",
        str_detect(tree_species_name_clean, "pseudosamanea tabaco") ~ "pseudosamanea",
        str_detect(tree_species_name_clean, "pterogyne nitens") ~ "pterogyne nitens",
        str_detect(tree_species_name_clean, "tabernaemontana catharinensis") ~ "tabernaemontana catharinensis",
        str_detect(tree_species_name_clean, "schinus terebentifolius") ~ "schinus terebinthifolius",
        str_detect(tree_species_name_clean, "xanthorcesis zambeasiaca") ~ "xanthocercis zambesiaca",
        str_detect(tree_species_name_clean, "andira legalis") ~ "andira legalis",
        str_detect(tree_species_name_clean, "chorisia specios") ~ "chorisia speciosa",
        tree_species_name_clean %in% c("albizia sp") ~ "albizia",
        tree_species_name_clean %in% c("cassia sp") ~ "cassia",
        tree_species_name_clean %in% c("cedrela spp") ~ "cedrela",
        str_detect(tree_species_name_clean, "ekerbagia capensis|ekerbergia kapensis") ~ "ekebergia capensis",
        str_detect(tree_species_name_clean, "eucalyptus maideni") ~ "eucalyptus maidenii",
        str_detect(tree_species_name_clean, "ficus eriobotryoidesa") ~ "eriobotrya japonica",
        str_detect(tree_species_name_clean, "markamia lutea") ~ "markhamia lutea",
        str_detect(tree_species_name_clean, "vachellia abyssinica") ~ "vachellia abyssinica",
        TRUE ~ tree_species_name_clean
      )
  )

# subset of data for PPC to edit ------------------------------------------

data_sub <- data %>%
  filter(tree_species_name_clean == "ako" |
         tree_species_name_clean == "adida" |
           tree_species_name_clean == "chopwa" |
           tree_species_name_clean == "ilbamaba" |
           tree_species_name_clean == "imwemwe" |
           tree_species_name_clean == "kwaker" |
           tree_species_name_clean == "mbimbel" |
           tree_species_name_clean == "mfsp" |
           str_detect(tree_species_name_clean, "scientific tree_species_name_clean unknown") |
           tree_species_name_clean == "mimosia" |
           tree_species_name_clean == "mpoule" |
           tree_species_name_clean == "mulora" |
           tree_species_name_clean == "olong" |
           tree_species_name_clean == "plume de madagascar" |
           tree_species_uuid %in% c("570f420c-14d4-4d98-9072-911f251212eb", "8013e4b4-f547-4058-8677-cebab96c8e5f") |
           tree_species_name_clean == "paralonchurus brasiliensis" |
           tree_species_name_clean == "resinosa richardii" |
           tree_species_name_clean == "brachtelia" |
           tree_species_name_clean == "bois vert" |
           tree_species_name_clean == "combuja" |
           tree_species_name_clean == "vangeulia indica"
           )

# tree species to check with PMs (12/24/24) :
# c3744217-0bd8-4e33-962f-e02775cd0740 (Ako)
# 9fd82442-9550-49a7-8ac0-b378fff682d7 (Adida)
# 1f5e3569-1f39-4fed-bd24-bd2625a78d07 (chindora)
# 57547c12-5ad7-4eb4-be32-929397b63fee (chindora)
# 3c1fec89-4ad7-4c10-b0d9-9a82bbd94183 (chopwa)
# f6954c0d-901c-446f-b330-77d3acd9439b (kachere)
# 0c2a69e7-3d40-4847-bd75-8151fbfde1b1 (kaumbwe)
# 73c04380-604c-4887-85de-c44aab8f12ce (ilbamba)
# 59043317-55c8-4245-8752-32e456e97571 (imwemwe)
# 7cd11e45-4a1f-4fea-8f35-bc7a0303466e (kwaker)
# c1d06b56-4790-4359-ad29-e54059b613c6 (mbimbel)
# mfsp 1 - 10
# f2880ec6-75da-4cf4-b465-a91aec5adef3, 9c410343-79d7-4390-a43d-c39b25757a02, afdbb295-f59d-4496-be79-0b55e0c3d9aa (mgoza)
# mimosia
# 83355e03-b5b3-4f2c-b569-468a2c6de472 (mpoule)
# 4c6b962e-c95d-45f7-9a4c-07340be74333 (mulora)
# 2689b4ec-07bd-4e77-8401-07bfbb627bef (olong)
# 16317ffb-ee44-466d-aff4-8448860a24cd (openg)
# 74e8974a-a9bd-4e6b-adf5-76885c5b233d (plume de madagascar)
# 570f420c-14d4-4d98-9072-911f251212eb (psidium cattleianumjacaranda mimosaefolia d don)
# 8013e4b4-f547-4058-8677-cebab96c8e5f (senna alata, pithecellobium dulce, homalium trichostemon, ceiba aesculifolia, leucaena lanceolata, tabebuia donnel-smithii,)
# f7719017-699e-45b9-a8b1-267c2aeca1f3, 41226d86-1083-4679-9bc8-2821238e2891 (paralonchurus brasiliensis) - that's a fish
# 6d734235-57b8-4565-a04e-da95c12f8d75 (resinosa richardii)
# 49a91ea6-8a97-4a23-8099-1a070f836045 (brachtelia)
# 8efe9e53-93a5-4a6f-bee0-3217990d59d3 (bois vert)
# c6787bb9-c17c-448a-83f4-3579d7430cb6, 114bfe7b-a30a-4540-92e5-96472c477689 (combuja)
# 828f5454-cf8c-4d90-8677-2966126e9e63, d46eeac5-a387-4bc0-91b1-6ddca25ca82b, 7989fc6f-7a8a-4313-9d33-285863b77fb0 (vangeulia indica)

# tree_species_uuid to edit in TM (12/24/24):
# separate the below:
# cf82e6de-2fd1-4c27-833b-078c8e53160b
# 9103185c-4013-454a-a4fc-0e745b298268
# 039069e2-5698-41cb-a01a-7e72af94ae66
# 2c9d6858-4b7b-467c-9314-529010abfbbe
# 96e3c2ec-bd73-4e2e-8114-2c68a6f4c305
# bde5ec58-c6f0-4863-b16e-243249eab314
# b0a72f1c-f3f4-4607-a7be-97b5f9f48a97
# ab8d3b6a-a970-4833-8bd3-27974d2717d6
# e1d9644c-ad65-4f17-97f9-8dabed3d3c02
# b7da3b95-4bdf-46b9-867b-eb6650df417b
# 98c2d514-7629-42fc-a310-fc9eb1e61042
# 1746c896-5a6c-4ff3-b32d-45aa582ac4f5
# 9a29b34f-205a-472a-8e7c-c3a1349c5ed3
# c27c9c74-3964-4184-aa9c-d6d6574b9945
# 4c671067-c3bc-4811-9f3d-13d616953fe6
# 3a5bff3c-4e21-432f-83c2-2060627acf54 (separate into 2 rows)
# 77b80499-08e0-4e00-8453-d33f696c32f5 (separate into 2 rows)
# e3ba7757-a42d-45bb-b8ab-cb743019118d (separate into 2 rows)
# c50c9c08-4c17-47aa-b50a-80a1e0a75a2a (separate into 2 rows)
# 655ce30d-f0f5-4498-92e4-3b09a81912dd (separate into 2 rows)
# 4eab904e-ac8e-4cd3-a960-d31029fedb54 (separate into 2 rows)
# 1626e603-808d-4af4-9072-ce290466e319 (separate into 2 rows)

#subset_to_edit <- data %>%
#  filter(tree_species_uuid %in% c("3a5bff3c-4e21-432f-83c2-2060627acf54", "77b80499-08e0-4e00-8453-d33f696c32f5", "e3ba7757-a42d-45bb-b8ab-cb743019118d",
#                                  "c50c9c08-4c17-47aa-b50a-80a1e0a75a2a", "655ce30d-f0f5-4498-92e4-3b09a81912dd", "4eab904e-ac8e-4cd3-a960-d31029fedb54",
#                                  "1626e603-808d-4af4-9072-ce290466e319"))

# Make first letter capitalized -------------------------------------------

data <- data %>%
  mutate(tree_species_name_clean = str_to_sentence(tree_species_name_clean))

# drop NAs ----------------------------------------------------------------

data <- data %>%
  filter(!is.na(tree_species_name_clean))

data %>%
  distinct(tree_species_name_clean, tree_species_name) %>%
  arrange(tree_species_name_clean) %>%
  view()

# save data ---------------------------------------------------------------

timestamp <- format(Sys.time(), "%Y-%m-%d_%H-%M-%S")

# all NULL taxon ID data



# Cleaned TerraFund project report data
#write_excel_csv(data,
#                file = here(
#                  "Tree Species",
#                  "Data",
#                  "Processed",
#                  "TerraFund Tree Species",
#                  paste0(
#                    "01_clean_terrafund_tree_species_project_reports_",
#                    timestamp,
#                    ".csv"
#                  )
#                ))

# Cleaned TerraFund project establishment data
#write_excel_csv(data,
#                file = here(
#                  "Tree Species",
#                  "Data",
#                  "Processed",
#                  "TerraFund Tree Species",
#                  paste0(
#                    "01_clean_terrafund_tree_species_project_establishment_",
#                    timestamp,
#                    ".csv"
#                  )
#                ))

# Cleaned PPC project report data
#write_excel_csv(data,
#                file = here(
#                  "Tree Species",
#                  "Data",
#                  "Processed",
#                  "PPC Tree Species",
#                  paste0(
#                    "01_clean_ppc_tree_species_project_reports_",
#                    timestamp,
#                    ".csv"
#                  )
#                ))

# Cleaned PPC establishment data
#write_excel_csv(data,
#                file = here(
#                  "Tree Species",
#                  "Data",
#                  "Processed",
#                  "PPC Tree Species",
#                  paste0(
#                    "01_clean_ppc_tree_species_project_establishment_",
#                    timestamp,
#                    ".csv"
#                  )
#                ))
#
#write_excel_csv(subset_to_edit,
#                file = here(
#                  "Tree Species",
#                  "Data",
#                  "Processed",
#                  "TerraFund Tree Species",
#                  paste0(
#                    "terrafund_project_establishment_tree_species_to_separate_",
#                    timestamp,
#                    ".csv"
#                  )
#                ))
