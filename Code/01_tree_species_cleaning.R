# Description -------------------------------------------------------------

# Author: Ben Steiger
# Date Created: 07/03/2024
# Last Updated: 10/30/2024
# Description: Cleaning TerraFund Project Report Tree Species Data before Matching
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

data <- read.csv(
  here(
    "Tree Species",
    "Data",
    "Raw",
    "TerraFund Tree Species",
    "terrafund_tree_species_202410301559.csv"
  ),
  encoding = "UTF-8"
)

# convert project_data_raw columns to snake_case ------------------------------

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

data <- data %>%
  filter(!project_name %in% c("3SC Production 2.3", "Thef"))

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
    tree_species_name_clean = str_remove_all(
      tree_species_name_clean,
      "spp |\\bspp\\b|\\bsp\\b|\\bspecies\\b|species feb|\\bspecie\\b|\\bssp\\b"
    ),
    tree_species_name_clean = str_remove_all(tree_species_name_clean, ""),
    tree_species_name_clean = str_replace_all(tree_species_name_clean, "’", "'"),
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
        "miombo"
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
      "avocatier|avocat|ovacado|avacado|aavocado|avocadoes|avocados|ovacodos|ovocado|avovado",
      "avocado"
    ),
    tree_species_name_clean = str_replace_all(
      tree_species_name_clean,
      "percea|persa|persia|persis|\\bperisa\\b",
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
      "gravellia|gravilia|grevelia|grevelea|grebillea|grevellea|grevillia|gravellea|gravillea|\\bgrave\\b|gravellae|greveillea|grevellia|greveria|grevilia|grivellea",
      "grevillea"
    ),
    tree_species_name_clean = str_replace_all(tree_species_name_clean, "upaca", "uapaca"),
    tree_species_name_clean = str_replace_all(
      tree_species_name_clean,
      "ctrucs|ctirus|ctirus|cirtrus",
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
      "lucena|luceana|lucaena|leauceana|leucena",
      "leucaena"
    ),
    tree_species_name_clean = str_replace_all(tree_species_name_clean, "\\babizia\\b|albizzia", "albizia")
  )

# case_when str_replace ---------------------------------------------------

data <- data %>%
  mutate(
    tree_species_name_clean =
      case_when(
        str_detect(tree_species_name_clean, "elaeis guineensis|\\bpalmia\\b") ~ "elaeis guineensis",
        str_detect(tree_species_name_clean, "avocado|persea") ~ "persea americana",
        str_detect(
          tree_species_name_clean,
          "cashew|anacarde anacardium occidentale|anacardium occidentale|\\banacardium\\b"
        ) ~ "anacardium occidentale",
        str_detect(
          tree_species_name_clean,
          "andasonia digitata|adansonia digitata|andansonia digitata|adasonia digitata|anansonia digitata"
        ) ~ "adansonia digitata",
        str_detect(
          tree_species_name_clean,
          "acacia angustissima|acasia angustissima"
        ) ~ "acacia angustissima",
        str_detect(tree_species_name_clean, "arundinaria alpine") ~ "arundinaria alpina",
        str_detect(tree_species_name_clean, "bamboo yushania") ~ "yushania",
        str_detect(
          tree_species_name_clean,
          "bamboo bamboosodea|bamboo bambusodea|bamboo"
        ) ~ "bambusa",
        str_detect(
          tree_species_name_clean,
          "citrus limon|citronier|citronnier|lemon|citroniers|citrus limom|\\blimon\\b"
        ) ~ "citrus limon",
        str_detect(
          tree_species_name_clean,
          "mangifera indica|mangifera endica|mangifer endica|\\bmango\\b|\\bmangoes\\b|\\bmangos\\b|manjifera indidca"
        ) |
          tree_species_name_clean %in% c("mangifera") ~ "mangifera indica",
        str_detect(tree_species_name_clean, "cassava|casaava") ~ "manihot glaziovii",
        str_detect(tree_species_name_clean, "orita multicola|achuechue") ~ "lannea welwitschii",
        str_detect(
          tree_species_name_clean,
          "\\bmaos\\b|\\bmaize\\b|\\bmao\\b|\\bmais\\b|\\bmaoes\\b|\\bzea\\b|\\bmai\\b"
        ) ~ "zea mays",
        str_detect(tree_species_name_clean, "uapaca spp|milanga") ~ "uapaca",
        str_detect(tree_species_name_clean, "uapaca guineensis") ~ "uapaca guineensis",
        str_detect(tree_species_name_clean, "soja") ~ "glycine max",
        str_detect(
          tree_species_name_clean,
          "balanities eygptica|balanites aegyptiaca"
        ) ~ "balanites aegyptiaca",
        str_detect(
          tree_species_name_clean,
          "robusta coffee|cafe robuster|cafe robusta|cofea robusta|caffea robusta"
        ) ~ "coffea robusta",
        str_detect(
          tree_species_name_clean,
          "coffea arabica|cofea arabica|cafe arabica"
        ) ~ "coffea arabica",
        str_detect(
          tree_species_name_clean,
          "african tulip tree|spathodea campanulata|african tulip"
        ) ~ "spathodea campanulata",
        str_detect(
          tree_species_name_clean,
          "terminalis ivonronsis|terminalia ivorensis|ivory coast almond|tamalia ivorensis|gbaji|\\bemire\\b|\\bframiré\\b"
        ) ~ "terminalia ivorensis",
        str_detect(
          tree_species_name_clean,
          "acajou de bassam|accajou de bassam|khaya ivorensis|khama ivorensis|khya ivorensis|ivorensis"
        ) ~ "khaya ivorensis",
        str_detect(tree_species_name_clean, "khaya senegalensis") ~ "khaya senegalensis",
        str_detect(tree_species_name_clean, "swietenia macrophylla") ~ "swietenia macrophylla",
        str_detect(tree_species_name_clean, "khaya anthotheca|khaya anthethe") ~ "khaya anthotheca",
        str_detect(tree_species_name_clean, "khaya grandifolia") ~ "khaya grandifolia",
        str_detect(tree_species_name_clean, "khaya senegalensis") ~ "khaya senegalensis",
        str_detect(tree_species_name_clean, "\\bmahogany\\b|\\bmahagany\\b") ~ "khaya",
        str_detect(
          tree_species_name_clean,
          "terminalia superba|terminalia super|terminalia of superba"
        ) ~ "terminalia superba",
        str_detect(tree_species_name_clean, "\\btree tomatoes\\b|tamarillo") ~ "solanum betaceum",
        str_detect(
          tree_species_name_clean,
          "\\btomato\\b|\\btomate\\b|\\btomates\\b|\\btomatoes\\b|\\btotame\\b|\\btomaotoes\\b"
        ) ~ "solanum lycopersicum",
        str_detect(tree_species_name_clean, "trema orientalis") ~ "trema orientalis",
        str_detect(tree_species_name_clean, "papaya|pawpaw|paw paw|papayer") ~ "carica papaya",
        str_detect(tree_species_name_clean, "crossolier|corossolier") ~ "annona muricata",
        str_detect(tree_species_name_clean, "annona cherimola") ~ "annona cherimola",
        str_detect(tree_species_name_clean, "heritaria utilis|haritaria utilis|heritaria utlis") ~ "heritiera utilis",
        str_detect(
          tree_species_name_clean,
          "melia azedach|melia azerach|china berry|chinaberry"
        ) ~ "melia azedarach",
        str_detect(
          tree_species_name_clean,
          "melia volkensii"
        ) ~ "melia volkensii",
        str_detect(
          tree_species_name_clean,
          "isobelina duka|isobelinia duka|isoberlinia duka|isoberlina doka|etenghi"
        ) ~ "isoberlinia doka",
        str_detect(
          tree_species_name_clean,
          "senna siamea|senna enamea|senna senamia"
        ) ~ "senna siamea",
        str_detect(tree_species_name_clean, "bauhinia petersianasian") ~ "bauhinia petersiana",
        str_detect(tree_species_name_clean, "plantin|plantain|plaintain") ~ "musa x paradisiaca",
        str_detect(
          tree_species_name_clean,
          "musaceae banana|\\bbananas\\b|\\bbanana\\b"
        ) ~ "musa acuminata",
        str_detect(
          tree_species_name_clean,
          "faidherbia albida|faideiherbia albida|musangu"
        ) ~ "faidherbia albida",
        tree_species_name_clean %in% c("apple", "apples") ~ "malus domestica",
        str_detect(
          tree_species_name_clean,
          "syzgium guineense|syzygium guineese|syzygium quinensi|syzigium guineines"
        ) ~ "syzygium guineense",
        str_detect(tree_species_name_clean, "vitex payos") ~ "vitex payos",
        str_detect(tree_species_name_clean, "swiss chard|beetroot") ~ "beta vulgaris",
        str_detect(tree_species_name_clean, "\\bdecurrens\\b") ~ "acacia decurrens",
        str_detect(tree_species_name_clean, "\\bneem\\b|axadirachta") |
          tree_species_name_clean %in% c("azadirachita", "azadirachta") ~ "azadirachita indica",
        str_detect(
          tree_species_name_clean,
          "swatziz madagacahensis|swartzia madagacahensis"
        ) ~ "swartzia mangabalensis",
        str_detect(tree_species_name_clean, "beans|bean - planted in ha") ~ "vicia",
        str_detect(tree_species_name_clean, "brastchegia spiciformis") ~ "brachystegia spiciformis",
        str_detect(tree_species_name_clean, "cedrella ordorata|cedrella odorata") ~ "cedrela odorata",
        str_detect(tree_species_name_clean, "cedrela angusifolia") ~ "cedrela angustifolia",
        tree_species_name_clean %in% c("cedrela") ~ "cedrela serrata",
        str_detect(tree_species_name_clean, "ciba patendo|ceiba pentandra|ceiba pendratra") ~ "ceiba pentandra",
        str_detect(tree_species_name_clean, "cola acuminata") ~ "cola acuminata",
        str_detect(tree_species_name_clean, "cola lepidota") ~ "cola lepidota",
        str_detect(tree_species_name_clean, "petit cola") ~ "garcinia kola",
        str_detect(tree_species_name_clean, "cola laleritia") ~ "cola laleritia",
        str_detect(tree_species_name_clean, "cola lepidota|monkey kola") ~ "cola lepidota",
        str_detect(tree_species_name_clean, "combtryum zeyheri") ~ "combretum zeyheri",
        str_detect(tree_species_name_clean, "\\bcoton\\b") ~ "croton",
        str_detect(tree_species_name_clean, "croton megalocarpus") ~ "croton megalocarpus",
        str_detect(
          tree_species_name_clean,
          "cupressaceae lustanica|cypress lusitanica|cupressus lusitanica"
        ) ~ "cupressus lusitanica",
        str_detect(tree_species_name_clean, "cypriot cedar") ~ "cedrus brevifolia",
        str_detect(tree_species_name_clean, "douglas fir") ~ "pseudotsuga menziesii",
        str_detect(tree_species_name_clean, "erithrina lystermon") ~ "erythrina lysistemon",
        str_detect(tree_species_name_clean, "eucalyptus camalnd") ~ "eucalyptus camaldulensis",
        str_detect(tree_species_name_clean, "eucalnus saligna") ~ "eucalyptus saligna",
        str_detect(tree_species_name_clean, "acacia saligina") ~ "acacia saligna",
        str_detect(tree_species_name_clean, "ficus vallis choudea") ~ "ficus vallis-choudae",
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
          "mandariniers|mandarins|citrus reticula"
        ) ~ "citrus reticulata",
        str_detect(
          tree_species_name_clean,
          "citrus sinensis|citrus x sinensis|citrus ◊ sinensis|\\borange\\b|\\boranges\\b|\\boranger\\b|\\borangers\\b"
        ) ~ "citrus sinensis",
        str_detect(tree_species_name_clean, "mansonia altissima|mansonia ultisima") ~ "mansonia altissima",
        str_detect(tree_species_name_clean, "musanga cecropioides") ~ "musanga cecropioides",
        str_detect(
          tree_species_name_clean,
          "measopsis eminni|maesopsis e|maesopses eminii|maesospsis eminii|musizi|\\bmaesopsis\\b"
        ) ~ "maesopsis eminii",
        str_detect(tree_species_name_clean, "millitia ferruginol") ~ "millettia ferruginea",
        str_detect(tree_species_name_clean, "mitragyna librostipulosa") ~ "mitragyna stipulosa",
        str_detect(
          tree_species_name_clean,
          "moringa oleifera|moringa oleifer|moringa oleifere"
        ) | tree_species_name_clean %in% c("moringa") ~ "moringa oleifera",
        str_detect(tree_species_name_clean, "morus alba") ~ "morus alba",
        str_detect(tree_species_name_clean, "parkia biglobosa") ~ "parkia biglobosa",
        str_detect(tree_species_name_clean, "parkia bicolor|parquia biclor") ~ "parkia bicolor",
        str_detect(
          tree_species_name_clean,
          "olea europaea sub cuspidate|olea europaea subsp cuspidata"
        ) ~ "olea africana",
        str_detect(tree_species_name_clean, "olea capensis") ~ "olea capensis",
        str_detect(tree_species_name_clean, "olea welwichii|olea wewyshii") ~ "olea welwitschii",
        str_detect(tree_species_name_clean, "passion fruits") ~ "passiflora edulis",
        str_detect(tree_species_name_clean, "philosyigma thonnongi") ~ "piliostigma thonningii",
        str_detect(tree_species_name_clean, "pidgeon pea|pigeon pea") ~ "cajanus cajan",
        str_detect(tree_species_name_clean, "mkpen|pterocarpus erinaceus") ~ "pterocarpus erinaceus",
        str_detect(tree_species_name_clean, "mimuspos kummel") ~ "mimuspos kummel",
        str_detect(tree_species_name_clean, "pear packham's triumph") ~ "pyrus communis",
        str_detect(
          tree_species_name_clean,
          "\\bpumpkin\\b|\\bpumpkins\\b|\\bcourge\\b"
        ) ~ "cucurbita pepo",
        str_detect(tree_species_name_clean, "rahminus prinoides") ~ "rhamnus prinoides",
        str_detect(tree_species_name_clean, "ricinodondron heudeleuti|ricinodondron heudelotii") ~ "ricinodendron heudelotii",
        str_detect(tree_species_name_clean, "sausage tree|\\bkigelia\\b") ~ "kigelia africana",
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
        str_detect(tree_species_name_clean, "ricinodendron heudelotii") ~ "ricinodendron heudelotii",
        str_detect(tree_species_name_clean, "tieghemella heckelii") ~ "tieghemella heckelii",
        str_detect(
          tree_species_name_clean,
          "cocoa|theobroma cacao|cacao|theobroma caoa|theobroma cocoa"
        ) ~ "theobroma cacao",
        str_detect(
          tree_species_name_clean,
          "coconuts|coccos nucifera|cocos nucifera"
        ) ~ "cocos nucifera",
        str_detect(tree_species_name_clean, "pomagranate|pomegranate") ~ "punica granatum",
        str_detect(tree_species_name_clean, "cabbage|kale|brocoli") ~ "brassica oleracea",
        str_detect(
          tree_species_name_clean,
          "grevillea robusts|grevillea robusta|\\bgrevillea\\b|southern silky oak"
        ) ~ "grevillea robusta",
        str_detect(tree_species_name_clean, "theka|teak") ~ "milicia excelsa",
        str_detect(tree_species_name_clean, "cola boxiana") ~ "cola boxiana",
        str_detect(tree_species_name_clean, "capsicum frutescens") ~ "capsicum frutescens",
        str_detect(tree_species_name_clean, "\\bpepper\\b|\\bpoivrier\\b") ~ "piper nigrum",
        str_detect(tree_species_name_clean, "\\bbirch\\b") ~ "betula",
        str_detect(tree_species_name_clean, "ficus sycamora") ~ "ficus sycomorus",
        str_detect(tree_species_name_clean, "celery") ~ "apium graveolens",
        str_detect(tree_species_name_clean, "grewia bicolar") ~ "grewia bicolor",
        str_detect(tree_species_name_clean, "maple") ~ "acer",
        str_detect(tree_species_name_clean, "haricot") ~ "phaseolus vulgaris",
        str_detect(
          tree_species_name_clean,
          "leucaena leucocephala|luceana leucocephala|leuceana leucocephala|leuceana lycocefala|leucaena lecocephala|leucaena leucecephala|leucaena leucocephiala|leucaena leucpcephala|luceane lococifra"
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
        str_detect(tree_species_name_clean, "ricinodendron heudoletii") ~ "ricinodendron heudoletii",
        str_detect(tree_species_name_clean, "\\bananas\\b") ~ "ananas comusus",
        str_detect(tree_species_name_clean, "calotropis procera") ~ "calotropis procera",
        str_detect(tree_species_name_clean, "eucalyptus/gum trees") ~ "eucalyptus",
        str_detect(
          tree_species_name_clean,
          "eucalyptus globules seedlings|eucalyptus glaulus|eucalyptus globlus|eucalyptus globules|eucalyptus globulus|teucalyptus globules"
        ) ~ "eucalyptus globulus",
        str_detect(tree_species_name_clean, "guajava|psidium guajava|guava") ~ "psidium guajava",
        str_detect(tree_species_name_clean, "eucalyptus mannifera") ~ "eucalyptus mannifera",
        str_detect(tree_species_name_clean, "amaranth|lengalenga") ~ "amaranth",
        str_detect(tree_species_name_clean, "\\bwillow\\b") ~ "salix",
        str_detect(tree_species_name_clean, "casimiroa edulis|white sapote") ~ "casimiroa edulis",
        str_detect(tree_species_name_clean, "semia chlorophora") ~ "chlorophora excelsa",
        str_detect(tree_species_name_clean, "paradise apple") ~ "malus dasyphylla",
        str_detect(
          tree_species_name_clean,
          "ziziphus morticians|ziziphus morticians|ziziphusmoriciana"
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
        tree_species_name_clean %in% c("prunus", "prunas") ~ "prunus africana",
        str_detect(tree_species_name_clean, "prunus persic") ~ "prunus persica",
        tree_species_name_clean %in% c("mwanga") ~ "pericopsis angolensis",
        str_detect(tree_species_name_clean, "pericopsis elata") ~ "pericopsis elata",
        str_detect(tree_species_name_clean, "\\btsimitetra\\b") ~ "ilex mitis",
        str_detect(tree_species_name_clean, "leucaena dephasfolia") ~ "leucaena diversifolia",
        str_detect(tree_species_name_clean, "cocoyam") ~ "colocasia esculenta",
        str_detect(tree_species_name_clean, "entandrophragma angolense") ~ "entandrophragma angolense",
        str_detect(tree_species_name_clean, "entandrophragma cylindricum") ~ "entandrophragma cylindricum",
        str_detect(tree_species_name_clean, "allinum cepa|allunum cepa") ~ "allium cepa",
        str_detect(tree_species_name_clean, "aloe africana|aloe europea") ~ "aloe africana",
        str_detect(tree_species_name_clean, "aloe var barbadensis") ~ "aloe barbadensis",
        str_detect(tree_species_name_clean, "aloe var chinensis") ~ "aloe chinensis",
        str_detect(tree_species_name_clean, "aloe var secundiflora") ~ "aloe secundiflora",
        str_detect(tree_species_name_clean, "avicenia germinans") |
          tree_species_name_clean %in% c("avicennia") ~ "avicenia germinans",
        str_detect(tree_species_name_clean, "black thorn|honey acacias") ~ "senegalia mellifera",
        str_detect(tree_species_name_clean, "molucata") | tree_species_name_clean %in% c("mol") ~ "acacia mangium",
        str_detect(tree_species_name_clean, "carapa procera") ~ "carapa procera",
        str_detect(tree_species_name_clean, "cassia spectabilis") ~ "cassia spectabilis",
        str_detect(
          tree_species_name_clean,
          "cordia africana|cordial africana|cordyla africana"
        ) ~ "cordia africana",
        str_detect(tree_species_name_clean, "cucumber") ~ "cucumis sativus",
        str_detect(tree_species_name_clean, "eucalyptus gunnii") ~ "eucalyptus gunnii",
        str_detect(tree_species_name_clean, "hagenia abyssinica") ~ "hagenia abyssinica",
        tree_species_name_clean %in% c("indigofera so") ~ "indigofera",
        str_detect(tree_species_name_clean, "juniperus procera") ~ "juniperus procera",
        str_detect(
          tree_species_name_clean,
          "umbrella thorn|umbrella tree|umbrella invoresia"
        ) |
          tree_species_name_clean %in% c("umbrella") ~ "vachellia tortilis",
        tree_species_name_clean %in% c("macadamia") ~ "macadamia integrifolia",
        str_detect(tree_species_name_clean, "colophospermum mopane") ~ "colophospermum mopane",
        str_detect(tree_species_name_clean, "myrsine africana") ~ "myrsine africana",
        str_detect(tree_species_name_clean, "nuxia congesta") ~ "nuxia congesta",
        str_detect(
          tree_species_name_clean,
          "pseudolachnostylis|pseuudolachnostylis"
        ) ~ "pseudolachnostylis maprouneifolia",
        str_detect(tree_species_name_clean, "tropical almond") ~ "terminalia catappa",
        str_detect(tree_species_name_clean, "albizia lebbeck") ~ "albizia lebbeck",
        str_detect(tree_species_name_clean, "dombeya burgessiae") ~ "dombeya burgessiae",
        str_detect(tree_species_name_clean, "dombeya torrida|dombeya goetzenii|dombeya goetzi") ~ "dombeya torrida",
        str_detect(tree_species_name_clean, "monotes") ~ "monotes africanus",
        str_detect(tree_species_name_clean, "pennisetum purpureum") ~ "pennisetum purpureum",
        str_detect(tree_species_name_clean, "podo latifolia|podocarpus latifolia") ~ "podocarpus latifolius",
        str_detect(tree_species_name_clean, "schizolobium") ~ "schizolobium parahyba",
        str_detect(tree_species_name_clean, "zingiber|zingliber") | tree_species_name_clean %in% c("zing", "zi") ~ "zingiber officinale",
        str_detect(tree_species_name_clean, "royal pointiciana|flambodea") ~ "delonix regia",
        str_detect(tree_species_name_clean, "cornius volkensi") ~ "cornus volkensii",
        str_detect(tree_species_name_clean, "allophyllus abysssinica") ~ "allophylus abyssinicus",
        str_detect(tree_species_name_clean, "manihot scleroxylon") ~ "triplochiton scleroxylon", # might be incorrect - check with PM
        TRUE ~ tree_species_name_clean
      )
  )

# now start sifting through the matches, filtering fuzzy joins

# species I am unsure of:

# le ma
# mil
# wild crippers
# miombo
# le ha
# local grasses
# triplochiton scleroxylon
# acacia mangifera

# Make first letter capitalized -------------------------------------------

data <- data %>%
  mutate(tree_species_name_clean = str_to_sentence(tree_species_name_clean))

# drop NAs ----------------------------------------------------------------

data <- data %>%
  filter(!is.na(tree_species_name_clean))

data %>%
  distinct(tree_species_name, tree_species_name_clean) %>%
  arrange(tree_species_name_clean) %>%
  view()

# save data ---------------------------------------------------------------

# Cleaned project report data
write_excel_csv(
  data,
  file = here(
    "Tree Species",
    "Data",
    "Processed",
    "TerraFund Tree Species",
    "01_clean_terrafund_tree_species.csv"
  )
)
