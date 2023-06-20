# Sys.setlocale(locale = "Danish.utf8")
Sys.setlocale(locale = "English_United Kingdom.utf8")

if (!"pacman" %in% installed.packages()) install.packages("pacman")

# Make sure some packages are installed, even though we don't have to load them
pacman::p_install("scales", force = FALSE)
pacman::p_install("rmarkdown", force = FALSE)
pacman::p_install("knitr", force = FALSE)
pacman::p_install("leaflet", force = FALSE)
pacman::p_install("curl", force = FALSE)
pacman::p_install("weights", force = FALSE)
pacman::p_install("kableExtra", force = FALSE)

# Load packages
pacman::p_load(
  readr,
  dplyr,
  ggplot2,
  stringr,
  janitor,
  assertthat,
  lubridate,
  tidyr,
  readxl,
  writexl,
  sf,
  zoo,
  forcats,
  purrr,
  here,
  glue,
  cli,
  ISOweek,
  ggthemes,
  conflicted,
  scales,
  tidyquant,
  spatstat,
  details
)
conflict_prefer("filter", "dplyr", quiet = TRUE)
conflict_prefer("lag", "dplyr", quiet = TRUE)
conflict_prefer("between", "dplyr", quiet = TRUE)

# Load functions
source(here("R", "00-1_functions.R"), encoding = 'UTF-8')
source(here("R", "00-3_functions-aggregated-graphs.R"), encoding = 'UTF-8')

# ggplot default theme
theme_set(theme_minimal() +
            theme(panel.grid.minor = element_blank(),
                  # panel.grid.major.x = element_blank(),
                  plot.title.position = "plot",
                  plot.background = element_rect(fill = "white",
                                                 colour = "white"),
                  axis.text = element_text(colour = "grey40"),
                  axis.title = element_text(colour = "grey20"),
                  title = element_text(colour = "grey20"),
                  text = element_text(colour = "grey20")))

# List of geographies
geogs <- list()

geogs$landsdele <- c("Bornholm",
                     "Fyn",
                     "København",
                     "Københavns omegn",
                     "Nordjylland",
                     "Nordsjælland",
                     "Sydjylland",
                     "Vest- og Sydsjælland",
                     "Vestjylland",
                     "Østjylland",
                     "Østsjælland")

geogs$regioner <- c("Region Hovedstaden",
                    "Region Midtjylland",
                    "Region Nordjylland",
                    "Region Sjælland",
                    "Region Syddanmark")


# English versions to use in `recode()`. Included with and without the "Region "
# prefix.
geogs$regioner_english <- c(
  "Region Hovedstaden" = "Capital Region",
  "Region Midtjylland" = "Central Denmark",
  "Region Nordjylland" = "North Denmark",
  "Region Sjælland"    = "Zealand",
  "Region Syddanmark"  = "Southern Denmark",
  "Hovedstaden" = "Capital Region",
  "Midtjylland" = "Central Denmark",
  "Nordjylland" = "North Denmark",
  "Sjælland"    = "Zealand",
  "Syddanmark"  = "Southern Denmark"
)



# Flag that setup script has been run
isrun_setup <- TRUE
