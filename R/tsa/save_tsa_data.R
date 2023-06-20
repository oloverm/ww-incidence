# Saving all the bits of data to be able to run the analysis after leaving SSI.

# Read from scratch
read_ww(cache_limit = days(7))

# OWID hospitalisation data
raw_owid <- read_csv(
  "https://covid.ourworldindata.org/data/owid-covid-data.csv",
  col_types = cols(
    .default = col_skip(),
    location = col_character(),
    date = col_date(format = ""),
    icu_patients = col_double(),
    # icu_patients_per_million = col_double(),
    hosp_patients = col_double(),
    # hosp_patients_per_million = col_double(),
    # weekly_icu_admissions = col_double(),
    # weekly_icu_admissions_per_million = col_double(),
    weekly_hosp_admissions = col_double(),
    new_cases = col_double(),
    # weekly_hosp_admissions_per_million = col_double(),
    new_tests_per_thousand = col_double(),
  )
)

# Care home testing data
raw_pleje <- read_xlsx("Q:/SUMkoncern/Signalgruppe/Personale i ældreplejen/PLEJE_SUND_test_04AUG2022.xlsx")


# Variant data
load(here("data/extra_data/variant_week_Oliver.RData"))



list.dirs("Q:/SUMkoncern/zip_ArcGIS_Dashboards_hjemmeside",
          recursive = FALSE) %>%
  max() %>%
  paste0("/ArcGIS_covid_DB_data.zip") %>%
  zip::unzip(
    files = c("Regionalt_DB/08_bekraeftede_tilfaelde_pr_dag_pr_regions.csv",
              "Regionalt_DB/16_pcr_og_antigen_test_pr_region.csv"),
    exdir = tempdir())


raw_regional_population <- read_excel(
  fp_latest(here("data"), "pops-regioner\\.xlsx")
)

raw_regional_cases <- read_csv_dk(
  glue(tempdir(), "/Regionalt_DB/08_bekraeftede_tilfaelde_pr_dag_pr_regions.csv"),
)

raw_regional_tests <- read_csv_dk(
  glue(tempdir(), "/Regionalt_DB/16_pcr_og_antigen_test_pr_region.csv")
)




#
# ls()[ls() %>%
#   map(get) %>%
#   map_lgl(~ is.data.frame(.) | is.vector(.))]
#
# xxxx <- ls()[ls() %>%
#        map(get) %>%
#        map_lgl(~ is.data.frame(.) | is.vector(.))]
#
# tibble(name = xxxx) %>%
#   rowwise() %>%
#   mutate(size = object.size(get(name))) %>%
#   ungroup() %>%
#   arrange(desc(size)) %>%
#   mutate(size = size %>% format(units = "Mb"))
#
#
# ls() %>%
#   map(get) %>%
#   map_dbl(object.size)




save(
  all_ww,
  combined_ww,
  dk_human,
  geogs,
  # grading_colours,
  # isrun_setup,
  lookup_oplande_names,
  # lookup_oplande_names_all_list,
  # lookup_oplande_names_new,
  owid_hosp,
  pops,
  raw_owid,
  raw_pleje,
  raw_regional_cases,
  raw_regional_population,
  raw_regional_tests,
  regional_human,
  # shp_oplande,
  wgs_week_export,
  ww,
  ww_human,
  file = here("R/wip/tsa/tsa_data.RData")
)
