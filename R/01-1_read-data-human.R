

# Read data ---------------------------------------------------------------
fp_dias_human <- "Q:/SSI/Spildevand/covid_positive"

fps_folders_dias_human <- list.dirs(fp_dias_human)

fp_last_folder_dias_human <- fps_folders_dias_human %>%
  keep(str_detect, "\\d{4}-\\d{2}-\\d{2}$") %>%
  sort() %>%
  tail(1) %>%
  str_extract("\\d{4}-\\d{2}-\\d{2}$")


sb_human <- cli::cli_status(msg = "Reading human: ")

raw_human <- glue(
  "{fp_dias_human}/{date}/Covid_pos_PCR_Antigen.xlsx",
  date = seq.Date(dmy("20 jun 2021"), ymd(fp_last_folder_dias_human),
                  by = "1 day")
) %>%
  as.character() %>%
  # Don't try to read the ones that don't exist (yet)
  keep(file.exists) %>%
  set_names(str_extract, "\\d{4}-\\d{2}-\\d{2}") %>%
  map_dfr(function(x) {
    # message(x)
    cli::cli_status_update(sb_human, c(
      "Reading human: ",
      str_remove_all(
        x,
        ".*covid_positive/|/Covid_pos_PCR_Antigen\\.xlsx"
      )
    ))
    read_xlsx(x,
              col_types = c("text", "skip", "text", "date", "skip",
                            "skip", "skip", "skip", "skip", "numeric",
                            "numeric"))
  },
  .id = "file") %>%
  clean_names()

cli::cli_status_clear(sb_human)
cli::cli_alert_success("Finished reading human")


# Clean data --------------------------------------------------------------
cli::cli_alert("Cleaning human (within data reading step)")
# There are people who have positive antigen on day 1, and positive PCR on day
# 2. For them, choose first test, but add variable saying they also had PCR, so
# we're confident.
human <- raw_human %>%
  select(-file) %>%
  rename(cpr = cprnr10,
         date_test = minimum_positive_negative_prdate) %>%
  mutate_if(is.POSIXt, as.Date) %>%
  mutate(testtype = factor(
    testtype,
    levels = c("PCR_test", "Antigen_test"),
    labels = c("pcr", "antigen")
  )) %>%
  # For each person, add a column for if they ever had a positive PCR, and only
  # keep their first test (preferring PCR if two on same day)
  arrange(date_test, testtype) %>%
  select(-testtype) %>%
  group_by(cpr) %>%
  slice_head(n = 1) %>%  # choose only one row per CPR number
  ungroup() %>%
  select(
    date_test,
    x_koordinat,
    y_koordinat,
  ) %>%
  arrange(date_test)


cli::cli_alert("Geocoding human from X and Y")

shp_human <- human %>%
  filter(!is.na(x_koordinat), !is.na(y_koordinat)) %>%
  st_as_sf(coords = c("x_koordinat", "y_koordinat"),
           crs = st_crs(25832))

cli::cli_alert_success("Human data read and cleaned")
