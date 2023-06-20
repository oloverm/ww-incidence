# Missing shapes
missing_shapes <- lookup_oplande_names %>%
  filter(missing_shp,
         in_use == TRUE | is.na(in_use)) %>%
  select(anlaeg_eurofins, anlaeg_display,
         anlaeg_master, company_master,
         overlap_level, comments)




# Missing samples ---------------------------------------------------------



# # Places that never had a sample
# never_sent_sample <- lookup_oplande_names %>%
#   filter(in_use == TRUE | is.na(in_use)) %>%
#   select(anlaeg_display, company,
#          anlaeg_master, company_master,
#          overlap_level) %>%
#   anti_join(ww_human, by = c("anlaeg_display" = "anlaeg_rando"))

# Number of samples in reanalyse and which
no_samples_reanalyse <- all_ww %>%
  select(anlaeg_rando,
         proevested,
         date_receipt,
         sample,
         week,
         status,
         comments_proeve) %>%
  filter(week >= ISOweek(today()-days(7)),
         status == "Igang")




# Anlæg who have had samples before, but not in the latest week (latest week
# based on all anlægs' samples)
no_recent_samples <- ww_human %>%
  select(anlaeg_rando,
         proevested,
         date_receipt,
         sample,
         overlap_level,
         week) %>%
  filter(!is.na(sample),
         week >= ISOweek(today()-days(7))) %>%
  # For each anlæg, find out when they sent their last sample
  group_by(anlaeg_rando, proevested, overlap_level) %>%
  summarise(last_sample = max(week),
            .groups = "drop") %>%
  ungroup() %>%
  arrange(last_sample) %>%
  # Filter down to just the ones who haven't sent a sample in the latest week
  filter(last_sample < ISOweek(floor_date(today(), "week", week_start = 1) -
                                 days(7))) %>%
  # Join in names in master sheet for ease
  left_join(
    lookup_oplande_names %>%
      select(anlaeg_display, anlaeg_master, company_master),
    by = c("anlaeg_rando" = "anlaeg_display")
  )

# Anlæg who have samples, but <2 in the latest week
insufficient_recent_samples <- ww_human %>%
  select(anlaeg_rando,
         proevested,
         date_receipt,
         sample,
         overlap_level,
         week) %>%
  filter(!is.na(sample)) %>%
  filter(week == max(week)) %>%
  # For each anlæg, find out when they sent their last sample
  group_by(anlaeg_rando, proevested, overlap_level, week) %>%
  summarise(n = n(),
            .groups = "drop_last") %>%
  ungroup() %>%
  filter(n < 2) %>%
  arrange(anlaeg_rando) %>%
  # Join in names in master sheet for ease
  left_join(
    lookup_oplande_names %>%
      select(anlaeg_display, anlaeg_master, company_master),
    by = c("anlaeg_rando" = "anlaeg_display")
  )


cli_alert_info(
  "Saving information on missing data in {.file outputs/diagnostic}"
)
list(
  no_recent_samples           = no_recent_samples,
  insufficient_recent_samples = insufficient_recent_samples,
  missing_shapes              = missing_shapes
) %>%
  save_xlsx("missing-data", folder = here("outputs", "diagnostic"))








# Duplicate dates ---------------------------------------------------------


dupe_dates <- ww %>%
  add_count(anlaeg_rando, date_receipt) %>%
  filter(n > 1) %>%
  select(sample, version, anlaeg_rando,
         starts_with("date_"), rna_mean, file) %>%
  arrange(anlaeg_rando, date_receipt, date_sample, date_report)

if (nrow(dupe_dates) >= 1) {
  n_distinct(dupe_dates$anlaeg_rando)

  cli::cli_alert_warning(glue(
    "You have duplicate dates. ",
    "{n_anlaeg} anlæg are affected, ",
    "{n_dupes} duplicates overall",
    n_anlaeg = n_distinct(dupe_dates$anlaeg_rando),
    n_dupes = nrow(dupe_dates)
  ))

  fp_dupes_excel <- glue("outputs/diagnostic/{today()}_dupe-dates.xlsx")

  cli::cli_alert("Saving the duplicates here: {.file {getwd()}/{fp_dupes_excel}}")

  write_xlsx(dupe_dates, fp_dupes_excel)
}







# First receipt -----------------------------------------------------------

# Every place we got Eurofins data from, and the first date when we got data.

all_ww %>%
  group_by(anlaeg_rando) %>%
  filter(date_receipt == min(date_receipt)) %>%
  slice_head(n = 1) %>%
  ungroup() %>%
  select(anlaeg_rando,
         proevested,
         date_receipt) %>%
  left_join(lookup_oplande_names %>%
              select(anlaeg_display,
                     overlap_level,
                     in_use),
            by = c("anlaeg_rando" = "anlaeg_display")) %>%
  save_xlsx("first-receipt", here("outputs", "diagnostic"))







