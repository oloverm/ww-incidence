
# Detect signals




# Weekly differences ------------------------------------------------------

#' If there is only 1 <LOD data point in the previous two weeks, and <2 <LOQ
#' points, we've deleted the <LOD point. This is because it seems quite unlikely
#' to reflect a true lack of shedders in the opland. And <LOD pulls the mean
#' down quite a lot, so this often gives signals that we wouldn't trust.

#+ data_prep

cli_alert("Preparing signals")


# Weekly differences (NEW VERSION) ------------------------------------------------------

# Method until now
weeks_step1 <- ww_human %>%
  select(anlaeg_rando,
         region,
         date_receipt,
         week,
         rna_corr,
         rna_corr_log,
         påvist_mean,
         positive,
         population) %>%
  # Have to go 3 weeks back to have data enough to generate signals for week 16
  filter(date_receipt >= ymd("2021-10-01"),
         date_receipt <= ymd("2022-04-03")) %>%
  mutate(
    week_next = ISOweek(date_receipt + days(7)),
    week_comparison = ISOweek(date_receipt - days(7))
  ) %>%
  arrange(anlaeg_rando, date_receipt)

summary_weekly_for_signal <- function(df) {
  df %>%
    mutate(n_human = positive) %>%
    summarise(date_start = min(date_receipt),
              date_end = max(date_receipt),
              n_days = as.numeric(date_end - date_start) + 1,
              mean_rna_log = mean(rna_corr_log, na.rm = TRUE),
              n = sum(!is.na(rna_corr_log)),
              n_påvist = sum(påvist_mean == "påvist", na.rm = TRUE),
              n_ikkepåvist = sum(påvist_mean == "ikke påvist", na.rm = TRUE),
              n_human = sum(n_human, na.rm = TRUE),
              n_human_week = n_human / n_days * 7,
              .groups = "drop")
}

comparison_weeks <- weeks_step1 %>%
  select(-week_comparison) %>%
  pivot_longer(c(week, week_next),
               names_to = "week_type",
               values_to = "comparison_week2") %>%
  arrange(anlaeg_rando, comparison_week2) %>%
  # Remove "ikke påvist" samples when there's only 1 "ikke påvist" in the two
  # historical weeks (likely to be a mistake)
  group_by(anlaeg_rando, comparison_week2) %>%
  mutate(single_ikke_påvist =
           sum(påvist_mean == "ikke påvist", na.rm = TRUE) == 1 &
           sum(påvist_mean == "<LOQ", na.rm = TRUE) < 2) %>%
  ungroup() %>%
  filter(!(single_ikke_påvist & påvist_mean == "ikke påvist")) %>%
  select(-single_ikke_påvist) %>%
  # Get weekly numbers
  group_by(region, anlaeg_rando, population, comparison_week2) %>%
  summary_weekly_for_signal() %>%
  mutate(inc_human_week = n_human_week / population * 100000) %>%
  select(-population, -n_human, -date_start, -date_end)

weekly_log_diff <- weeks_step1 %>%
  # For each week, get mean RNA result, and number of samples it's based on
  group_by(region, anlaeg_rando, population, week, week_comparison) %>%
  summary_weekly_for_signal() %>%
  ungroup() %>%
  mutate(inc_human_week = n_human_week / population * 100000) %>%
  select(-c(date_start, date_end)) %>%
  arrange(anlaeg_rando, week) %>%
  # Get rid of anlæg where there's only 1 week of data
  add_count(anlaeg_rando, name = "n_weeks") %>%
  filter(n_weeks >= 2) %>%
  select(-n_weeks) %>%
  arrange(anlaeg_rando, week) %>%
  # Get rid of rows with NaN, meaning no observations in that week.
  filter(!is.nan(mean_rna_log)) %>%
  left_join(
    comparison_weeks,
    by = c("anlaeg_rando" = "anlaeg_rando",
           "region" = "region",
           "week_comparison" = "comparison_week2"),
    suffix = c("", "_prev")
  ) %>%
  mutate(rna_diff = mean_rna_log - mean_rna_log_prev)



# New method (faeces normalisation)

weeks_step2 <- ww_human %>%
  select(anlaeg_rando,
         region,
         date_receipt,
         week,
         rna_normalised_faeces,
         påvist_mean,
         positive,
         population) %>%
  filter(date_receipt >= ymd("2022-04-04")) %>% #RET HER NÅR DENNE UGE ER GÅET
  mutate(
    rna_norm_faeces_log = log10(rna_normalised_faeces),
    week_next = ISOweek(date_receipt + days(7)),
    week_comparison = ISOweek(date_receipt - days(7))
  ) %>%
  arrange(anlaeg_rando, date_receipt)


summary_weekly_for_signal_faeces <- function(df) {
  df %>%
    summarise(date_start = min(date_receipt),
              date_end = max(date_receipt),
              n_days = as.numeric(date_end - date_start) + 1,
              mean_rna_faeces = mean(rna_norm_faeces_log, na.rm = TRUE),
              n = sum(!is.na(rna_norm_faeces_log)),
              n_påvist = sum(påvist_mean == "påvist", na.rm = TRUE),
              n_ikkepåvist = sum(påvist_mean == "ikke påvist", na.rm = TRUE),
              n_human = sum(positive, na.rm = TRUE),
              n_human_week = n_human / n_days * 7,
              .groups = "drop")
}


comparison_weeks2 <- weeks_step2 %>%
  select(-week_comparison) %>%
  pivot_longer(c(week, week_next),
               names_to = "week_type",
               values_to = "comparison_week2") %>%
  arrange(anlaeg_rando, comparison_week2) %>%
  # Remove "ikke påvist" samples when there's only 1 "ikke påvist" in the two
  # historical weeks (likely to be a mistake)
  group_by(anlaeg_rando, comparison_week2) %>%
  mutate(single_ikke_påvist =
           sum(påvist_mean == "ikke påvist", na.rm = TRUE) == 1 &
           sum(påvist_mean == "<LOQ", na.rm = TRUE) < 2) %>%
  ungroup() %>%
  filter(!(single_ikke_påvist & påvist_mean == "ikke påvist")) %>%
  select(-single_ikke_påvist) %>%
  # Get weekly numbers
  group_by(region, anlaeg_rando, population, comparison_week2) %>%
  summary_weekly_for_signal_faeces() %>%
  mutate(inc_human_week = n_human_week / population * 100000) %>%
  select(-population, -n_human, -date_start, -date_end)


weekly_log_diff2 <- weeks_step2 %>%
  # For each week, get mean RNA result, and number of samples it's based on
  group_by(region, anlaeg_rando, population, week, week_comparison) %>%
  summary_weekly_for_signal_faeces() %>%
  ungroup() %>%
  mutate(inc_human_week = n_human_week / population * 100000) %>%
  select(-c(date_start, date_end)) %>%
  arrange(anlaeg_rando, week) %>%
  # Get rid of anlæg where there's only 1 week of data
  add_count(anlaeg_rando, name = "n_weeks") %>%
  filter(n_weeks >= 2) %>%
  select(-n_weeks) %>%
  arrange(anlaeg_rando, week) %>%
  # Get rid of rows with NaN, meaning no observations in that week.
  filter(!is.nan(mean_rna_faeces)) %>%
  left_join(
    comparison_weeks2,
    by = c("anlaeg_rando" = "anlaeg_rando",
           "region" = "region",
           "week_comparison" = "comparison_week2"),
    suffix = c("", "_prev")
  ) %>%
  mutate(rna_diff = mean_rna_faeces - mean_rna_faeces_prev)






##### JOIN DE TO #####

weekly_log_diff_full <- weekly_log_diff %>%
  full_join(weekly_log_diff2) %>%
  arrange(anlaeg_rando)



grading <- weekly_log_diff_full %>%
  mutate(
    cat_prev = case_when(
      n_ikkepåvist_prev == n_prev ~ "all <LOD",
      n_påvist_prev == 0 ~ "all <LOQ",
      n_påvist_prev < n_prev / 2 ~ "most <LOQ",
      n_påvist_prev == n_prev ~ "all påvist",
      n_påvist_prev >= n_prev / 2 ~ "most påvist",
      TRUE ~ "error (first week?)"
    ) %>%
      fct_relevel("all <LOQ", "most <LOQ", "most påvist", "all påvist"),
    cat_curr = case_when(
      n_påvist == n ~ "all påvist",
      n_påvist > 0 ~ "some påvist"
    ) %>%
      fct_relevel("some påvist", "all påvist"),
    missing_data = n_prev < 6 | n < 3,
    rna_diff_cat = cut(rna_diff, c(-Inf, 0, 0.6, 1.2, Inf),
                       include.lowest = TRUE, right = FALSE,
                       ordered_result = TRUE),
    grading = case_when(
      # No signal if the mean is lower than before, if there are no påvist
      # samples this week, or if there are <2 samples per week
      rna_diff <= 0 ~ 0,
      n_påvist == 0 ~ 0,
      n_prev < 4 ~ 0,
      
      # All nothing, now something
      cat_prev == "all <LOD" & cat_curr %in% c("some påvist", "all påvist") ~
        1,
      
      n < 2 ~ 0,
      
      # All <LOQ, now 2+ påvist
      cat_prev == "all <LOQ" & n_påvist >= 2 ~
        1,
      
      # In all other cases, you have to be at least 0.6 log higher than before
      rna_diff_cat < "[0.6,1.2)" ~ 0,
      
      cat_prev %in% c("most <LOQ", "all <LOQ") & cat_curr == "all påvist" &
        missing_data == FALSE & rna_diff_cat == "[1.2, Inf]" ~
        4,
      cat_prev %in% c("most <LOQ", "all <LOQ") & cat_curr == "all påvist" &
        missing_data == TRUE & rna_diff_cat == "[1.2, Inf]" ~
        3,
      cat_prev %in% c("most <LOQ", "all <LOQ") & cat_curr == "all påvist" &
        missing_data == TRUE & rna_diff_cat == "[0.6,1.2)" ~
        2,
      cat_prev %in% c("most <LOQ", "all <LOQ") & cat_curr == "all påvist" &
        missing_data == FALSE & rna_diff_cat == "[0.6,1.2)" ~
        3,
      cat_prev %in% c("most <LOQ", "all <LOQ") & cat_curr == "some påvist" &
        rna_diff_cat == "[1.2, Inf]" ~
        2,
      
      cat_prev == "most påvist" & cat_curr == "all påvist" &
        missing_data == FALSE & rna_diff_cat == "[1.2, Inf]" ~
        5,
      cat_prev == "most påvist" & cat_curr == "all påvist" &
        missing_data == TRUE & rna_diff_cat == "[1.2, Inf]" ~
        4,
      cat_prev == "most påvist" & cat_curr == "all påvist" &
        missing_data == TRUE & rna_diff_cat == "[0.6,1.2)" ~
        3,
      cat_prev == "most påvist" & cat_curr == "all påvist" &
        missing_data == FALSE & rna_diff_cat == "[0.6,1.2)" ~
        3,
      cat_prev == "most påvist" & cat_curr == "some påvist" ~
        2,
      
      cat_prev == "all påvist" & cat_curr == "all påvist" &
        missing_data == FALSE & rna_diff_cat == "[1.2, Inf]" ~
        5,
      cat_prev == "all påvist" & cat_curr == "all påvist" &
        missing_data == TRUE & rna_diff_cat == "[1.2, Inf]" ~
        5,
      cat_prev == "all påvist" & cat_curr == "all påvist" &
        missing_data == FALSE & rna_diff_cat == "[0.6,1.2)" ~
        4,
      cat_prev == "all påvist" & cat_curr == "all påvist" &
        missing_data == TRUE & rna_diff_cat == "[0.6,1.2)" ~
        3,
      cat_prev == "all påvist" & cat_curr == "some påvist" ~
        3,
    )
  ) %>%
  arrange(desc(week), anlaeg_rando)



# Underlying data 
signal_data <- ww_human %>%
  mutate(rna_norm_faeces_log = log10(rna_normalised_faeces)) %>%
  select(anlaeg_rando,
         date_receipt,
         rna_norm_faeces_log,
         rna_corr_log,
         påvist_mean,
         inc_7) %>%
  left_join(
    grading %>%
      filter(week == ISOweek(today() - days(7)),
             grading >= 1) %>%
      select(anlaeg_rando,
             grading),
    by = c("anlaeg_rando")
  ) %>%
  mutate(
    anlaeg_grading = glue("{grading}: {anlaeg_rando}"),
    signal_period = date_receipt >=
      ((floor_date(today(), "week", week_start = 1)) - weeks(3))
  ) %>%
  filter(
    grading >= 1,
    date_receipt >= ((today() %>%
                        floor_date("week", week_start = 1)) - weeks(6))
  )


# Write data --------------------------------------------------------------

cli_alert("Writing signal data in {.file data/signals/}")

write_xlsx(grading, glue("data/signals/{today()}_signals.xlsx"))

# Weekly differences (OLD VERSION) ------------------------------------------------------

# weeks_step1 <- ww_human %>%
#   select(anlaeg_rando,
#          region,
#          date_receipt,
#          week,
#          rna_corr,
#          rna_corr_log,
#          påvist_mean,
#          n_human,
#          population) %>%
#   filter(date_receipt >= dmy("1 jul 2021")) %>%
#   mutate(
#     week_next = ISOweek(date_receipt + days(7)),
#     week_comparison = ISOweek(date_receipt - days(7))
#   ) %>%
#   arrange(anlaeg_rando, date_receipt)
# 
# 
# 
# 
# summary_weekly_for_signal <- function(df) {
#   df %>%
#     summarise(date_start = min(date_receipt),
#               date_end = max(date_receipt),
#               n_days = as.numeric(date_end - date_start) + 1,
#               mean_rna_log = mean(rna_corr_log, na.rm = TRUE),
#               n = sum(!is.na(rna_corr_log)),
#               n_påvist = sum(påvist_mean == "påvist", na.rm = TRUE),
#               n_ikkepåvist = sum(påvist_mean == "ikke påvist", na.rm = TRUE),
#               n_human = sum(n_human, na.rm = TRUE),
#               n_human_week = n_human / n_days * 7,
#               .groups = "drop")
# }
# 
# 
# comparison_weeks <- weeks_step1 %>%
#   select(-week_comparison) %>%
#   pivot_longer(c(week, week_next),
#                names_to = "week_type",
#                values_to = "comparison_week2") %>%
#   arrange(anlaeg_rando, comparison_week2) %>%
#   # Remove "ikke påvist" samples when there's only 1 "ikke påvist" in the two
#   # historical weeks (likely to be a mistake)
#   group_by(anlaeg_rando, comparison_week2) %>%
#   mutate(single_ikke_påvist =
#            sum(påvist_mean == "ikke påvist", na.rm = TRUE) == 1 &
#            sum(påvist_mean == "<LOQ", na.rm = TRUE) < 2) %>%
#   ungroup() %>%
#   filter(!(single_ikke_påvist & påvist_mean == "ikke påvist")) %>%
#   select(-single_ikke_påvist) %>%
#   # Get weekly numbers
#   group_by(region, anlaeg_rando, population, comparison_week2) %>%
#   summary_weekly_for_signal() %>%
#   mutate(inc_human_week = n_human_week / population * 100000) %>%
#   select(-population, -n_human, -date_start, -date_end)
# 
# 
# 
# 
# 
# weekly_log_diff <- weeks_step1 %>%
#   # For each week, get mean RNA result, and number of samples it's based on
#   group_by(region, anlaeg_rando, population, week, week_comparison) %>%
#   summary_weekly_for_signal() %>%
#   ungroup() %>%
#   mutate(inc_human_week = n_human_week / population * 100000) %>%
#   select(-c(date_start, date_end)) %>%
#   arrange(anlaeg_rando, week) %>%
#   # Get rid of anlæg where there's only 1 week of data
#   add_count(anlaeg_rando, name = "n_weeks") %>%
#   filter(n_weeks >= 2) %>%
#   select(-n_weeks) %>%
#   arrange(anlaeg_rando, week) %>%
#   # Get rid of rows with NaN, meaning no observations in that week.
#   filter(!is.nan(mean_rna_log)) %>%
#   left_join(
#     comparison_weeks,
#     by = c("anlaeg_rando" = "anlaeg_rando",
#            "region" = "region",
#            "week_comparison" = "comparison_week2"),
#     suffix = c("", "_prev")
#   ) %>%
#   mutate(rna_diff = mean_rna_log - mean_rna_log_prev)
# 
# 
# 
# 
# # Grading signals ---------------------------------------------------------
# 
# cli_alert("Grading signals")
# 
# 
# grading <- weekly_log_diff %>%
#   mutate(
#     cat_prev = case_when(
#       n_ikkepåvist_prev == n_prev ~ "all <LOD",
#       n_påvist_prev == 0 ~ "all <LOQ",
#       n_påvist_prev < n_prev / 2 ~ "most <LOQ",
#       n_påvist_prev == n_prev ~ "all påvist",
#       n_påvist_prev >= n_prev / 2 ~ "most påvist",
#       TRUE ~ "error (first week?)"
#     ) %>%
#       fct_relevel("all <LOQ", "most <LOQ", "most påvist", "all påvist"),
#     cat_curr = case_when(
#       n_påvist == n ~ "all påvist",
#       n_påvist > 0 ~ "some påvist"
#     ) %>%
#       fct_relevel("some påvist", "all påvist"),
#     missing_data = n_prev < 6 | n < 3,
#     rna_diff_cat = cut(rna_diff, c(-Inf, 0, 0.6, 1.2, Inf),
#                        include.lowest = TRUE, right = FALSE,
#                        ordered_result = TRUE),
#     grading = case_when(
#       # No signal if the mean is lower than before, if there are no påvist
#       # samples this week, or if there are <2 samples per week
#       rna_diff <= 0 ~ 0,
#       n_påvist == 0 ~ 0,
#       n_prev < 4 ~ 0,
# 
#       # All nothing, now something
#       cat_prev == "all <LOD" & cat_curr %in% c("some påvist", "all påvist") ~
#         1,
# 
#       n < 2 ~ 0,
# 
#       # All <LOQ, now 2+ påvist
#       cat_prev == "all <LOQ" & n_påvist >= 2 ~
#         1,
# 
#       # In all other cases, you have to be at least 0.6 log higher than before
#       rna_diff_cat < "[0.6,1.2)" ~ 0,
# 
#       cat_prev %in% c("most <LOQ", "all <LOQ") & cat_curr == "all påvist" &
#         missing_data == FALSE & rna_diff_cat == "[1.2, Inf]" ~
#         4,
#       cat_prev %in% c("most <LOQ", "all <LOQ") & cat_curr == "all påvist" &
#         missing_data == TRUE & rna_diff_cat == "[1.2, Inf]" ~
#         3,
#       cat_prev %in% c("most <LOQ", "all <LOQ") & cat_curr == "all påvist" &
#         missing_data == TRUE & rna_diff_cat == "[0.6,1.2)" ~
#         2,
#       cat_prev %in% c("most <LOQ", "all <LOQ") & cat_curr == "all påvist" &
#         missing_data == FALSE & rna_diff_cat == "[0.6,1.2)" ~
#         3,
#       cat_prev %in% c("most <LOQ", "all <LOQ") & cat_curr == "some påvist" &
#         rna_diff_cat == "[1.2, Inf]" ~
#         2,
# 
#       cat_prev == "most påvist" & cat_curr == "all påvist" &
#         missing_data == FALSE & rna_diff_cat == "[1.2, Inf]" ~
#         5,
#       cat_prev == "most påvist" & cat_curr == "all påvist" &
#         missing_data == TRUE & rna_diff_cat == "[1.2, Inf]" ~
#         4,
#       cat_prev == "most påvist" & cat_curr == "all påvist" &
#         missing_data == TRUE & rna_diff_cat == "[0.6,1.2)" ~
#         3,
#       cat_prev == "most påvist" & cat_curr == "all påvist" &
#         missing_data == FALSE & rna_diff_cat == "[0.6,1.2)" ~
#         3,
#       cat_prev == "most påvist" & cat_curr == "some påvist" ~
#         2,
# 
#       cat_prev == "all påvist" & cat_curr == "all påvist" &
#         missing_data == FALSE & rna_diff_cat == "[1.2, Inf]" ~
#         5,
#       cat_prev == "all påvist" & cat_curr == "all påvist" &
#         missing_data == TRUE & rna_diff_cat == "[1.2, Inf]" ~
#         5,
#       cat_prev == "all påvist" & cat_curr == "all påvist" &
#         missing_data == FALSE & rna_diff_cat == "[0.6,1.2)" ~
#         4,
#       cat_prev == "all påvist" & cat_curr == "all påvist" &
#         missing_data == TRUE & rna_diff_cat == "[0.6,1.2)" ~
#         3,
#       cat_prev == "all påvist" & cat_curr == "some påvist" ~
#         3,
#     )
#   ) %>%
#   arrange(desc(week), anlaeg_rando)
# 
# 
# 
# 
# ## Underlying data ---------------------------------------------------------
# 
# # Individual measurements underlying this week's signals
# signal_data <- ww_human %>%
#   select(anlaeg_rando,
#          date_receipt,
#          rna_corr_log,
#          påvist_mean,
#          inc_7) %>%
#   left_join(
#     grading %>%
#       filter(week == ISOweek(today() - days(7)),
#              grading >= 1) %>%
#       select(anlaeg_rando,
#              grading),
#     by = c("anlaeg_rando")
#   ) %>%
#   mutate(
#     anlaeg_grading = glue("{grading}: {anlaeg_rando}"),
#     signal_period = date_receipt >=
#       ((floor_date(today(), "week", week_start = 1)) - weeks(3))
#   ) %>%
#   filter(
#     grading >= 1,
#     date_receipt >= ((today() %>%
#                         floor_date("week", week_start = 1)) - weeks(6))
#   )
# 
# 
# 
# 
# # Write data --------------------------------------------------------------
# 
# cli_alert("Writing signal data in {.file data/signals/}")
# 
# write_xlsx(grading, glue("data/signals/{today()}_signals.xlsx"))
