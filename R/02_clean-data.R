
cli_alert("Cleaning data")




# OWID --------------------------------------------------------------------

cli_alert("Cleaning OWID data")

owid_hosp <- raw_owid %>%
  filter(location == "Denmark") %>%
  select(date,
         icu_patients,
         hosp_patients,
         weekly_hosp_admissions) %>%
  mutate(across(c(icu_patients,
                  hosp_patients),
                rollmean, 7, fill = NA, align = "right", na.rm = TRUE)) %>%
  filter(date >= ymd("2021-06-01"))

# OWID --------------------------------------------------------------------

cli_alert("Cleaning test and case data")

raw_test_historical <- raw_test_historical %>%
  # Ændring af dato format da det er forskelligt fra de daglige filer
  mutate(dato = suppressWarnings(as.Date(dato, "%Y-%m-%d")))

# Ændring af kolonne rækkefølge så det matcher historisk og let kan tilføjes
raw_test_daily <- raw_test_daily %>%
  relocate(dato, .before = "testede") %>%
  mutate(dato = as.Date(dato, "%Y-%m-%d"))


# Samle historisk og daglig

test_all <- raw_test_historical %>%
  bind_rows(raw_test_daily) %>%
  inner_join(lookup_oplande_names %>%
               select(anlaeg_shp, anlaeg_display),
             c("anlaeg" = "anlaeg_shp")) %>%
  select(-company) %>%
  left_join(pops %>%
              select(anlaeg, population),
            by = "anlaeg") %>%
  # Sørg for der kun er de ønskede anlæg inkluderet
  filter(case_when(
    dato >= ymd("2022-07-11") ~
      anlaeg_display %in% lookup_oplande_names_new$anlaeg_display,
    TRUE ~
      anlaeg_display %in% lookup_oplande_names_all_list$anlaeg_display)
  ) %>%
  # Beregning af 7-dages gennemsnit
  group_by(anlaeg) %>%
  mutate(tests_7 = rollmean(testede, 7, fill = NA, align = "center"),
         cases_7 = rollmean(positive, 7, fill = NA, align = "center"),
         inc_7 = cases_7 / population * 100000, # per 100.000
         inc_daily = positive / population * 100000, # per 100.000
         tests_pop = tests_7 / population * 1000, # per 1.000
         tests_pop_daily = testede / population * 1000) # per 1.000


# Human -------------------------------------------------------------------

# cli_alert("Joining human to shapes")
#
# human_oplande <- shp_oplande %>%
#   as_tibble() %>%
#   st_as_sf() %>%
#   inner_join(lookup_oplande_names %>%
#                select(anlaeg_shp, anlaeg_display),
#              c("anlaeg" = "anlaeg_shp")) %>%
#   left_join(pops %>%
#               select(anlaeg, population),
#             by = "anlaeg") %>%
#   select(anlaeg = anlaeg_display, population) %>%
#   st_join(shp_human, st_intersects) %>%
#   as_tibble() %>%
#   select(anlaeg, population, date_test) %>%
#   # Filter out oplande where no one ever tested positive
#   filter(!is.na(date_test))
#
#
# cli_alert("N daily cases per opland")
#
# human_n_daily <- human_oplande %>%
#   count(anlaeg, population, date_test, name = "n_human") %>%
#   # Fill in 0s for days with no cases
#   complete(nesting(anlaeg, population),
#            date_test = seq(min(date_test),
#                            max(date_test),
#                            by = "1 day"),
#            fill = list(n_human = 0)) %>%
#   arrange(anlaeg, date_test) %>%
#   group_by(anlaeg) %>%
#   mutate(n7_human = rollmean(n_human, 7, fill = NA, align = "center"),
#          inc7 = n7_human / population * 100000,
#          inc_daily = n_human / population * 100000
#   ) %>%
#   ungroup()


# WW NEW VERSION - FAECES NORMALISATION (2022) --------------------------------


cli_alert("Cleaning WW data")

pilot_anlaeg <- c(
  "Aarhus (Egå)",
  "Aarhus (Marselisborg)",
  "Hvidovre (Avedøre)",
  "Avedøre (Ejby)",
  "Avedøre (Vallensbæk)",
  "København (Lynetten)",
  "Lynetten (søndre tilløb)",
  "Lynetten (nordre tilløb)",
  "Esbjerg Vest",
  "Esbjerg Øst",
  "Fredericia",
  "Hillerød",
  "Horsens",
  "Kolding",
  "Løkken (Lyngby)",
  "Maribo (Hunseby)",
  "Odense (Ejby Mølle)",
  "Skagen",
  "Sønderborg (Sønderborg)",
  "Thisted"
)

all_ww <- combined_ww %>%
  mutate_if(is.POSIXt, as.Date) %>%
  # Manual fix for Nykøbing
  # mutate(proevested = proevested %>%
  #          replace(. == "Nykøbing F. Nord",
  #                  "Nykøbing (Odsherred)")) %>%
  left_join(lookup_oplande_names %>%
              select(anlaeg_eurofins,
                     anlaeg_display),
            by = c("proevested" = "anlaeg_eurofins"),
            na_matches = "never") %>%
  mutate(
    version = str_extract(rapportnr, "[^-]+$"),
    sample = str_remove(rapportnr, fixed(paste0("-", version))),
    anlaeg_rando = coalesce(anlaeg_display, proevested) %>%
      str_remove(regex(" ?renseanlæg ?", ignore_case = TRUE)),
    # Different order for when it's in a list
    proevested,
    # TODO: incorporate anlaeg names from shapefile
    status,
    date_sample = proevetagnings_dato,
    date_receipt = as.Date(modagelses_dato),
    date_report = rapporteringsdato,
    comments_batch = batchkommentar,
    comments_proeve = proevekommentar,
    # This comment means the analysis failed. No comment means it didn't
    analysis_failed = coalesce(
      comments_proeve ==
        "Prøven kan ikke analyseres grundet interferens fra prøvens matrix.",
      FALSE
    ),
    loq_rdrp      = parse_number(loq_rdrp_gen_kopier_l, na = c("", "NM")),
    loq_n         = parse_number(loq_n_gen_kopier_l, na = c("", "NM")),
    loq_n1        = parse_number(raw_loq_n1, na = c("", "NM")),
    loq_n2        = parse_number(raw_loq_n2, na = c("", "NM")),
    loq_rdrp_2022 = parse_number(raw_loq_rdrp_2022, na = c("", "NM")),
    loq_pmmov     = parse_number(raw_loq_pmmov, na = c("", "NM")),
    loq_crass     = parse_number(raw_loq_crass, na = c("", "NM")),
    loq_mean = means(loq_rdrp,  # Do I need loq_mean?
                     loq_n,
                     loq_n1,
                     loq_n2,
                     # loq_rdrp_2022,
                     na.rm = TRUE),
    påvist_rdrp = clean_påvist(kopier_l = sarscov2_rdrp_gen_kopier_l,
                               analysis_failed = analysis_failed,
                               kvalitativ = sarscov2_kvalitativ),
    påvist_n = clean_påvist(kopier_l = sarscov2_n_gen_kopier_l,
                            analysis_failed = analysis_failed,
                            kvalitativ = sarscov2_kvalitativ),
    påvist_n1 = clean_påvist(kopier_l = n1_kopier_l,  # Can't use yet
                             analysis_failed = analysis_failed,
                             kvalitativ = sarscov2_kvalitativ),
    påvist_n2 = clean_påvist(kopier_l = n2_kopier_l,
                             analysis_failed = analysis_failed,
                             kvalitativ = sarscov2_kvalitativ),
    påvist_rdrp_2022 = clean_påvist(kopier_l = rdrp_kopier_l_2022,
                                    analysis_failed = analysis_failed,
                                    kvalitativ = sarscov2_kvalitativ),
    påvist_pmmov = clean_påvist(kopier_l = pmmov_kopier_l,
                                analysis_failed = analysis_failed,
                                kvalitativ = NA),
    påvist_crass = clean_påvist(kopier_l = crass_kopier_l,
                                analysis_failed = analysis_failed,
                                kvalitativ = NA),
    påvist_mean = combine_påvist(
      påvist_rdrp %>% na_predicate(~ date_receipt >= ymd("2022-01-01")),
      påvist_n %>% na_predicate(~ date_receipt >= ymd("2022-01-01")),
      # påvist_n1,  # N1 not sensitive enough (yet)
      påvist_n2,
      påvist_rdrp_2022
    ),
    # påvist_both = påvist_n == "påvist" & påvist_rdrp == "påvist",
    påvist_bin = fct_recode(påvist_mean, "påvist" = "<LOQ"),
    flow = clean_flow(vandmaengde_l, anlaeg_rando),
    # sample_method = proevetagnings_metode,
    # sample_volume_ml = as.numeric(volumen_proevemaengde_ml),
    temperature = parse_number(temperatur_c) %>%
      na_predicate(~ . < 1 | . > 30),
    .before = 1
  ) %>%
  arrange(anlaeg_rando, date_receipt, date_sample, sample) %>%
  # Dry flow: just the minimum ever measured flow
  group_by(anlaeg_rando) %>%
  mutate(dry_flow = suppressWarnings(min(flow, na.rm = TRUE)) %>%
           na_if(Inf),
  ) %>%
  ungroup() %>%
  mutate(
    dilution_raw = flow / dry_flow,
    dilution = pmax(1, dilution_raw, na.rm = TRUE),
    # Manually correct date_receipt (always knew there could be duplicates
    # if multiple received on same day)
    date_receipt = case_when(
      # Kolding samples that all came in on the 23rd
      sample == "AR-21-CA-21081886" ~ dmy("20 jul 2021"),
      sample == "AR-21-CA-21081887" ~ dmy("21 jul 2021"),
      sample == "AR-21-CA-21081888" ~ dmy("22 jul 2021"),
      sample == "AR-21-CA-21081889" ~ dmy("23 jul 2021"),
      # Sønderborg delayed
      sample == "AR-21-CA-21090974" ~ dmy("17 aug 2021"),
      TRUE ~ date_receipt
    ),
    week = ISOweek(date_receipt)
  )


ww <- all_ww %>%
  filter(status == "Afsluttet",
         date_receipt <= today(),
         # Remove the ones that say "Ingen prøve" in the comments
         !str_detect(comments_proeve, "ngen prøve") | is.na(comments_proeve),
         !str_detect(comments_batch,  "ngen prøve") | is.na(comments_batch),
         !proevested %in% c("Aså", "Hjallerup")) %>%
  # Only keep one version of each sample
  group_by(sample) %>%
  filter(version == max(version) | all(is.na(version))) %>%
  ungroup() %>%
  mutate(
    # Can only do the LODs after we've done exclusions, because we impute means
    # when it's missing. (Is it ever missing?)
    lod_rdrp      = clean_lod(lod_rdrp,   anlaeg_rando),
    lod_n         = clean_lod(lod_n,      anlaeg_rando),
    lod_n1        = clean_lod(raw_lod_n1,        anlaeg_rando),
    lod_n2        = clean_lod(raw_lod_n2,        anlaeg_rando),
    lod_rdrp_2022 = clean_lod(raw_lod_rdrp_2022, anlaeg_rando),
    lod_pmmov     = clean_lod(raw_lod_pmmov, anlaeg_rando),
    lod_crass     = clean_lod(raw_lod_crass, anlaeg_rando),
    lod_mean = means(lod_rdrp,  # Do I even use this?
                     lod_n,
                     # lod_n1,  # N1 not yet
                     lod_n2,
                     na.rm = TRUE),
    rna_rdrp = clean_rna(sarscov2_rdrp_gen_kopier_l,
                         påvist = påvist_rdrp,
                         loq = loq_rdrp,
                         lod = lod_rdrp),
    rna_n = clean_rna(sarscov2_n_gen_kopier_l,
                      påvist = påvist_n,
                      loq = loq_n,
                      lod = lod_n),
    rna_n1 = clean_rna(n1_kopier_l,
                       påvist = påvist_n1,
                       loq = loq_n1,
                       lod = lod_n1),
    rna_n2 = clean_rna(n2_kopier_l,
                       påvist = påvist_n2,
                       loq = loq_n2,
                       lod = lod_n2),
    rna_rdrp_2022 = clean_rna(rdrp_kopier_l_2022,
                              påvist = påvist_rdrp_2022,
                              loq = loq_rdrp_2022,
                              lod = lod_rdrp_2022),
    rna_pmmov = clean_rna(pmmov_kopier_l,
                          påvist = påvist_pmmov,
                          loq = loq_pmmov,
                          lod = lod_pmmov),
    rna_crass = clean_rna(crass_kopier_l,
                          påvist = påvist_crass,
                          loq = loq_crass,
                          lod = lod_crass),

    # The values behind the calculations are set to NA, thus rna_mean, faeces
    # etc. will not be calculated. Making a ratio between the measurements to
    # set a limit of inclusion/exclusion. We have one for covid measures and one
    # for faeces/covid. This one for filtering out wrong samples (include both)
    ratio_values_faeces = case_when(
      date_receipt >= ymd("2022-01-01") ~
        (rna_pmmov / rna_crass) * (rna_n2 / rna_rdrp_2022)
    ),
    rna_rdrp_2022 = rna_rdrp_2022 %>% na_predicate(~ ratio_values_faeces == 1),
    rna_n2 = rna_n2 %>% na_predicate(~ ratio_values_faeces == 1),
    rna_pmmov = rna_pmmov %>% na_predicate(~ ratio_values_faeces == 1),
    rna_crass = rna_crass %>% na_predicate(~ ratio_values_faeces == 1),

    # This is now the mean of the logs
    rna_mean = 10^means(
      # Only use new kit's results starting in 2022. This is so we can have a
      # clean break between the old and the new method
      log10(rna_rdrp) %>% na_predicate(~ date_receipt >= ymd("2022-01-01")),
      log10(rna_n) %>% na_predicate(~ date_receipt >= ymd("2022-01-01")),
      # log10(rna_n1),  # N1 not yet
      log10(rna_n2),
      log10(rna_rdrp_2022),
      na.rm = TRUE
    ),
    faeces_mean = means(
      rna_pmmov,
      rna_crass,
      na.rm = TRUE
    ),

    rna_corr = rna_mean * dilution,
    rna_corr_log = log10(rna_corr),
    rna_normalised_faeces = case_when(
      date_receipt >= ymd("2022-01-01") ~ rna_mean / faeces_mean * 10e6
    ),
    rna_faeces_log = log10(rna_normalised_faeces),
    ############### TEST AF LOG VÆRDIER FRA START ################
    log_rna_rdrp = log10(rna_rdrp),
    log_rna_rdrp_2022 = log10(rna_rdrp_2022),
    log_rna_n = log10(rna_n),
    log_rna_n2 = log10(rna_n2),
    log_rna_pmmov = log10(rna_pmmov),
    log_rna_crass = log10(rna_crass)
  ) %>%
  # Udregner log ratios inden for hvert kit (obs på ratio af log er subtraktion)
  # Derefter udregnes gennemsnittet af de to kits på almindelig vis
  mutate(ratio_rdrp = log_rna_rdrp_2022 - log_rna_crass,
         ratio_n2 = log_rna_n2 - log_rna_pmmov,
         log_rna_mean_faeces = means(
           # Only use new kit's results starting in 2022. This is so we can have
           # a clean break between the old and the new method
           ratio_rdrp %>% na_predicate(~ date_receipt >= ymd("2022-01-01")),
           ratio_n2 %>% na_predicate(~ date_receipt >= ymd("2022-01-01")),
           ratio_rdrp,
           ratio_n2, na.rm = TRUE)) %>% # Obs på at regne "tilbage" til normal værdier ved 10^x * 10e6(skalering -> svarer til 10^7)
  ############### SLUT TEST AF LOG VÆRDIER FRA START ################
  distinct(date_receipt, anlaeg_rando, sample, rna_mean, .keep_all = TRUE) %>%
  relocate(anlaeg_rando, date_receipt, rna_mean, sample, version,
           påvist_mean)


# Finding deviating values to be filtered out.
# If the log_mean of the faeces indicators deviate with more than 3 std from the
# average. Log værdier fra start implementeret er også
std_out <- ww %>%
  mutate(faeces_mean_log = means(log_rna_pmmov, log_rna_crass,
                                 na.rm = TRUE)) %>%
    #faeces_mean_log = log10(faeces_mean)) %>%
  group_by(anlaeg_rando) %>%
  mutate(s_3_log = 3 * sd(faeces_mean_log, na.rm = TRUE),
         avg = mean(faeces_mean_log, na.rm = TRUE)
  ) %>%
  filter(faeces_mean_log >= avg + s_3_log |
           faeces_mean_log <= avg - s_3_log) %>%
  ungroup() %>%
  select(-s_3_log, -avg, faeces_mean_log)


ww <- suppressMessages(
  ww %>% anti_join(std_out)
)




# Combine WW human (NEW VERSION 2022 - FAECES NORMALISATION) -------------------

cli_alert("Combining WW and human")

ww_human <- ww %>%
  complete(nesting(anlaeg_rando, proevested),
           date_receipt = seq.Date(dmy("1 jul 2021"),
                                   max(date_receipt),
                                   "1 day")) %>%
  full_join(
    test_all,
    by = c("anlaeg_rando" = "anlaeg_display",
           "date_receipt" = "dato")
  ) %>%
  filter(date_receipt <= today()) %>%
  arrange(anlaeg_rando, date_receipt) %>%
  group_by(anlaeg_rando) %>%
  # Get rid of anlæg where we've never had Eurofins data
  filter(sum(!is.na(rna_mean)) > 0) %>%
  mutate(
    rna_log_interp = na.approx(rna_corr_log, maxgap = 5, na.rm = FALSE),
    rna7_log = rollmean(rna_log_interp, 7, fill = NA, align = "right"),
  ) %>%
  ungroup() %>%
  left_join(lookup_oplande_names,
            by = c("anlaeg_rando" = "anlaeg_display")) %>%
  filter(in_use == TRUE | is.na(in_use)) %>%
  # To make sure we only include data from the new "model" from 11 july 2022 (89
  # places)
  filter(case_when(
    date_receipt >= ymd("2022-07-11") ~
      anlaeg_rando %in% lookup_oplande_names_new$anlaeg_display,
    TRUE ~
      anlaeg_rando %in% lookup_oplande_names_all_list$anlaeg_display
  ))


cli_alert("Saving {.file ww-human.xlsx}")

save_xlsx(ww_human, "ww-human", here("data/ww-human"))





# Cache cleaned data -----------------------------------------------------------

cli_alert("Caching cleaned data")

save(
  shp_oplande,
  lookup_oplande_names,
  lookup_oplande_names_all_list,
  lookup_oplande_names_new,
  combined_ww,
  all_ww,
  ww,
  ww_human,
  owid_hosp,
  pops,
  file = here(glue("data/cache/{dttm}.RData",
                   dttm = format(Sys.time(), "%Y-%m-%d %H%M")))
)




# WW cleaning from database ----------------------------------------------------
#
#
# cli_alert("Cleaning WW data from database")
# 
# pilot_anlaeg <- c(
#   "Aarhus (Egå)",
#   "Aarhus (Marselisborg)",
#   "Hvidovre (Avedøre)",
#   "Avedøre (Ejby)",
#   "Avedøre (Vallensbæk)",
#   "København (Lynetten)",
#   "Lynetten (søndre tilløb)",
#   "Lynetten (nordre tilløb)",
#   "Esbjerg Vest",
#   "Esbjerg Øst",
#   "Fredericia",
#   "Hillerød",
#   "Horsens",
#   "Kolding",
#   "Løkken (Lyngby)",
#   "Maribo (Hunseby)",
#   "Odense (Ejby Mølle)",
#   "Skagen",
#   "Sønderborg (Sønderborg)",
#   "Thisted"
# )
# 
# all_ww_db <- df_eurofins %>%
#   mutate_if(is.POSIXt, as.Date) %>%
#   # Manual fix for Nykøbing
#   # mutate(proevested = proevested %>%
#   #          replace(. == "Nykøbing F. Nord",
#   #                  "Nykøbing (Odsherred)")) %>%
#   left_join(lookup_oplande_names %>%
#               select(anlaeg_eurofins,
#                      anlaeg_display),
#             by = c("proevested" = "anlaeg_eurofins"),
#             na_matches = "never") %>%
#   mutate(
#     version = str_extract(rapportnr, "[^-]+$"),
#     sample = str_remove(rapportnr, fixed(paste0("-", version))),
#     anlaeg_rando = coalesce(anlaeg_display, proevested) %>%
#       str_remove(regex(" ?renseanlæg ?", ignore_case = TRUE)),
#     # Different order for when it's in a list
#     proevested,
#     # TODO: incorporate anlaeg names from shapefile
#     status,
#     date_sample = proevetagnings_dato,
#     date_receipt = as.Date(modagelses_dato),
#     date_report = rapporteringsdato,
#     comments_batch = batchkommentar,
#     comments_proeve = proevekommentar,
#     # This comment means the analysis failed. No comment means it didn't
#     analysis_failed = coalesce(
#       comments_proeve ==
#         "Prøven kan ikke analyseres grundet interferens fra prøvens matrix.",
#       FALSE
#     ),
#     loq_rdrp      = parse_number(loq_rdrp_gen_kopier_l, na = c("", "NM")),
#     loq_n         = parse_number(loq_n_gen_kopier_l, na = c("", "NM")),
#     loq_n1        = parse_number(raw_loq_n1, na = c("", "NM")),
#     loq_n2        = parse_number(raw_loq_n2, na = c("", "NM")),
#     loq_rdrp_2022 = parse_number(raw_loq_rdrp_2022, na = c("", "NM")),
#     loq_pmmov     = parse_number(raw_loq_pmmov, na = c("", "NM")),
#     loq_crass     = parse_number(raw_loq_crass, na = c("", "NM")),
#     loq_mean = means(loq_rdrp,  # Do I need loq_mean?
#                      loq_n,
#                      loq_n1,
#                      loq_n2,
#                      # loq_rdrp_2022,
#                      na.rm = TRUE),
#     påvist_rdrp = clean_påvist(kopier_l = sarscov2_rdrp_gen_kopier_l,
#                                analysis_failed = analysis_failed,
#                                kvalitativ = sarscov2_kvalitativ),
#     påvist_n = clean_påvist(kopier_l = sarscov2_n_gen_kopier_l,
#                             analysis_failed = analysis_failed,
#                             kvalitativ = sarscov2_kvalitativ),
#     påvist_n1 = clean_påvist(kopier_l = n1_kopier_l,  # Can't use yet
#                              analysis_failed = analysis_failed,
#                              kvalitativ = sarscov2_kvalitativ),
#     påvist_n2 = clean_påvist(kopier_l = n2_kopier_l,
#                              analysis_failed = analysis_failed,
#                              kvalitativ = sarscov2_kvalitativ),
#     påvist_rdrp_2022 = clean_påvist(kopier_l = rdrp_kopier_l_2022,
#                                     analysis_failed = analysis_failed,
#                                     kvalitativ = sarscov2_kvalitativ),
#     påvist_pmmov = clean_påvist(kopier_l = pmmov_kopier_l,
#                                 analysis_failed = analysis_failed,
#                                 kvalitativ = NA),
#     påvist_crass = clean_påvist(kopier_l = crass_kopier_l,
#                                 analysis_failed = analysis_failed,
#                                 kvalitativ = NA),
#     påvist_mean = combine_påvist(
#       påvist_rdrp %>% na_predicate(~ date_receipt >= ymd("2022-01-01")),
#       påvist_n %>% na_predicate(~ date_receipt >= ymd("2022-01-01")),
#       # påvist_n1,  # N1 not sensitive enough (yet)
#       påvist_n2,
#       påvist_rdrp_2022
#     ),
#     # påvist_both = påvist_n == "påvist" & påvist_rdrp == "påvist",
#     påvist_bin = fct_recode(påvist_mean, "påvist" = "<LOQ"),
#     flow = clean_flow(vandmaengde_l, anlaeg_rando),
#     # sample_method = proevetagnings_metode,
#     # sample_volume_ml = as.numeric(volumen_proevemaengde_ml),
#     temperature = parse_number(temperatur_c) %>%
#       na_predicate(~ . < 1 | . > 30),
#     .before = 1
#   ) %>%
#   arrange(anlaeg_rando, date_receipt, date_sample, sample) %>%
#   # Dry flow: just the minimum ever measured flow
#   group_by(anlaeg_rando) %>%
#   mutate(dry_flow = suppressWarnings(min(flow, na.rm = TRUE)) %>%
#            na_if(Inf),
#   ) %>%
#   ungroup() %>%
#   mutate(
#     dilution_raw = flow / dry_flow,
#     dilution = pmax(1, dilution_raw, na.rm = TRUE),
#     # Manually correct date_receipt (always knew there could be duplicates
#     # if multiple received on same day)
#     date_receipt = case_when(
#       # Kolding samples that all came in on the 23rd
#       sample == "AR-21-CA-21081886" ~ dmy("20 jul 2021"),
#       sample == "AR-21-CA-21081887" ~ dmy("21 jul 2021"),
#       sample == "AR-21-CA-21081888" ~ dmy("22 jul 2021"),
#       sample == "AR-21-CA-21081889" ~ dmy("23 jul 2021"),
#       # Sønderborg delayed
#       sample == "AR-21-CA-21090974" ~ dmy("17 aug 2021"),
#       TRUE ~ date_receipt
#     ),
#     week = ISOweek(date_receipt)
#   )
# 
# ww_db <- all_ww_db %>%
#   filter(status == "Afsluttet",
#          date_receipt <= today(),
#          # Remove the ones that say "Ingen prøve" in the comments
#          !str_detect(comments_proeve, "ngen prøve") | is.na(comments_proeve),
#          !str_detect(comments_batch,  "ngen prøve") | is.na(comments_batch),
#          !proevested %in% c("Aså", "Hjallerup"),
#          # Remove data before 1/7-2021
#          date_receipt >= ymd("2021-07-01")) %>%
#   # Only keep one version of each sample
#   group_by(sample) %>%
#   filter(version == max(version) | all(is.na(version))) %>%
#   ungroup() %>%
#   mutate(
#     # Can only do the LODs after we've done exclusions, because we impute means
#     # when it's missing. (Is it ever missing?)
#     lod_rdrp      = clean_lod(lod_rdrp,   anlaeg_rando),
#     lod_n         = clean_lod(lod_n,      anlaeg_rando),
#     lod_n1        = clean_lod(raw_lod_n1,        anlaeg_rando),
#     lod_n2        = clean_lod(raw_lod_n2,        anlaeg_rando),
#     lod_rdrp_2022 = clean_lod(raw_lod_rdrp_2022, anlaeg_rando),
#     lod_pmmov     = clean_lod(raw_lod_pmmov, anlaeg_rando),
#     lod_crass     = clean_lod(raw_lod_crass, anlaeg_rando),
#     lod_mean = means(lod_rdrp,  # Do I even use this?
#                      lod_n,
#                      # lod_n1,  # N1 not yet
#                      lod_n2,
#                      na.rm = TRUE),
#     rna_rdrp = clean_rna(sarscov2_rdrp_gen_kopier_l,
#                          påvist = påvist_rdrp,
#                          loq = loq_rdrp,
#                          lod = lod_rdrp),
#     rna_n = clean_rna(sarscov2_n_gen_kopier_l,
#                       påvist = påvist_n,
#                       loq = loq_n,
#                       lod = lod_n),
#     rna_n1 = clean_rna(n1_kopier_l,
#                        påvist = påvist_n1,
#                        loq = loq_n1,
#                        lod = lod_n1),
#     rna_n2 = clean_rna(n2_kopier_l,
#                        påvist = påvist_n2,
#                        loq = loq_n2,
#                        lod = lod_n2),
#     rna_rdrp_2022 = clean_rna(rdrp_kopier_l_2022,
#                               påvist = påvist_rdrp_2022,
#                               loq = loq_rdrp_2022,
#                               lod = lod_rdrp_2022),
#     rna_pmmov = clean_rna(pmmov_kopier_l,
#                           påvist = påvist_pmmov,
#                           loq = loq_pmmov,
#                           lod = lod_pmmov),
#     rna_crass = clean_rna(crass_kopier_l,
#                           påvist = påvist_crass,
#                           loq = loq_crass,
#                           lod = lod_crass),
#     
#     # The values behind the calculations are set to NA, thus rna_mean, faeces
#     # etc. will not be calculated. Making a ratio between the measurements to
#     # set a limit of inclusion/exclusion. We have one for covid measures and one
#     # for faeces/covid. This one for filtering out wrong samples (include both)
#     ratio_values_faeces = case_when(
#       date_receipt >= ymd("2022-01-01") ~
#         (rna_pmmov / rna_crass) * (rna_n2 / rna_rdrp_2022)
#     ),
#     rna_rdrp_2022 = rna_rdrp_2022 %>% na_predicate(~ ratio_values_faeces == 1),
#     rna_n2 = rna_n2 %>% na_predicate(~ ratio_values_faeces == 1),
#     rna_pmmov = rna_pmmov %>% na_predicate(~ ratio_values_faeces == 1),
#     rna_crass = rna_crass %>% na_predicate(~ ratio_values_faeces == 1),
#     
#     # This is now the mean of the logs
#     rna_mean = 10^means(
#       # Only use new kit's results starting in 2022. This is so we can have a
#       # clean break between the old and the new method
#       log10(rna_rdrp) %>% na_predicate(~ date_receipt >= ymd("2022-01-01")),
#       log10(rna_n) %>% na_predicate(~ date_receipt >= ymd("2022-01-01")),
#       # log10(rna_n1),  # N1 not yet
#       log10(rna_n2),
#       log10(rna_rdrp_2022),
#       na.rm = TRUE
#     ),
#     faeces_mean = means(
#       rna_pmmov,
#       rna_crass,
#       na.rm = TRUE
#     ),
#     
#     rna_corr = rna_mean * dilution,
#     rna_corr_log = log10(rna_corr),
#     rna_normalised_faeces = case_when(
#       date_receipt >= ymd("2022-01-01") ~ rna_mean / faeces_mean * 10e6
#     ),
#     rna_faeces_log = log10(rna_normalised_faeces),
#     ############### TEST AF LOG VÆRDIER FRA START ################
#     log_rna_rdrp = log10(rna_rdrp),
#     log_rna_rdrp_2022 = log10(rna_rdrp_2022),
#     log_rna_n = log10(rna_n),
#     log_rna_n2 = log10(rna_n2),
#     log_rna_pmmov = log10(rna_pmmov),
#     log_rna_crass = log10(rna_crass)
#   ) %>%
#   # Udregner log ratios inden for hvert kit (obs på ratio af log er subtraktion)
#   # Derefter udregnes gennemsnittet af de to kits på almindelig vis
#   mutate(ratio_rdrp = log_rna_rdrp_2022 - log_rna_crass,
#          ratio_n2 = log_rna_n2 - log_rna_pmmov,
#          log_rna_mean_faeces = means(
#            # Only use new kit's results starting in 2022. This is so we can have
#            # a clean break between the old and the new method
#            ratio_rdrp %>% na_predicate(~ date_receipt >= ymd("2022-01-01")),
#            ratio_n2 %>% na_predicate(~ date_receipt >= ymd("2022-01-01")),
#            ratio_rdrp,
#            ratio_n2, na.rm = TRUE)) %>% # Obs på at regne "tilbage" til normal værdier ved 10^x * 10e6(skalering -> svarer til 10^7)
#   ############### SLUT TEST AF LOG VÆRDIER FRA START ################
# distinct(date_receipt, anlaeg_rando, sample, rna_mean, .keep_all = TRUE) %>%
#   relocate(anlaeg_rando, date_receipt, rna_mean, sample, version,
#            påvist_mean)
# 
# # Finding deviating values to be filtered out
# 
# std_out <- ww_db %>%
#   mutate(faeces_mean_log = log10(faeces_mean)) %>%
#   group_by(anlaeg_rando) %>%
#   mutate(s_3 = 3*sd(faeces_mean, na.rm = TRUE),
#          s_3_log = 3*sd(faeces_mean_log, na.rm = TRUE),
#          avg = mean(faeces_mean_log, na.rm = TRUE)
#   ) %>%
#   filter(faeces_mean_log >= avg + s_3_log |
#            faeces_mean_log <= avg - s_3_log) %>%
#   ungroup() %>%
#   select(-s_3, -s_3_log, -avg, faeces_mean_log)
# 
# suppressMessages(ww_db <- ww_db %>%
#                    anti_join(std_out))
# 
# 
# 
# # Combine WW human (NEW VERSION 2022 - FAECES NORMALISATION) --------------------------------------------------------
# 
# cli_alert("Combining WW database and human")
# 
# ww_human_db <- ww_db %>%
#   complete(nesting(anlaeg_rando, proevested),
#            date_receipt = seq.Date(dmy("1 jul 2021"),
#                                    max(date_receipt),
#                                    "1 day")) %>%
#   full_join(
#     test_all,
#     by = c("anlaeg_rando" = "anlaeg_display",
#            "date_receipt" = "dato")
#   ) %>%
#   filter(date_receipt <= today()) %>%
#   arrange(anlaeg_rando, date_receipt) %>%
#   group_by(anlaeg_rando) %>%
#   # Get rid of anlæg where we've never had Eurofins data
#   filter(sum(!is.na(rna_mean)) > 0) %>%
#   mutate(
#     rna_log_interp = na.approx(rna_corr_log, maxgap = 5, na.rm = FALSE),
#     rna7_log = rollmean(rna_log_interp, 7, fill = NA, align = "right"),
#   ) %>%
#   ungroup() %>%
#   left_join(lookup_oplande_names,
#             by = c("anlaeg_rando" = "anlaeg_display")) %>%
#   filter(in_use == TRUE | is.na(in_use)) %>%
#   # To make sure we only include data from the new "model" from 11 july 2022 (89
#   # places)
#   filter(case_when(
#     date_receipt >= ymd("2022-07-11") ~
#       anlaeg_rando %in% lookup_oplande_names_new$anlaeg_display,
#     TRUE ~
#       anlaeg_rando %in% lookup_oplande_names_all_list$anlaeg_display
#   ))
# 
# 
# cli_alert("Saving {.file ww-human_db.xlsx}")
# 
# save_xlsx(ww_human_db, "ww-human_db", here("data/ww-human"))


#
#
# # Cache cleaned data ------------------------------------------------------
#
# cli_alert("Caching cleaned data")
#
# save(
#   shp_oplande,
#   lookup_oplande_names,
#   df_eurofins,
#   all_ww_db,
#   ww_db,
#   ww_human_db,
#   owid_hosp,
#   pops,
#   file = here(glue("data/cache/{dttm}.RData",
#                    dttm = format(Sys.time(), "%Y-%m-%d %H%M"))))


# WW OLD VERSION (2021) --------------------------------------------------------
#
# cli_alert("Cleaning WW data")
#
# pilot_anlaeg <- c(
#   "Aarhus (Egå)",
#   "Aarhus (Marselisborg)",
#   "Hvidovre (Avedøre)",
#   "Avedøre (Ejby)",
#   "Avedøre (Vallensbæk)",
#   "København (Lynetten)",
#   "Lynetten (søndre tilløb)",
#   "Lynetten (nordre tilløb)",
#   "Esbjerg Vest",
#   "Esbjerg Øst",
#   "Fredericia",
#   "Hillerød",
#   "Horsens",
#   "Kolding",
#   "Løkken (Lyngby)",
#   "Maribo (Hunseby)",
#   "Odense (Ejby Mølle)",
#   "Skagen",
#   "Sønderborg (Sønderborg)",
#   "Thisted"
# )
#
# all_ww <- combined_ww %>%
#   mutate_if(is.POSIXt, as.Date) %>%
#   # Manual fix for Nykøbing
#   mutate(proevested = proevested %>%
#            replace(. == "Nykøbing F. Nord",
#                    "Nykøbing (Odsherred)")) %>%
#   left_join(lookup_oplande_names %>%
#               select(anlaeg_eurofins,
#                      anlaeg_display),
#             by = c("proevested" = "anlaeg_eurofins"),
#             na_matches = "never") %>%
#   mutate(
#     version = str_extract(rapportnr, "[^-]+$"),
#     sample = str_remove(rapportnr, fixed(paste0("-", version))),
#     anlaeg_rando = coalesce(anlaeg_display, proevested) %>%
#       str_remove(regex(" ?renseanlæg ?", ignore_case = TRUE)),
#     # Different order for when it's in a list
#     proevested,
#     # TODO: incorporate anlaeg names from shapefile
#     status,
#     date_sample = proevetagnings_dato,
#     date_receipt = as.Date(modagelses_dato),
#     date_report = rapporteringsdato,
#     comments_batch = batchkommentar,
#     comments_proeve = proevekommentar,
#     # This comment means the analysis failed. No comment means it didn't
#     analysis_failed = coalesce(
#       comments_proeve ==
#         "Prøven kan ikke analyseres grundet interferens fra prøvens matrix.",
#       FALSE
#     ),
#     loq_rdrp      = parse_number(loq_rdrp_gen_kopier_l, na = c("", "NM")),
#     loq_n         = parse_number(loq_n_gen_kopier_l, na = c("", "NM")),
#     loq_n1        = parse_number(raw_loq_n1, na = c("", "NM")),
#     loq_n2        = parse_number(raw_loq_n2, na = c("", "NM")),
#     loq_rdrp_2022 = parse_number(raw_loq_rdrp_2022, na = c("", "NM")),
#     loq_pmmov     = parse_number(raw_loq_pmmov, na = c("", "NM")),
#     loq_crass     = parse_number(raw_loq_crass, na = c("", "NM")),
#     loq_mean = means(loq_rdrp,  # Do I need loq_mean?
#                      loq_n,
#                      loq_n1,
#                      loq_n2,
#                      # loq_rdrp_2022,
#                      na.rm = TRUE),
#     påvist_rdrp = clean_påvist(kopier_l = sarscov2_rdrp_gen_kopier_l,
#                                analysis_failed = analysis_failed,
#                                kvalitativ = sarscov2_kvalitativ),
#     påvist_n = clean_påvist(kopier_l = sarscov2_n_gen_kopier_l,
#                             analysis_failed = analysis_failed,
#                             kvalitativ = sarscov2_kvalitativ),
#     påvist_n1 = clean_påvist(kopier_l = n1_kopier_l,  # Can't use yet
#                              analysis_failed = analysis_failed,
#                              kvalitativ = sarscov2_kvalitativ),
#     påvist_n2 = clean_påvist(kopier_l = n2_kopier_l,
#                              analysis_failed = analysis_failed,
#                              kvalitativ = sarscov2_kvalitativ),
#     påvist_rdrp_2022 = clean_påvist(kopier_l = rdrp_kopier_l_2022,
#                                     analysis_failed = analysis_failed,
#                                     kvalitativ = sarscov2_kvalitativ),
#     påvist_pmmov = clean_påvist(kopier_l = pmmov_kopier_l,
#                                 analysis_failed = analysis_failed,
#                                 kvalitativ = NA),
#     påvist_crass = clean_påvist(kopier_l = crass_kopier_l,
#                                 analysis_failed = analysis_failed,
#                                 kvalitativ = NA),
#     påvist_mean = combine_påvist(
#       påvist_rdrp %>% na_predicate(~ date_receipt >= ymd("2022-01-01")),
#       påvist_n %>% na_predicate(~ date_receipt >= ymd("2022-01-01")),
#       # påvist_n1,  # N1 not sensitive enough (yet)
#       påvist_n2,
#       påvist_rdrp_2022
#     ),
#     # påvist_both = påvist_n == "påvist" & påvist_rdrp == "påvist",
#     påvist_bin = fct_recode(påvist_mean, "påvist" = "<LOQ"),
#     flow = clean_flow(vandmaengde_l, anlaeg_rando),
#     # sample_method = proevetagnings_metode,
#     # sample_volume_ml = as.numeric(volumen_proevemaengde_ml),
#     .before = 1
#   ) %>%
#   left_join(
#     dry_flow,
#     by = c("anlaeg_rando" = "anlaeg_display")
#   ) %>%
#   arrange(anlaeg_rando, date_receipt, date_sample, sample) %>%
#   # For anlæg where we don't have the dry flow, just use the minimum ever
#   # measured flow for that anlæg
#   group_by(anlaeg_rando) %>%
#   mutate(dry_flow = dry_flow %>%
#            coalesce(suppressWarnings(min(flow, na.rm = TRUE))) %>%
#            na_if(Inf),
#   ) %>%
#   # Fix for missing flow - replaced by mean within anlæg from 19/4-2022
#   mutate(flow = case_when(date_receipt >= ymd("2022-04-19") ~ if_else(is.na(flow),
#                                                                       mean(flow, na.rm= TRUE),
#                                                                       flow),
#                               TRUE ~ as.numeric(flow))) %>%
#   ungroup() %>%
#   mutate(
#     dilution_raw = flow / dry_flow,
#     dilution = pmax(1, dilution_raw, na.rm = TRUE),
#     # Manually correct date_receipt (always knew there could be duplicates
#     # if multiple received on same day)
#     date_receipt = case_when(
#       # Kolding samples that all came in on the 23rd
#       sample == "AR-21-CA-21081886" ~ dmy("20 jul 2021"),
#       sample == "AR-21-CA-21081887" ~ dmy("21 jul 2021"),
#       sample == "AR-21-CA-21081888" ~ dmy("22 jul 2021"),
#       sample == "AR-21-CA-21081889" ~ dmy("23 jul 2021"),
#       # Sønderborg delayed
#       sample == "AR-21-CA-21090974" ~ dmy("17 aug 2021"),
#       TRUE ~ date_receipt
#     ),
#     week = ISOweek(date_receipt)
#   )
#
#
# ww <- all_ww %>%
#   filter(status == "Afsluttet",
#          date_receipt <= today(),
#          # Remove the ones that say "Ingen prøve" in the comments
#          !str_detect(comments_proeve, "ngen prøve") | is.na(comments_proeve),
#          !str_detect(comments_batch,  "ngen prøve") | is.na(comments_batch),
#          !proevested %in% c("Aså", "Hjallerup")) %>%
#   # Only keep one version of each sample
#   group_by(sample) %>%
#   filter(version == max(version) | all(is.na(version))) %>%
#   ungroup() %>%
#   mutate(
#     # Can only do the LODs after we've done exclusions, because we impute means
#     # when it's missing. (Is it ever missing?)
#     lod_rdrp      = clean_lod(lod_rdrp,   anlaeg_rando),
#     lod_n         = clean_lod(lod_n,      anlaeg_rando),
#     lod_n1        = clean_lod(raw_lod_n1,        anlaeg_rando),
#     lod_n2        = clean_lod(raw_lod_n2,        anlaeg_rando),
#     lod_rdrp_2022 = clean_lod(raw_lod_rdrp_2022, anlaeg_rando),
#     lod_pmmov     = clean_lod(raw_lod_pmmov, anlaeg_rando),
#     lod_crass     = clean_lod(raw_lod_crass, anlaeg_rando),
#     lod_mean = means(lod_rdrp,  # Do I even use this?
#                      lod_n,
#                      # lod_n1,  # N1 not yet
#                      lod_n2,
#                      na.rm = TRUE),
#     rna_rdrp = clean_rna(sarscov2_rdrp_gen_kopier_l,
#                       påvist = påvist_rdrp,
#                       loq = loq_rdrp,
#                       lod = lod_rdrp),
#     rna_n = clean_rna(sarscov2_n_gen_kopier_l,
#                       påvist = påvist_n,
#                       loq = loq_n,
#                       lod = lod_n),
#     rna_n1 = clean_rna(n1_kopier_l,
#                       påvist = påvist_n1,
#                       loq = loq_n1,
#                       lod = lod_n1),
#     rna_n2 = clean_rna(n2_kopier_l,
#                       påvist = påvist_n2,
#                       loq = loq_n2,
#                       lod = lod_n2),
#     rna_rdrp_2022 = clean_rna(rdrp_kopier_l_2022,
#                               påvist = påvist_rdrp_2022,
#                               loq = loq_rdrp_2022,
#                               lod = lod_rdrp_2022),
#     rna_pmmov = clean_rna(pmmov_kopier_l,
#                           påvist = påvist_pmmov,
#                           loq = loq_pmmov,
#                           lod = lod_pmmov),
#     rna_crass = clean_rna(crass_kopier_l,
#                           påvist = påvist_crass,
#                           loq = loq_crass,
#                           lod = lod_crass),
#     # Not sure if this should be the mean of the logs, or the log of the mean...
#     rna_mean = means(
#       # Only use new kit's results starting in 2022. This is so we can have a
#       # clean break between the old and the new method
#       rna_rdrp %>% na_predicate(~ date_receipt >= ymd("2022-01-01")),
#       rna_n %>% na_predicate(~ date_receipt >= ymd("2022-01-01")),
#       # log10(rna_n1),  # N1 not yet
#       rna_n2,
#       rna_rdrp_2022,
#       na.rm = TRUE
#     ),
#     faeces_mean = means(
#       rna_pmmov,
#       rna_crass,
#       na.rm = TRUE
#     ),
#     rna_corr = rna_mean * coalesce(dilution, 1),
#     rna_corr_log = log10(rna_corr),
#     rna_normalised_faeces = case_when(date_receipt >= ymd("2022-01-01") ~ rna_mean / faeces_mean * 100000000)
#   ) %>%
#   distinct(date_receipt, anlaeg_rando, sample, rna_mean, .keep_all = TRUE) %>%
#   relocate(anlaeg_rando, date_receipt, rna_mean, sample, version,
#            påvist_mean)
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
# # Combine WW human --------------------------------------------------------
#
# cli_alert("Combining WW and human")
#
# ww_human <- ww %>%
#   complete(nesting(anlaeg_rando, proevested),
#            date_receipt = seq.Date(dmy("1 jul 2021"),
#                                    max(date_receipt),
#                                    "1 day")) %>%
#   full_join(
#     human_n_daily,
#     by = c("anlaeg_rando" = "anlaeg",
#            "date_receipt" = "date_test")
#   ) %>%
#   filter(date_receipt <= today()) %>%
#   arrange(anlaeg_rando, date_receipt) %>%
#   group_by(anlaeg_rando) %>%
#   # Get rid of anlæg where we've never had Eurofins data
#   filter(sum(!is.na(rna_mean)) > 0) %>%
#   mutate(
#     rna_log_interp = na.approx(rna_corr_log, maxgap = 5, na.rm = FALSE),
#     rna7_log = rollmean(rna_log_interp, 7, fill = NA, align = "right"),
#   ) %>%
#   ungroup() %>%
#   left_join(lookup_oplande_names,
#             by = c("anlaeg_rando" = "anlaeg_display")) %>%
#   filter(in_use == TRUE | is.na(in_use))
#   # Manual remove of measure in week 12 in Damhusåen
#   # mutate(rna_mean = case_when(anlaeg_rando == "København (Damhusåen) (R)" &
#   #                               date_receipt == ymd("2022-03-22") ~ NA))
#
# # Manual removal of week 12-2022 in Damhusåen
# # removal_rows <- ww_human %>%
# #   subset(anlaeg_rando == "København (Damhusåen) (R)" &
# #            date_receipt >= ymd("2022-03-21") & date_receipt <= ymd("2022-03-26"))
# #
# # ww_human <- ww_human %>%
# #   anti_join(removal_rows)
#
#
#
# cli_alert("Saving {.file ww-human.xlsx}")
#
# save_xlsx(ww_human, "ww-human", here("data/ww-human"))
#
#
#
#
#
# # Cache cleaned data ------------------------------------------------------
#
# cli_alert("Caching cleaned data")
#
# save(
#   shp_oplande,
#   lookup_oplande_names,
#   combined_ww,
#   all_ww,
#   ww,
#   ww_human,
#   owid_hosp,
#   pops,
#   file = here(glue("data/cache/{dttm}.RData",
#                    dttm = format(Sys.time(), "%Y-%m-%d %H%M")))
# )





