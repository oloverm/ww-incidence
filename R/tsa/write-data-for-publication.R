# Saving data safe for publication

dk %>%
  select(date_receipt,
         cpp_wtd_med,
         pop_ww,
         flow_wtd_med,
         temp_ww_wtd_med,
         n_samples,
         tests,
         population,
         incidence,
         new_tests_per_thousand,
         variant,
         p_delta,
         p_omicron) %>%
  write_csv("data/dk_restricted.csv")


raw_owid %>%
  write_csv("data/raw_owid.csv")
