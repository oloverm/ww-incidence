# Table of info per anlaeg, for appendix


table_anlaeg <- ww_human %>%
  filter_analysis() %>%
  select(region, anlaeg_rando, population, rna_mean, flow, temperature) %>%
  group_by(region, anlaeg_rando, population) %>%
  mutate(across(c(rna_mean, flow), log10)) %>%
  summarise(
    across(where(is.numeric), list(median = median,
                                   iqr = IQR),
           na.rm = TRUE),
    n_samples = n(),
    .groups = "drop"
  ) %>%
  relocate(n_samples, .after = population) %>%
  # mutate(across(where(is.numeric), round, 1)) %>%
  group_by(region) %>%
  # slice_head(n = 3) %>%
  ungroup() %>%
  # sample_n(30) %>%
  arrange(region, anlaeg_rando) %>%
  mutate(anlaeg_rando = anlaeg_rando %>% str_remove(" \\(R\\)")) %>%
  group_by(region) %>%
  gt::gt(rowname_col = "anlaeg_rando") %>%
  gt::fmt_number(
    columns = -c(population, n_samples),
    decimals = 2,
    use_seps = TRUE
  ) %>%
  gt::fmt_number(
    columns = c(population, n_samples),
    decimals = 0,
    use_seps = TRUE
  ) %>%
  gt::tab_spanner(label = "RNA copies per litre",
              columns = c(rna_mean_median, rna_mean_iqr)) %>%
  gt::tab_spanner(label = "24-hour flow (litres)",
              columns = c(flow_median, flow_iqr)) %>%
  gt::tab_spanner(label = "Temperature",
              columns = c(temperature_median, temperature_iqr)) %>%
  gt::cols_label(
    # anlaeg_rando = "Catchment area",
    population = "Population",
    n_samples = "Samples included",
    rna_mean_median = "Median",
    rna_mean_iqr = "IQR",
    flow_median = "Median",
    flow_iqr = "IQR",
    temperature_median = "Median",
    temperature_iqr = "IQR"
  ) %>%
  gt::tab_footnote(
    footnote = "Calculated on log scale.",
    locations = gt::cells_column_spanners(c("RNA copies per litre",
                                            "24-hour flow (litres)"))
  ) %>%
  gt::tab_footnote(
    footnote = "Contains missing values.",
    locations = gt::cells_column_spanners("Temperature")
  )

table_anlaeg


gt::gtsave(table_anlaeg, "paper/figures/anlaeg_table.html")

