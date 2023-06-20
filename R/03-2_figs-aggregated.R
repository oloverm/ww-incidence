

source(here("R/00-3_functions-aggregated-graphs.R"), encoding = 'UTF-8')

cli_alert("Saving aggregated figures (DK)")

# AGGREAGTED NEW VERSION (2022 - FAECES NORMALISATION) ----------------------------------------------------------------------

# Danmark -----------------------------------------------------------------

aggregated_ww_dk <- ww_human %>%
  filter(
    overlap_level == "1. anlæg"
  ) %>%
  # Manually remove data from friday 1/7-2022 due to Tour de France
  mutate(log_rna_mean_faeces = ifelse(
    (date_receipt >= ymd("2022-06-30") & date_receipt <= ymd("2022-07-01")),
    NA,
    log_rna_mean_faeces)
  ) %>%
  aggregate_ww() %>%
  bind_rows(owid_hosp %>%
              rename(date_receipt = date) %>%
              pivot_longer(-date_receipt,
                           names_to = "measure"
              )) %>%
  # Filter from the day where all anlæg where included
  filter(date_receipt >= ymd("2022-01-01"),
         !is.na(value)) %>%
 arrange(date_receipt, measure)


aggregated_ww_dk %>%
  plot_aggregated(smoothing = "weekly_avg",
                  plot_ci = TRUE) %>%
  save_plot("{today()}_danmark.png",
            folder = here("outputs", "aggregated"))


# Danmark Løbende gennemsnit -----------------------------------------------------------------

roll_mean_dk <- ww_human %>%
  filter(overlap_level == "1. anlæg") %>%
  # Manually remove data from friday 1/7-2022 due to Tour de France
  mutate(log_rna_mean_faeces = ifelse((date_receipt >= ymd("2022-06-30") &
                                           date_receipt <= ymd("2022-07-01")),
                                        NA, log_rna_mean_faeces)) %>%
  group_by(date_receipt) %>%
  # Adjust weekdays, so there is only samples from same three days
  # UPDATE: calibration - two samples, set to tuesday and thursday
  mutate(weekday = weekdays(date_receipt),
         new_date = case_when(date_receipt <= ymd("2022-07-10") & weekday == "mandag" ~ date_receipt-days(3),
                              date_receipt <= ymd("2022-07-10") & weekday == "torsdag" ~ date_receipt-days(1),
                              date_receipt <= ymd("2022-07-10") & weekday == "lørdag" ~ date_receipt-days(1),
                              date_receipt <= ymd("2022-07-10") & weekday == "søndag" ~ date_receipt-days(2),
                              date_receipt <= ymd("2022-07-10") & weekday == "fredag" & week == "2022-W21" ~ date_receipt-days(2),
                              date_receipt > ymd("2022-07-10") & weekday == "onsdag" ~ date_receipt+days(1),
                              date_receipt > ymd("2022-07-10") & weekday == "fredag" ~ date_receipt-days(1),
                              date_receipt > ymd("2022-07-10") & weekday == "lørdag" ~ date_receipt-days(2),
                              date_receipt > ymd("2022-07-10") & weekday == "søndag" ~ date_receipt-days(3),
                              TRUE ~ date_receipt)) %>%
  ungroup() %>%
  group_by(new_date)%>%
  # Weighted average
  summarise(
    n_samples = sum(!is.na(log_rna_mean_faeces)),
    pop_ww = sum(population),
    weighted_t_test = list(suppressWarnings(
      weights::wtd.t.test(log_rna_mean_faeces, NA, weight = log10(population))
    )),
    log_rna_mean_faeces = weighted_t_test[[1]]$additional["Mean"],
    se = weighted_t_test[[1]]$additional["Std. Err"],
    ci_lo = log_rna_mean_faeces - 1.96 * se,
    ci_hi = log_rna_mean_faeces + 1.96 * se,
    .groups = "drop") %>%
  ungroup() %>%
  select(-weighted_t_test) %>%
  filter(log_rna_mean_faeces != "NaN") %>%
  mutate(rna_mean_faeces = (10^log_rna_mean_faeces)*10e6) %>%
  # Convert back to "normal" scale values
  pivot_longer(cols = c("rna_mean_faeces"), names_to = "measure", values_to = "value") %>%
  # Remove days with few samples (e.g. easter)
  filter(n_samples >= 30)

# Selve plottet

ggplot(data = roll_mean_dk, mapping = aes(x = new_date, y = value))+
  geom_point(size = 1) +
  # Laver et moving average over de 3 seneste punkter
  geom_ma(ma_fun = SMA, n = 3, color = "#3498D8", size = 0.65, linetype = "solid")+
  scale_x_date(date_breaks = "weeks",
               date_labels = "%W",
               expand = expansion(c(0.01, 0.05))) +
  scale_y_continuous(trans = "log10")+
  guides(colour = guide_legend()) +
  labs(x = "Uge",
       y = "RNA-kopier ift. fæces indhold",
       title = "Resultater fra spildevandsmålinger",
       subtitle = "Løbende gennemsnit af 3 seneste målinger",
       colour = NULL,
       size = "Indbyggere i oplande") +
  theme(legend.position = "bottom",
        legend.box = "vertical") +
  theme_axes_leftright(left = "#3498D8")

save_plot(last_plot(),
          "{today()}_danmark_roll_mean.png",
          folder = here("outputs", "aggregated"))


# Danmark (ENGELSK) -----------------------------------------------------------------
# aggregated_ww_dk_eng <- ww_human %>%
#   filter(
#     overlap_level == "1. anlæg"
#   ) %>%
#   # Manually remove data from friday 1/7-2022 due to Tour de France
#   mutate(log_rna_mean_faeces = ifelse(
#     (date_receipt >= ymd("2022-06-30") & date_receipt <= ymd("2022-07-01")),
#     NA,
#     log_rna_mean_faeces)
#   ) %>%
#   aggregate_ww() %>%
#   bind_rows(owid_hosp %>%
#               rename(date_receipt = date) %>%
#               pivot_longer(-date_receipt,
#                            names_to = "measure"
#               )) %>%
#   # Filter from the day where all anlæg where included
#   filter(date_receipt >= ymd("2021-07-01"),
#          date_receipt <= ymd("2022-07-01"),
#          !is.na(value),
#          measure != "icu_patients", measure != "hosp_patients") %>%
#   arrange(date_receipt, measure)
# 
# aggregated_ww_dk_eng %>%
#   plot_aggregated_eng(smoothing = "weekly_avg",
#                   plot_ci = TRUE) %>%
#   save_plot("{today()}_danmark_eng.png",
#             folder = here("outputs", "aggregated"))


# Regioner ----------------------------------------------------------------

aggregated_ww_region <- ww_human %>%
  # Manually remove data from friday 1/7-2022 due to Tour de France
  mutate(log_rna_mean_faeces = ifelse(
    (date_receipt >= ymd("2022-06-30") &
       date_receipt <= ymd("2022-07-01") &
       region == "Region Hovedstaden"),
    NA,
    log_rna_mean_faeces
  )) %>%
  filter(overlap_level == "1. anlæg") %>%
  aggregate_ww(region) %>%
  filter(date_receipt >= ymd("2022-01-01"),
         !is.na(value),
         measure != "total_testede_pop") %>%
  arrange(date_receipt, measure)

figs_aggregated_ww_region <- aggregated_ww_region %>%
  nest_by(region) %>%
  mutate(plot = list(plot_aggregated(
    data,
    title = glue("SARS-CoV-2 incidens og resultater ",
                 "fra spildevandsmålinger, {region}",
                 region = str_remove(region, "Region ")),
    plot_ci = TRUE))) %>%
  ungroup()

figs_aggregated_ww_region %>%
  mutate(region_fp = make_clean_names(region)) %>%
  select(region_fp, plot) %>%
  pwalk(~ save_plot(plot = ..2,
                    filename = "{today()}_region-{region_fp}.png",
                    region_fp = ..1,
                    folder = here("outputs", "aggregated")))

## All regions in one (+ natural scale) --------------------------------------------------------------


aggregated_ww_region %>%
  filter(date_receipt >= ymd("2022-01-01"),
         measure == "rna_mean_faeces") %>%
  group_by(region, date_receipt = floor_date(date_receipt, "week", 1)+days(3)) %>%
  ggplot(aes(date_receipt, value,
             colour = region,
             fill = region,
             group = region)) +
  # geom_ribbon(aes(ymin = ci_lo,
  #                 ymax = ci_hi),
  #             alpha = 0.15,
  #             colour = "transparent") +
  geom_line(size = 1.3) +
  geom_point(data = ~ group_by(., region) %>%
               filter(date_receipt == max(date_receipt, na.rm = TRUE)) %>%
               ungroup() %>%
               arrange(measure),
             aes(fill = region),
             colour = "white",
             shape = 21,
             size = 2.5,
             stroke = 0.75,
             show.legend = FALSE) +
  scale_x_date(date_breaks = "1 month",
               date_labels = "%B",
               date_minor_breaks = "month") +
  ggthemes::scale_color_few() +
  ggthemes::scale_fill_few("Medium") +
  # theme(panel.grid.minor.x = element_line()) +
  scale_y_log10(labels = scales::comma_format(big.mark = ".",
                                              decimal.mark = ",")) +
  labs(
    title = "Resultater fra spildevandsmålinger",
    colour = NULL,
    fill = NULL,
    x = "Måned",
    y = "RNA-kopier ift. fæces indhold"
  )

save_plot(last_plot(),
          "{today()}_regioner.png",
          folder = here("outputs", "aggregated"))


save_plot(
  last_plot() +
    scale_y_continuous(labels = scales::comma_format(big.mark = ".",
                                                     decimal.mark = ",")),
  "{today()}_regioner-natural-scale.png",
  folder = here("outputs", "aggregated")
)

# All regions from april and forward
aggregated_ww_region %>%
  filter(date_receipt >= ymd("2022-03-31"),
         measure == "rna_mean_faeces") %>%
  group_by(region, date_receipt = floor_date(date_receipt, "week", 1)+days(3)) %>%
  ggplot(aes(date_receipt, value,
             colour = region,
             fill = region,
             group = region)) +
  # geom_ribbon(aes(ymin = ci_lo,
  #                 ymax = ci_hi),
  #             alpha = 0.15,
  #             colour = "transparent") +
  geom_line(size = 1.3) +
  geom_point(data = ~ group_by(., region) %>%
               filter(date_receipt == max(date_receipt, na.rm = TRUE)) %>%
               ungroup() %>%
               arrange(measure),
             aes(fill = region),
             colour = "white",
             shape = 21,
             size = 2.5,
             stroke = 0.75,
             show.legend = FALSE) +
  scale_x_date(date_breaks = "1 month",
               date_labels = "%B",
               date_minor_breaks = "month") +
  ggthemes::scale_color_few() +
  ggthemes::scale_fill_few("Medium") +
  # theme(panel.grid.minor.x = element_line()) +
  scale_y_log10(labels = scales::comma_format(big.mark = ".",
                                              decimal.mark = ",")) +
  labs(
    title = "Resultater fra spildevandsmålinger",
    colour = NULL,
    fill = NULL,
    x = "Måned",
    y = "RNA-kopier ift. fæces indhold"
  )

save_plot(last_plot(),
          "{today()}_regioner_april.png",
          folder = here("outputs", "aggregated"))

save_plot(last_plot() +
            scale_y_continuous(labels = comma_dk),
          "{today()}_regioner_april_normal.png",
          folder = here("outputs", "aggregated"))

## All regions facet wrap (8 weeks) --------------------------------------------------------------

aggregated_ww_region_8w <- ww_human %>%
  # Manually remove data from friday 1/7-2022 due to Tour de France
  mutate(log_rna_mean_faeces = ifelse(
    (date_receipt >= ymd("2022-06-30") &
       date_receipt <= ymd("2022-07-01") &
       region == "Region Hovedstaden"),
    NA,
    log_rna_mean_faeces
  )) %>%
  filter(overlap_level == "1. anlæg") %>%
  aggregate_ww(region) %>%
  mutate(week = ISOweek(date_receipt)) %>%
  filter(#date_receipt >= ymd("2022-04-18"),
    week >= ISOweek(today() - days(56)),
         !is.na(value),
         measure == "rna_mean_faeces")

aggregated_ww_region_8w %>%
  plot_aggregated_8w(smoothing = "weekly_avg",
                  plot_ci = TRUE,
                  title = "Resultater fra spildevandsmålinger - regioner") %>%
  save_plot("{today()}_regioner_8w.png",
            folder = here("outputs", "aggregated"))


## All regions facet wrap for article --------------------------------------------------------------

# aggregated_ww_region_article <- ww_human %>%
#   # Manually remove data from friday 1/7-2022 due to Tour de France
#   mutate(log_rna_mean_faeces = ifelse(
#     (date_receipt >= ymd("2022-06-30") &
#        date_receipt <= ymd("2022-07-01") &
#        region == "Region Hovedstaden"),
#     NA,
#     log_rna_mean_faeces
#   )) %>%
#   filter(overlap_level == "1. anlæg") %>%
#   aggregate_ww(region) %>%
#   mutate(week = ISOweek(date_receipt)) %>%
#   filter(date_receipt <= ymd("2022-07-01"),
#     !is.na(value),
#     measure == "rna_mean_faeces" | measure == "total_inc_7" | measure == "copies_per_person") %>%
#   mutate(region = case_when(region == "Region Hovedstaden" ~ "Capital",
#                             region == "Region Midtjylland" ~ "Central Jutland",
#                             region == "Region Nordjylland" ~ "Nothern Jutland",
#                             region == "Region Syddanmark" ~ "Southern Denmark",
#                             region == "Region Sjælland" ~ "Zealand"))
# 
# aggregated_ww_region_article %>%
#   plot_aggregated_eng(smoothing = "weekly_avg",
#                      plot_ci = TRUE,
#                      subtitle = "Regions in Denmark")+
#   facet_wrap(~region)
# 
#   save_plot1(last_plot(),
#     "{today()}_regioner_article.png",
#             folder = here("outputs", "aggregated"))

# Landsdele ---------------------------------------------------------------

aggregated_ww_landsdel <- ww_human %>%
  # Manually remove data from friday 1/7-2022 due to Tour de France
  mutate(log_rna_mean_faeces = ifelse(
    date_receipt >= ymd("2022-06-30") &
      date_receipt <= ymd("2022-07-01") &
      landsdel %in% c("København", "Københavns omegn"),
    NA,
    log_rna_mean_faeces
  )) %>%
  filter(overlap_level == "1. anlæg") %>%
  aggregate_ww(landsdel) %>%
  filter(date_receipt >= ymd("2022-01-01"),
         !is.na(value),
         measure != "total_testede_pop") %>%
  arrange(date_receipt, measure)

figs_aggregated_ww_landsdel <- aggregated_ww_landsdel %>%
  nest_by(landsdel) %>%
  filter(landsdel != "Københavns omegn") %>%
  mutate(plot = list(plot_aggregated(
    data,
    title = glue("SARS-CoV-2 incidens og resultater ",
                 "fra spildevandsmålinger, {landsdel}"),
    plot_ci = TRUE))) %>%
  ungroup()


# STIPLEDE LINJER
aggregated_ww_landsdel %>%
  filter(landsdel == "Københavns omegn") %>%
  plot_aggregated(smoothing = "weekly_avg",
                  plot_ci = TRUE,
                  plot_kbh_omegn = TRUE,
                  title = glue("SARS-CoV-2 incidens og resultater ",
                               "fra spildevandsmålinger, Københavns omegn")) +
  annotate("text", x = ymd("2022-03-31"), y = 750, label = "Usikkert estimat", size = 1.5)

save_plot(last_plot(),
          "{today()}_landsdel-kobenhavns_omegn.png",
          folder = here("outputs", "aggregated"))


# Shaded område
# aggregated_ww_landsdel %>%
#   filter(landsdel == "Københavns omegn") %>%
#   plot_aggregated(smoothing = "weekly_avg",
#                   plot_ci = TRUE,
#                   plot_kbh_omegn = TRUE,
#                   title = glue("Covid-19 incidens og resultater ",
#                                "fra spildevandsmålinger, Københavns omegn")) +
#   annotate("text", x = ymd("2022-03-31"), y = 750, label = "Usikkert estimat")
#
#
# # FORSØG MED INDSÆT NA
# aggregated_ww_landsdel %>%
#   filter(landsdel == "Københavns omegn") %>%
#   mutate(value = ifelse(date_receipt == ymd("2022-03-31") &
#                                 measure == "rna_normalised_faeces",  NA, value)) %>%
#   plot_aggregated(smoothing = "weekly_avg",
#                   plot_ci = TRUE,
#                   plot_kbh_omegn = TRUE,
#                   title = glue("Covid-19 incidens og resultater ",
#                                "fra spildevandsmålinger, Københavns omegn"))

figs_aggregated_ww_landsdel %>%
  mutate(landsdel_fp = make_clean_names(landsdel)) %>%
  select(landsdel_fp, plot) %>%
  pwalk(~ save_plot(plot = ..2,
                    filename = "{today()}_landsdel-{landsdel_fp}.png",
                    landsdel_fp = ..1,
                    folder = here("outputs", "aggregated")))


# OPTIONAL SOMETIMES (CT-VALUES AND COPIES/L FOR EACH GENE)---------------------

# Dette kan køres somme tider, hvis vi vil tjekke Ct værdier samt tilhørende kopier/L for hvert gen.
# Typisk hvis vi ser meget jumpy værdier er det værd at dykke dybere ned i.
# 
# copies_ct_mean <- ww_human %>%
#   filter(      !is.na(log_rna_mean_faeces),
#                !is.na(population),
#                !anlaeg_rando %in% c("Avedøre (Ejby)",
#                                     "Avedøre (Vallensbæk)"),
#                date_receipt >= ymd("2022-01-01")) %>%
#   # Ændrer til numerisk værdi og laver datoer om til ugestart (+3 dage fordi det er midt i ugen på plottet)
#   mutate(rdrp_trend_vaerdi_2022 = as.numeric(rdrp_trend_vaerdi_2022),
#          n2_trend_vaerdi = as.numeric(n2_trend_vaerdi),
#          week_start = floor_date(date_receipt, "week", 1) + days(3)) %>%
#   group_by(week_start)%>%
#   # Beregner de vægtede gennemsnit (med log(population))
#   summarise(
#     pop_ww = sum(population),
#     weighted_t_test = list(suppressWarnings(
#       weights::wtd.t.test(rdrp_trend_vaerdi_2022, NA, weight = log10(population))
#     )),
#     rdrp_trend_vaerdi_2022 = weighted_t_test[[1]]$additional["Mean"],
#     weighted_t_test = list(suppressWarnings(
#       weights::wtd.t.test(rna_rdrp_2022, NA, weight = log10(population))
#     )),
#     rna_rdrp_2022 = weighted_t_test[[1]]$additional["Mean"],
#     weighted_t_test = list(suppressWarnings(
#       weights::wtd.t.test(n2_trend_vaerdi, NA, weight = log10(population))
#     )),
#     n2_trend_vaerdi = weighted_t_test[[1]]$additional["Mean"],
#     weighted_t_test = list(suppressWarnings(
#       weights::wtd.t.test(rna_n2, NA, weight = log10(population))
#     )),
#     rna_n2 = weighted_t_test[[1]]$additional["Mean"],) %>%
#   select(-weighted_t_test)
# 
# p1 <- copies_ct_mean %>%
#   pivot_longer(cols = c("rdrp_trend_vaerdi_2022", "n2_trend_vaerdi"), 
#                names_to = "measure",
#                values_to = "value") %>%
#   ggplot(data = ., aes(x = week_start, y = value, color = measure)) +
#   geom_point(alpha = 1,
#              size = 3) +
#   geom_line(linetype = "dotted")+
#   geom_smooth(method = "loess",
#               formula = y ~ x,
#               alpha = 0.05)+
#   scale_color_manual(breaks = c("n2_trend_vaerdi", "rdrp_trend_vaerdi_2022"),
#                      values = c("#ED6553",
#                                 "#3498D8"),
#                      labels = c("N2 Ct værdi", "RdRp Ct værdi"))+
#   scale_x_date(date_breaks = "1 week",
#                date_labels = "%W",
#                expand = expansion(c(0.01, 0.05)))+
#   scale_y_continuous(limits = c(29.5,35.5),
#                      breaks = c(29,30,31,32,33,34,35,36))+
#   labs(title = "Gennemsnits ct værdier (vægtet med population)",
#        x = "Uge",
#        y = "Ct",
#        color = "Ct værdier")+
#   theme(legend.position = "bottom",
#         legend.box = "vertical")
# 
# 
# p2 <- copies_ct_mean %>%
#   pivot_longer(cols = c("rna_rdrp_2022", "rna_n2"), 
#                names_to = "measure",
#                values_to = "value") %>%
#   ggplot(data = ., aes(x = week_start,  y = value, color = measure)) +
#   geom_point(alpha = 1,
#              size = 3) +
#   geom_line(linetype = "dotted")+
#   geom_smooth(method = "loess",
#               formula = y ~ x,
#               alpha = 0.05)+
#   scale_color_manual(breaks = c("rna_n2", "rna_rdrp_2022"),
#                      values = c("#ED6553",
#                                 "#3498D8"),
#                      labels = c("N2 kopier/L", "RdRp kopier/L"))+
#   scale_x_date(date_breaks = "1 week",
#                date_labels = "%W",
#                expand = expansion(c(0.01, 0.05)))+
#   scale_y_continuous(limits = c(0,135000),
#                      breaks = c(0,10000,20000,30000,40000,50000,60000, 70000, 80000, 90000, 100000, 110000, 120000, 130000, 140000))+
#   labs(title = "Kopier/L gennemsnit (vægtet med population)",
#        x = "Uge",
#        y = "RdRp Kopier/L",
#        color = "Kopier/L")+
#   theme(legend.position = "bottom",
#         legend.box = "vertical")
# 
# p1+p2


# AGGREAGTED OLD VERSION (2021) ----------------------------------------------------------------------


# Danmark -----------------------------------------------------------------
#
# aggregated_ww_dk <- ww_human %>%
#   filter(overlap_level == "1. anlæg") %>%
#   aggregate_ww() %>%
#   bind_rows(owid_hosp %>%
#               rename(date_receipt = date) %>%
#               pivot_longer(-date_receipt,
#                            names_to = "measure")) %>%
#   arrange(date_receipt, measure)
#
# aggregated_ww_dk %>%
#   plot_aggregated(left_axis_colour = NULL, smoothing = "weekly_avg",
#                   plot_ci = TRUE) %>%
#   save_plot("{today()}_danmark.png",
#             folder = here("outputs", "aggregated"))
#
#
# # Regioner ----------------------------------------------------------------
#
# cli_alert("Saving aggregated figures (Regioner)")
#
# aggregated_ww_region <- ww_human %>%
#   filter(overlap_level == "1. anlæg") %>%
#   aggregate_ww(region)
#
# figs_aggregated_ww_region <- aggregated_ww_region %>%
#   nest_by(region) %>%
#   mutate(plot = list(plot_aggregated(
#     data,
#     title = glue("Covid-19 incidens og resultater ",
#                  "fra spildevandsmålinger, {region}",
#                  region = str_remove(region, "Region "))))) %>%
#   ungroup()
#
# figs_aggregated_ww_region %>%
#   mutate(region_fp = make_clean_names(region)) %>%
#   select(region_fp, plot) %>%
#   pwalk(~ save_plot(plot = ..2,
#                     filename = "{today()}_region-{region_fp}.png",
#                     region_fp = ..1,
#                     folder = here("outputs", "aggregated")))
#
#
# ## Comparison --------------------------------------------------------------
#
# ww_human %>%
#   filter(date_receipt >= ymd("2021-08-30")) %>%
#   group_by(region, date_receipt = floor_date(date_receipt, "week", 1)) %>%
#   summary_copies_per_person() %>%
#   ggplot(aes(date_receipt, copies_per_person,
#              colour = region,
#              fill = region,
#              group = region)) +
#   # geom_ribbon(aes(ymin = ci_lo,
#   #                 ymax = ci_hi),
#   #             alpha = 0.15,
#   #             colour = "transparent") +
#   geom_line(size = 1.3) +
#   geom_point(data = ~ group_by(., region) %>%
#                filter(date_receipt == max(date_receipt, na.rm = TRUE)) %>%
#                ungroup() %>%
#                arrange(copies_per_person),
#              aes(fill = region),
#              colour = "white",
#              shape = 21,
#              size = 2.5,
#              stroke = 0.75,
#              show.legend = FALSE) +
#   scale_x_date(date_breaks = "2 week",
#                date_labels = "%V",
#                date_minor_breaks = "1 week") +
#   ggthemes::scale_color_few() +
#   ggthemes::scale_fill_few("Medium") +
#   # theme(panel.grid.minor.x = element_line()) +
#   scale_y_log10(labels = scales::comma_format(big.mark = ".",
#                                               decimal.mark = ",")) +
#   labs(
#     title = "Resultater fra spildevandsmålinger",
#     colour = NULL,
#     fill = NULL,
#     x = "Uge",
#     y = "RNA-kopier pr. indbygger"
#   )
#
# save_plot(last_plot(),
#           "{today()}_regioner.png",
#           folder = here("outputs", "aggregated"))
#
#
# save_plot(
#   last_plot() +
#     scale_y_continuous(labels = scales::comma_format(big.mark = ".",
#                                                      decimal.mark = ",")),
#   "{today()}_regioner-natural-scale.png",
#   folder = here("outputs", "aggregated")
# )
#
#
#
# # Landsdele ---------------------------------------------------------------
#
# cli_alert("Saving aggregated figures (Landsdele)")
#
# aggregated_ww_landsdel <- ww_human %>%
#   filter(overlap_level == "1. anlæg") %>%
#   aggregate_ww(landsdel)
#
# figs_aggregated_ww_landsdel <- aggregated_ww_landsdel %>%
#   nest_by(landsdel) %>%
#   mutate(plot = list(plot_aggregated(
#     data,
#     title = glue("Covid-19 incidens og resultater ",
#                  "fra spildevandsmålinger, {landsdel}")))) %>%
#   ungroup()
#
#
#
# figs_aggregated_ww_landsdel %>%
#   mutate(landsdel_fp = make_clean_names(landsdel)) %>%
#   select(landsdel_fp, plot) %>%
#   pwalk(~ save_plot(plot = ..2,
#                     filename = "{today()}_landsdel-{landsdel_fp}.png",
#                     landsdel_fp = ..1,
#                     folder = here("outputs", "aggregated")))





