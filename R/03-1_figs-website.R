# Figures for the website

# For report --------------------------------------------------------------

cli_alert("Saving figures for the website (small per anlæg, yellow-green)",
          wrap = TRUE)





## Regional ww/human -------------------------------------------------------

fig_sizing <- tribble(
               ~landsdel, ~width, ~height,  ~ncol,
            "Østjylland",     8,      6,       NA,
      "Københavns omegn",     5,      2.7,     NA,
             "København",     4,      2.7,     NA,
            "Sydjylland",     9,      6.8,      5,
          "Nordsjælland",     8,      4.8,     NA,
           "Nordjylland",     8,      6,       NA,
  "Vest- og Sydsjælland",     8,      6.4,      5,
                   "Fyn",     8,      6,       NA,
           "Vestjylland",     8,      6,       NA,
           "Østsjælland",     5,      3.3,     NA,
                      NA,     8,      6,       NA,
              "Bornholm",     4,      2.7,      2,
  )





# # Regional ww/human graph
# for (current_landsdel in landsdele) {
#
#   current_fig_sizing <- fig_sizing %>%
#     filter(landsdel == current_landsdel)
#
#   df_fig_ww_human_region <- ww_human %>%
#     mutate(region = coalesce(landsdel, region)) %>%
#     filter(region == current_landsdel,
#            date_receipt >= dmy("1 jul 2021")) %>%
#     select(anlaeg_rando, date_receipt, rna7_log, inc7) %>%
#     group_by(anlaeg_rando, date_receipt) %>%
#     summarise(across(c(rna7_log, inc7), mean, na.rm = TRUE),
#               .groups = "drop") %>%
#     ungroup() %>%
#     # Don't plot ones with only 1 WW measurement
#     group_by(anlaeg_rando) %>%
#     mutate(n_this_anlaeg = sum(!is.na(rna7_log))) %>%
#     ungroup() %>%
#     filter(n_this_anlaeg >= 3)
#
#   # Skip the loop iteration if there's no data
#   if (nrow(df_fig_ww_human_region) == 0) next
#
#   fig_ww_human_region <- df_fig_ww_human_region %>%
#     ggplot(aes(date_receipt, pmax(rna7_log * 20, 0))) +
#     facet_wrap(~ anlaeg_rando, ncol = current_fig_sizing$ncol) +
#     geom_area(
#       # Fix for when there's multiple samples delivered on one day
#       data = ~ distinct(., anlaeg_rando, date_receipt, inc7) %>%
#         filter(!is.na(inc7)),
#       aes(y = inc7), fill = "#F9E1AF", colour = "transparent"
#     ) +
#     geom_line(data = ~ filter(., !is.na(rna7_log), !is.nan(rna7_log)),
#               colour = "#ED6553", size = 1, na.rm = TRUE) +
#     scale_x_date(labels = isoweek, date_breaks = "3 weeks",
#                  expand = expansion(0.04)) +
#     scale_y_continuous(
#       breaks = seq(0, 200, by = 40),
#       sec.axis = sec_axis(
#         trans = ~ . / 20,
#         breaks = c(0, 2, 4),
#         labels = function(x) scales::comma(10^x,
#                                            big.mark = ".",
#                                            decimal.mark = ",",
#                                            accuracy = 1),
#         name = "RNA-kopier pr. liter spildevand\n(gennemsnit af de seneste 7 dage)"
#       )
#     ) +
#     guides(colour = "none") +
#     theme(panel.grid.major.x = element_line(),
#           panel.spacing.x = unit(15, "pt"),
#           panel.spacing.y = unit(10, "pt"),
#           legend.position = "bottom") +
#     theme_axes_leftright() +
#     labs(x = "Uge for spildevandsprøve",
#          y = "Daglig incidens pr. 100.000\n(gennemsnit af 7 dage)",
#          shape = NULL)
#
#   # ggsave(glue("outputs/{today()}_ww-human-{current_landsdel}.png",
#   #             current_landsdel = current_landsdel),
#   #        width = current_fig_sizing$width,
#   #        height = current_fig_sizing$height,
#   #        scale = 1.3)
#
#   save_plot(fig_ww_human_region,
#             filename = "{today()}_ww-human-{current_landsdel}.png",
#             folder = here("outputs", "website"),
#             width = current_fig_sizing$width,
#             height = current_fig_sizing$height,
#             scale = 1.3)
#
# }






## Regional binary ---------------------------------------------------------

# Regional binary graph
for (current_landsdel in geogs$landsdele) {

  df_fig_yellow_green_region <- ww_human %>%
    mutate(region = coalesce(landsdel, region),
           max_sample = pmin(max(date_receipt[!is.na(sample)]),
                             today(),
                             na.rm = TRUE)) %>%
    select(date_receipt, anlaeg_rando, påvist_bin, region, max_sample) %>%
    filter(date_receipt >=
             floor_date(today(), "week", week_start = 1) - days(28),
           date_receipt <= max_sample,
           region == current_landsdel,
           anlaeg_rando %in% lookup_oplande_names_new$anlaeg_display)

  n_anlaeg_landsdel <- n_distinct(df_fig_yellow_green_region$anlaeg_rando)

  # Skip the loop iteration if there's no data
  if (nrow(df_fig_yellow_green_region) == 0) next

  fig_yellow_green_region <- df_fig_yellow_green_region %>%
    complete(
      date_receipt = seq(
        min(date_receipt, na.rm = TRUE),
        max(date_receipt, na.rm = TRUE),
        "1 day"
      ),
      anlaeg_rando
    ) %>%
    mutate(påvist_na = fct_explicit_na(påvist_bin, na_level = "ingen data")) %>%
    ggplot(aes(date_receipt, anlaeg_rando, fill = påvist_na)) +
    geom_tile() +
    scale_x_date(date_breaks = "1 week", labels = isoweek,
                 expand = expansion(0)) +
    scale_y_discrete(limits = rev, expand = expansion(), drop = TRUE) +
    scale_fill_manual(
      breaks = c("påvist",
                 "ikke påvist",
                 "ingen data"),
      values = c("gold",
                 "limegreen",
                 "grey90")
    ) +
    theme(panel.grid.major.y = element_blank(),
          panel.grid.major.x = element_blank(),
          panel.background = element_rect(fill = "grey90",
                                          colour = "transparent"),
          axis.ticks.x = element_line(colour = "grey80")) +
    labs(
      x = NULL,
      y = NULL,
      fill = NULL
    )
  

  # ggsave(glue("outputs/{today()}_ww-tile-binary-{current_landsdel}.png",
  #             current_landsdel = current_landsdel %>% str_remove("Region ")),
  #        width = 7,
  #        height = n_anlaeg_landsdel * 0.2 + 0.2,
  #        scale = 1.3)

  save_plot(fig_yellow_green_region,
            filename = "{today()}_ww-tile-binary-{current_landsdel_fp}.png",
            current_landsdel_fp = make_clean_names(current_landsdel),
            folder = here("outputs", "website"),
            width = 7,
            height = n_anlaeg_landsdel * 0.2 + 0.2,
            scale = 1.3)

}
