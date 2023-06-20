# AGGREAGTED NEW VERSION (2022 - FAECES NORMALISATION) -----------------------------------------------


# This function is used for the data before 2022
#
summary_copies_per_person <- function(df) {

  # assert_that(has_name(df,
  #                      c("anlaeg_rando",
  #                        "rna_mean",
  #                        "flow",
  #                        "population")),
  #             nrow(df) >= 1)

  df %>%
    filter(!is.na(rna_mean),
           !is.na(flow),
           !is.na(population),
           flow > 200 | anlaeg_rando != "København (Lynetten)",
           !anlaeg_rando %in% c("Avedøre (Ejby)",
                                "Avedøre (Vallensbæk)"),
           date_receipt <= ymd("2021-12-31")) %>%
    mutate(copies_per_person = (rna_mean * flow) / population) %>%
    summarise(pop_ww = sum(population),
              weighted_t_test = list(suppressWarnings(
                weights::wtd.t.test(copies_per_person, NA, weight = population)
              )),
              copies_per_person = weighted_t_test[[1]]$additional["Mean"],
              se = weighted_t_test[[1]]$additional["Std. Err"],
              ci_lo = copies_per_person - 1.96 * se,
              ci_hi = copies_per_person + 1.96 * se,
              .groups = "drop") %>%
    ungroup() %>%
    select(-weighted_t_test)

}

# This function is used for data after 2021
# By july updated to use log values from the beginning and weighted by log population

summary_rna_normalised_faeces <- function(df) {
  # assert_that(has_name(df, c("anlaeg_rando",
  #                            "rna_mean",
  #                            "population")),
  #             nrow(df) >= 1
  # )

  df %>%
    filter(
      !is.na(log_rna_mean_faeces),
      !is.na(population),
      !anlaeg_rando %in% c("Avedøre (Ejby)",
                           "Avedøre (Vallensbæk)")
    ) %>%
    #mutate(copies_per_person_faeces = rna_normalised_faeces / population) %>%
    summarise(
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
    mutate(rna_mean_faeces = (10^log_rna_mean_faeces)*10e6,
           se = (10^se)*10e6,
           ci_lo = (10^ci_lo)*10e6,
           ci_hi = (10^ci_hi)*10e6)
}

summary_inc7 <- function(df) {

  # assert_that(has_name(df,
  #                      c("anlaeg_rando",
  #                        "date_receipt",
  #                        "n_human",
  #                        "population")),
  #             nrow(df) >= 1)

  df %>%
    filter(!is.na(population),
           # Remove sub-oplande, because otherwise we'll be counting them twice
           !anlaeg_rando %in% c("Avedøre (Ejby)",
                                "Avedøre (Vallensbæk)"),
           date_receipt >= ymd("2021-07-03")) %>%
    distinct(date_receipt, population, positive, testede) %>%
    group_by(date_receipt, .add = TRUE) %>%
    summarise(total_positive = sum(positive, na.rm = TRUE),
              total_testede = sum(testede, na.rm = TRUE),
              pop_human = sum(population, na.rm = TRUE),
              .groups = "drop_last") %>%
    mutate(total_positive_7 = rollmean(total_positive, 7, fill = NA, align = "right"),
           total_testede_7 = rollmean(total_testede, 7, fill = NA, align = "right"),
           total_inc_7 = total_positive_7 / pop_human * 100000,
           total_testede_pop = total_testede_7 / pop_human * 1000) %>%
    ungroup() #%>%
  # select(-c(total_human, total_human_7))
}

aggregate_ww <- function(df, groups = NULL) {
  groups_chr <- as_label(enquo(groups))

  if (groups_chr == "NULL") groups_chr <- NULL
  
  copies_per_person <- df %>%
    group_by({{ groups }},
             date_receipt = floor_date(date_receipt, "week", 1) + days(3)
    ) %>%
    summary_copies_per_person()

  copies_per_person_faeces <- df %>%
    group_by({{ groups }},
             date_receipt = floor_date(date_receipt, "week", 1) + days(3)
    ) %>%
    summary_rna_normalised_faeces()

  incidence_tests <- df %>%
    group_by({{ groups }}) %>%
    summary_inc7()

  bind_rows(
    copies_per_person %>%
      mutate(measure = "copies_per_person") %>%
      rename(value = copies_per_person),
    copies_per_person_faeces %>%
      mutate(measure = "rna_mean_faeces") %>%
      rename(value = rna_mean_faeces),
    incidence_tests %>%
      mutate(measure = "total_inc_7") %>%
      rename(value = total_inc_7),
    incidence_tests %>%
      mutate(measure = "total_testede_pop") %>%
      rename(value = total_testede_pop)
  ) %>%
    filter(!is.na(value)) %>%
    #Manually change the date of the last week in 2021 to make the graphs connect
    mutate(date_receipt = if_else(date_receipt == ymd("2021-12-30") &
                                    measure == "copies_per_person",  ymd("2022-01-01"), date_receipt)) %>%
    arrange(date_receipt, measure)
}


plot_aggregated <- function(df,
                            title = NULL,
                            left_axis_colour = "#e5c087",
                            smoothing = "weekly_avg",
                            span_loess = NULL,
                            plot_ci = FALSE,
                            plot_kbh_omegn = FALSE) {

  assert_that(has_name(df, c("date_receipt", "pop_ww",
                             "measure", "value")),
              is.data.frame(df),
              nrow(df) >= 1)

  assert_that(all(smoothing %in% c("weekly_avg", "loess")),
              msg = "`smoothing` must be \"weekly_avg\" or \"loess\".")


  title <- title %||% "SARS-CoV-2 incidens og resultater fra spildevandsmålinger"
  span_loess <- span_loess %||% (df %>%
                                   filter(measure == "rna_mean_faeces",
                                          !is.na(value)) %>%
                                   nrow() %>%
                                   `/`(16, .) %>%
                                   pmin(0.6))


  layer_loess <- if ("loess" %in% smoothing) {
    geom_smooth(data = ~ filter(., measure == "rna_mean_faeces"),
                aes(weight = pop_ww),
                method = "loess",
                formula = y ~ x,
                span = span_loess,
                colour = "#3498D8",
                fill = "#3498D8",
                alpha = 0.3,
                na.rm = TRUE)
  } else {
    NULL
  }

  layer_weekly_avg <- if ("weekly_avg" %in% smoothing) {
    geom_line(data = ~ filter(., measure == "rna_mean_faeces"),
              aes(colour = measure),
              size = 1.2,
              na.rm = TRUE)
  } else {
    NULL
  }

  layer_kbh_omegn <- if (plot_kbh_omegn == TRUE) {

    geom_vline(xintercept = c(ymd("2022-03-24"),ymd("2022-04-07")),
               linetype = "dashed")

    # geom_rect(aes(xmin = ymd("2022-03-24"), xmax = ymd("2022-04-07"), ymin = 0, ymax=Inf),
    #           alpha = 0.3,
    #           fill = "blue")




  } else {
    NULL
  }

  layer_weekly_ci <- if (plot_ci == TRUE & has_name(df, c("ci_lo", "ci_hi"))) {
    geom_ribbon(data = ~ filter(., !is.na(ci_lo), !is.na(ci_hi), date_receipt >= ymd("2022-01-01")),
                aes(ymin = ci_lo, ymax = ci_hi, group = 1),
                fill = "#3498D8",
                alpha = 0.3)
  } else NULL


  df %>%
    filter(!is.na(value),
           measure %in% c("copies_per_person",
                          "hosp_patients",
                          "icu_patients",
                          "total_testede_pop",
                          "rna_mean_faeces",
                          "total_inc_7")) %>%
    ggplot(aes(date_receipt, value)) +
    # geom_vline(xintercept = ymd("2022-07-11"),
    #            colour = "grey20",
    #            linetype = "dashed") +
    geom_line(data = ~ filter(., !is.na(value),
                              measure != "rna_mean_faeces"),
              aes(colour = measure),
              size = 1.2,
    ) +
    # geom_line(data = ~ filter(., date_receipt >= ymd("2022-01-01"),
    #                                                  date_receipt <= ymd("2022-01-06"),
    #                           measure == "copies_per_person" | measure == "rna_mean_faeces"),
    #           aes(x = date_receipt, y = value, colour = measure),
    #           size = 1.2,
    #           colour = "#3498D8"
    #           ) +
    layer_loess +
    layer_weekly_ci +
    layer_weekly_avg +
    layer_kbh_omegn +
    # geom_point(data = ~ filter(., measure == "copies_per_person"),
    #            aes(size = pop_ww),
    #            colour = "#ED6553", alpha = 0.7) +
    scale_x_date(date_breaks = "1 month",
                 date_labels = "%B",
                 expand = expansion(c(0.01, 0.05))) +
    scale_y_log10(
      labels = scales::comma_format(big.mark = ".",
                                    decimal.mark = ","),
      sec.axis = sec_axis(
        trans = ~ .,
        name = "RNA-kopier ift. fæcesindhold",
        labels = scales::comma_format(big.mark = ".",
                                      decimal.mark = ",")
      )
    ) +
    scale_size(limits = c(10, max(df$pop_ww)),
               labels = scales::comma_format(big.mark = ".",
                                             decimal.mark = ",")) +
    scale_colour_manual(
      breaks = c(
        "rna_mean_faeces",
        #"copies_per_person",
        "total_inc_7",
        "total_testede_pop",
        "hosp_patients",
        "icu_patients"
      ),
      values = c(
        "#3498D8",
        #"#ED6553",
        "#E5C087",
        "orange",
        "olivedrab",
        "#2ECC71"
      ),
      labels = c(
        "RNA-kopier ift. \nfæcesindhold",
        #"Data 2021: RNA-kopier \npr. indbygger",
        "Daglig incidens \npr. 100.000*",
        "Antal testede \npr. 1.000 pr. dag*",
        "Indlagte patienter \npr. dag*",
        "Intensiv patienter \npr. dag*"
      )
    ) +
    guides(colour = guide_legend()) +
    labs(title = title,
         x = "Måned",
         y = "Nye tilfælde",
         colour = NULL,
         size = "Indbyggere i oplande",
         caption = "*Beregnet som et glidende gennemsnit \nover de seneste syv dage ") +
    theme_axes_leftright(left = "#E5C087", right = "#3498D8") +
    theme(legend.position = "bottom",
          legend.box = "vertical",
          plot.caption = element_text(size = 5, face = "italic", hjust = 1)) +
    guides(colour = guide_legend(nrow = 1))
}


## FUNCTION TO PLOT 8 WEEKS

plot_aggregated_8w <- function(df,
                            title = NULL,
                            left_axis_colour = "#e5c087",
                            smoothing = "weekly_avg",
                            span_loess = NULL,
                            plot_ci = FALSE,
                            plot_kbh_omegn = FALSE) {


  assert_that(all(smoothing %in% c("weekly_avg", "loess")),
              msg = "`smoothing` must be \"weekly_avg\" or \"loess\".")


  title <- title %||% "SARS-CoV-2 incidens og resultater fra spildevandsmålinger"
  span_loess <- span_loess %||% (df %>%
                                   filter(measure == "rna_mean_faeces",
                                          !is.na(value)) %>%
                                   nrow() %>%
                                   `/`(16, .) %>%
                                   pmin(0.6))


  layer_loess <- if ("loess" %in% smoothing) {
    geom_smooth(data = ~ filter(., measure == "rna_mean_faeces"),
                aes(weight = pop_ww),
                method = "loess",
                formula = y ~ x,
                span = span_loess,
                colour = "#3498D8",
                fill = "#3498D8",
                alpha = 0.3,
                na.rm = TRUE)
  } else {
    NULL
  }

  layer_weekly_avg <- if ("weekly_avg" %in% smoothing) {
    geom_line(data = ~ filter(., measure == "rna_mean_faeces"),
              aes(colour = measure),
              size = 1.2,
              na.rm = TRUE)
  } else {
    NULL
  }


  layer_weekly_ci <- if (plot_ci == TRUE & has_name(df, c("ci_lo", "ci_hi"))) {
    geom_ribbon(data = ~ filter(., !is.na(ci_lo), !is.na(ci_hi), date_receipt >= ymd("2022-01-01")),
                aes(ymin = ci_lo, ymax = ci_hi, group = 1),
                fill = "#3498D8",
                alpha = 0.3)
  } else NULL


  df %>%
    filter(!is.na(value),
           measure %in% c("copies_per_person",
                          "hosp_patients",
                          "icu_patients",
                          "total_testede_pop",
                          "rna_mean_faeces",
                          "total_inc_7")) %>%
    ggplot(aes(date_receipt, value)) +
    geom_vline(xintercept = ymd("2022-01-01"),
               colour = "grey20") +
    geom_line(data = ~ filter(., !is.na(value),
                              measure != "rna_mean_faeces"),
              aes(colour = measure),
              size = 1.2,
    ) +
    geom_line(data = ~ filter(., date_receipt >= ymd("2022-01-01"),
                              date_receipt <= ymd("2022-01-06"),
                              measure == "copies_per_person" | measure == "rna_mean_faeces"),
              aes(x = date_receipt, y = value, colour = measure),
              size = 1.2,
              colour = "#3498D8"
    ) +
    facet_wrap(~region, nrow = 1)+
    layer_loess +
    layer_weekly_ci +
    layer_weekly_avg +
    scale_x_date(date_breaks = "weeks",
                 date_labels = "%W",
                 expand = expansion(c(0.01, 0.05))) +
    scale_y_continuous(trans = "log10")+
    scale_colour_manual(
      breaks = c(
        "rna_mean_faeces"
      ),
      values = c(
        "#3498D8"
      ),
      labels = c(
        "RNA-kopier ift. fæcesindhold"
      )
    ) +
    guides(colour = guide_legend()) +
    labs(title = title,
         x = "Uge",
         y = "RNA-kopier ift. fæcesindhold",
         colour = NULL,
         size = "Indbyggere i oplande") +
    theme(legend.position = "bottom",
          legend.box = "vertical") +
    theme_axes_leftright(left = "#3498D8") +
    guides(colour = guide_legend(nrow = 1))
}


# ENGELSK VERSION TIL ARTIKEL --------------------------------------------------
# 
# plot_aggregated_eng <- function(df,
#                             title = NULL,
#                             subtitle = NULL,
#                             left_axis_colour = "#e5c087",
#                             smoothing = "weekly_avg",
#                             span_loess = NULL,
#                             plot_ci = FALSE,
#                             plot_kbh_omegn = FALSE) {
# 
#   # Change language to english in months when plotted
#   Sys.setlocale("LC_TIME", "English")
# 
#   assert_that(has_name(df, c("date_receipt", "pop_ww",
#                              "measure", "value")),
#               is.data.frame(df),
#               nrow(df) >= 1)
# 
#   assert_that("copies_per_person" %in% df$measure,
#               msg = paste0("One of the values of `df$measure` ",
#                            "must be 'copies_per_person'."))
# 
#   assert_that(all(smoothing %in% c("weekly_avg", "loess")),
#               msg = "`smoothing` must be \"weekly_avg\" or \"loess\".")
# 
# 
#   title <- title %||% "SARS-CoV-2 incidence and wastewater surveillance data"
#   span_loess <- span_loess %||% (df %>%
#                                    filter(measure == "rna_mean_faeces",
#                                           !is.na(value)) %>%
#                                    nrow() %>%
#                                    `/`(16, .) %>%
#                                    pmin(0.6))
# 
# 
#   layer_loess <- if ("loess" %in% smoothing) {
#     geom_smooth(data = ~ filter(., measure == "rna_mean_faeces"),
#                 aes(weight = pop_ww),
#                 method = "loess",
#                 formula = y ~ x,
#                 span = span_loess,
#                 colour = "#3498D8",
#                 fill = "#3498D8",
#                 alpha = 0.3,
#                 na.rm = TRUE)
#   } else {
#     NULL
#   }
# 
#   layer_weekly_avg <- if ("weekly_avg" %in% smoothing) {
#     geom_line(data = ~ filter(., measure == "rna_mean_faeces"),
#               aes(colour = measure),
#               size = 1,
#               na.rm = TRUE)
#   } else {
#     NULL
#   }
# 
#   layer_weekly_ci_2 <- if (plot_ci == TRUE & has_name(df, c("ci_lo", "ci_hi"))) {
#     geom_ribbon(data = ~ filter(., !is.na(ci_lo), !is.na(ci_hi), date_receipt <= ymd("2022-01-01")),
#                 aes(ymin = ci_lo, ymax = ci_hi, group = 1),
#                 fill = "#ED6553",
#                 alpha = 0.3)
# 
# 
# 
# 
#   } else {
#     NULL
#   }
# 
#   layer_weekly_ci <- if (plot_ci == TRUE & has_name(df, c("ci_lo", "ci_hi"))) {
#     geom_ribbon(data = ~ filter(., !is.na(ci_lo), !is.na(ci_hi), date_receipt >= ymd("2022-01-01")),
#                 aes(ymin = ci_lo, ymax = ci_hi, group = 1),
#                 fill = "#3498D8",
#                 alpha = 0.3)
#   } else NULL
# 
# 
#   df %>%
#     filter(!is.na(value),
#            measure %in% c("copies_per_person",
#                           "hosp_patients",
#                           "icu_patients",
#                           "total_testede_pop",
#                           "rna_mean_faeces",
#                           "total_inc_7")) %>%
#     ggplot(aes(date_receipt, value)) +
#     # geom_vline(xintercept = ymd("2022-01-01"),
#     #            colour = "grey20") +
#     geom_line(data = ~ filter(., !is.na(value),
#                               measure != "rna_mean_faeces",
#                               measure != "copies_per_person"),
#               aes(colour = measure),
#               size = 0.5,
#     ) +
#     geom_line(data = ~ filter(., !is.na(value),
#                               measure == "copies_per_person"),
#               aes(colour = measure),
#               size = 1,
#     ) +
#     geom_line(data = ~ filter(., date_receipt >= ymd("2022-01-01"),
#                               date_receipt <= ymd("2022-01-06"),
#                               measure == "copies_per_person" | measure == "rna_mean_faeces"),
#               aes(x = date_receipt, y = value, colour = measure),
#               size = 1,
#               colour = "#3498D8"
#     ) +
#     layer_loess +
#     layer_weekly_ci +
#     layer_weekly_avg +
#     layer_weekly_ci_2 +
#     # geom_point(data = ~ filter(., measure == "copies_per_person"),
#     #            aes(size = pop_ww),
#     #            colour = "#ED6553", alpha = 0.7) +
#     scale_x_date(date_breaks = "1 month",
#                  date_labels = "%b \n%y",
#                  expand = expansion(c(0.01, 0.05))) +
#     scale_y_log10(
#       labels = scales::comma_format(big.mark = ".",
#                                     decimal.mark = ","),
#       sec.axis = sec_axis(
#         trans = ~ .,
#         name = "Normalised SARS-CoV-2 concentrations \nin wastewater",
#         labels = scales::comma_format(big.mark = ".",
#                                       decimal.mark = ",")
#       )
#     ) +
#     scale_size(limits = c(10, max(df$pop_ww)),
#                labels = scales::comma_format(big.mark = ".",
#                                              decimal.mark = ",")) +
#     scale_colour_manual(
#       breaks = c(
#         "rna_mean_faeces",
#         "copies_per_person",
#         "total_inc_7",
#         "total_testede_pop",
#         "hosp_patients",
#         "icu_patients"
#       ),
#       values = c(
#         "#3498D8",
#         "#ED6553",
#         "#E5C087",
#         "orange",
#         "olivedrab",
#         "#2ECC71"
#       ),
#       labels = c(
#         "Data 2022: \nNormalised SARS-CoV-2 concentrations \nin wastewater",
#         "Data 2021: \nRNA-copies per inhabitant",
#         "Daily incidence \nper 100.000*",
#         "Daily tests \nper 1.000*",
#         "Hospitalized \npatients*",
#         "ICU \npatients"
#       )
#     ) +
#     guides(colour = guide_legend()) +
#     labs(title = title,
#          subtitle = subtitle,
#          x = "Month",
#          y = "Incidence",
#          colour = NULL,
#          size = "Indbyggere i oplande",
#          caption = "*Calculated as a 7-day moving average ") +
#     theme_axes_leftright(left = "#E5C087", right = "#3498D8") +
#     theme(legend.position = "bottom",
#           panel.grid.minor.x = element_line(),
#           #legend.text = element_text(size = 14),
#           #axis.title = element_text(size = 14),
#           #axis.text = element_text(size = 10),
#           legend.box = "vertical",
#           plot.caption = element_text(size = 5, face = "italic", hjust = 1)) +
#     guides(colour = guide_legend(nrow = 1))
# }


# AGGREAGTED OLD VERSION (2021) ----------------------------------------------------------------------


# summary_copies_per_person <- function(df) {
#
#   assert_that(has_name(df,
#                        c("anlaeg_rando",
#                          "rna_mean",
#                          "flow",
#                          "population")),
#               nrow(df) >= 1)
#
#   df %>%
#     filter(!is.na(rna_mean),
#            !is.na(flow),
#            !is.na(population),
#            flow > 200 | anlaeg_rando != "København (Lynetten)",
#            !anlaeg_rando %in% c("Avedøre (Ejby)",
#                                 "Avedøre (Vallensbæk)")) %>%
#     mutate(copies_per_person = (rna_mean * flow) / population) %>%
#     summarise(pop_ww = sum(population),
#               weighted_t_test = list(suppressWarnings(
#                 weights::wtd.t.test(copies_per_person, NA, weight = population)
#               )),
#               copies_per_person = weighted_t_test[[1]]$additional["Mean"],
#               se = weighted_t_test[[1]]$additional["Std. Err"],
#               ci_lo = copies_per_person - 1.96 * se,
#               ci_hi = copies_per_person + 1.96 * se,
#               .groups = "drop") %>%
#     ungroup() %>%
#     select(-weighted_t_test)
#
# }
#
#
#
#
# summary_inc7 <- function(df) {
#
#   assert_that(has_name(df,
#                        c("anlaeg_rando",
#                          "date_receipt",
#                          "n_human",
#                          "population")),
#               nrow(df) >= 1)
#
#   df %>%
#     filter(!is.na(population),
#            # Remove sub-oplande, because otherwise we'll be counting them twice
#            !anlaeg_rando %in% c("Avedøre (Ejby)",
#                                 "Avedøre (Vallensbæk)")) %>%
#     distinct(date_receipt, population, n_human) %>%
#     group_by(date_receipt, .add = TRUE) %>%
#     summarise(total_human = sum(n_human, na.rm = TRUE),
#               pop_human = sum(population, na.rm = TRUE),
#               .groups = "drop_last") %>%
#     mutate(total_human_7 = rollmean(total_human, 7, fill = NA, align = "right"),
#            total_inc_7 = total_human_7 / pop_human * 100000) %>%
#     ungroup() %>%
#     select(-c(total_human, total_human_7))
# }
#
#
#
#
#
# aggregate_ww <- function(df, groups = NULL) {
#   groups_chr <- as_label(enquo(groups))
#
#   if (groups_chr == "NULL") groups_chr <- NULL
#
#   copies_per_person <- df %>%
#     group_by({{groups}},
#              date_receipt = floor_date(date_receipt, "week", 1) + days(3)) %>%
#     summary_copies_per_person()
#
#   incidence <- df %>%
#     group_by({{groups}}) %>%
#     summary_inc7()
#
#   full_join(
#     copies_per_person,
#     incidence,
#     by = c(groups_chr, "date_receipt")
#   ) %>%
#     mutate(total_inc_weekly = 7 * total_inc_7) %>%
#     select(-total_inc_7) %>%
#     pivot_longer(c(copies_per_person, total_inc_weekly),
#                  names_to = "measure") %>%
#     filter(!is.na(value)) %>%
#     arrange(date_receipt, measure)
# }
#
#
#
#
#
#
# plot_aggregated <- function(df,
#                             title = NULL,
#                             left_axis_colour = "#e5c087",
#                             smoothing = "weekly_avg",
#                             span_loess = NULL,
#                             plot_ci = FALSE) {
#
#   assert_that(has_name(df, c("date_receipt", "pop_ww",
#                              "measure", "value")),
#               is.data.frame(df),
#               nrow(df) >= 1)
#
#   assert_that("copies_per_person" %in% df$measure,
#               msg = paste0("One of the values of `df$measure` ",
#                            "must be 'copies_per_person'."))
#
#   assert_that(all(smoothing %in% c("weekly_avg", "loess")),
#               msg = "`smoothing` must be \"weekly_avg\" or \"loess\".")
#
#
#   title <- title %||% "Covid-19 incidens og resultater fra spildevandsmålinger"
#   span_loess <- span_loess %||% (df %>%
#                                    filter(measure == "copies_per_person",
#                                           !is.na(value)) %>%
#                                    nrow() %>%
#                                    `/`(16, .) %>%
#                                    pmin(0.6))
#
#
#   layer_loess <- if ("loess" %in% smoothing) {
#     geom_smooth(data = ~ filter(., measure == "copies_per_person"),
#                 aes(weight = pop_ww),
#                 method = "loess",
#                 formula = y ~ x,
#                 span = span_loess,
#                 colour = "#ED6553",
#                 fill = "#ED6553",
#                 alpha = 0.3,
#                 na.rm = TRUE)
#   } else {
#     NULL
#   }
#
#   layer_weekly_avg <- if ("weekly_avg" %in% smoothing) {
#     geom_line(data = ~ filter(., measure == "copies_per_person"),
#               colour = "#ED6553",
#               size = 1.2,
#               na.rm = TRUE)
#   } else {
#     NULL
#   }
#
#   layer_weekly_ci <- if (plot_ci == TRUE & has_name(df, c("ci_lo", "ci_hi"))) {
#     geom_ribbon(data = ~ filter(., !is.na(ci_lo), !is.na(ci_hi)),
#                 aes(ymin = ci_lo, ymax = ci_hi, group = 1),
#                 fill = "#ED6553",
#                 alpha = 0.3)
#   } else NULL
#
#   df %>%
#     filter(!is.na(value)) %>%
#     ggplot(aes(date_receipt, value)) +
#     geom_vline(xintercept = ymd("2022-01-03"),
#                colour = "grey20") +
#     geom_line(data = ~ filter(., !is.na(value),
#                               measure != "copies_per_person"),
#               aes(colour = measure),
#               size = 1.2) +
#     layer_loess +
#     layer_weekly_ci +
#     layer_weekly_avg +
#     # geom_point(data = ~ filter(., measure == "copies_per_person"),
#     #            aes(size = pop_ww),
#     #            colour = "#ED6553", alpha = 0.7) +
#     scale_x_date(date_breaks = "2 week", labels = isoweek,
#                  expand = expansion(c(0.01, 0.05))) +
#     scale_y_log10(
#       labels = scales::comma_format(big.mark = ".",
#                                     decimal.mark = ","),
#       sec.axis = sec_axis(
#         trans = ~ .,
#         name = "RNA-kopier pr. indbygger",
#         labels = scales::comma_format(big.mark = ".",
#                                       decimal.mark = ",")
#       )
#     ) +
#     scale_size(limits = c(10, max(df$pop_ww)),
#                labels = scales::comma_format(big.mark = ".",
#                                              decimal.mark = ",")) +
#     scale_colour_manual(
#       breaks = c(
#         "total_inc_weekly",
#         "new_tests_per_thousand",
#         "hosp_patients",
#         "icu_patients"
#       ),
#       values = c(
#         "#e5c087",
#         "orange",
#         "olivedrab",
#         "turquoise3"
#       ),
#       labels = c(
#         "Ugentlig incidens pr. 100.000",
#         "Antal testede pr. 1.000 pr. dag",
#         "Indlagte patienter pr. dag",
#         "Intensiv patienter pr. dag"
#       )
#     ) +
#     guides(colour = guide_legend()) +
#     labs(title = title,
#          x = "Uge",
#          y = "Nye tilfælde",
#          colour = NULL,
#          size = "Indbyggere i oplande") +
#     theme_axes_leftright(left = left_axis_colour) +
#     theme(legend.position = "bottom",
#           panel.grid.minor.x = element_line(),
#           legend.box = "vertical")
# }







