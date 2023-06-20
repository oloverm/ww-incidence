fig_ts_presentation <- function(df) {
  df %>%
    ggplot(aes(date_receipt, value, colour = measure)) +
    geom_vline(xintercept = ymd("2022-01-01"), colour = "grey80") +
    geom_line(size = 1.5) +
    scale_y_log10(labels = scales::label_comma(),
                  limits = c(3, 30000)) +
    scale_x_date(date_breaks = "1 month",
                 labels = label_month_first_year(month_format = "%b",
                                                 month_year_format = "%b\n%Y")) +
    scale_colour_manual(breaks = c(
      "cpp_wtd_med",
      "pleje_incidence",
      "incidence",
      "pleje_new_tests_per_thousand",
      "new_tests_per_thousand"
    ),
    labels = c(
      "Wastewater RNA\ncopies per person",
      "Care personnel\nincidence per 100k",
      "Incidence per 100k",
      "Care personnel\ntests per 1,000",
      "Tests per 1,000"
    ),
    values = c(
      "deepskyblue",
      "rosybrown2",
      "rosybrown4",
      "palegreen2",
      "green4"
    )) +
    theme(legend.key.height = unit(24, "pt"),
          axis.text.x = element_text(hjust = 0)) +
    labs(title = NULL,
         x = NULL,
         y = NULL,
         colour = NULL)
}

dk %>%
  filter_studyperiod() %>%
  pivot_longer(c(incidence),
               names_to = "measure") %>%
  fig_ts_presentation()


save_plot(plot = last_plot(), "{today()}_ts-incidence.png",
          folder = "outputs/other",
          width = 16, height = 9, scale = 0.4)






dk %>%
  filter_studyperiod() %>%
  pivot_longer(c(cpp_wtd_med, incidence),
               names_to = "measure") %>%
  fig_ts_presentation()


save_plot(plot = last_plot(), "{today()}_ts-ww-incidence.png",
          folder = "outputs/other",
          width = 16, height = 9, scale = 0.4)





dk %>%
  filter_studyperiod() %>%
  pivot_longer(c(cpp_wtd_med, incidence, new_tests_per_thousand),
               names_to = "measure") %>%
  fig_ts_presentation()



save_plot(plot = last_plot(), "{today()}_ts-ww-incidence-testing.png",
          folder = "outputs/other",
          width = 16, height = 9, scale = 0.4)






dk %>%
  filter_studyperiod() %>%
  pivot_longer(c(cpp_wtd_med, incidence, new_tests_per_thousand,
                 pleje_incidence, pleje_new_tests_per_thousand),
               names_to = "measure") %>%
  fig_ts_presentation()


save_plot(plot = last_plot(), "{today()}_ts-ww-incidence-testing-pleje.png",
          folder = "outputs/other",
          width = 16, height = 9, scale = 0.4)
