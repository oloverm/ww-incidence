


# Here I'm trying to make the big faceted forecast graph




trained_arimas %>%
  select(m5_100) %>%
  forecast(test_data) %>%
  hilo(level = 95) %>%
  rename(conf = "95%") %>%
  unpack_hilo(cols = conf) %>%
  relocate(.mean, conf_lower, conf_upper,
           .after = date_receipt) %>%
  ggplot(aes(date_receipt)) +
  geom_line(data = dk,
            aes(y = incidence,
                colour = "Observed"),
            size = 2) +
  geom_ribbon(aes(ymin = conf_lower,
                  ymax = conf_upper),
              fill = "purple",
              alpha = 0.2) +
  geom_line(aes(y = .mean,
                colour = "Forecast (95% PI)"),
            size = 1.5) +
  geom_line(data = trained_arimas %>%
              select(m5_100) %>%
              augment(),
            aes(y = .fitted,
                colour = "Prediction"),
            size = 1.5) +
  scale_x_date(date_breaks = "1 month",
               labels = label_month_first_year()) +
  scale_y_log10() +
  scale_colour_manual(breaks = c("Observed",
                                 "Prediction",
                                 "Forecast (95% PI)"),
                      values = c("black",
                                 "darkorange",
                                 "purple")) +
  theme(axis.text.x = element_text(hjust = 0)) +
  labs(
    title = "Multi-step forecast",
    x = NULL,
    y = "Incidence per 100,000",
    colour = NULL,
    fill = NULL
  )





trained_arimas %>%
  select(m5_100) %>%
  forecast(test_data) %>%
  hilo(level = 95) %>%
  rename(conf = "95%") %>%
  unpack_hilo(cols = conf) %>%
  select(.model, date_receipt, .mean, conf_lower, conf_upper) %>%
  ggplot(aes(date_receipt)) +
  geom_line(data = dk,
            aes(y = incidence,
                colour = "Observed"),
            size = 2) +
  geom_ribbon(aes(ymin = conf_lower,
                  ymax = conf_upper),
              fill = "purple",
              alpha = 0.2) +
  geom_line(aes(y = .mean,
                colour = "Forecast (95% PI)"),
            size = 1.5) +
  geom_line(data = trained_arimas %>%
              select(m5_100) %>%
              augment(),
            aes(y = .fitted,
                colour = "Prediction"),
            size = 1.5) +
  scale_x_date(date_breaks = "1 month",
               labels = label_month_first_year()) +
  scale_y_log10() +
  scale_colour_manual(breaks = c("Observed",
                                 "Prediction",
                                 "Forecast (95% PI)"),
                      values = c("black",
                                 "darkorange",
                                 "purple")) +
  theme(axis.text.x = element_text(hjust = 0)) +
  labs(
    title = "Multi-step forecast",
    x = NULL,
    y = "Incidence per 100,000",
    colour = NULL,
    fill = NULL
  )

trained_pleje %>%
  select(pm1_100) %>%
  forecast(test_data) %>%
  hilo(level = 95) %>%
  rename(conf = "95%") %>%
  unpack_hilo(cols = conf) %>%
  select(.model, date_receipt, .mean, conf_lower, conf_upper) %>%
  ggplot(aes(date_receipt)) +
  geom_line(data = dk,
            aes(y = pleje_incidence,
                colour = "Observed"),
            size = 2) +
  geom_ribbon(aes(ymin = conf_lower,
                  ymax = conf_upper),
              fill = "purple",
              alpha = 0.2) +
  geom_line(aes(y = .mean,
                colour = "Forecast (95% PI)"),
            size = 1.5) +
  geom_line(data = trained_pleje %>%
              select(pm1_100) %>%
              augment(),
            aes(y = .fitted,
                colour = "Prediction"),
            size = 1.5) +
  scale_x_date(date_breaks = "1 month",
               labels = label_month_first_year()) +
  scale_y_log10() +
  scale_colour_manual(breaks = c("Observed",
                                 "Prediction",
                                 "Forecast (95% PI)"),
                      values = c("black",
                                 "darkorange",
                                 "purple")) +
  theme(axis.text.x = element_text(hjust = 0)) +
  labs(
    title = "Multi-step forecast",
    x = NULL,
    y = "Incidence among care personnel",
    colour = NULL,
    fill = NULL
  )


regional_weekly


regional_arimas %>%
  select(rm4_100) %>%
  forecast(test_regional) %>%
  hilo(level = 95) %>%
  rename(conf = "95%") %>%
  unpack_hilo(cols = conf) %>%
  select(.model, date_receipt, .mean, conf_lower, conf_upper, region) %>%
  ggplot(aes(date_receipt)) +
  facet_wrap(~ region, ncol = 2) +
  geom_line(data = regional_weekly,
            aes(y = incidence,
                colour = "Observed"),
            size = 2) +
  geom_ribbon(aes(ymin = conf_lower,
                  ymax = conf_upper),
              fill = "purple",
              alpha = 0.2) +
  geom_line(aes(y = .mean,
                colour = "Forecast (95% PI)"),
            size = 1.5) +
  geom_line(data = regional_arimas %>%
              select(rm4_100) %>%
              augment(),
            aes(y = .fitted,
                colour = "Prediction"),
            size = 1.5) +
  scale_x_date(date_breaks = "2 month",
               labels = label_month_first_year()) +
  scale_y_log10() +
  scale_colour_manual(breaks = c("Observed",
                                 "Prediction",
                                 "Forecast (95% PI)"),
                      values = c("black",
                                 "darkorange",
                                 "purple")) +
  theme(axis.text.x = element_text(hjust = 0)) +
  labs(
    title = "Multi-step forecast",
    x = NULL,
    y = "Regional incidence",
    colour = NULL,
    fill = NULL
  )



regional_arimas %>%
  select(rm4_100) %>%
  forecast(test_regional) %>%
  accuracy(test_regional) %>%
  select(-c(ME, MPE, MASE, RMSSE)) %>%
  pivot_longer(RMSE:ACF1) %>%
  filter(!is.na(value)) %>%
  ggplot(aes(value, region)) +
  facet_wrap(~ name, scales = "free_x") +
  # facet_grid(vars(region), vars(name), scales = "free_x") +
  geom_col()
