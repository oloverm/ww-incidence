# Raw vs moving average
raw_owid %>%
  filter(location == "Denmark") %>%
  select(date, hosp_patients) %>%
  mutate(hosp_patients7 = rollmean(hosp_patients, 7, fill = NA)) %>%
  pivot_longer(-date) %>%
  ggplot(aes(date, value, colour = name)) +
  geom_line()


raw_owid %>%
  filter(location == "Denmark",
         !is.na(hosp_patients)) %>%
  select(date, hosp_patients) %>%
  tsibble::as_tsibble(index = date) %>%
  feasts::PACF(hosp_patients) %>%
  autoplot() +
  labs(title = "Hospitalisations PACF, daily")





library(magrittr)

raw_owid %>%
  filter(location == "Denmark") %>%
  select(date, hosp_patients) %>%
  filter(!is.na(hosp_patients)) %>%
  mutate(hosp_patients = hosp_patients + if_else(wday(date) == 3, 0, 0)) %>%
  pull(hosp_patients) %>%
  TSA::periodogram(plot = FALSE) %$%
  tibble(period = 1 / freq,
         log_density = log10(spec)) %>%
  filter(period < 200) %>%
  ggplot(aes(period, log_density)) +
  geom_line()




raw_owid %>%
  filter(location == "Denmark") %>%
  select(date, hosp_patients) %>%
  filter(!is.na(hosp_patients)) %>%
  mutate(wday = wday(date, label = TRUE, week_start = 1)) %>%
  group_by(wday) %>%
  summarise(mean_hosp_patients = mean(hosp_patients)) %>%
  ggplot(aes(wday, mean_hosp_patients)) +
  geom_col()

raw_owid %>%
  filter(location == "Denmark") %>%
  select(date, hosp_patients) %>%
  filter(!is.na(hosp_patients)) %>%
  mutate(wday = wday(date, label = TRUE, week_start = 1)) %>%
  ggplot(aes(wday, hosp_patients, group = wday)) +
  geom_boxplot() +
  scale_y_log10()





raw_owid %>%
  filter(location == "Denmark") %>%
  select(date, hosp_patients) %>%
  filter(!is.na(hosp_patients)) %>%
  mutate(wday = wday(date, label = TRUE, week_start = 1),
         week = ISOweek(date)) %>%
  group_by(week) %>%
  mutate(weekly_mean = mean(hosp_patients)) %>%
  ungroup() %>%
  mutate(diff_from_weekly = weekly_mean - hosp_patients) %>%
  ggplot(aes(wday, diff_from_weekly, group = wday)) +
  geom_boxplot()


raw_owid %>%
  filter(location == "Denmark") %>%
  select(date, hosp_patients) %>%
  filter(!is.na(hosp_patients)) %>%
  mutate(wday = wday(date, label = TRUE, week_start = 1),
         week = ISOweek(date)) %>%
  group_by(week) %>%
  mutate(weekly_mean = mean(hosp_patients)) %>%
  ungroup() %>%
  mutate(diff_from_weekly = weekly_mean - hosp_patients) %>%
  group_by(wday) %>%
  summarise(diff_from_weekly = mean(diff_from_weekly)) %>%
  ungroup() %>%
  ggplot(aes(wday, diff_from_weekly)) +
  geom_point()


raw_owid %>%
  filter(location == "Denmark") %>%
  select(date, hosp_patients) %>%
  filter(!is.na(hosp_patients)) %>%
  mutate(wday = wday(date, label = TRUE, week_start = 1) %>%
           factor(ordered = FALSE),
         week = ISOweek(date)) %>%
  group_by(week) %>%
  mutate(weekly_mean = mean(hosp_patients)) %>%
  ungroup() %>%
  mutate(diff_from_weekly = weekly_mean - hosp_patients) %>%
  lm(data = ., diff_from_weekly ~ wday) %>%
  summary()
