#' ---
#' title: Trends
#' output:
#'   html_document:
#'     code_folding: hide
#'     toc: no
#'     toc_float: no
#'     number_sections: yes
#'     fig_caption: no
#'     fig_width: 6
#'     fig_height: 4
#'     mathjax: null
#'     theme: cerulean
#'     df_print: kable
#'     dev: png
#' ---
#'

# Setup -------------------------------------------------------------------
knitr::opts_chunk$set(
  echo = TRUE,
  message = FALSE,
  warning = FALSE
)
setwd("S:/Spildevand/Lille ØU-sag")
knitr::opts_knit$set(root.dir = "S:/Spildevand/Lille ØU-sag")


#+ results = "hide"
source("R/00_setup.R", encoding = "utf-8")

read_ww(cache_limit = hours(1))





# Data prep ---------------------------------------------------------------

#' We're looking at trends in the latest 6 weeks of data. We're only including
#' anlæg with at least 10 samples in that period, of which at least 5 must be
#' påvist (quantifiable).
trends_data <- ww_human %>%
  select(anlaeg_rando,
         date_receipt,
         rna_corr_log,
         påvist_mean,
         inc7) %>%
  # Down-weight non-påvist samples, because we don't know exactly where below
  # the LOQ/LOD they are
  mutate(weighting = if_else(påvist_mean == "påvist", 1, 0.5)) %>%
  # Keep latest 6 weeks
  filter(date_receipt >= (max(date_receipt) - weeks(6))) %>%
  # Only keep anlæg with 5+ påvist samples (in these latest 6 weeks)
  group_by(anlaeg_rando) %>%
  filter(sum(påvist_mean == "påvist", na.rm = TRUE) >= 5,
         n() >= 10) %>%
  ungroup()

#' The model is a linear regression: `rna_corr_log ~ date_receipt`. We express
#' the estimates as percent daily change, using the [CDC's
#' calculation](https://www.cdc.gov/healthywater/surveillance/wastewater-surveillance/data-reporting-analytics.html#trends):
#' `PDC = (10^slope - 1) * 100`.
#'
#' For positive measurements <LOQ, we've imputed the value of the LOQ for that
#' test. For negative measurements, we've used 1/10 of the LOQ. Because both of
#' these numbers are uncertain, I've weighted them only 50% as much as
#' quantifiable (>LOQ) measurements.
trends <- trends_data %>%
  filter(!is.na(rna_corr_log)) %>%
  nest_by(anlaeg_rando) %>%
  # group_by(anlaeg_rando) %>%
  mutate(model = list(lm(rna_corr_log ~ date_receipt,
                         data = data,
                         weights = weighting)),
         tidied = list(broom::tidy(model, conf.int = TRUE) %>%
                         filter(term == "date_receipt") %>%
                         select(estimate, p.value, conf.low, conf.high)),
         augmented = list(broom::augment(
           model,
           newdata = tibble(date_receipt = seq(min(data$date_receipt),
                                               max(data$date_receipt),
                                               "1 day")),
           interval = "confidence")
         )) %>%
  select(-c(data)) %>%
  unnest(tidied) %>%
  ungroup() %>%
  mutate(across(c(estimate, conf.low, conf.high), ~ (10^estimate - 1) * 100),
         anlaeg_rando = fct_reorder(anlaeg_rando, estimate))




#' # Results
# Results -----------------------------------------------------------------

#' Each point is one anlæg. This shows the percent daily change in RNA (comes
#' from the slope of the regression line) and the p-value.
#'
#' We need to choose some kind of cut-off for what we think is a real trend. I'm
#' looking just at positive trends with p-values below 0.2 (green area below),
#' but that's arbitrary.
trends %>%
  ggplot() +
  geom_rect(aes(xmin = 5, xmax = Inf,
                ymin = 0, ymax = 0.10),
            data = tibble(),  # otherwise it plots one rect for each row
            inherit.aes = FALSE,
            fill = "limegreen",
            alpha = 0.5) +
  geom_vline(xintercept = 0) +
  geom_point(aes(estimate, p.value)) +
  scale_x_continuous(labels = scales::percent_format(1, scale = 1)) +
  scale_y_continuous(breaks = c(0, 0.05, 0.1, 0.15, 0.2, 0.5, 1)) +
  labs(title = "Model estimate x p-value, for each anlæg",
       x = "% daily change",
       y = "p-value")


trends_display <- trends %>%
  filter(p.value <= 0.10,
         estimate > 5)

trends_display_fitted <- trends_display %>%
  unnest(augmented) %>%
  select(anlaeg_rando, date_receipt, .fitted, .lower, .upper)



#' This is the raw data for the areas with selected trends, ordered most to the
#least steep

#+ fig.width = 10, fig.height = 8
trends_data %>%
  inner_join(trends_display, by = "anlaeg_rando") %>%
  # repeat ordering after join turned it into character
  mutate(anlaeg_rando = fct_reorder(anlaeg_rando, estimate, .desc = TRUE)) %>%
  ggplot(aes(date_receipt, pmax(rna_corr_log * 20, 0))) +
  facet_wrap(~ anlaeg_rando) +
  geom_area(
    # Fix for when there's multiple samples delivered on one day
    data = ~ distinct(., anlaeg_rando, date_receipt, inc7) %>%
      filter(!is.na(inc7)),
    aes(y = inc7), fill = "#F9E1AF", colour = "transparent"
  ) +
  geom_ribbon(data = trends_display_fitted,
              aes(x = date_receipt,
                  ymin = .lower * 20, ymax = .upper * 20),
              inherit.aes = FALSE,
              fill = "slategrey", alpha = 0.1) +
  geom_line(data = trends_display_fitted,
            aes(y = .fitted * 20),
            size = 1,
            colour = "slategrey") +
  geom_point(aes(shape = påvist_mean),
             colour = "#ED6553", size = 1.5, na.rm = TRUE) +
  scale_x_date(date_breaks = "2 weeks",
               labels = isoweek,
               expand = expansion()) +
  scale_shape_manual(
    breaks = c("påvist", "<LOQ", "ikke påvist"),
    values = c(19, 21, 4)
  ) +
  scale_y_continuous(
    breaks = seq(0, 200, by = 40),
    sec.axis = sec_axis(
      trans = ~ . / 20,
      breaks = c(0, 2, 4),
      labels = function(x) scales::comma(10^x,
                                         big.mark = ".",
                                         decimal.mark = ",",
                                         accuracy = 1),
      name = "RNA-kopier pr. liter spildevand"
    )
  ) +
  guides(colour = "none") +
  theme(panel.grid.major.x = element_line(),
        panel.grid.minor.x = element_line(),
        panel.spacing.x = unit(15, "pt"),
        panel.spacing.y = unit(10, "pt"),
        legend.position = "bottom",
        axis.title.y.left = element_text(colour = "#e5c087"),
        axis.text.y.left = element_text(colour = "#e5c087"),
        axis.title.y.right = element_text(colour = "#ED6553"),
        axis.text.y.right = element_text(colour = "#ED6553")) +
  labs(x = "Uge for spildevandsprøve",
       y = "Daglig incidens pr. 100.000\n(gennemsnit af 7 dage)",
       shape = NULL)





#' Distribution of the trends we see

#+ fig.height = 3
trends %>%
  ggplot() +
  geom_vline(xintercept = 0, colour = "grey60") +
  geom_histogram(aes(estimate), bins = 20, boundary = 0,
                 alpha = 0.8) +
  scale_x_continuous(labels = scales::percent_format(1, scale = 1)) +
  labs(x = "% daily change",
       y = "Number of signals")




#' Classification of trends
trends_classification <- trends %>%
  select(anlaeg_rando,
         estimate,
         p.value) %>%
  mutate(p_cat = case_when(
    p.value < 0.05 ~ "<0.05",
    p.value < 0.10 ~ "<0.10",
    p.value < 0.20 ~ "<0.20",
    p.value >= 0.20 ~ ">=0.20",
  ),
  direction = sign(estimate))

trends_classification %>% count(direction, p_cat)

#' Trend per anlæg
trends_classification %>%
  arrange(desc(direction), p_cat, desc(estimate)) %>%
  mutate(pdc = scales::percent(estimate, 1, scale = 1)) %>%
  select(anlaeg_rando, p_cat, pdc)
