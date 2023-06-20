#' ---
#' title: WW report
#' output:
#'   html_document:
#'     toc: yes
#'     toc_float:
#'       collapsed: false
#'       smooth_scroll: false
#'     number_sections: yes
#'     fig_caption: no
#'     fig_width: 10
#'     fig_height: 5.5
#'     mathjax: null
#'     theme: cerulean
#'     df_print: kable
#'     dev: png
#' ---

#+ setup, echo = FALSE
knitr::opts_chunk$set(
  echo = FALSE
)



#' # Aggregated figures {.tabset}
#'
#' These are saved in `outputs/aggregated/`.
#'
#' ## Danmark

#+ denmark_graph
fp_latest(here("outputs", "aggregated"), "_danmark\\.png") %>%
  knitr::include_graphics()


#' ## Regioner
#'

#+ regional_comparison
fp_latest(here("outputs", "aggregated"), "_regioner\\.png") %>%
  knitr::include_graphics()

#+ individual_regions
geogs$regioner %>%
  make_clean_names() %>%
  map_chr(~ fp_latest(here("outputs", "aggregated"),
                      glue("region-{.}\\.png"))) %>%
  knitr::include_graphics()


#' ## Landsdele
geogs$landsdele %>%
  make_clean_names() %>%
  map_chr(~ fp_latest(here("outputs", "aggregated"),
                      glue("landsdel-{.}\\.png"))) %>%
  knitr::include_graphics()


#' # Signaler {.tabset}
#'
#' These are saved in `outputs/signaler/`.
#'
#' ## N over time
#'
fp_latest(here("outputs", "signaler"), "antal-signaler\\.png") %>%
  knitr::include_graphics()

#' Seneste uge: antal per kategori
grading %>%
  filter(week == ISOweek(today() - days(7)),
         grading >= 1) %>%
  count(grading) %>%
  adorn_totals()


#' ## Graphs
#'

no_signals <- grading %>%
  filter(week == ISOweek(today() - days(7)),
         grading >= 1) %>%
  count(grading) %>%
  adorn_totals()

no_signals <- sum(no_signals$n)

if (no_signals != 0) {
  fp_latest(here("outputs", "signaler"), "_signaler\\.png") %>%
    knitr::include_graphics()
  
} else if (no_signals == 0) {
  cli_alert_success("There are no signals for this week - thus no plot")
}



#' ## Map
#'

#+ out.width = "100%"
if (no_signals != 0) {
fp_latest(here("outputs", "signaler"), "_kort-signaler\\.html") %>%
  knitr::include_url(height = "600px")

} else if (no_signals == 0) {
  cli_alert_success("There are no signals for this week - thus no map")
}




#' # Data quality
#'
#' ## Coverage
#'
n_dk_covered <- ww_human %>%
  filter(overlap_level == "1. anlæg",
         !is.na(population),
         date_receipt >= ymd("2022-07-11")) %>%
  distinct(anlaeg_rando, population) %>%
  summarise(total_pop = sum(population)) %>%
  pull(total_pop)

p_dk_covered <- n_dk_covered %>%
  `/`(5831000) %>%
  scales::percent()

#' The oplande/anlæg included at the moment cover `r as.character(n_dk_covered)` people:
#' `r p_dk_covered` of Denmark's population.
#'
#'
#'
#' ### Samples received
#'
#' If the recent bars are unexpectedly low, there's something wrong. The newest
#' one might be a bit lower, if the lab has to reanalyse some of them or
#' something.

#+ fig.width = 8.2, fig.height = 2.5
ww_human %>%
  select(anlaeg_rando, date_receipt, rna_mean) %>%
  filter(!is.na(rna_mean),
         date_receipt >= today() - days(60)) %>%
  count(date_receipt) %>%
  ggplot(aes(date_receipt, n)) +
  geom_col(width = 1, colour = "white", fill = "black") +
  scale_x_date(date_breaks = "1 weeks",
               labels = week) +
  # theme(panel.grid.minor.x = element_line()) +
  labs(x = "Modtagelsesdato (uge)",
       y = "Antal afsluttede rapporter")


#' ## Missing data
#'
#' A file with the lists of places with missing shapes or samples is saved in
#' `outputs/diagnostic/`. It's called `xxx_missing-data.xlsx`.
#'
#' ### Missing shapes
#'
#'As by july 2022 we have shapes for every anlæg included.
# This is the number of places where we don't have shapes, split by whether
# they're renseanlæg or part of the decentral testing. We haven't got a full
# list of the decentral ones yet, so this is only counting the ones where we
# _do_ have samples already.
# missing_shapes %>%
#   count(overlap_level) %>%
#   adorn_totals() %>%
#   as_tibble()


#' ### Places with samples that are "Igang" -> reanalyse
#' Taken from the all_ww files, which contains all wastewater data before any filtering

no_samples_reanalyse %>%
  as_tibble()

#' Number of places that have had samples before, but who don't have any in the
#' most recent week.
no_recent_samples %>%
  count(overlap_level) %>%
  adorn_totals() %>%
  as_tibble()


#' Number of places that had only 0 or 1 samples in the latest week. ("ingen prøve" counts as a sample)
insufficient_recent_samples %>%
  count(overlap_level) %>%
  adorn_totals() %>%
  as_tibble()



#' ### Expected samples (new version)
#' 
#' The number of samples expected each week compared to what we actually got. 
#' NOTE: this is **AFTER** quality checks etc, so it is how much data we have left that seems 
#' useful compared to what we expect. So this is rather explaining the amount of not useful data compared to the one above which is ALL data.
#' This is without data from the airports.
#' 
#' In week 15, 16, 19, 21 and 23, we only expect 2x samples per week
#' 
#' From july we expect only 2 samples a week from a total of 
options(dplyr.summarise.inform = FALSE)

ww_human %>%
  filter(week >= ISOweek(today() - days(7))) %>%
  group_by(week, overlap_level) %>%
  summarise(n_samples = sum(!is.na(log_rna_mean_faeces))) %>%
  mutate(n_expected = case_when(overlap_level == "1. anlæg" ~ 2 * 87,
                              overlap_level == "2. indløb" ~ 2*2),
         n_missing = n_expected - n_samples) %>%
  ungroup() %>%
  select(-week)%>%
  adorn_totals() %>%
  mutate(percent_missing = round((n_expected - n_samples) / n_expected *100,1)) %>%
  as_tibble()


#' ### Samples from 5 largest anlæg in each region
#' 
#' To easily identify if we are missing samples from any of the largest areas 

ww_human %>%
  filter(week >= ISOweek(today() - days(7)),
         overlap_level == "1. anlæg") %>%
  group_by(region)%>%
  arrange(region, desc(population)) %>%
  select(anlaeg_rando, week, region, population, log_rna_mean_faeces) %>%
  ungroup() %>%
  group_by(week, region, anlaeg_rando) %>%
  summarise(n_samples = sum(!is.na(log_rna_mean_faeces)),
            population = population) %>%
  distinct(anlaeg_rando, .keep_all = TRUE) %>%
  arrange(region, desc(population)) %>%
  ungroup() %>%
  group_by(region)%>%
  slice_head(n = 5)%>%
  as_tibble()




#' <style type="text/css">
#' ::selection {
#'   background: #c9deff;
#' }
#'
#' pre {
#'   display: inline-block
#' }
#'
#' iframe {
#'   border-style: none;
#' }
#'
#' .table {
#'   width: auto;
#'   min-width: 20%
#' }
#'
#' </style>
