#' ---
#' title: History
#' output:
#'   html_document:
#'     toc: no
#'     toc_float:
#'       collapsed: false
#'       smooth_scroll: false
#'     number_sections: yes
#'     fig_caption: no
#'     fig_width: 8
#'     fig_height: 4
#'     theme: cerulean
#'     highlight: null
#'     mathjax: null
#'     dev: png
#' ---
#'
#'

#+ setup, include = FALSE
knitr::opts_chunk$set(
  echo = FALSE,
  message = FALSE,
  warning = FALSE
)

here::i_am("R/historical-signals.R")

if (!exists("isrun_setup")) source(here("R/00_setup.R"))


#+ read data
#read_ww(cache_limit = hours(1))


grading <- fp_latest(here("data/signals"), "_signals\\.xlsx$") %>%
  read_xlsx()


#+ clean data
windows <- grading %>%
  select(anlaeg_rando, week, grading) %>%
  mutate(min_date = week %>%
           paste0("-1") %>%
           ISOweek2date(),
         max_date = min_date + days(7),
         ymin = -Inf, ymax = Inf) %>%
  filter(grading >= 1)

# Historical signals (NEW VERSION) -------------------------------------------------------


# The plots needs to be adjusted so it fits the signal figures and the faeces normalisation

# From the 4/4-2022 it should be log_rna_faeces plotted and not log_rna_corr (just aligned with the signals produced
# and save in the grading file

#+ results = "asis", fig.width = 9, fig.height = 2.9, dpi = 80
for (curr_anlaeg in unique(ww_human$anlaeg_rando)) { #%>% discard(~ is.na(.))) {
  
  curr_windows <- windows %>%
    filter(anlaeg_rando == curr_anlaeg)
  
  cat("\n\n", curr_anlaeg, "\n\n")
  
  ww_human %>%
    filter(date_receipt >= ymd("2021-10-01"),
           anlaeg_rando == curr_anlaeg,
           !is.na(rna_corr_log) | !is.na(inc7)) %>%
    select(anlaeg_rando,
           date_receipt,
           rna_corr_log,
           rna_normalised_faeces,
           rna7_log,
           påvist_mean,
           inc7) %>%
    mutate(rna_norm_faeces_log = log10(rna_normalised_faeces),
           rna_log_interp_faeces = na.approx(rna_norm_faeces_log, maxgap = 5, na.rm = FALSE),
           rna7_faeces_log = rollmean(rna_log_interp_faeces, 7, fill = NA, align = "right")) %>%
    select(-rna_log_interp_faeces, -rna_normalised_faeces) %>%
    pivot_longer(cols = starts_with("rna"), names_to = "measure", values_to = "value") %>%
    ggplot(aes(date_receipt, value)) +
    geom_vline(xintercept = ymd("2022-01-03")) +
    geom_vline(xintercept = ymd("2022-04-04"),
               linetype = "dashed") +
    geom_area(data = ~ filter(., !is.na(inc7)),
              aes(y = log10(inc7) %>% pmax(0)), alpha = 0.2,
              position = position_identity()) +
    geom_rect(data = curr_windows,
              aes(ymin = ymin, ymax = ymax,
                  xmin = min_date, xmax = max_date,
                  fill = factor(grading)),
              inherit.aes = FALSE,
              alpha = 0.25) +
    geom_line(data = ~ filter(., !is.na(value),
                              date_receipt <= ymd("2022-04-04"),
              measure == "rna_corr_log"),
              aes(group = 1), na.rm = TRUE) +
    geom_line(data = ~ filter(., !is.na(value),
                              date_receipt <= ymd("2022-04-02"),
              measure == "rna7_log"),
              aes(y = value, group = anlaeg_rando), na.rm = TRUE,
              size = 2, alpha = 0.3) +
    geom_line(data = ~ filter(., !is.na(value),
                              date_receipt >= ymd("2022-04-04"),
                              measure == "rna_norm_faeces_log"),
              aes(group = 1), na.rm = TRUE) +
    geom_line(data = ~ filter(., !is.na(value),
                              date_receipt >= ymd("2022-04-04"),
              measure == "rna7_faeces_log"),
              aes(y = value, group = anlaeg_rando), na.rm = TRUE,
              size = 2, alpha = 0.3)+
    geom_point(data = ~ filter(., !is.na(value),
                               date_receipt <= ymd("2022-04-04"),
                               measure == "rna_corr_log"),
               aes(shape = påvist_mean),
               size = 2, fill = "white", na.rm = TRUE) +
    geom_point(data = ~ filter(., !is.na(value),
                               date_receipt >= ymd("2022-04-04"),
                               measure == "rna_norm_faeces_log"),
               aes(shape = påvist_mean),
               size = 2, fill = "white", na.rm = TRUE) +
    annotate("text", x = ymd("2022-04-13"), y = 5.5, label = "Fæces \nnormalisering", size = 2.5) +
    scale_x_date(date_breaks = "1 week",
                 labels = isoweek,
                 expand = expansion(c(0.01, 0.04))) +
    scale_y_continuous(
      labels = ~ scales::comma_format(1, big.mark = ".", decimal.mark = ",")(10^.)
    ) +
    scale_shape_manual(
      breaks = c("påvist", "<LOQ", "ikke påvist"),
      values = c(19, 21, 4)
    ) +
    scale_fill_manual(breaks = grading_colours$breaks,
                      values = grading_colours$values) +
    theme(
      axis.text.x = element_text(hjust = 0)
      # panel.grid.minor.x = element_line()
    ) +
    labs(
      title = curr_anlaeg,
      x = "Uge",
      y = NULL,
      shape = "Resultat",
      fill = "Kategori"
    )
  
  
  print(last_plot())
  
}


# Historical signals (OLD VERSION) -------------------------------------------------------

#+ results = "asis", fig.width = 9, fig.height = 2.9, dpi = 80
# for (curr_anlaeg in unique(ww_human$anlaeg_rando) %>% discard(~ is.na(.))) {
# 
#   curr_windows <- windows %>%
#     filter(anlaeg_rando == curr_anlaeg)
# 
#   cat("\n\n", curr_anlaeg, "\n\n")
# 
#   ww_human %>%
#     filter(anlaeg_rando == curr_anlaeg,
#            !is.na(rna_corr_log) | !is.na(inc7)) %>%
#     select(anlaeg_rando,
#            date_receipt,
#            rna_corr_log,
#            rna7_log,
#            påvist_mean,
#            inc7) %>%
#     ggplot(aes(date_receipt, rna_corr_log)) +
#     geom_vline(xintercept = ymd("2022-01-03")) +
#     geom_area(data = ~ filter(., !is.na(inc7)),
#               aes(y = log10(inc7) %>% pmax(0)), alpha = 0.2,
#               position = position_identity()) +
#     geom_rect(data = curr_windows,
#               aes(ymin = ymin, ymax = ymax,
#                   xmin = min_date, xmax = max_date,
#                   fill = factor(grading)),
#               inherit.aes = FALSE,
#               alpha = 0.25) +
#     geom_line(data = ~ filter(., !is.na(rna_corr_log)),
#               aes(group = 1), na.rm = TRUE) +
#     geom_line(data = ~ filter(., !is.na(rna7_log)),
#               aes(y = rna7_log, group = anlaeg_rando), na.rm = TRUE,
#               size = 2, alpha = 0.3) +
#     geom_point(aes(shape = påvist_mean),
#                size = 2, fill = "white", na.rm = TRUE) +
#     scale_x_date(date_breaks = "1 week",
#                  labels = isoweek,
#                  expand = expansion(c(0.01, 0.04))) +
#     scale_y_continuous(
#       labels = ~ scales::comma_format(1, big.mark = ".", decimal.mark = ",")(10^.)
#     ) +
#     scale_shape_manual(
#       breaks = c("påvist", "<LOQ", "ikke påvist"),
#       values = c(19, 21, 4)
#     ) +
#     scale_fill_manual(breaks = grading_colours$breaks,
#                       values = grading_colours$values) +
#     theme(
#       axis.text.x = element_text(hjust = 0)
#       # panel.grid.minor.x = element_line()
#     ) +
#     labs(
#       title = curr_anlaeg,
#       x = "Uge",
#       y = NULL,
#       shape = "Resultat",
#       fill = "Kategori"
#     )
# 
# 
#   print(last_plot())
# 
# }
