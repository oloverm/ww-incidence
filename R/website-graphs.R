#' ---
#' title: "Website graphs"
#' output:
#'   word_document:
#'     fig_height: 12.0
#'     fig_width: 20.0
#'     highlight: null
#' editor_options:
#'   chunk_output_type: console
#' ---

#+ r setup, include=FALSE
knitr::opts_chunk$set(
  echo = FALSE,
  message = FALSE,
  warning = FALSE,
  dpi = 120
)


here::i_am("R/website-graphs.R")

# This runs a setup script. It should automatically install any extra packages
# you need.
source(here("R/00_setup.R"), encoding = 'UTF-8')

# read_ww(cache_limit = hours(24))
#
#
# # WEBSITE NEW VERSION (2021 - COPIES PR. PERSON, 2022 - FAECES NORMALISATION) ----------------------------------------------------------------------
#
# # Bruger denne for anlæg med manglende populationer
# weekly <- ww_human %>%
#   mutate(week_start = floor_date(date_receipt, "week", 1)+3) %>%
#   select(anlaeg_rando, week_start, rna_normalised_faeces, n_human, population) %>%
#   group_by(anlaeg_rando, population, week_start) %>%
#   summarise(
#     n_samples = sum(!is.na(rna_normalised_faeces)),
#     rna_normalised_faeces_week = mean(rna_normalised_faeces, na.rm = TRUE),
#     n_human_week = sum(n_human, na.rm = TRUE),
#     .groups = "drop") %>%
#   # Increase the incidence by 0.1,
#   mutate(inc_week = (0.1 + n_human_week / population * 100000)) %>%
#   filter(!is.na(rna_normalised_faeces_week) | !is.na(inc_week))
#
# Benytter aggregated for at lave data til hvert anlæg

aggregated_ww_anlaeg <- ww_human %>%
  filter(overlap_level == "1. anlæg" |
           overlap_level == "2. indløb" |
           overlap_level == "3. decentral",
         anlaeg_rando %in% lookup_oplande_names_new$anlaeg_display) %>%
  aggregate_ww(anlaeg_rando) %>%
  filter(date_receipt >= ymd("2022-01-01"),
         !is.na(value),
         measure != "total_testede_pop") %>%
  arrange(date_receipt, measure) %>%
  select(-se, -ci_lo, -ci_hi)




#+  results = "asis"


for (curr_landsdel in geogs$landsdele) {

  cat("#", curr_landsdel, "\n\n")

  names_anlaeg_curr_landsdel <- ww_human %>%
    filter(landsdel == curr_landsdel,
           anlaeg_rando %in% lookup_oplande_names_new$anlaeg_display) %>%
    pull(anlaeg_rando) %>%
    unique()

  for (curr_anlaeg in names_anlaeg_curr_landsdel) {

    # Create data for just this anlæg
    weekly_curr_anlaeg <- aggregated_ww_anlaeg %>%
      filter(anlaeg_rando == curr_anlaeg)



    # If there isn't enough data (new anlæg), don't make the graph
    #if (sum(!is.na(weekly_curr_anlaeg$rna_normalised_faeces)) < 3) next

    curr_pop <- ww_human %>%
      filter(anlaeg_rando == curr_anlaeg) %>%
      head(1) %>%
      pull(population)

    pop_caption <- if (length(curr_pop) == 0 || is.na(curr_pop) ) {
      "Antal indbyggere: ukendt"
    } else {
      paste("Antal indbyggere:", scales::comma(curr_pop,
                                               big.mark = ".",
                                               decimal.mark = ","))
    }

    # Solution for plotting anlægs without population

    if (length(curr_pop) == 0 || is.na(curr_pop) ) {
      weekly_curr_anlaeg <- weekly %>%
        filter(anlaeg_rando == curr_anlaeg)

      weekly_curr_anlaeg %>%
        pivot_longer(c(rna_normalised_faeces_week, inc_week)) %>%
        filter(!is.na(value)) %>%
        ggplot(aes(week_start, value, colour = name)) +
        geom_vline(xintercept = ymd("2022-01-01")) +
        geom_line(size = 1.3) +
        scale_colour_manual(
          breaks = c("inc_week",
                     "rna_normalised_faeces_week"),
          labels = c("Ugentlig incidens pr. 100.000",
                     "RNA-kopier ift. fæces indhold"),
          values = c("#e5c087",
                     "#3498D8")
        )  +
        scale_x_date(date_breaks = "1 month",
                     date_labels = "%B",
                     date_minor_breaks = "15 days",
                     expand = expansion(c(0.01, 0.05))) +
        scale_y_continuous(breaks = c(0,1,10,100,1000,10000,100000),
                           minor_breaks = c(3, 30, 300, 3000, 30000), # for adjusting extra ticks
                           trans = "log10", #remove for normal scale
                           labels = scales::comma_format(accuracy = 1,
                                                         big.mark = ".",
                                                         decimal.mark = ","),
                           limits = c(0.1, NA)
        ) +
        #scale_y_continuous(labels = exp10_comma,
        #expand = expansion(c(0.03))) +
        # expand_limits(y = c(0, 100000) +
        theme(
          panel.grid.minor.y = element_line(color = "grey97",
                                            size = 0.025,
                                            linetype = 1),
          legend.position = "bottom",
          legend.margin = margin(-8, 0, 0, 0),
          plot.margin = margin(5.5, 5.5, 0, 5.5, "pt"),
          axis.text = element_text(size = 20),
          axis.title = element_text(size = 24),
          plot.title = element_text(size = 30),
          legend.text = element_text(size = 20),
          plot.caption = element_text(size = 20)
        ) +
        labs(title = curr_anlaeg,
             x = "Måned",
             y = NULL,
             colour = NULL,
             caption = pop_caption)

      print(last_plot())

      cat("\n\n")

    } else {
      # Graph
      weekly_curr_anlaeg %>%
        filter(!is.na(value)) %>%
        ggplot(aes(date_receipt, value, colour = measure)) +
        geom_vline(xintercept = ymd("2022-01-01")) +
        geom_line(size = 1.3) +
        # geom_line(data = ~ filter(., date_receipt >= ymd("2022-01-01"),
        #                           date_receipt <= ymd("2022-01-06"),
        #                           measure == "copies_per_person" | measure == "rna_mean_faeces"),
        #           aes(x = date_receipt, y = value, colour = measure),
        #           size = 1.2,
        #           colour = "#3498D8"
        # ) +
        # geom_point() +
        scale_colour_manual(
          breaks = c("total_inc_7",
                     #"copies_per_person",
                     "rna_mean_faeces"),
          labels = c("Ugentlig incidens pr. 100.000",
                     #"Data 2021: RNA-kopier pr. indbygger",
                     "RNA-kopier ift. fæces indhold"),
          values = c("#e5c087",
                     #"#ED6553",
                     "#3498D8")
        ) +
        scale_x_date(date_breaks = "1 month",
                     date_labels = "%B",
                     date_minor_breaks = "15 days",
                     expand = expansion(c(0.01, 0.05))) +
        scale_y_continuous(breaks = c(0,1,10,100,1000,10000,100000),
                           minor_breaks = c(3, 30, 300, 3000, 30000), # for adjusting extra ticks
                           trans = "log10", #remove for normal scale
                           labels = scales::comma_format(accuracy = 1,
                                                         big.mark = ".",
                                                         decimal.mark = ","),
                           limits = c(0.1, NA)
        ) +
        #scale_y_continuous(labels = exp10_comma,
        #expand = expansion(c(0.03))) +
        # expand_limits(y = c(0, 100000) +
        theme(
          panel.grid.minor.y = element_line(color = "grey97",
                                            size = 0.025,
                                            linetype = 1),
          legend.position = "bottom",
          legend.margin = margin(-8, 0, 0, 0),
          plot.margin = margin(5.5, 5.5, 0, 5.5, "pt"),
          axis.text = element_text(size = 20),
          axis.title = element_text(size = 24),
          plot.title = element_text(size = 30),
          legend.text = element_text(size = 20),
          plot.caption = element_text(size = 20)
        ) +
        labs(title = curr_anlaeg,
             x = "Måned",
             y = NULL,
             colour = NULL,
             caption = pop_caption)

      print(last_plot())

      cat("\n\n")

    }
  }
}






# WEBSITE OLD VERSION (2021 - NOT COPIES PR. PERSON) ----------------------------------------------------------------------


#Using regular data (convert to log in plot)
# weekly <- ww_human %>%
#   mutate(week_start = floor_date(date_receipt, "week", 1)) %>%
#   select(anlaeg_rando, week_start, rna_corr, n_human, population) %>%
#   group_by(anlaeg_rando, population, week_start) %>%
#   summarise(
#     n_samples = sum(!is.na(rna_corr)),
#     rna_corr_week = mean(rna_corr, na.rm = TRUE),
#     n_human_week = sum(n_human, na.rm = TRUE),
#     .groups = "drop") %>%
#   # Increase the incidence by 0.1,
#   mutate(inc_week = (0.1 + n_human_week / population * 100000)) %>%
#   filter(!is.na(rna_corr_week) | !is.na(inc_week))
#
#
#
# #+  results = "asis"
#
# for (curr_landsdel in geogs$landsdele) {
#
#   cat("#", curr_landsdel, "\n\n")
#
#   names_anlaeg_curr_landsdel <- ww_human %>%
#     filter(landsdel == curr_landsdel) %>%
#     pull(anlaeg_rando) %>%
#     unique()
#
#   for (curr_anlaeg in names_anlaeg_curr_landsdel) {
#
#     # Create data for just this anlæg
#     weekly_curr_anlaeg <- weekly %>%
#       filter(anlaeg_rando == curr_anlaeg)
#
#     # If there isn't enough data (new anlæg), don't make the graph
#     if (sum(!is.na(weekly_curr_anlaeg$rna_corr_week)) < 3) next
#
#     curr_pop <- ww_human %>%
#       filter(anlaeg_rando == curr_anlaeg) %>%
#       head(1) %>%
#       pull(population)
#
#     pop_caption <- if (length(curr_pop) == 0 || is.na(curr_pop) ) {
#       "Antal indbyggere: ukendt"
#     } else {
#       paste("Antal indbyggere:", scales::comma(curr_pop,
#                                                big.mark = ".",
#                                                decimal.mark = ","))
#     }
#
#
#     # Graph
#     weekly_curr_anlaeg %>%
#       pivot_longer(c(rna_corr_week, inc_week)) %>%
#       filter(!is.na(value)) %>%
#       ggplot(aes(week_start, value, colour = name)) +
#       geom_vline(xintercept = ymd("2022-01-01")) +
#       geom_line(size = 1.3) +
#       # geom_point() +
#       scale_colour_manual(
#         breaks = c("inc_week",
#                    "rna_corr_week"),
#         labels = c("Ugentlig incidens pr. 100.000",
#                    "Ugentlig RNA-kopier pr. liter spildevand"),
#         values = c("#e5c087",
#                    "#ED6553")
#       ) +
#       scale_y_continuous(breaks = c(0,1,10,100,1000,10000,100000,1000000),
#                          minor_breaks = c(3, 30, 300, 3000, 30000, 300000), # for adjusting extra ticks
#                          trans = "log10", #remove for normal scale
#                          labels = scales::comma_format(accuracy = 1,
#                                                        big.mark = ".",
#                                                        decimal.mark = ","),
#                          limits = c(0.1, NA)
#       ) +
#       #scale_y_continuous(labels = exp10_comma,
#       #expand = expansion(c(0.03))) +
#       # expand_limits(y = c(0, 100000) +
#       theme(
#         panel.grid.minor.y = element_line(color = "grey97",
#                                           size = 0.025,
#                                           linetype = 1),
#         legend.position = "bottom",
#         legend.margin = margin(-8, 0, 0, 0),
#         plot.margin = margin(5.5, 5.5, 0, 5.5, "pt"),
#         axis.text = element_text(size = 20),
#         axis.title = element_text(size = 24),
#         plot.title = element_text(size = 30),
#         legend.text = element_text(size = 20),
#         plot.caption = element_text(size = 20)
#       ) +
#       labs(title = curr_anlaeg,
#            x = "Dato for spildevandsprøve",
#            y = NULL,
#            colour = NULL,
#            caption = pop_caption)
#
#     print(last_plot())
#
#     cat("\n\n")
#   }
# }


# ### TEST (FAECES NORMALISATION) ###
#
# weekly <- ww_human %>%
#   mutate(week_start = floor_date(date_receipt, "week", 1))%>%
#   select(anlaeg_rando, week_start, flow, rna_corr, rna_normalised_faeces, n_human, population) %>%
#   group_by(anlaeg_rando, population, week_start) %>%
#     mutate()
#   summarise(
#     n_samples = sum(!is.na(rna_corr)),
#     rna_corr_week = mean(rna_corr, na.rm = TRUE),
#     rna_faeces_week = mean(rna_normalised_faeces, na.rm = TRUE),
#     n_human_week = sum(n_human, na.rm = TRUE),
#     .groups = "drop") %>%
#   # Increase the incidence by 0.1,
#   mutate(inc_week = (0.1 + n_human_week / population * 100000)) %>%
#   filter(!is.na(rna_corr_week) | !is.na(inc_week))
#
#
#
#
#
#
# #+  results = "asis"
#
# for (curr_landsdel in geogs$landsdele) {
#
#   cat("#", curr_landsdel, "\n\n")
#
#   names_anlaeg_curr_landsdel <- ww_human %>%
#     filter(landsdel == curr_landsdel) %>%
#     pull(anlaeg_rando) %>%
#     unique()
#
#   for (curr_anlaeg in names_anlaeg_curr_landsdel) {
#
#     # Create data for just this anlæg
#     weekly_curr_anlaeg <- weekly %>%
#       filter(anlaeg_rando == curr_anlaeg)
#
#     # If there isn't enough data (new anlæg), don't make the graph
#     if (sum(!is.na(weekly_curr_anlaeg$rna_corr_week)) < 3) next
#
#     curr_pop <- ww_human %>%
#       filter(anlaeg_rando == curr_anlaeg) %>%
#       head(1) %>%
#       pull(population)
#
#     pop_caption <- if (length(curr_pop) == 0 || is.na(curr_pop) ) {
#       "Antal indbyggere: ukendt"
#     } else {
#       paste("Antal indbyggere:", scales::comma(curr_pop,
#                                                big.mark = ".",
#                                                decimal.mark = ","))
#     }
#
#
#     # Graph
#     weekly_curr_anlaeg %>%
#       pivot_longer(c(rna_corr_week, rna_faeces_week, inc_week)) %>%
#       filter(!is.na(value)) %>%
#       ggplot(aes(week_start, value, colour = name)) +
#       geom_vline(xintercept = ymd("2022-01-01")) +
#       geom_line(size = 1.3) +
#       # geom_point() +
#       scale_colour_manual(
#         breaks = c("inc_week",
#                    "rna_corr_week",
#                    "rna_faeces_week"),
#         labels = c("Ugentlig incidens pr. 100.000",
#                    "Ugentlig RNA-kopier pr. liter spildevand",
#                    "Ugentlig RNA-kopier pr. liter spildevand (faeces)"),
#         values = c("#e5c087",
#                    "#ED6553",
#                    "#00BFC4")
#       ) +
#       scale_x_date(date_breaks = "1 month", date_labels = "%b",
#                    expand = expansion(c(0.01, 0.05))) +
#       scale_y_continuous(breaks = c(0,1,10,100,1000,10000,100000,1000000),
#                          minor_breaks = c(3, 30, 300, 3000, 30000, 300000), # for adjusting extra ticks
#                          trans = "log10", #remove for normal scale
#                          labels = scales::comma_format(accuracy = 1,
#                                                        big.mark = ".",
#                                                        decimal.mark = ","),
#                          limits = c(0.1, NA)
#       ) +
#       #scale_y_continuous(labels = exp10_comma,
#       #expand = expansion(c(0.03))) +
#       # expand_limits(y = c(0, 100000) +
#       theme(
#         panel.grid.minor.y = element_line(color = "grey97",
#                                           size = 0.025,
#                                           linetype = 1),
#         legend.position = "bottom",
#         legend.margin = margin(-8, 0, 0, 0),
#         plot.margin = margin(5.5, 5.5, 0, 5.5, "pt"),
#         axis.text = element_text(size = 20),
#         axis.title = element_text(size = 24),
#         plot.title = element_text(size = 30),
#         legend.text = element_text(size = 20),
#         plot.caption = element_text(size = 20)
#       ) +
#       labs(title = curr_anlaeg,
#            x = "Dato for spildevandsprøve",
#            y = NULL,
#            colour = NULL,
#            caption = pop_caption)
#
#     print(last_plot())
#
#     cat("\n\n")
#   }
# }


# 
# weekly <- ww_human %>%
#   filter(!is.na(rna_normalised_faeces),
#          # overlap_level == "1. anlæg"
#          ) %>%
#   mutate(log_population = log10(population),
#          week_start = floor_date(date_receipt, "week", 1) + days(3),
#          ratio_rdrp = log_rna_rdrp_2022 - log_rna_crass,
#          ratio_n2 = log_rna_n2 - log_rna_pmmov,
#          total_mean = means(ratio_rdrp, ratio_n2, na.rm = TRUE))  %>%
#   group_by(anlaeg_rando, population, week_start) %>%
#   summarise(
#     n_samples = sum(!is.na(total_mean)),
#     rna_norm_week = mean(rna_normalised_faeces, na.rm = TRUE),
#     total_mean_week = mean(total_mean, na.rm = TRUE),
#     n_human_week = sum(n_human, na.rm = TRUE),
#     .groups = "drop") %>%
#   # Increase the incidence by 0.1,
#   mutate(inc_week = (0.1 + n_human_week / population * 100000),
#          norm_week = 10^total_mean_week*10e6)
# 
# 
# 
# 
# #+  results = "asis"
# 
# for (curr_landsdel in geogs$landsdele) {
# 
#   cat("#", curr_landsdel, "\n\n")
# 
#   names_anlaeg_curr_landsdel <- ww_human %>%
#     semi_join(weekly, by = "anlaeg_rando") %>%
#     filter(landsdel == curr_landsdel) %>%
#     pull(anlaeg_rando) %>%
#     unique()
# 
#   for (curr_anlaeg in names_anlaeg_curr_landsdel) {
# 
#     # Create data for just this anlæg
#     weekly_curr_anlaeg <- weekly %>%
#       filter(anlaeg_rando == curr_anlaeg)
# 
#     # If there isn't enough data (new anlæg), don't make the graph
#     #if (sum(!is.na(weekly_curr_anlaeg$rna_corr_week)) < 3) next
# 
#     curr_pop <- ww_human %>%
#       filter(anlaeg_rando == curr_anlaeg) %>%
#       head(1) %>%
#       pull(population)
# 
#     pop_caption <- if (length(curr_pop) == 0 || is.na(curr_pop) ) {
#       "Antal indbyggere: ukendt"
#     } else {
#       paste("Antal indbyggere:", scales::comma(curr_pop,
#                                                big.mark = ".",
#                                                decimal.mark = ","))
#     }
# 
# 
#     # Graph
#     weekly_curr_anlaeg %>%
#       pivot_longer(c(#norm_week,
#                      rna_norm_week,
#                      inc_week)) %>%
#       filter(!is.na(value)) %>%
#       ggplot(aes(week_start, value, colour = name)) +
#       geom_vline(xintercept = ymd("2022-01-01")) +
#       geom_line(size = 1.5) +
#       # geom_point() +
#       scale_colour_manual(
#         breaks = c(#"norm_week",
#                    "rna_norm_week",
#                    "inc_week"),
#         labels = c(#"Ugentlig RNA-kopier pr. liter spildevand (ny metode - log data)",
#                    # "Ugentlig RNA-kopier pr. liter spildevand (oprindelig metode)",
#                    "Ugentlig RNA-kopier pr. liter spildevand",
#                    "Ugentlig incidens pr. 100.000"),
#         values = c(#"#D55E00",
#                    "#56B4E9",
#                    "#e5c087")
#       ) +
#       scale_y_continuous(breaks = c(0,1,10,100,1000,10000,100000,1000000),
#                          minor_breaks = c(3, 30, 300, 3000, 30000, 300000), # for adjusting extra ticks
#                          trans = "log10", #remove for normal scale
#                          labels = scales::comma_format(accuracy = 1,
#                                                        big.mark = ".",
#                                                        decimal.mark = ","),
#                          limits = c(0.1, NA)
#       ) +
#       #scale_y_continuous(labels = exp10_comma,
#       #expand = expansion(c(0.03))) +
#       # expand_limits(y = c(0, 100000) +
#       theme(
#         panel.grid.minor.y = element_line(color = "grey97",
#                                           size = 0.025,
#                                           linetype = 1),
#         legend.position = "bottom",
#         legend.margin = margin(-8, 0, 0, 0),
#         plot.margin = margin(5.5, 5.5, 0, 5.5, "pt"),
#         axis.text = element_text(size = 20),
#         axis.title = element_text(size = 24),
#         plot.title = element_text(size = 30),
#         legend.text = element_text(size = 20),
#         plot.caption = element_text(size = 20)
#       ) +
#       labs(title = str_remove(curr_anlaeg, " \\(R\\)"),
#            x = "Dato for spildevandsprøve",
#            y = NULL,
#            colour = NULL,
#            caption = pop_caption)
# 
#     print(last_plot())
# 
#     cat("\n\n")
#   }
# }
# 
