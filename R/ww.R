#' ---
#' title: WW exploration
#' output:
#'   html_document:
#'     code_folding: hide
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
#'
#' # Setup
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







# Exploring data ----------------------------------------------------------


#'
#' # Exploring data
#'
#' ## Anlæg enrolled
#'
#+ results = "asis"
glue(
  "The oplande where we have both WW data and shapes ",
  "have a population of **{total_oplande_pop_txt}**. ",
  "That's **{p_denmark_pop}%** of Denmark's 5.8 million inhabitants. ",
  "(It's actually a bit less because we're double-counting where ",
  "shapefiles overlap.)",
  total_oplande_pop = ww_human %>%
    filter(!is.na(rna_mean),
           !anlaeg_rando %in% c("Avedøre (Ejby)",
                                "Avedøre (Vallensbæk)",
                                "Lynetten (nordre tilløb)",
                                "Lynetten (søndre tilløb)")) %>%
    distinct(anlaeg_rando) %>%
    inner_join(lookup_oplande_names,
               by = c("anlaeg_rando" = "anlaeg_display")) %>%
    distinct(anlaeg_shp) %>%
    inner_join(pops, by = c("anlaeg_shp" = "anlaeg")) %>%
    summarise(total_pop = sum(population)) %>%
    pull(),
  total_oplande_pop_txt = scales::comma(total_oplande_pop),
  p_denmark_pop = round(total_oplande_pop / 5.8e6 * 100)) %>%
  cat()
#'
#'
#'
#'
ww_human %>%
  filter(!is.na(rna_mean)) %>%
  select(anlaeg_rando, date_receipt, region) %>%
  mutate(week = floor_date(date_receipt, "week", week_start = 1)) %>%
  group_by(region, week) %>%
  summarise(n_plants = n_distinct(anlaeg_rando)) %>%
  ungroup() %>%
  ggplot(aes(week, n_plants, fill = region)) +
  geom_area() +
  scale_x_date(date_breaks = "2 weeks",
               labels = isoweek) +
  scale_y_continuous(position = "right") +
  scale_fill_brewer(palette = "Accent") +
  theme(legend.position = c(0.15, 0.7),
        legend.background = element_rect(colour = "transparent")) +
  labs(
    title = "Number of anlæg reporting per week",
    x = "Week number",
    y = "Number of anlæg reporting",
    fill = NULL
  )
#'
#'
#' ## N vs RdRp
#'
#' This is to show that it's a good idea to use the mean of the N and RdRp
#' genes. We used to only use N, but I've now changed it to use the mean
#' everywhere.
ww %>%
  filter(phase_anlaeg == "pilot") %>%
  select(anlaeg_list, anlaeg_plot,
         date_receipt,
         rna_rdrp, rna_n, rna_mean) %>%
  pivot_longer(c(rna_rdrp, rna_n, rna_mean),
               names_to = "gene",
               names_pattern = "rna_(.+)$",
               values_to = "concentration_l") %>%
  filter(!is.na(concentration_l)) %>%
  mutate(gene = factor(gene, levels = c("n", "rdrp", "mean"))) %>%
  ggplot(aes(date_receipt, concentration_l, colour = gene)) +
  facet_wrap(~ anlaeg_plot, scales = "free_y") +
  geom_line(size = 0.8) +
  # scale_y_log10() +
  scale_colour_manual(values = c("darkorange", "deepskyblue", "black")) +
  labs(title = "Both gene results over time",
       fill = "LOQ",
       colour = "Gen",
       shape = "Resultat") +
  theme(panel.grid.major.x = element_line())

#' Direct comparison of gene results. Seems like it's pretty close to a 1:1
#' relationship on average, but with quite a lot of variance. Does that suggest
#' it'd be a good idea to take the average of the two?
#'
#+ fig.width = 6.5, fig.height = 6.5
ww_human %>%
  filter(
    phase_anlaeg == "pilot",
    # Filter out NA, <LOD, and <LOQ
    across(c(påvist_n, påvist_rdrp),
           ~ !(. %in% c("ikke påvist", "<LOQ") | is.na(.)))
  ) %>%
  ggplot(aes(rna_n, rna_rdrp, colour = anlaeg_list, shape = anlaeg_list)) +
  facet_wrap(~ anlaeg_plot) +
  geom_abline(colour = "grey60") +
  geom_point(size = 4, alpha = 0.4) +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE,
              size = 1.2, colour = "grey20") +
  scale_x_log10() +
  scale_y_log10() +
  coord_fixed() +
  scale_shape_manual(values = rep(15:18, 10)) +
  guides(colour = "none", shape = "none") +
  theme(panel.grid.major.x = element_line()) +
  labs(x = "N gene",
       y = "RdRp gene")

# ggsave(glue("outputs/{today()}_compare-genes-n-rdrp.png"),
#        width = 7, height = 4)

#+ fig.height = 3, fig.width = 5
ww %>%
  mutate(gene_diff = rna_rdrp - rna_n) %>%
  filter(across(c(påvist_n, påvist_rdrp),
                ~ !(. %in% c("ikke påvist", "<LOQ") | is.na(.)))) %>%
  ggplot(aes(gene_diff)) +
  geom_density(fill = "seagreen4", colour = "transparent") +
  geom_vline(xintercept = 0, size = 1) +
  scale_x_continuous(expand = expansion()) +
  scale_y_continuous(expand = expansion(c(0.02, 0.04))) +
  theme(axis.text.y = element_blank()) +
  labs(title = "Difference between RdRp and N gene measurements",
       subtitle = "<< more N --- more RdRp >>",
       x = "Difference in gene measurements",
       y = NULL,
       caption = "Excluding 0 and <LOQ measurements")



#'
#' ## Flow
#'
#' This is the actual flow reported to us for each day
ww_human %>%
  filter(phase_anlaeg == "pilot") %>%
  ggplot(aes(date_receipt, flow, colour = company)) +
  facet_wrap(~ anlaeg_plot, scales = "free_y") +
  geom_hline(data = ~ distinct(., anlaeg_plot, dry_flow),
             aes(yintercept = dry_flow)) +
  geom_line(data = ~ filter(., !is.na(flow)),
            size = 1, show.legend = FALSE) +
  scale_y_continuous(labels = scales::label_comma()) +
  expand_limits(y = 0) +
  theme(panel.grid.major.x = element_line()) +
  labs(title = "Flow over time",
       subtitle = "Black lines are the dry flow from stamdata",
       x = NULL,
       y = "Flow (litres)")



#'
#' ## Dilution
#'
#' Corrected vs raw RNA results
#+ fig.height = 3, fig.width = 5
plot_dilution <- ww_human %>%
  filter(phase_anlaeg == "pilot") %>%
  ggplot(aes(rna_mean, rna_corr, colour = dilution)) +
  geom_abline(slope = 1, colour = "grey60") +
  geom_point(size = 5, alpha = 0.4) +
  scale_colour_viridis_c(option = "plasma",
                         direction = -1,
                         na.value = "grey20") +
  scale_x_log10(oob = scales::squish, labels = scales::label_comma(),
                minor_breaks = c(50, 500, 5000, 50000)) +
  scale_y_log10(oob = scales::squish, labels = scales::label_comma(),
                minor_breaks = c(50, 500, 5000, 50000)) +
  theme(
    panel.grid.major.x = element_line(),
    panel.grid.minor.x = element_line(colour = "grey96"),
    panel.grid.minor.y = element_line(colour = "grey96")
  ) +
  labs(title = "Correction of RNA copies for flow",
       x = "Raw RNA copies",
       y = "Corrected RNA copies"); plot_dilution


#' Corrected vs raw by anlæg
plot_dilution_facet <- plot_dilution +
  facet_wrap(~ anlaeg_plot) +
  theme(panel.grid.major.x = element_line(),
        panel.background = element_rect(fill = "grey98",
                                        colour = "transparent"),
        strip.background = element_rect(fill = "grey98",
                                        colour = "transparent")) +
  labs(
    title = "Correction of RNA copies for flow, by anlæg",
    subtitle = "High dilutions mainly in BIOFOS areas"
  ); plot_dilution_facet


#' Different kind of visualisation, don't know if this is more or less helpful
plot_dilution_v_concentration_facet <- ww_human %>%
  filter(påvist_mean == "påvist",
         !is.na(dilution),
         phase_anlaeg == "pilot") %>%
  ggplot(aes(rna_mean, dilution, colour = rna_corr)) +
  facet_wrap(~ anlaeg_plot) +
  geom_hline(yintercept = 1, colour = "grey70") +
  geom_point(size = 4, alpha = 0.5) +
  scale_colour_viridis_c(option = "turbo",
                         direction = 1,
                         na.value = "grey70",
                         trans = "log10",
                         labels = scales::label_comma()) +
  scale_x_log10(labels = scales::label_comma()) +
  theme(panel.grid.major.x = element_line(),
        panel.background = element_rect(fill = "grey98",
                                        colour = "transparent"),
        strip.background = element_rect(fill = "grey98",
                                        colour = "transparent")) +
  labs(
    title = "Raw copies vs dilution",
    x = "Uncorrected copies/L (log scale)",
    y = "Dilution factor",
    colour = "Corrected\ncopies/l\n(log scale)",
  ); plot_dilution_v_concentration_facet




# Dilution by day of the week. It might be interesting if there's a clear
# pattern. It does seem like some areas have higher variation in the weekends,
# like most of the BIOFOS areas.
ww_human %>%
  filter(phase_anlaeg == "pilot",
         # Lynetten had one very weird value, so filtering that out like this
         dilution_raw > 0.001) %>%
  mutate(weekday = wday(date_receipt, label = TRUE, week_start = 1)) %>%
  ggplot(aes(weekday, dilution_raw, fill = weekday)) +
  facet_wrap(~ anlaeg_plot) +
  geom_hline(yintercept = 1, colour = "grey70") +
  geom_boxplot(show.legend = FALSE) +
  scale_y_log10() +
  scale_fill_hue(l = 80) +
  theme(panel.grid.major.x = element_line(),
        panel.background = element_rect(fill = "grey98",
                                        colour = "transparent"),
        strip.background = element_rect(fill = "grey98",
                                        colour = "transparent")) +
  labs(
    title = "Dilution by day of the week",
    x = "Day of week",
    y = "Dilution factor",
    caption = "When we actually use the dilution factor, we floor it at 1"
  )
#' By the way, above is the raw dilution factor: just `measured flow / dry
#' expected flow`. But for the flow correction, we don't use dilution factors
#' under 1. The idea is that we're correcting for added water from rain, but not
#' for days when there's just less water flowing. I'm not sure this is actually
#' the right idea.
#'
#'




# Distribution of results -------------------------------------------------

#' ## Distribution of results
#'
#' It seems like our WW results are more spread out than in other countries. At
#' least compared to Switzerland: https://sensors-eawag.ch/sars/zurich.html.


#+ fig.height = 7
ww_human %>%
  filter(phase_anlaeg == "pilot",
         !is.na(rna_corr),
         påvist_mean == "påvist") %>%
  ggplot(aes(rna_corr, anlaeg_list)) +
  geom_boxplot(fill = "grey90", outlier.colour = "transparent") +
  geom_jitter(aes(colour = anlaeg_list), size = 2, alpha = 0.5, height = 0.2,
              width = 0.1, show.legend = FALSE) +
  scale_x_log10(breaks = c(100, 200, 400, 800, 1000, 2000, 4000, 8000,
                           10000, 20000, 40000, 80000)) +
  theme(panel.grid.major.x = element_line(),
        panel.grid.major.y = element_blank()) +
  labs(
    title = "Distribution of WW results per anlæg",
    x = "RNA copies per litre",
    y = NULL
  )



# Results -----------------------------------------------------------------


#'
#' # Results
#'
#' ## Whole country
#'

## Whole country -----------------------------------------------------------


#'
#' This only works for the areas where we have populations.
#'
#' I've multiplied the copies/L by the flow for all the measurements, to get the
#' total number of copies that went into each anlæg per day. Then, for each day,
#' I added together all the copies that flowed through, and all the opland
#' populations that had a measurement for that day (for multiple samples in one
#' day, the population could be duplicated, I think that's ok enough). Then I
#' calculated total copies / total population for each day. The smoothed line is
#' weighted by the population covered by each day's samples.
#'
#' I like this graph because you can say "on day X, there were on average 1000
#' virus particles shedded per inhabitant of Denmark", which is probably as
#' intuitive as we're going to get. It's also nice that there's not loads of
#' variation in the latter part of the graph, in the expansion phase.
#'
#' The human (yellow) part is weekly incidence per 100k. It's across all the
#' oplande where we have population data, so it doesn't match up with the first
#' part of the WW graph (only the pilot oplande).
#'
#' The light orange line is people in hospital; the dark orange is people in
#' ICU.
dk_inc <- ww_human %>%
  filter(!is.na(population),
         # Remove sub-oplande, because otherwise we'll be counting them twice
         !anlaeg_rando %in% c("Avedøre (Ejby)",
                              "Avedøre (Vallensbæk)",
                              "Lynetten (nordre tilløb)",
                              "Lynetten (søndre tilløb)")) %>%
  distinct(date_receipt, population, n_human) %>%
  group_by(date_receipt) %>%
  summarise(total_human = sum(n_human, na.rm = TRUE),
            total_pop = sum(population, na.rm = TRUE)) %>%
  mutate(total_human_7 = rollmean(total_human, 7, fill = NA),
         total_inc_7 = total_human_7 / total_pop * 100000) %>%
  filter(date_receipt >= dmy("1 jul 2021"))










ww_human %>%
  filter(
    date_receipt >= dmy("1 jul 2021"),
    !is.na(population),
    !is.na(flow),
    !is.na(rna_mean),
    !anlaeg_rando %in% c("Avedøre (Ejby)",
                         "Avedøre (Vallensbæk)",
                         "Lynetten (nordre tilløb)",
                         "Lynetten (søndre tilløb)"),
    # population >= 20000,
    # wday(date_receipt) %in% c(3),
    # wday(date_receipt) %in% c(3, 4, 6)
  ) %>%
  mutate(
    total_rna = rna_mean * flow,
  ) %>%
  select(anlaeg_rando, date_receipt, total_rna, population) %>%
  filter(!is.na(total_rna)) %>%
  group_by(date_receipt) %>%
  summarise(total_rna = sum(total_rna),
            total_pop = sum(population)) %>%
  mutate(copies_per_person = total_rna / total_pop) %>%
  ggplot(aes(date_receipt, copies_per_person)) +
  geom_line(data = dk_inc, aes(date_receipt, total_inc_7 * 7),
            colour = "gold2", size = 1) +
  geom_smooth(aes(weight = total_pop),
              method = "loess",
              formula = y ~ x,
              span = 0.5,
              colour = "black", fill = "grey70", alpha = 0.3) +
  geom_point(aes(size = total_pop, colour = total_pop)) +
  geom_line(data = owid_hosp,
            aes(date, hosp_patients),
            colour = "darkorange",
            size = 1) +
  geom_line(data = owid_hosp,
            aes(date, icu_patients),
            colour = "darkorange3",
            size = 1) +
  scale_x_date(date_breaks = "1 week", labels = isoweek) +
  scale_y_log10() +
  scale_size(breaks = 1:5 * 1000000,
             labels = scales::comma_format(1)) +
  scale_colour_viridis_c(direction = -1, option = "mako",
                         breaks = 1:5 * 1000000,
                         labels = scales::comma_format(1)) +
  guides(colour = guide_legend()) +
  labs(title = "Wastewater and human results, across Denmark",
       x = "Prøvedato",
       y = "RNA copies per person (log scale)\nWeekly human incidence",
       colour = "Population covered",
       size = "Population covered") +
  theme(legend.position = "bottom")



#'
#' ## Wastewater over time
ww %>%
  filter(!is.na(anlaeg_plot),
         !is.na(rna_corr),
         phase_anlaeg == "pilot") %>%
  ggplot(aes(date_receipt, rna_corr)) +
  facet_wrap(~ anlaeg_plot, scales = "free_y") +
  geom_smooth(colour = "grey80", alpha = 0.1,
              method = "loess", formula = y ~ x) +
  geom_point(aes(colour = rna_corr, shape = påvist_mean), size = 2) +
  scale_x_date(date_breaks = "1 month", date_labels = "%d %b") +
  coord_cartesian(y = c(0, NA)) +
  scale_colour_viridis_c(option = "rocket", direction = -1, end = 0.9,
                         limits = c(0, 30000), oob = scales::squish) +
  scale_shape_manual(breaks = c("påvist",
                                "<LOQ",
                                "ikke påvist"),
                     values = c(19,
                                21,
                                4)) +
  theme(panel.background = element_rect(fill = "#f5faf9",
                                        colour = "transparent"),
        panel.grid.major.x = element_line(colour = "#ebf2f1"),
        panel.grid.minor.x = element_line(colour = "#ebf2f1"),
        panel.grid.major.y = element_line(colour = "#ebf2f1")) +
  labs(title = "Flow-corrected WW over time",
       x = NULL,
       y = "Copies per litre",
       caption = "Excluding results > 20,000 copies/litre")


#' Wastewater over time, but on the log scale.
ww %>%
  filter(!is.na(anlaeg_plot),
         !is.na(rna_corr),
         phase_anlaeg == "pilot") %>%
  ggplot(aes(date_receipt, rna_corr)) +
  facet_wrap(~ anlaeg_plot, scales = "free_y") +
  geom_smooth(colour = "grey80", alpha = 0.1,
              method = "loess", formula = y ~ x) +
  geom_point(aes(colour = rna_corr, shape = påvist_mean), size = 1.6) +
  scale_x_date(date_breaks = "1 month", date_labels = "%d %b") +
  scale_y_log10() +
  scale_colour_viridis_c(option = "rocket", direction = -1, end = 0.9,
                         limits = c(0, 30000), oob = scales::squish) +
  scale_shape_manual(breaks = c("påvist",
                                "<LOQ",
                                "ikke påvist"),
                     values = c(19,
                                21,
                                4)) +
  theme(panel.background = element_rect(fill = "#f5faf9",
                                        colour = "transparent"),
        panel.grid.major.x = element_line(colour = "#ebf2f1"),
        panel.grid.minor.x = element_line(colour = "#ebf2f1"),
        panel.grid.major.y = element_line(colour = "#ebf2f1")) +
  labs(title = "Flow-corrected WW over time",
       x = NULL,
       y = "Copies per litre")



#'
#' ## Pilot areas
#'

## Pilot areas -------------------------------------------------------------

ww_human %>%
  filter(date_receipt >= dmy("1 jul 2021"),
         phase_anlaeg == "pilot") %>%
  ggplot(aes(date_receipt, pmax(rna7_log * 20, 0))) +
  facet_wrap(~ anlaeg_plot) +
  geom_area(
    # Fix for when there's multiple samples delivered on one day
    data = ~ distinct(., anlaeg_plot, date_receipt, inc7) %>%
      filter(!is.na(inc7)),
    aes(y = inc7), fill = "#F9E1AF", colour = "transparent"
  ) +
  geom_line(data = ~ filter(., !is.na(rna7_log)),
            colour = "#ED6553", size = 1, na.rm = TRUE) +
  scale_x_date(date_breaks = "3 weeks",
               labels = isoweek,
               expand = expansion()) +
  scale_y_continuous(
    breaks = seq(0, 200, by = 40),
    # expand = expansion(),
    sec.axis = sec_axis(
      trans = ~ . / 20,
      breaks = c(0, 2, 4),
      labels = function(x) scales::comma(10^x,
                                         big.mark = ".",
                                         decimal.mark = ",",
                                         accuracy = 1),
      name = "RNA-kopier pr. liter spildevand\n(gennemsnit af de seneste 7 dage)"
    )
  ) +
  guides(colour = "none") +
  theme(panel.grid.major.x = element_line(),
        panel.spacing.x = unit(15, "pt"),
        panel.spacing.y = unit(10, "pt"),
        legend.position = "bottom") +
  theme_axes_leftright() +
  labs(x = "Uge for spildevandsprøve",
       y = "Daglig incidens pr. 100.000\n(gennemsnit af 7 dage)",
       shape = NULL)




#'
#'
#'
#'
#' ## Association with human data

## Association with human --------------------------------------------------

ww_human %>%
  filter(phase_anlaeg == "pilot",
         !is.na(rna_corr), !is.na(inc7),
         påvist_mean == "påvist") %>%
  ggplot(aes(rna_corr, inc7, fill = company)) +
  facet_wrap(~ anlaeg_plot, scales = "free_y") +
  geom_hline(yintercept = 0, colour = "grey70") +
  geom_smooth(method = "lm", formula = y ~ x,
              colour = "black") +
  geom_point(size = 3, shape = 21, colour = "white") +
  scale_x_log10() +
  scale_y_log10() +
  guides(color = "none", fill = "none") +
  theme(panel.grid.major.x = element_line()) +
  labs(
    title = "Association between WW and human results",
    subtitle = "Excluding 0s and <LOQ for the WW",
    x = "RNA copies in WW (log scale)",
    y = "Human incidence (7-day smoothed)"
  )









# Other data --------------------------------------------------------------

#' # Other data
#'
#'
#' Number of anlæg we have data from: **`r n_distinct(ww$anlaeg_rando)`**.
#'
#' Number we're presenting (2+ non-NA samples): **`r ww_human %>% add_count(anlaeg_rando) %>% filter(!is.na(rna_n), n >= 2) %>% pull(anlaeg_rando) %>% n_distinct()`**
#'
#'
#'
#'
#' Table with categorised results
ww %>%
  mutate(week_receipt = ISOweek::ISOweek(date_receipt)) %>%
  count(week_receipt, påvist_mean) %>%
  pivot_wider(names_from = påvist_mean,
              values_from = n) %>%
  arrange(desc(week_receipt)) %>%
  select(week_receipt,
         `ikke påvist`,
         `<LOQ`,
         påvist,
         ukendt = `NA`) %>%
  adorn_percentages() %>%
  adorn_pct_formatting(digits = 0)





# Export data -------------------------------------------------------------

# Raw
combined_ww %>%
  write_xlsx(glue("outputs/{today()}_data-raw-ww.xlsx"))

# Processed
ww_human %>%
  filter(date_receipt >= dmy("1 jul 2021")) %>%
  write_xlsx(glue("outputs/{today()}_data-ww-human.xlsx"))


# Anlaeg names for Lene to assign regions
anlaeg_names_for_regions <- all_ww %>%
  filter(!is.na(proevested)) %>%
  group_by(anlaeg_rando, proevested) %>%
  summarise(first_date = min(date_email, na.rm = TRUE)) %>%
  ungroup() %>%
  left_join(
    lookup_oplande_names %>%
      select(anlaeg_display, company),
    by = c("anlaeg_rando" = "anlaeg_display")
  ) %>%
  left_join(
    ww_human %>%
      distinct(anlaeg_rando, region),
    by = "anlaeg_rando"
  ) %>%
  relocate(company, .before = first_date) %>%
  arrange(first_date)

# Data dictionary for Lene to remember
anlaeg_names_for_regions_dict <- tibble(
  column = c("anlaeg_rando", "proevested", "company", "first_date", "region"),
  explanation = c(
    "Our name for the renseanlæg, or from Eurofins if we haven't named it yet",
    "The name from Eurofins",
    "The forsyning/area (comes from the shapefiles, could be unhelpful)",
    "The first date (date_email) we received a result for this renseanlæg",
    "The region already assigned"
  )
)

# Export anlaeg names
list(anlaeg_list = anlaeg_names_for_regions,
     dictionary =  anlaeg_names_for_regions_dict) %>%
  write_xlsx(glue("outputs/{today()}_anlaeg-names-for-regions.xlsx"))




# All anlæg that are reporting
lookup_oplande_names %>%
  semi_join(combined_ww, by = c("anlaeg_eurofins" = "proevested"),
            na_matches = "never") %>%
  select(anlaeg_eurofins, anlaeg_display, anlaeg_shp, company) %>%
  write_xlsx(glue("outputs/{today()}_anlaeg-reporting.xlsx"))


# # Export WW results that are of interest to KK2
# combined_ww %>%
#   filter(proevested %in% c("Lynetten opland, Søndre Indløb",
#                            "Lynetten opland, Nordre indløb")) %>%
#   write_excel_csv2(glue("outputs/{today()}_ww-kk-lynetten.csv"))





## Missing Eurofins data -------------------------------------------------------

# Both anlæg who haven't recently sent samples, and those who are in the master
# but never sent anything


# Anlæg who have never sent samples but who are in the master sheet
never_sent_sample <- lookup_oplande_names %>%
  filter(!is.na(anlaeg_master),
         is.na(comments) | tolower(comments) != "reserve") %>%
  anti_join(ww_human, by = c("anlaeg_display" = "anlaeg_rando")) %>%
  select(anlaeg_master, company_master, missing_euro, missing_shp) %>%
  arrange(company_master, anlaeg_master)


# Anlæg who have sent samples before, but not in the latest week (latest week
# based on all anlægs' samples)
no_recent_samples <- ww_human %>%
  select(anlaeg_rando, proevested, date_receipt, sample) %>%
  mutate(week = ISOweek::ISOweek(date_receipt)) %>%
  filter(!is.na(sample)) %>%
  # For each anlæg, find out when they sent their last sample
  group_by(anlaeg_rando, proevested) %>%
  summarise(last_sample = max(week),
            .groups = "drop") %>%
  ungroup() %>%
  arrange(last_sample) %>%
  # Filter down to just the ones who haven't sent a sample in the latest week
  filter(last_sample != max(last_sample)) %>%
  # Join in names in master sheet for ease
  left_join(
    lookup_oplande_names %>%
      select(anlaeg_display, anlaeg_master, company_master),
    by = c("anlaeg_rando" = "anlaeg_display")
  )


list(
  never_sent_sample = never_sent_sample,
  no_recent_samples = no_recent_samples
) %>%
  write_xlsx(glue("outputs/{today()}_missing-eurofins-samples.xlsx"))

