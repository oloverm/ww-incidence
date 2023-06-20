


if (!exists("isrun_setup")) source("R/00_setup.R")


cli_alert("Reading shapes")

# Read DK shapefile
if (!exists("dk_kommuner")) {
  dk_kommuner <- st_read("Oplande/dk_kommuner.geojson", quiet = TRUE)
}
# Read in simplified opland shapes
shp_simple <- st_read("Oplande/all-anlaeg-simple.geojson", quiet = TRUE)


# Signals over time (NEW VERSION) -------------------------------------------------------


grading %>%
  filter(grading > 0) %>%
  ggplot(aes(week, fill = factor(grading), group = factor(grading))) +
  geom_bar(colour = "white", size = 0.4) +
  scale_fill_manual(breaks = grading_colours$breaks,
                    values = grading_colours$values) +
  geom_vline(xintercept = "2022-W14",
             colour = "grey20",
             linetype = "dashed")+
  scale_x_discrete(labels = ~ str_remove(., "\\d+-W")) +
  scale_y_continuous(expand = expansion(c(0.01, 0.05))) +
  theme(panel.grid.major.x = element_blank()) +
  labs(fill = "Kategori",
       x = "Uge",
       y = "Antal signaler")

# På engelsk til artikel
# grading %>%
#   filter(grading > 0) %>%
#   ggplot(aes(week, fill = factor(grading), group = factor(grading))) +
#   geom_bar(colour = "white", size = 0.4) +
#   scale_fill_manual(breaks = grading_colours$breaks,
#                     values = grading_colours$values) +
#   geom_vline(xintercept = "2022-W14",
#              colour = "grey20",
#              linetype = "dashed")+
#   scale_x_discrete(labels = ~ str_remove(., "\\d+-W")) +
#   scale_y_continuous(breaks = c(0,10,20,30,40,50,60,70,80,90),
#                      expand = expansion(c(0.01, 0.05))) +
#   theme(panel.grid.major.x = element_blank(),
#         axis.text.x = element_text(size = 6, angle = 0)) +
#   labs(fill = "Category",
#        x = "Week",
#        y = "Number of signals")

save_plot(last_plot(),
          "{today()}_antal-signaler.png",
          folder = "outputs/signaler",
          width = 8, height = 3.5)



# Underlying data graph -------------------------------------------------------

#' These are the data points under our signals

#+ fig.width = 11, fig.height = 6

cli_alert("Graph underlying signal data")

if (is_empty(compact(signal_data)) == FALSE) {
  
  
  # Adjusting the data to have a measure and value column instead of two columns 
  # then easier to plot the two data types according to shift in method
signal_data %>%
    pivot_longer(cols = starts_with("rna"), names_to = "measure", values_to = "value") %>%
    mutate(grading = if_else(signal_period, grading, NA_real_),
           inc7_log = log10(inc_7)) %>%
    ggplot(aes(date_receipt, value * 0.5)) +
    facet_wrap(~ anlaeg_grading) +
    geom_vline(xintercept = ymd("2022-01-03")) +
    geom_line(data = ~filter(., !is.na(inc7_log)),
              aes(y = inc7_log),
              colour = "#e5c087",
              size = 1.5,
              position = position_identity()) +
    # geom_line(data = ~ filter(., !is.na(value),
    #                           date_receipt <= ymd("2022-04-04"),
    #                           measure == "rna_corr_log"),
    #           aes(colour = factor(grading), group = 1)) +
    # Kun nødvendig til vi kun har fæces data - et par uger
    geom_line(data = ~ filter(., !is.na(value),
                              date_receipt >= ymd("2022-04-04"),
                              measure == "rna_norm_faeces_log"),
              aes(colour = factor(grading), group = 1)) +
    # geom_point(data = ~ filter(., !is.na(value),
    #                            date_receipt <= ymd("2022-04-04"),
    #                            measure == "rna_corr_log"),
    #            aes(shape = påvist_mean,
    #                colour = factor(grading)),
    #            size = 1.8, fill = "white") +
    geom_point(data = ~ filter(., !is.na(value),
                               date_receipt >= ymd("2022-04-04"),
                               measure == "rna_norm_faeces_log"),
               aes(shape = påvist_mean,
                   colour = factor(grading)),
               size = 1.8, fill = "white") +
    scale_shape_påvist() +
    scale_y_continuous(
      # breaks = seq(0, 1000, by = 100),
      labels = function(x) scales::comma(10^x,
                                         big.mark = ".",
                                         decimal.mark = ",",
                                         accuracy = 1),
      # expand = expansion(),
      sec.axis = sec_axis(
        trans = ~ . / 0.5,
        # breaks = c(0, 2, 4, 6),
        labels = function(x) scales::comma(10^x,
                                           big.mark = ".",
                                           decimal.mark = ",",
                                           accuracy = 1),
        name = "RNA-kopier ift. fæces indhold"
      )
    ) +
    geom_vline(xintercept = ymd("2022-04-04"),
               colour = "grey20",
               linetype = "dashed") +
    scale_x_date(date_breaks = "1 week", labels = isoweek) +
    scale_colour_manual(breaks = grading_colours$breaks,
                        values = grading_colours$values,
                        na.value = "grey70") +
    guides(colour = "none") +
    theme(panel.spacing.x = unit(24, "pt"),
          legend.position = "bottom",
          axis.title.y.left = element_text(colour = "#e5c087"),
          axis.text.y.left = element_text(colour = "#e5c087"),
          panel.grid.major = element_line(colour = "grey92")) +
    labs(
      x = "Uge",
      y = "Daglig incidens pr. 100.000 (gennemsnit af 7 dage)",
      shape = NULL
    ) 
  
  
  # ggsave(glue("STPS/{today()}_signals-data.png"),
  #        width = 9, height = 7, scale = 1.3)
  
  
  save_plot(last_plot(),
            "{today()}_signaler.png",
            folder = "outputs/signaler",
            width = 13, height = 9)
} else if (is_empty(compact(signal_data)) == TRUE) {
  cli_alert_success("There are no signals for this week - thus no plot")
}


# Excel output ------------------------------------------------------------

cli_alert("Signal Excel output")

stps_dictionary <- tibble(
  kolonne = c(
    "kategori",
    "stigning i spildevandet",
    "n_human_prøver_uge",
    "n_human_prøver_foregående_uger",
    "incidens_uge",
    "incidens_foregående_uger"
  ),
  forklaring = c(
    "Graduering af signalet på baggrund af SSI's model. Modellen er baseret på stigninger på log-skalaen.",
    "Stigning i antallet af SARS-CoV-2 RNA kopier ift. fæces indhold (før uge 14 - 2022 som kopier/l spildevand), når man sammenligner aktuel uge med de to foregående uger. Stigningen er opgivet på normalskala.",
    "Antal af personer, der er testet positiv for SARS-CoV-2 med PCR- og antigentest i oplandet i den aktuelle uge (OBS antallet kan være forbundet med usikkerhed på grund af manglende  data).",
    "Gennemsnitligt antal af personer, der er testet positiv for SARS-CoV-2 med PCR- og antigentest i oplandet per uge i de to foregående uger.",
    "Ugentlig incidens per 100.000 i oplandet i aktuel uge (OBS incidensen kan være upræcis på grund af manglende data)",
    "Gennemsnitlig ugentlig incidens per 100.000 i oplandet i de foregående to uger"
  )
)

stps_history <- grading %>%
  arrange(desc(week), desc(grading), desc(rna_diff), anlaeg_rando) %>%
  mutate(increase_ww = paste0(round(10^rna_diff), "x") %>%
           # No xXX increase for cat 1 signals, doesn't make sense
           na_predicate(~ grading == 1)) %>%
  filter(grading >= 1) %>%
  mutate(across(c(n_human_week,
                  n_human_week_prev), round),
         across(c(inc_human_week,
                  inc_human_week_prev), round, 1)) %>%
  select(
    uge = week,
    "navn på prøveudtagningssted" = anlaeg_rando,
    region = region,
    population,
    kategori = grading,
    "stigning i spildevandet" = increase_ww,
    n_human_prøver_uge = n_human_week,
    n_human_prøver_foregående_uger = n_human_week_prev,
    incidens_uge = inc_human_week,
    incidens_foregående_uger = inc_human_week_prev,
  )

stps_latest <- stps_history %>%
  filter(uge == ISOweek(today() - days(7)))

list("Aktuelle signaler" = stps_latest,
     "Tidligere signaler" = stps_history,
     "Ordforklaringer" = stps_dictionary) %>%
  write_xlsx(glue("outputs/signaler/{today()}_signaler.xlsx"))


# Map ---------------------------------------------------------------------

cli_alert("Map: prep")

# TODO: I think I'm doing some duplicate stuff with these maps (beyond the
# obvious fact that it's the same map twice). Can recycle some data I think

shp_stps <- grading %>%
  filter(week == ISOweek(today() - days(7)),
         grading > 0) %>%
  left_join(lookup_oplande_names %>%
              select(anlaeg_display, anlaeg_shp),
            by = c("anlaeg_rando" = "anlaeg_display")) %>%
  filter(!is.na(anlaeg_shp)) %>%
  left_join(shp_simple, by = c("anlaeg_shp" = "anlaeg")) %>%
  st_as_sf()

shp_stps_buffer <- shp_stps %>%
  st_simplify(dTolerance = 100) %>%
  st_buffer(2000) %>%
  st_simplify(dTolerance = 200)




## Interactive map ---------------------------------------------------------

pacman::p_load(leaflet)

shp_simple <- st_read("Oplande/all-anlaeg-simple.geojson", quiet = TRUE)

shp_simple_longlat <- shp_simple %>%
  st_transform('+proj=longlat +datum=WGS84')



shp_grading <- grading %>%
  filter(week == ISOweek(today() - days(7))) %>%
  left_join(lookup_oplande_names %>%
              select(anlaeg_display, anlaeg_shp),
            by = c("anlaeg_rando" = "anlaeg_display"),
            na_matches = "never") %>%
  left_join(shp_simple_longlat, by = c("anlaeg_shp" = "anlaeg")) %>%
  st_as_sf() %>%
  filter(!st_is_empty(geometry))


pal_signals <- colorFactor(
  c("dodgerblue",
    "orange",
    "orangered",
    "magenta3",
    "purple4"),
  1:5,
  na.color = "#CCCCCC"
)

shp_signal <- shp_grading %>%
  filter(grading >= 1)
shp_nosignal <- shp_grading %>%
  filter(grading == 0)

length(st_is_empty(shp_signal))

cli_alert("Map: interactive")

# Only plotting if some signal is found


if (length(st_is_empty(shp_signal)) != 0) {
  #+ fig.height = 7
  leaflet_signals <- leaflet() %>%
    addProviderTiles(providers$CartoDB.Positron) %>%
    # Non-signal oplande in grey
    addPolygons(
      data = shp_nosignal,
      fillColor = "#DDDDDD",
      fillOpacity = 0.3,
      stroke = TRUE,
      weight = 0.2,
      color = "black",
      label = ~ glue("{anlaeg_rando}"),
    ) %>%
    # Buffered areas just to help highlight the signals
    addPolygons(
      data = shp_stps_buffer %>%
        st_transform('+proj=longlat +datum=WGS84'),
      fillColor = ~ pal_signals(grading),
      fillOpacity = 0.1,
      stroke = FALSE,
    ) %>%
    # Signal oplande
    addPolygons(
      data = shp_signal,
      fillColor = ~ pal_signals(grading),
      fillOpacity = 0.3,
      stroke = TRUE,
      weight = 0.2,
      color = "black",
      highlightOptions = highlightOptions(color = "black",
                                          stroke = TRUE, weight = 2,
                                          bringToFront = TRUE),
      label = ~ glue("{anlaeg_rando}"),
    )
  
  leaflet_signals
  
  
  # Save the leaflet signal map
  htmlwidgets::saveWidget(
    leaflet_signals,
    glue(here("outputs", "signaler"), "/{today()}_kort-signaler.html"),
    selfcontained = TRUE
  )
} else if (length(st_is_empty(shp_signal)) == 0) {
  cli_alert_success("There are no signals for this week - thus no interactive map")
} 






## Static map --------------------------------------------------------------

cli_alert("Map: static")

if (length(st_is_empty(shp_stps)) != 0) {
  #+ fig.height = 7
  ggplot() +
    geom_sf(data = dk_kommuner %>%
              st_transform(st_crs(shp_stps)),
            fill = "grey70", colour = "white") +
    geom_sf(data = shp_stps_buffer,
            aes(fill = factor(grading)),
            colour = "transparent",
            alpha = 0.3) +
    geom_sf(data = shp_stps,
            aes(fill = factor(grading)), colour = "transparent") +
    geom_sf_label(data = shp_stps_buffer %>%
                    mutate(geometry = st_centroid(geometry)),
                  size = 2.5,
                  nudge_y = 8000,
                  alpha = 0.3,
                  label.size = 0,
                  label.r = unit(0, "lines"),
                  colour = alpha("black", 0.6),
                  show.legend = FALSE,
                  aes(label = anlaeg_rando, fill = factor(grading))) +
    scale_fill_manual(aesthetics = c("colour", "fill"),
                      breaks = grading_colours$breaks,
                      values = grading_colours$values) +
    theme_void() +
    theme(plot.background = element_rect(fill = "white",
                                         colour = "white"),
          legend.position = c(0.9, 0.9)) +
    labs(fill = "Signal grading",
         title = glue("Signals reported to STPS, {week}",
                      week = max(shp_stps$week)),
         subtitle = glue("Excludes {n_nogis} areas where we have no GIS data yet",
                         n_nogis = nrow(stps_latest) - nrow(shp_stps)))
  
  
  
  
  save_plot(last_plot(),
            "{today()}_map-signals.png",
            folder = "outputs/signaler",
            width = 4.8, height = 4, scale = 2.5)
} else if (length(st_is_empty(shp_stps)) == 0) {
  cli_alert_success("There are no signals for this week - thus no static map")
} 




# Signals over time (OLD VERSION) -------------------------------------------------------

#cli_alert("Graph antal signaler")

#' Signals over time
#' grading %>%
#'   filter(grading > 0) %>%
#'   ggplot(aes(week, fill = factor(grading), group = factor(grading))) +
#'   geom_bar(colour = "white", size = 0.4) +
#'   scale_fill_manual(breaks = grading_colours$breaks,
#'                     values = grading_colours$values) +
#'   scale_x_discrete(labels = ~ str_remove(., "\\d+-W")) +
#'   scale_y_continuous(expand = expansion(c(0.01, 0.05))) +
#'   theme(panel.grid.major.x = element_blank()) +
#'   labs(fill = "Kategori",
#'        x = "Uge",
#'        y = "Antal signaler")
#' 
#' save_plot(last_plot(),
#'           "{today()}_antal-signaler.png",
#'           folder = "outputs/signaler",
#'           width = 8, height = 3.5)
#' 
#' 
#' 
#' # Underlying data graph -------------------------------------------------------
#' 
#' #' These are the data points under our signals
#' 
#' #+ fig.width = 11, fig.height = 6
#' 
#' cli_alert("Graph underlying signal data")
#' 
#' if (is_empty(compact(signal_data)) == FALSE) {
#' 
#'     signal_data %>%
#'       mutate(grading = if_else(signal_period, grading, NA_real_),
#'              inc7_log = log10(inc_7)) %>%
#'       ggplot(aes(date_receipt, rna_corr_log * 0.5)) +
#'       facet_wrap(~ anlaeg_grading) +
#'       geom_vline(xintercept = ymd("2022-01-03")) +
#'       geom_line(data = ~filter(., !is.na(inc7_log)),
#'                 aes(y = inc7_log),
#'                 colour = "#e5c087",
#'                 size = 1.5,
#'                 position = position_identity()) +
#'       geom_line(data = ~ filter(., !is.na(rna_corr_log)),
#'                 aes(colour = factor(grading), group = 1)) +
#'       geom_point(data = ~ filter(., !is.na(rna_corr_log)),
#'                  aes(shape = påvist_mean,
#'                      colour = factor(grading)),
#'                  size = 1.8, fill = "white") +
#'       scale_shape_påvist() +
#'       scale_y_continuous(
#'         # breaks = seq(0, 1000, by = 100),
#'         labels = function(x) scales::comma(10^x,
#'                                            big.mark = ".",
#'                                            decimal.mark = ",",
#'                                            accuracy = 1),
#'         # expand = expansion(),
#'         sec.axis = sec_axis(
#'           trans = ~ . / 0.5,
#'           # breaks = c(0, 2, 4, 6),
#'           labels = function(x) scales::comma(10^x,
#'                                              big.mark = ".",
#'                                              decimal.mark = ",",
#'                                              accuracy = 1),
#'           name = "RNA-kopier ift. fæces indhold"
#'         )
#'       ) +
#'       scale_x_date(date_breaks = "1 week", labels = isoweek) +
#'       scale_colour_manual(breaks = grading_colours$breaks,
#'                           values = grading_colours$values,
#'                           na.value = "grey70") +
#'       guides(colour = "none") +
#'       theme(panel.spacing.x = unit(24, "pt"),
#'             legend.position = "bottom",
#'             axis.title.y.left = element_text(colour = "#e5c087"),
#'             axis.text.y.left = element_text(colour = "#e5c087"),
#'             panel.grid.major = element_line(colour = "grey92")) +
#'       labs(
#'         x = "Uge",
#'         y = "Daglig incidens pr. 100.000 (gennemsnit af 7 dage)",
#'         shape = NULL
#'       )
#' 
#' 
#'     # ggsave(glue("STPS/{today()}_signals-data.png"),
#'     #        width = 9, height = 7, scale = 1.3)
#' 
#' 
#'     save_plot(last_plot(),
#'               "{today()}_signaler_test.png",
#'               folder = "outputs/signaler",
#'               width = 13, height = 9)
#' } else if (is_empty(compact(signal_data)) == TRUE) {
#'   cli_alert_success("There are no signals for this week - thus no plot")
#' }
#' 
#' 
#' 
#' # Excel output ------------------------------------------------------------
#' 
#' cli_alert("Signal Excel output")
#' 
#' stps_dictionary <- tibble(
#'   kolonne = c(
#'     "kategori",
#'     "stigning i spildevandet",
#'     "n_human_prøver_uge",
#'     "n_human_prøver_foregående_uger",
#'     "incidens_uge",
#'     "incidens_foregående_uger"
#'   ),
#'   forklaring = c(
#'     "Graduering af signalet på baggrund af SSI's model. Modellen er baseret på stigninger på log-skalaen.",
#'     "Stigning i antallet af SARS-CoV-2 RNA kopier/l spildevand, når man sammenligner aktuel uge med de to foregående uger. Stigningen er opgivet på normalskala.",
#'     "Antal af personer, der er testet positiv for SARS-CoV-2 med PCR- og antigentest i oplandet i den aktuelle uge (OBS antallet kan være forbundet med usikkerhed på grund af manglende  data).",
#'     "Gennemsnitligt antal af personer, der er testet positiv for SARS-CoV-2 med PCR- og antigentest i oplandet per uge i de to foregående uger.",
#'     "Ugentlig incidens per 100.000 i oplandet i aktuel uge (OBS incidensen kan være upræcis på grund af manglende data)",
#'     "Gennemsnitlig ugentlig incidens per 100.000 i oplandet i de foregående to uger"
#'   )
#' )
#' 
#' stps_history <- grading %>%
#'   arrange(desc(week), desc(grading), desc(rna_diff), anlaeg_rando) %>%
#'   mutate(increase_ww = paste0(round(10^rna_diff), "x") %>%
#'            # No xXX increase for cat 1 signals, doesn't make sense
#'            na_predicate(~ grading == 1)) %>%
#'   filter(grading >= 1) %>%
#'   mutate(across(c(n_human_week,
#'                   n_human_week_prev), round),
#'          across(c(inc_human_week,
#'                   inc_human_week_prev), round, 1)) %>%
#'   select(
#'     uge = week,
#'     "navn på prøveudtagningssted" = anlaeg_rando,
#'     region = region,
#'     population,
#'     kategori = grading,
#'     "stigning i spildevandet" = increase_ww,
#'     n_human_prøver_uge = n_human_week,
#'     n_human_prøver_foregående_uger = n_human_week_prev,
#'     incidens_uge = inc_human_week,
#'     incidens_foregående_uger = inc_human_week_prev,
#'   )
#' 
#' stps_latest <- stps_history %>%
#'   filter(uge == ISOweek(today() - days(7)))
#' 
#' list("Aktuelle signaler" = stps_latest,
#'      "Tidligere signaler" = stps_history,
#'      "Ordforklaringer" = stps_dictionary) %>%
#'   write_xlsx(glue("outputs/signaler/{today()}_signaler.xlsx"))
#' 
#' 
#' 
#' 
#' 
#' 
#' 
#' 
#' 
#' 
#' 
#' 
#' 
#' 

#
# # Map ---------------------------------------------------------------------
#
# cli_alert("Map: prep")
#
# # TODO: I think I'm doing some duplicate stuff with these maps (beyond the
# # obvious fact that it's the same map twice). Can recycle some data I think
#
# shp_stps <- grading %>%
#   filter(week == ISOweek(today() - days(7)),
#          grading > 0) %>%
#   left_join(lookup_oplande_names %>%
#               select(anlaeg_display, anlaeg_shp),
#             by = c("anlaeg_rando" = "anlaeg_display")) %>%
#   filter(!is.na(anlaeg_shp)) %>%
#   left_join(shp_simple, by = c("anlaeg_shp" = "anlaeg")) %>%
#   st_as_sf()
#
# shp_stps_buffer <- shp_stps %>%
#   st_simplify(dTolerance = 100) %>%
#   st_buffer(2000) %>%
#   st_simplify(dTolerance = 200)
#
#
#
#
# ## Interactive map ---------------------------------------------------------
#
# pacman::p_load(leaflet)
#
# shp_simple <- st_read("Oplande/all-anlaeg-simple.geojson", quiet = TRUE)
#
# shp_simple_longlat <- shp_simple %>%
#   st_transform('+proj=longlat +datum=WGS84')
#
#
#
# shp_grading <- grading %>%
#   filter(week == ISOweek(today() - days(7))) %>%
#   left_join(lookup_oplande_names %>%
#               select(anlaeg_display, anlaeg_shp),
#             by = c("anlaeg_rando" = "anlaeg_display"),
#             na_matches = "never") %>%
#   left_join(shp_simple_longlat, by = c("anlaeg_shp" = "anlaeg")) %>%
#   st_as_sf() %>%
#   filter(!st_is_empty(geometry))
#
#
# pal_signals <- colorFactor(
#   c("dodgerblue",
#     "orange",
#     "orangered",
#     "magenta3",
#     "purple4"),
#   1:5,
#   na.color = "#CCCCCC"
# )
#
# shp_signal <- shp_grading %>%
#   filter(grading >= 1)
# shp_nosignal <- shp_grading %>%
#   filter(grading == 0)
#
#   length(st_is_empty(shp_signal))
#
# cli_alert("Map: interactive")
#
# # Only plotting if some signal is found
#
#
# if (length(st_is_empty(shp_signal)) != 0) {
# #+ fig.height = 7
# leaflet_signals <- leaflet() %>%
#   addProviderTiles(providers$CartoDB.Positron) %>%
#   # Non-signal oplande in grey
#   addPolygons(
#     data = shp_nosignal,
#     fillColor = "#DDDDDD",
#     fillOpacity = 0.3,
#     stroke = TRUE,
#     weight = 0.2,
#     color = "black",
#     label = ~ glue("{anlaeg_rando}"),
#   ) %>%
#   # Buffered areas just to help highlight the signals
#   addPolygons(
#     data = shp_stps_buffer %>%
#       st_transform('+proj=longlat +datum=WGS84'),
#     fillColor = ~ pal_signals(grading),
#     fillOpacity = 0.1,
#     stroke = FALSE,
#   ) %>%
#   # Signal oplande
#   addPolygons(
#     data = shp_signal,
#     fillColor = ~ pal_signals(grading),
#     fillOpacity = 0.3,
#     stroke = TRUE,
#     weight = 0.2,
#     color = "black",
#     highlightOptions = highlightOptions(color = "black",
#                                         stroke = TRUE, weight = 2,
#                                         bringToFront = TRUE),
#     label = ~ glue("{anlaeg_rando}"),
#   )
#
# leaflet_signals
#
#
# # Save the leaflet signal map
# htmlwidgets::saveWidget(
#   leaflet_signals,
#   glue(here("outputs", "signaler"), "/{today()}_kort-signaler.html"),
#   selfcontained = TRUE
# )
# } else if (length(st_is_empty(shp_signal)) == 0) {
#   cli_alert_success("There are no signals for this week - thus no interactive map")
# }
#
#
#
#
#
#
# ## Static map --------------------------------------------------------------
#
# cli_alert("Map: static")
#
# if (length(st_is_empty(shp_stps)) != 0) {
# #+ fig.height = 7
# ggplot() +
#   geom_sf(data = dk_kommuner %>%
#             st_transform(st_crs(shp_stps)),
#           fill = "grey70", colour = "white") +
#   geom_sf(data = shp_stps_buffer,
#           aes(fill = factor(grading)),
#           colour = "transparent",
#           alpha = 0.3) +
#   geom_sf(data = shp_stps,
#           aes(fill = factor(grading)), colour = "transparent") +
#   geom_sf_label(data = shp_stps_buffer %>%
#                   mutate(geometry = st_centroid(geometry)),
#                 size = 2.5,
#                 nudge_y = 8000,
#                 alpha = 0.3,
#                 label.size = 0,
#                 label.r = unit(0, "lines"),
#                 colour = alpha("black", 0.6),
#                 show.legend = FALSE,
#                 aes(label = anlaeg_rando, fill = factor(grading))) +
#   scale_fill_manual(aesthetics = c("colour", "fill"),
#                     breaks = grading_colours$breaks,
#                     values = grading_colours$values) +
#   theme_void() +
#   theme(plot.background = element_rect(fill = "white",
#                                        colour = "white"),
#         legend.position = c(0.9, 0.9)) +
#   labs(fill = "Signal grading",
#        title = glue("Signals reported to STPS, {week}",
#                     week = max(shp_stps$week)),
#        subtitle = glue("Excludes {n_nogis} areas where we have no GIS data yet",
#                        n_nogis = nrow(stps_latest) - nrow(shp_stps)))
#
#
#
#
# save_plot(last_plot(),
#           "{today()}_map-signals.png",
#           folder = "outputs/signaler",
#           width = 4.8, height = 4, scale = 2.5)
# } else if (length(st_is_empty(shp_stps)) == 0) {
#   cli_alert_success("There are no signals for this week - thus no static map")
# }



# # Working on series-of-0s detection
#
# ww_human %>%
#   filter(anlaeg_rando == "Vedbæk") %>%
#   select(anlaeg_rando, date_receipt, påvist_loq) %>%
#   mutate(low = påvist_loq %in% c("<LOQ", "ikke påvist"),
#          row_low = cumsum(low))
#
#
# x <- c(F, T, NA, T, F, F, T, T)
# x <- c(NA, T, NA, T, F, NA, F, T, NA, F, NA, T, NA)
#
#
#
# lookalong <- function(x) {
#   out <- numeric(length(x))
#   y <- lag(x, 1)
#
#   for (i in seq_along(x)) {
#
#     if (i == 1) {
#       if (is.na(x[i])) {
#         out[i] <- 0
#       } else if (x[i] == TRUE) {
#         out[i] <- 1
#       } else {
#         out[i] <- 0
#       }
#     } else if (is.na(x[i])) {
#       out[i] <- out[i - 1]
#     } else if (x[i] == TRUE) {
#       out[i] <- out[i - 1] + 1
#     } else if (x[i] == FALSE) {
#       out[i] <- 0
#     }
#   }
#
#   out
# }
#
# lookalong(x)
