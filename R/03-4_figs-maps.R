# Med udgangspunkt i CDC's overvågning forsøges at implementere lignende

# Starter med den de kalder "Current virus levels in ww by site"
# Man kategoriserer det nuværende niveau baseret på tidligere niveau - opdeles i 5 kategorier i procent
# Vi gør det for ugentligt gennemsnit fra 1/1-2022 for at have ens data


# Read DK shapefile
if (!exists("dk_kommuner")) {
  dk_kommuner <- st_read("Oplande/dk_kommuner.geojson", quiet = TRUE)
}
# Read in simplified opland shapes
shp_simple <- st_read("Oplande/all-anlaeg-simple.geojson", quiet = TRUE)

# HELE DANMARK - HVERT ANLÆG ------------------------------------------------

map_all_anlaeg <- ww_human %>%
  select(anlaeg_rando, week, log_rna_mean_faeces, landsdel, region, påvist_mean, overlap_level) %>%
  # Manuelt fjerne NaN værdier da de af en eller anden grund ikke blot er NA
  filter(log_rna_mean_faeces != "NaN",
         anlaeg_rando %in% lookup_oplande_names_new$anlaeg_display) %>%
  mutate(påvist_binary = case_when(påvist_mean == "påvist" ~ 1,
                                   påvist_mean == "<LOQ" ~ 1,
                                   påvist_mean == "ikke påvist" ~ 0)) %>%
  group_by(anlaeg_rando, week, landsdel, region, påvist_binary) %>%
  summarise(
    n_samples = sum(!is.na(log_rna_mean_faeces)),
    rna_normalised_faeces_week = mean(log_rna_mean_faeces, na.rm = TRUE),
    n_påvist = sum(påvist_binary),
    .groups = "drop") %>%
  filter(!is.na(rna_normalised_faeces_week)) %>%
  ungroup() %>%
  group_by(anlaeg_rando, week, landsdel,region) %>%
  summarise(
  n_samples_sum = sum(n_samples),
  rna_normalised_faeces_week = mean(rna_normalised_faeces_week, na.rm = TRUE),
  n_påvist_sum = sum(n_påvist),
  .groups = "drop") %>%
  ungroup() %>%
  group_by(anlaeg_rando) %>%
  mutate(new_scale = scales::rescale(rna_normalised_faeces_week, to = c(0,100)),
         category = case_when(new_scale == 0 ~"Laveste måling hidtil",
                              new_scale == 100 ~"Højeste måling hidtil",
                              new_scale < 10 ~"Meget lavt",
                              new_scale < 20 ~"Lavt",
                              new_scale < 40 ~"Moderat lavt",
                              new_scale < 60 ~"Moderat",
                              new_scale < 80 ~"Moderat højt",
                              new_scale < 90 ~"Højt",
                              new_scale >= 90 ~"Meget højt")) %>%
  ungroup() 

  


# Trying to plot a color coded map based on the groups
# Skal have joinet med shape filer 

map_all_anlaeg_plot <- map_all_anlaeg %>%
  filter(week == ISOweek(today() - days(7))) %>%
  left_join(lookup_oplande_names %>%
              select(anlaeg_display, anlaeg_shp),
            by = c("anlaeg_rando" = "anlaeg_display")) %>%
  filter(!is.na(anlaeg_shp)) %>%
  left_join(shp_simple, by = c("anlaeg_shp" = "anlaeg")) %>%
  st_as_sf()


buffer_all_anlaeg <- map_all_anlaeg_plot %>%
  st_simplify(dTolerance = 100) %>%
  st_buffer(2000) %>%
  st_simplify(dTolerance = 200)



ggplot() +
  geom_sf(data = dk_kommuner %>%
            st_transform(st_crs(map_all_anlaeg_plot)),
          fill = "grey70", colour = "white") +
  geom_sf(data = buffer_all_anlaeg,
          aes(fill = factor(category)),
          colour = "transparent",
          alpha = 0.3) +
  geom_sf(data = map_all_anlaeg_plot,
          aes(fill = factor(category)), colour = "transparent") +
  scale_fill_manual(limits = c(
    "Ikke påvist",
    "Laveste måling hidtil",
    "Meget lavt",
    "Lavt",
    "Moderat lavt",
    "Moderat",
    "Moderat højt",
    "Højt",
    "Meget højt",
    "Højeste måling hidtil"
  ),
  values = c(
    "#2ECC71",
    "#82E0AA",
    "#D6EAF8",
    "#85C1E9",
    "#F7DC6F",
    "#F39C12",
    "#EC7063",
    "#C0392B",
    "#78241C",
    "#001A20"
    
  ),
  labels = c(
    "Ikke påvist",
    "Laveste måling hidtil",
    "Meget lavt",
    "Lavt",
    "Moderat lavt",
    "Moderat",
    "Moderat højt",
    "Højt",
    "Meget højt",
    "Højeste måling hidtil"
  ))+
  theme_void() +
  theme(plot.background = element_rect(fill = "white",
                                       colour = "white"),
        legend.position = c(0.9, 0.75)) +
  labs(fill = "Kategori",
       title = glue("SARS-CoV-2 niveau i uge {today}",
                    today = week(today()-days(7))))

save_plot(last_plot(),
          "week_{week(today()-days(7))}_map_all_anlaeg.png",
          folder = glue("S:/Spildevand/outputs/maps/week_{week}",
          week = week(today()-days(7))))



# Interaktivt map: HELE DANMARK - HVERT ANLÆG ------------------------------------------------


pacman::p_load(leaflet)

shp_simple_longlat <- shp_simple %>%
  st_transform('+proj=longlat +datum=WGS84')

map_all_anlaeg_plot <- map_all_anlaeg %>%
  filter(week == ISOweek(today() - days(7))) %>%
  left_join(lookup_oplande_names %>%
              select(anlaeg_display, anlaeg_shp),
            by = c("anlaeg_rando" = "anlaeg_display")) %>%
  filter(!is.na(anlaeg_shp)) %>%
  left_join(shp_simple_longlat, by = c("anlaeg_shp" = "anlaeg")) %>%
  st_as_sf() %>%
  filter(!st_is_empty(geometry)) %>%
  mutate(category = as.factor(category),
         color = case_when(category =="Laveste måling hidtil" ~ "#82E0AA",
                           category =="Meget lavt" ~ "#D6EAF8",
                           category =="Lavt" ~ "#85C1E9",
                           category =="Moderat lavt" ~ "#F7DC6F",
                           category =="Moderat" ~ "#F39C12",
                           category =="Moderat højt" ~ "#EC7063",
                           category =="Højt" ~ "#C0392B",
                           category =="Meget højt" ~ "#78241C",
                           category =="Højeste måling hidtil" ~ "#001A20",
                           category =="Ikke påvist" ~ "#2ECC71")) %>%
  arrange(category)
  

buffer_all_anlaeg <- map_all_anlaeg_plot %>%
  st_simplify(dTolerance = 100) %>%
  st_buffer(2000) %>%
  st_simplify(dTolerance = 200)


  leaflet_all_anlaeg <-leaflet() %>%
    addProviderTiles(providers$CartoDB.Positron) %>%
    # Non-signal oplande in grey
    addPolygons(
      data = dk_kommuner,
      fillColor = "#DDDDDD",
      fillOpacity = 0.3,
      stroke = TRUE,
      weight = 0.2,
      color = "black") %>%
    # Buffered areas just to help highlight the signals
    addPolygons(
      data = buffer_all_anlaeg %>%
        st_transform('+proj=longlat +datum=WGS84'),
      fillColor = buffer_all_anlaeg$color,
      fillOpacity = 0.25,
      stroke = FALSE,
    ) %>%
    # Signal oplande
    addPolygons(
      data = map_all_anlaeg_plot,
      fillColor =map_all_anlaeg_plot$color,
      #fillColor = ~col_pal(map_all_anlaeg_plot$category),
      fillOpacity = 0.6,
      stroke = TRUE,
      weight = 0.2,
      color = "black",
      highlightOptions = highlightOptions(color = "black",
                                          stroke = TRUE, weight = 2,
                                          bringToFront = TRUE),
      label = ~ glue("{anlaeg_rando} : {round(new_scale,1)}% (kategori: {category})")
    ) #%>%  # add legend to the map
    # addLegend(position = "bottomright", # position where the legend should appear
    #           colors = unique(map_all_anlaeg_plot$color), # palette object where the color is defined
    #           labels = unique(map_all_anlaeg_plot$category), # column variable or values that were used to derive the color pallete object
    #           title = "Kategori", # title of the legend
    #           opacity = 1)
  
  leaflet_all_anlaeg 
  
  # Save the leaflet signal map
  htmlwidgets::saveWidget(
    leaflet_all_anlaeg,
    glue("S:/Spildevand/outputs/maps/week_{week}", "/week_{week}_kort-all_anlaeg.html",
         week = week(today()-days(7))),
    selfcontained = TRUE)


# KATEGORI ÆNDRING fra forrige uge ------------------------------------------------

# Laver tibble med category og uge (kan nok godt gøres smartere)



cat_perc_change <- function(df){
  this_week <- tibble(
    week = ISOweek(today() - days(7)), #ISOweek(today() - days(14))),
    category = c(    "Ikke påvist",
                     "Laveste måling hidtil",
                     "Meget lavt",
                     "Lavt",
                     "Moderat lavt",
                     "Moderat",
                     "Moderat højt",
                     "Højt",
                     "Meget højt",
                     "Højeste måling hidtil"))
  
  previous_week <- tibble(
    week = ISOweek(today() - days(14)),
    category = c(    "Ikke påvist",
                     "Laveste måling hidtil",
                     "Meget lavt",
                     "Lavt",
                     "Moderat lavt",
                     "Moderat",
                     "Moderat højt",
                     "Højt",
                     "Meget højt",
                     "Højeste måling hidtil"))
  
  cat <- this_week %>%
    bind_rows(previous_week)
  
  two_weeks <- df %>%
    filter(week == ISOweek(today() - days(7)) |
           week == ISOweek(today() - days(14))) %>%
    group_by(week, category) %>%
    summarise(n = n()) %>%
    ungroup()%>%
    arrange(category,week)
  
  num_sites <- two_weeks %>%
    full_join(cat) %>%
    mutate(n = replace_na(n,0)) %>%
    filter(week == ISOweek(today() - days(7))) %>% 
    arrange(category) %>%
    select(n)
  
  two_weeks %>%
    full_join(cat) %>%
    mutate(n = replace_na(n,0)) %>%
    arrange(category,week) %>%
    mutate(cat_perc_change = round(((lead(n) - n) / n *100),1)) %>%
    filter(week == ISOweek(today() - days(14))) %>%
    mutate(cat_perc_change = case_when(cat_perc_change == Inf ~ 100,
                                       cat_perc_change == "NaN" ~ 0,
                                       TRUE ~ cat_perc_change)) %>%
    select(-n, -week)%>%
    bind_cols(num_sites) %>%
    mutate(perc_sites = round((n / sum(n)*100),1))%>%
    relocate(cat_perc_change, .after = last_col()) %>%
  as_tibble()
}

cat_change <-cat_perc_change(map_all_anlaeg) 


# totale antal prøvesteder denne uge
sum(cat_change$n) 

# HVER REGION ------------------------------------------------

shp_regioner <- st_read("Oplande/regioner.geojson")%>%
  mutate(navn = case_when(navn == "Region SjÃ¦lland" ~ "Region Sjælland",
                          TRUE ~ navn))



map_region <- ww_human %>%
  filter(overlap_level == "1. anlæg",
         date_receipt >= ymd("2022-01-01")) %>%
  aggregate_ww(region) %>%
  filter(measure == "rna_mean_faeces") %>%
  mutate(week = ISOweek(date_receipt)) %>%
  pivot_wider(names_from = measure, values_from = value) %>%
  group_by(region) %>%
  mutate(new_scale = scales::rescale(log_rna_mean_faeces, to = c(0,100)),
         category = case_when(new_scale == 0 ~"Laveste måling hidtil",
                              new_scale == 100 ~"Højeste måling hidtil",
                              new_scale < 10 ~"Meget lavt",
                              new_scale < 20 ~"Lavt",
                              new_scale < 40 ~"Moderat lavt",
                              new_scale < 60 ~"Moderat",
                              new_scale < 80 ~"Moderat højt",
                              new_scale < 90 ~"Højt",
                              new_scale >= 90 ~"Meget højt")) %>%
  ungroup() 

map_region_final <- map_all_anlaeg %>%
  group_by(week, region) %>%
  summarise(
    n_samples_sum = sum(n_samples_sum),
    n_påvist_sum = sum(n_påvist_sum) 
  ) %>%
  left_join(map_region, by = c("week", "region")) %>%
  mutate(category = case_when(n_påvist_sum == 0 ~ "Ikke påvist",
                              TRUE ~ category))
# Data for previous 8 weeks
map_region_plot_8w <- map_region %>%
  filter(week >= ISOweek(today() - days(7)) | 
  week >= ISOweek(today() - days(56)) ) %>%
  left_join(shp_regioner, by = c("region" = "navn")) %>%
  arrange(factor(category, levels = c("Laveste måling hidtil",
                                      "Meget lavt",
                                      "Lavt",
                                      "Moderat lavt",
                                      "Moderat",
                                      "Moderat højt",
                                      "Højt",
                                      "Meget højt",
                                      "Højeste måling hidtil"))) %>%
  st_as_sf()

# Data for the current week
map_region_plot <- map_region %>%
  filter(week >= ISOweek(today() - days(7))) %>%
  left_join(shp_regioner, by = c("region" = "navn")) %>%
  arrange(factor(category, levels = c("Laveste måling hidtil",
                                      "Meget lavt",
                                      "Lavt",
                                      "Moderat lavt",
                                      "Moderat",
                                      "Moderat højt",
                                      "Højt",
                                      "Meget højt",
                                      "Højeste måling hidtil"))) %>%
  st_as_sf()



ggplot() +
  geom_sf(data = dk_kommuner %>%
            st_transform(st_crs(map_region_plot_8w)),
          fill = "grey70", colour = "white") +
  geom_sf(data = map_region_plot_8w,
          aes(fill = category), colour = "white") +
  facet_wrap(~week, nrow = 2)+
  # geom_sf(data = map_all_anlaeg_plot,
  #         aes(fill = factor(category)), colour = "transparent") +
  scale_fill_manual(limits = c(
    "Ikke påvist",
    "Laveste måling hidtil",
    "Meget lavt",
    "Lavt",
    "Moderat lavt",
    "Moderat",
    "Moderat højt",
    "Højt",
    "Meget højt",
    "Højeste måling hidtil"
  ),
  values = c(
    "#2ECC71",
    "#82E0AA",
    "#D6EAF8",
    "#85C1E9",
    "#F7DC6F",
    "#F39C12",
    "#EC7063",
    "#C0392B",
    "#78241C",
    "#001A20"
    
  ),
  labels = c(
    "Ikke påvist",
    "Laveste måling hidtil",
    "Meget lavt",
    "Lavt",
    "Moderat lavt",
    "Moderat",
    "Moderat højt",
    "Højt",
    "Meget højt",
    "Højeste måling hidtil"
  ))+
  theme_void() +
  theme(plot.background = element_rect(fill = "white",
                                       colour = "white"))+
  #legend.position = c(0.9, 0.75)) +
  labs(fill = "Kategori",
       title = glue("Udvikling seneste 8 uger"),
       subtitle = "Regioner")


save_plot(last_plot(),
          "week_{week(today()-days(7))}_8_weeks_maps_region.png",
         #"week_{week(today()-days(7))}_map_region.png",
          folder = glue("S:/Spildevand/outputs/maps/week_{week}",
                        week = week(today()-days(7))))


ggplot() +
  geom_sf(data = dk_kommuner %>%
            st_transform(st_crs(map_region_plot)),
          fill = "grey70", colour = "white") +
  geom_sf(data = map_region_plot,
          aes(fill = category), colour = "white") +
  scale_fill_manual(limits = c(
    "Ikke påvist",
    "Laveste måling hidtil",
    "Meget lavt",
    "Lavt",
    "Moderat lavt",
    "Moderat",
    "Moderat højt",
    "Højt",
    "Meget højt",
    "Højeste måling hidtil"
  ),
  values = c(
    "#2ECC71",
    "#82E0AA",
    "#D6EAF8",
    "#85C1E9",
    "#F7DC6F",
    "#F39C12",
    "#EC7063",
    "#C0392B",
    "#78241C",
    "#001A20"
    
  ),
  labels = c(
    "Ikke påvist",
    "Laveste måling hidtil",
    "Meget lavt",
    "Lavt",
    "Moderat lavt",
    "Moderat",
    "Moderat højt",
    "Højt",
    "Meget højt",
    "Højeste måling hidtil"
  ))+
  theme_void() +
  theme(plot.background = element_rect(fill = "white",
                                       colour = "white"))+
  #legend.position = c(0.9, 0.75)) +
  labs(fill = "Kategori",
       title = glue("SARS-CoV-2 niveau i uge {today}",
                today = week(today()-days(7))),
       subtitle = "Regioner"
)

save_plot(last_plot(),
          "week_{week(today()-days(7))}_map_region.png",
          folder = glue("S:/Spildevand/outputs/maps/week_{week}",
                        week = week(today()-days(7))))


# HVER LANDSDEL ------------------------------------------------

# Read in shapes for landsdele

shp_landsdele <- st_read("Oplande/landsdele-simple.geojson") %>%
  mutate(navn = case_when(navn == "Byen København" ~ "København",
                          TRUE ~ navn))

map_landsdel <- ww_human %>%
  filter(overlap_level == "1. anlæg",
         date_receipt >= ymd("2022-01-01")) %>%
  aggregate_ww(landsdel) %>%
  filter(measure == "rna_mean_faeces") %>%
  mutate(week = ISOweek(date_receipt)) %>%
  pivot_wider(names_from = measure, values_from = value) %>%
  group_by(landsdel) %>%
  mutate(new_scale = scales::rescale(log_rna_mean_faeces, to = c(0,100)),
         category = case_when(new_scale == 0 ~"Laveste måling hidtil",
                              new_scale == 100 ~"Højeste måling hidtil",
                              new_scale < 10 ~"Meget lavt",
                              new_scale < 20 ~"Lavt",
                              new_scale < 40 ~"Moderat lavt",
                              new_scale < 60 ~"Moderat",
                              new_scale < 80 ~"Moderat højt",
                              new_scale < 90 ~"Højt",
                              new_scale >= 90 ~"Meget højt")) %>%
   ungroup() 





map_landsdel_final <- map_all_anlaeg %>%
  group_by(week,landsdel) %>%
  summarise(
    n_samples_sum = sum(n_samples_sum),
    n_påvist_sum = sum(n_påvist_sum) 
  ) %>%
  left_join(map_landsdel, by = c("week", "landsdel")) %>%
  mutate(category = case_when(n_påvist_sum == 0 ~ "Ikke påvist",
                              TRUE ~ category))




# Juster her for plots med flere og hvilke uger
map_landsdel_plot_8w <- map_landsdel %>%
  filter(week >= ISOweek(today() - days(7)) | 
           week >= ISOweek(today() - days(56)) ) %>%
  left_join(shp_landsdele, by = c("landsdel" = "navn")) %>%
  arrange(factor(category, levels = c(    "Laveste måling hidtil",
                                          "Meget lavt",
                                          "Lavt",
                                          "Moderat lavt",
                                          "Moderat",
                                          "Moderat højt",
                                          "Højt",
                                          "Meget højt",
                                          "Højeste måling hidtil"))) %>%
  st_as_sf() 

map_landsdel_plot <- map_landsdel %>%
  filter(week >= ISOweek(today() - days(7))) %>%
  left_join(shp_landsdele, by = c("landsdel" = "navn")) %>%
  arrange(factor(category, levels = c(    "Laveste måling hidtil",
                                          "Meget lavt",
                                          "Lavt",
                                          "Moderat lavt",
                                          "Moderat",
                                          "Moderat højt",
                                          "Højt",
                                          "Meget højt",
                                          "Højeste måling hidtil"))) %>%
  st_as_sf()



ggplot() +
  geom_sf(data = dk_kommuner %>%
            st_transform(st_crs(map_landsdel_plot_8w)),
          fill = "grey70", colour = "white") +
  geom_sf(data = map_landsdel_plot_8w,
          aes(fill = category), colour = "white") +
   facet_wrap(~week, nrow = 2)+ #, labeller = as_labeller(c("16" = "Uge 16","17" = "Uge 17",
                                                                      # "18" = "Uge 18", "19" = "Uge 19",
                                                                      # "20" = "Uge 20", "21" = "Uge 21",
                                                                      # "22" = "Uge 22", "23" = "Uge 23")))+
  # geom_sf(data = map_all_anlaeg_plot,
  #         aes(fill = factor(category)), colour = "transparent") +
  scale_fill_manual(limits = c(
    "Ikke påvist",
    "Laveste måling hidtil",
    "Meget lavt",
    "Lavt",
    "Moderat lavt",
    "Moderat",
    "Moderat højt",
    "Højt",
    "Meget højt",
    "Højeste måling hidtil"
  ),
  values = c(
    "#2ECC71",
    "#82E0AA",
    "#D6EAF8",
    "#85C1E9",
    "#F7DC6F",
    "#F39C12",
    "#EC7063",
    "#C0392B",
    "#78241C",
    "#001A20"
    
  ),
  labels = c(
    "Ikke påvist",
    "Laveste måling hidtil",
    "Meget lavt",
    "Lavt",
    "Moderat lavt",
    "Moderat",
    "Moderat højt",
    "Højt",
    "Meget højt",
    "Højeste måling hidtil"
  ))+
  theme_void() +
  theme(plot.background = element_rect(fill = "white",
                                       colour = "white"))+
        #legend.position = c(0.9, 0.75)) +
  labs(fill = "Kategori",
       title = glue("Udvikling seneste 8 uger"),
       subtitle = "Landsdele")
         #"SARS-CoV-2 niveau i uge {today} - landsdele",
                    #today = week(today()-days(7))))

save_plot(last_plot(),
          "week_{week(today()-days(7))}_8_weeks_maps_landsdel.png",
          #"week_{week(today()-days(7))}_map_landsdel.png",
          folder = glue("S:/Spildevand/outputs/maps/week_{week}",
                        week = week(today()-days(7))))

ggplot() +
  geom_sf(data = dk_kommuner %>%
            st_transform(st_crs(map_landsdel_plot)),
          fill = "grey70", colour = "white") +
  geom_sf(data = map_landsdel_plot,
          aes(fill = category), colour = "white") +
  scale_fill_manual(limits = c(
    "Ikke påvist",
    "Laveste måling hidtil",
    "Meget lavt",
    "Lavt",
    "Moderat lavt",
    "Moderat",
    "Moderat højt",
    "Højt",
    "Meget højt",
    "Højeste måling hidtil"
  ),
  values = c(
    "#2ECC71",
    "#82E0AA",
    "#D6EAF8",
    "#85C1E9",
    "#F7DC6F",
    "#F39C12",
    "#EC7063",
    "#C0392B",
    "#78241C",
    "#001A20"
    
  ),
  labels = c(
    "Ikke påvist",
    "Laveste måling hidtil",
    "Meget lavt",
    "Lavt",
    "Moderat lavt",
    "Moderat",
    "Moderat højt",
    "Højt",
    "Meget højt",
    "Højeste måling hidtil"
  ))+
  theme_void() +
  theme(plot.background = element_rect(fill = "white",
                                       colour = "white"))+
  #legend.position = c(0.9, 0.75)) +
  labs(fill = "Kategori",
       title = glue("SARS-CoV-2 niveau i uge {today}",
                    today = week(today()-days(7))),
       subtitle = "Landsdele")

save_plot(last_plot(),
          "week_{week(today()-days(7))}_map_landsdel.png",
          folder = glue("S:/Spildevand/outputs/maps/week_{week}",
                        week = week(today()-days(7))))


# LANDSDELE individuelt inklusiv anlæg i hver landsdel (occasional) ------------------------------------------------------------

# Shapes for hver landsdel
# 
# shp_for_landsdele <- shp_simple %>%
#   st_simplify(dTolerance = 20) %>%
#   inner_join(lookup_oplande_names %>%
#                filter(!is.na(anlaeg_eurofins)) %>%
#                select(anlaeg_display, anlaeg_shp, overlap_level, landsdel),
#              by = c("anlaeg" = "anlaeg_shp")) %>%
#   mutate(area = st_area(geometry),
#          anlaeg_display = fct_reorder(anlaeg_display, area, .desc = TRUE)) %>%
#   right_join(map_all_anlaeg %>% 
#               filter(week == ISOweek(today() - days(7))) %>%
#               select(anlaeg_rando, week, category), 
#             by = c("anlaeg_display" = "anlaeg_rando"))
# 
# 
# dk_kommuner %>%
#   st_transform(st_crs(shp_landsdele)) %>%
#   st_join(shp_landsdele %>%
#             filter(navn == "Sydjylland")) %>%
#   filter(!is.na(navn))
# 
# 
# shp_kommuner <- dk_kommuner %>%
#   st_transform(st_crs(shp_landsdele)) %>%
#   mutate(geometry = st_point_on_surface(geometry)) %>%
#   select(LAU_NAME, geometry) %>%
#   st_join(shp_landsdele) %>%
#   as_tibble() %>%
#   select(-geometry) %>%
#   left_join(dk_kommuner,
#             by = "LAU_NAME") %>%
#   transmute(kommune = LAU_NAME,
#             landsdel = navn %>%
#               str_remove("^Byen "),
#             geometry) %>%
#   st_as_sf()
# 
# buffer_landsdel <- shp_for_landsdele %>%
#   st_simplify(dTolerance = 100) %>%
#   st_buffer(2000) %>%
#   st_simplify(dTolerance = 200)
# 
# for (curr_landsdel in geogs$landsdele) {
#   bbox <- shp_for_landsdele %>%
#     filter(landsdel == curr_landsdel) %>%
#     st_buffer(2000, nQuadSegs = 0) %>%
#     st_bbox()
#   
#   n_anlaeg <- shp_for_landsdele %>%
#     filter(landsdel == curr_landsdel) %>%
#     pull(anlaeg_display) %>%
#     n_distinct()
#   
#   shp_for_landsdele %>%
#     filter(landsdel == curr_landsdel) %>%
#     ggplot(aes(fill = factor(category)),
#                colour = category) +
#     geom_sf(data = shp_kommuner,
#             inherit.aes = FALSE,
#             colour = "grey85", fill = "grey90") +
#     geom_sf(data = buffer_landsdel %>%
#               filter(landsdel == curr_landsdel),
#             aes(fill = factor(category)),
#             colour = "transparent",
#             alpha = 0.3) +
#     geom_sf() +
#     coord_sf(xlim = c(bbox$xmin, bbox$xmax),
#              ylim = c(bbox$ymin, bbox$ymax),
#              crs = 25832,
#              datum = 25832,
#              expand = TRUE) +
#     scale_fill_manual(breaks = c(
#       "Ikke påvist",
#       "Lowest ever",
#       "0-9%",
#       "10-19%",
#       "20-39%",
#       "40-59%",
#       "60-79%",
#       "80-89%",
#       "90-100%",
#       "Highest ever"
#     ),
#     values = c(
#       "#2ECC71",
#       "#82E0AA",
#       "#D6EAF8",
#       "#85C1E9",
#       "#F7DC6F",
#       "#F39C12",
#       "#EC7063",
#       "#C0392B",
#       "#78241C",
#       "#001A20"
#       
#     ),
#     labels = c(
#       "Ikke påvist",
#       "Laveste måling hidtil",
#       "0-9%",
#       "10-19%",
#       "20-39%",
#       "40-59%",
#       "60-79%",
#       "80-89%",
#       "90-100%",
#       "Højeste måling hidtil"
#     ))+
#     geom_sf_label(aes(label = anlaeg_display),
#                   size = 1.7, colour = "white", alpha = 0.8,
#                   label.padding = unit(0.1, "lines"),
#                   label.r = unit(0, "lines")) +
#     # guides(colour = "none",
#     #        fill = "none") +
#     theme_void() +
#     theme(plot.background = element_rect(
#       fill = "white",
#       colour = "transparent")
#     )+
#     labs(fill = "Kategori",
#          title = glue("{curr_landsdel} - SARS-CoV-2 niveau i uge {today}",
#                       today = week(today()-days(7)),
#                       curr_landsdel = curr_landsdel)) +
#     guides(
#       fill = guide_legend(
#         title = "Kategori",
#         override.aes = aes(label = "")
#       ))
#   
#   #print(last_plot())
#   
#   ggsave(glue("S:/Spildevand/outputs/maps/week_{week}/week_{week(today()-days(7))}_anlaeg-{curr_landsdel}.png",
#               week = week(today()-days(7)),
#               curr_landsdel = curr_landsdel %>%
#                 stringi::stri_trans_general("Latin-ASCII")),
#          width = 4.5, height = 4.5)
# }


# LINEÆR REGRESSION OG BEREGNING AF PROCENTVIS FALD/STIGNING ---------------------------

# Værdier skal log10 tranformeres inden beregningen
# Benyt alle punkter 3 uger tilbage i tid


  
# Test af filter for antal punkter (Sikrer at der er minimum 2 i hver af de foregående to uger)
lin_reg <- ww_human %>%
  mutate(week = ISOweek(date_receipt)) %>%
  select(anlaeg_rando, date_receipt, week, log_rna_mean_faeces, landsdel, region, påvist_mean) %>%
  # Manuelt fjerne NaN værdier da de af en eller anden grund ikke blot er NA
  filter(log_rna_mean_faeces != "NaN", 
         week >= ISOweek(today() - days(21)),
         anlaeg_rando %in% lookup_oplande_names_new$anlaeg_display) %>%
  group_by(anlaeg_rando, week) %>%
  nest() %>%
  mutate(n = map_dbl(data, nrow)) %>%
  filter(n >= 1) %>% #ændre her til at være mindst 1 prøve hver uge - men 4 prøver set over 3 uger?
  unnest(data) %>%
  ungroup() %>%
  group_by(anlaeg_rando)%>%
  nest() %>%
  # Laver data for hvert renseanlæg
  mutate(model = map(data, ~ lm(log_rna_mean_faeces ~ date_receipt, data =.))) %>%
  mutate(tidy = map(model, broom::tidy)) %>%
  unnest(tidy) %>%
  filter(term != "(Intercept)") %>%
  mutate(perc_change_21 = ((10^estimate)-1)*100) %>%
  # Her for antal rækker i hver data - vi vil gerne sikre 4 datapunkter (gerne 2 i hver uge mindst)
  mutate(
        n = map_dbl(data, nrow)) %>%
  filter(n >= 4) %>% # mindst 4 prøver over 3 uger
  arrange(anlaeg_rando) %>%
  ungroup() 

# Make categories for the change
lin_reg_change <- lin_reg %>%
  mutate(category = case_when(perc_change_21 <= -20 ~ "Meget kraftigt fald",
                              perc_change_21 <=  -12.5 ~ "Kraftigt fald",
                              perc_change_21 <=  -7.5 ~ "Moderat fald",
                              perc_change_21 <=  -2.5 ~ "Let fald",
                              perc_change_21 <=  2.5 ~ "Stabilt niveau",
                              perc_change_21 <=  7.5 ~ "Let stigning",
                              perc_change_21 <=  12.5 ~ "Moderat stigning",
                              perc_change_21 <=  20 ~ "Kraftig stigning",
                              perc_change_21 > 20 ~ "Meget kraftig stigning"))  %>%
  select(anlaeg_rando, data, p.value, perc_change_21, category) %>%
  unnest(data) %>%
  distinct(anlaeg_rando, .keep_all = TRUE) %>%
  arrange(category)

# Try to plot this and see how it looks

lin_reg_change_plot <- lin_reg_change %>%
  left_join(lookup_oplande_names %>%
              select(anlaeg_display, anlaeg_shp),
            by = c("anlaeg_rando" = "anlaeg_display")) %>%
  filter(!is.na(anlaeg_shp)) %>%
  left_join(shp_simple, by = c("anlaeg_shp" = "anlaeg")) %>%
  st_as_sf()


buffer_all_anlaeg <- lin_reg_change_plot %>%
  st_simplify(dTolerance = 100) %>%
  st_buffer(2000) %>%
  st_simplify(dTolerance = 200)

# Laver farverne så de tilhører (vhvis vi vil vise alle muligheder)

# cols <- c("-100" = "#1A5276", 
#           "-99% to -10%" = "#5499C7",
#           "-9% to 0%" = "#A9CCE3",
#           "1% to 9%" = "#FCF3CF",
#           "10% to 99%" = "#F39C12",
#           "100% to 99%" = "#D35400",
#           "1000% or more" = "#78241C"
#           )



ggplot() +
  geom_sf(data = dk_kommuner %>%
            st_transform(st_crs(lin_reg_change_plot)),
          fill = "grey70", colour = "white") +
  geom_sf(data = buffer_all_anlaeg,
          aes(fill = category),
          colour = "transparent",
          alpha = 0.3) +
  geom_sf(data = lin_reg_change_plot,
          aes(fill = category), 
          colour = "transparent") +
 # scale_fill_manual(values = cols)+
  scale_fill_manual(limits = c(
    "Meget kraftigt fald",
    "Kraftigt fald",
    "Moderat fald",
    "Let fald",
    "Stabilt niveau",
    "Let stigning",
    "Moderat stigning",
    "Kraftig stigning",
    "Meget kraftig stigning"
  ),
  values = c(
    "#2ECC71",
    "#82E0AA",
    "#D5F5E3",
    "#D6EAF8",
    "#85C1E9",
    "#FCF3CF",
    "#F7DC6F",
    "#F39C12",
    "#C0392B"
  ),
  labels = c(
    "Meget kraftigt fald",
    "Kraftigt fald",
    "Moderat fald",
    "Let fald",
    "Stabilt niveau",
    "Let stigning",
    "Moderat stigning",
    "Kraftig stigning",
    "Meget kraftig stigning"
  ))+
  theme_void() +
  theme(plot.background = element_rect(fill = "white",
                                       colour = "white"),
        legend.position = c(0.9, 0.75)) +
  labs(fill = "Kategori",
       title = glue("Procentvis ændring i SARS-CoV-2 niveau over 3 uger"),
       subtitle = glue("Uge {week}",
                       week = week(today() - days(7))))

save_plot(last_plot(),
          "week_{week(today()-days(7))}_percent-change_all_anlaeg.png",
          folder = glue("S:/Spildevand/outputs/maps/week_{week}",
                        week = week(today()-days(7))))


lin_reg_all_anlaeg_plot <- lin_reg_change %>%
  filter(week == ISOweek(today() - days(21))) %>%
  left_join(lookup_oplande_names %>%
              select(anlaeg_display, anlaeg_shp),
            by = c("anlaeg_rando" = "anlaeg_display")) %>%
  filter(!is.na(anlaeg_shp)) %>%
  left_join(shp_simple_longlat, by = c("anlaeg_shp" = "anlaeg")) %>%
  st_as_sf() %>%
  filter(!st_is_empty(geometry)) %>%
  mutate(category = as.factor(category),
         color = case_when(category ==  "Meget kraftigt fald" ~ "#2ECC71" , 
                           category ==  "Kraftigt fald" ~  "#82E0AA",
                           category ==  "Moderat fald" ~  "#D5F5E3",
                           category ==  "Let fald" ~  "#D6EAF8",
                           category ==  "Stabilt niveau"~ "#85C1E9", 
                           category ==  "Let stigning"~ "#FCF3CF", 
                           category ==  "Moderat stigning"~"#F7DC6F", 
                           category ==  "Kraftig stigning"~"#F39C12", 
                           category ==  "Meget kraftig stigning"~"#C0392B"))  %>%
  arrange(category)

buffer_lin_reg_all_anlaeg <- lin_reg_all_anlaeg_plot %>%
  st_simplify(dTolerance = 100) %>%
  st_buffer(2000) %>%
  st_simplify(dTolerance = 200)


leaflet_lin_reg <-leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  # Non-signal oplande in grey
  addPolygons(
    data = dk_kommuner,
    fillColor = "#DDDDDD",
    fillOpacity = 0.3,
    stroke = TRUE,
    weight = 0.2,
    color = "black") %>%
  # Buffered areas just to help highlight the signals
  addPolygons(
    data = buffer_lin_reg_all_anlaeg %>%
      st_transform('+proj=longlat +datum=WGS84'),
    fillColor = buffer_lin_reg_all_anlaeg$color,
    fillOpacity = 0.25,
    stroke = FALSE,
  ) %>%
  # Signal oplande
  addPolygons(
    data = lin_reg_all_anlaeg_plot,
    fillColor =lin_reg_all_anlaeg_plot$color,
    #fillColor = ~col_pal(map_all_anlaeg_plot$category),
    fillOpacity = 0.6,
    stroke = TRUE,
    weight = 0.2,
    color = "black",
    highlightOptions = highlightOptions(color = "black",
                                        stroke = TRUE, weight = 2,
                                        bringToFront = TRUE),
    label = ~ glue("{anlaeg_rando} : {category}")
  ) #%>%  # add legend to the map
  # addLegend(position = "bottomright", # position where the legend should appear
  #           colors = unique(lin_reg_all_anlaeg_plot$color), # pallete object where the color is defined
  #           labels = unique(lin_reg_all_anlaeg_plot$category), # column variable or values that were used to derive the color pallete object
  #           title = "Kategori", # title of the legend
  #           opacity = 1 # Opacity of legend
  # )

leaflet_lin_reg 

# Save the leaflet signal map
htmlwidgets::saveWidget(
  leaflet_lin_reg,
  glue("S:/Spildevand/outputs/maps/week_{week}", "/week_{week}_kort-lin_reg.html",
      week = week(today()-days(7))),
  selfcontained = TRUE)

# LINEÆR REGRESSION OG BEREGNING AF PROCENTVIS FALD/STIGNING - LANDSDELE ---------------------------

# Lavet ved at tage alle punkter inden for hver landsdel og lave den lineære regression på det. 

shp_landsdele <- st_read("Oplande/landsdele-simple.geojson") %>%
  mutate(navn = case_when(navn == "Byen København" ~ "København",
                          TRUE ~ navn))


# Ved at tage dagligt vægtet gennemsnit 2x ugentligt for hver landsdel - så det er baseret på +- 6 punkter
# Denne går vi med - giver større sikkerhed og flere datapunkter

lin_reg_landsdel_3 <- ww_human %>%
  group_by(date_receipt) %>%
  mutate(weekday = weekdays(date_receipt),
         new_date = case_when(date_receipt <= ymd("2022-07-10") & weekday == "mandag" ~ date_receipt-days(3),
                              date_receipt <= ymd("2022-07-10") & weekday == "torsdag" ~ date_receipt-days(1),
                              date_receipt <= ymd("2022-07-10") & weekday == "lørdag" ~ date_receipt-days(1),
                              date_receipt <= ymd("2022-07-10") & weekday == "søndag" ~ date_receipt-days(2),
                              date_receipt <= ymd("2022-07-10") & weekday == "fredag" & week == "2022-W21" ~ date_receipt-days(2),
                              date_receipt > ymd("2022-07-10") & weekday == "onsdag" ~ date_receipt+days(1),
                              date_receipt > ymd("2022-07-10") & weekday == "fredag" ~ date_receipt-days(1),
                              date_receipt > ymd("2022-07-10") & weekday == "lørdag" ~ date_receipt-days(2),
                              date_receipt > ymd("2022-07-10") & weekday == "søndag" ~ date_receipt-days(3),
                              TRUE ~ date_receipt)) %>%
  ungroup() %>%
  group_by(new_date, landsdel)%>%
  summarise(
    pop_ww = sum(population),
    weighted_t_test = list(suppressWarnings(
      weights::wtd.t.test(log_rna_mean_faeces, NA, weight = population)
    )),
    log_rna_mean_faeces = weighted_t_test[[1]]$additional["Mean"],
    se = weighted_t_test[[1]]$additional["Std. Err"],
    ci_lo = log_rna_mean_faeces - 1.96 * se,
    ci_hi = log_rna_mean_faeces + 1.96 * se,
    .groups = "drop") %>%
  ungroup() %>%
  select(-weighted_t_test) %>%
  filter(log_rna_mean_faeces != "NaN") %>%
  pivot_longer(cols = c("log_rna_mean_faeces"), names_to = "measure", values_to = "value")

lin_reg_landsdel_3_this_week <- lin_reg_landsdel_3 %>%
  mutate(week = ISOweek(new_date)) %>%
  filter(week >= ISOweek(today() - days(21))) %>%
  pivot_wider(names_from = measure, values_from = value)%>%
  group_by(landsdel) %>%
  nest() %>%
  mutate(model = map(data, ~ lm(log_rna_mean_faeces ~ new_date, data =.))) %>%
  mutate(tidy = map(model, broom::tidy)) %>%
  unnest(tidy) %>%
  filter(term != "(Intercept)") %>%
  mutate(perc_change_21 = ((10^estimate)-1)*100) %>%
  mutate(category = case_when(perc_change_21 <= -20 ~ "Meget kraftigt fald",
                              perc_change_21 <=  -12.5 ~ "Kraftigt fald",
                              perc_change_21 <=  -7.5 ~ "Moderat fald",
                              perc_change_21 <=  -2.5 ~ "Let fald",
                              perc_change_21 <=  2.5 ~ "Stabilt niveau",
                              perc_change_21 <=  7.5 ~ "Let stigning",
                              perc_change_21 <=  12.5 ~ "Moderat stigning",
                              perc_change_21 <=  20 ~ "Kraftig stigning",
                              perc_change_21 > 20 ~ "Meget kraftig stigning"))



lin_reg_landsdel_plot <- lin_reg_landsdel_3_this_week %>%
  left_join(shp_landsdele, by = c("landsdel" = "navn")) %>%
  st_as_sf()

buffer_landsdel <- lin_reg_landsdel_plot %>%
  st_simplify(dTolerance = 100) %>%
  st_buffer(2000) %>%
  st_simplify(dTolerance = 200)

ggplot() +
  geom_sf(data = dk_kommuner %>%
            st_transform(st_crs(lin_reg_landsdel_plot)),
          fill = "grey70", colour = "white") +
  # geom_sf(data = buffer_landsdel,
  #         aes(fill = factor(category)),
  #         colour = "transparent",
  #         alpha = 0.3) +
  geom_sf(data = lin_reg_landsdel_plot,
          aes(fill = factor(category)), colour = "white") +
  # facet_wrap(~week)+
  # geom_sf(data = map_all_anlaeg_plot,
  #         aes(fill = factor(category)), colour = "transparent") +
  # scale_fill_manual(values = cols)+
  scale_fill_manual(limits = c(
    "Meget kraftigt fald",
    "Kraftigt fald",
    "Moderat fald",
    "Let fald",
    "Stabilt niveau",
    "Let stigning",
    "Moderat stigning",
    "Kraftig stigning",
    "Meget kraftig stigning"
  ),
  values = c(
    "#2ECC71",
    "#82E0AA",
    "#D5F5E3",
    "#D6EAF8",
    "#85C1E9",
    "#FCF3CF",
    "#F7DC6F",
    "#F39C12",
    "#C0392B"
  ),
  labels = c(
    "Meget kraftigt fald",
    "Kraftigt fald",
    "Moderat fald",
    "Let fald",
    "Stabilt niveau",
    "Let stigning",
    "Moderat stigning",
    "Kraftig stigning",
    "Meget kraftig stigning"
  ))+
  theme_void() +
  theme(plot.background = element_rect(fill = "white",
                                       colour = "white"))+
  geom_sf_label(
    data = lin_reg_landsdel_plot %>%
      mutate(landsdel = case_when(landsdel == "Københavns omegn" ~ "KBH omegn",
                                  TRUE ~ landsdel),
        geometry = st_point_on_surface(geometry)),
    aes(geometry = geometry ,
        label = glue("{round(perc_change_21,2)}%, p: {round(p.value,3)}")),
    colour = "black",
    size = 1.5,
    label.padding = unit(0.12, "lines"),
    show.legend = FALSE,
    alpha = 0.56)+
  #legend.position = c(0.9, 0.75)) +
  labs(fill = "Kategori",
       title = glue("Procentvis ændring i SARS-CoV-2 niveau over 3 uger - Landsdele"),
       subtitle = glue("Uge {week}",
                       week = week(today()-days(7))))


save_plot(last_plot(),
          "week_{week(today()-days(7))}_percent-change_landsdel.png",
          folder = glue("S:/Spildevand/outputs/maps/week_{week}",
                        week = week(today()-days(7))))

# LINEÆR REGRESSION OG BEREGNING AF PROCENTVIS FALD/STIGNING - Regioner ---------------------------


lin_reg_region_3 <- ww_human %>%
  group_by(date_receipt) %>%
  mutate(weekday = weekdays(date_receipt),
         new_date = case_when(date_receipt <= ymd("2022-07-10") & weekday == "mandag" ~ date_receipt-days(3),
                              date_receipt <= ymd("2022-07-10") & weekday == "torsdag" ~ date_receipt-days(1),
                              date_receipt <= ymd("2022-07-10") & weekday == "lørdag" ~ date_receipt-days(1),
                              date_receipt <= ymd("2022-07-10") & weekday == "søndag" ~ date_receipt-days(2),
                              date_receipt <= ymd("2022-07-10") & weekday == "fredag" & week == "2022-W21" ~ date_receipt-days(2),
                              date_receipt > ymd("2022-07-10") & weekday == "onsdag" ~ date_receipt+days(1),
                              date_receipt > ymd("2022-07-10") & weekday == "fredag" ~ date_receipt-days(1),
                              date_receipt > ymd("2022-07-10") & weekday == "lørdag" ~ date_receipt-days(2),
                              date_receipt > ymd("2022-07-10") & weekday == "søndag" ~ date_receipt-days(3),
                              TRUE ~ date_receipt)) %>%
  ungroup() %>%
  group_by(new_date, region)%>%
  summarise(
    pop_ww = sum(population),
    weighted_t_test = list(suppressWarnings(
      weights::wtd.t.test(log_rna_mean_faeces, NA, weight = population)
    )),
    log_rna_mean_faeces = weighted_t_test[[1]]$additional["Mean"],
    se = weighted_t_test[[1]]$additional["Std. Err"],
    ci_lo = log_rna_mean_faeces - 1.96 * se,
    ci_hi = log_rna_mean_faeces + 1.96 * se,
    .groups = "drop") %>%
  ungroup() %>%
  select(-weighted_t_test) %>%
  filter(log_rna_mean_faeces != "NaN") %>%
  pivot_longer(cols = c("log_rna_mean_faeces"), names_to = "measure", values_to = "value")

lin_reg_region_3_this_week <- lin_reg_region_3 %>%
  mutate(week = ISOweek(new_date)) %>%
  filter(week >= ISOweek(today() - days(21))) %>%
  pivot_wider(names_from = measure, values_from = value)%>%
  group_by(region) %>%
  nest() %>%
  mutate(model = map(data, ~ lm(log_rna_mean_faeces ~ new_date, data =.))) %>%
  mutate(tidy = map(model, broom::tidy)) %>%
  unnest(tidy) %>%
  filter(term != "(Intercept)") %>%
  mutate(perc_change_21 = ((10^estimate)-1)*100) %>%
  mutate(category = case_when(perc_change_21 <= -20 ~ "Meget kraftigt fald",
                              perc_change_21 <=  -12.5 ~ "Kraftigt fald",
                              perc_change_21 <=  -7.5 ~ "Moderat fald",
                              perc_change_21 <=  -2.5 ~ "Let fald",
                              perc_change_21 <=  2.5 ~ "Stabilt niveau",
                              perc_change_21 <=  7.5 ~ "Let stigning",
                              perc_change_21 <=  12.5 ~ "Moderat stigning",
                              perc_change_21 <=  20 ~ "Kraftig stigning",
                              perc_change_21 > 20 ~ "Meget kraftig stigning")) 



lin_reg_region_plot <- lin_reg_region_3_this_week %>%
  left_join(shp_regioner, by = c("region" = "navn")) %>%
  st_as_sf()


ggplot() +
  geom_sf(data = dk_kommuner %>%
            st_transform(st_crs(lin_reg_region_plot)),
          fill = "grey70", colour = "white") +
  geom_sf(data = lin_reg_region_plot,
          aes(fill = factor(category)), colour = "white") +
  # facet_wrap(~week)+
  # geom_sf(data = map_all_anlaeg_plot,
  #         aes(fill = factor(category)), colour = "transparent") +
  # scale_fill_manual(values = cols)+
  scale_fill_manual(limits = c(
    "Meget kraftigt fald",
    "Kraftigt fald",
    "Moderat fald",
    "Let fald",
    "Stabilt niveau",
    "Let stigning",
    "Moderat stigning",
    "Kraftig stigning",
    "Meget kraftig stigning"
  ),
  values = c(
    "#2ECC71",
    "#82E0AA",
    "#D5F5E3",
    "#D6EAF8",
    "#85C1E9",
    "#FCF3CF",
    "#F7DC6F",
    "#F39C12",
    "#C0392B"
  ),
  labels = c(
    "Meget kraftigt fald",
    "Kraftigt fald",
    "Moderat fald",
    "Let fald",
    "Stabilt niveau",
    "Let stigning",
    "Moderat stigning",
    "Kraftig stigning",
    "Meget kraftig stigning"
  ))+
  theme_void() +
  theme(plot.background = element_rect(fill = "white",
                                       colour = "white"))+
  geom_sf_label(
    data = lin_reg_region_plot %>%
      mutate(geometry = st_centroid(geometry)),
    aes(geometry = geometry,
        label = glue("{region}: {round(perc_change_21,2)}% \np-værdi: {round(p.value,3)}")),
    colour = "black",
    size = 1.5,
    label.padding = unit(0.12, "lines"),
    show.legend = FALSE,
    alpha = 0.56)+
  #legend.position = c(0.9, 0.75)) +
  labs(fill = "Kategori",
       title = glue("Procentvis ændring i SARS-CoV-2 niveau over 3 uger - Regioner"),
       subtitle = glue("Uge {week}",
                       week = week(today()-days(7))))


save_plot(last_plot(),
          "week_{week(today()-days(7))}_percent-change_regioner.png",
          folder = glue("S:/Spildevand/outputs/maps/week_{week}",
                        week = week(today()-days(7))))


# FUNCTION ----------------------------


this_week <- tibble(
  week = ISOweek(today() - days(7)),
  category = c(            "Meget kraftigt fald",
                           "Kraftigt fald",
                           "Moderat fald",
                           "Let fald",
                           "Stabilt niveau",
                           "Let stigning",
                           "Moderat stigning",
                           "Kraftig stigning",
                           "Meget kraftig stigning"))

previous_week <- tibble(
  week = ISOweek(today() - days(14)),
  category = c(           "Meget kraftigt fald",
                          "Kraftigt fald",
                          "Moderat fald",
                          "Let fald",
                          "Stabilt niveau",
                          "Let stigning",
                          "Moderat stigning",
                          "Kraftig stigning",
                          "Meget kraftig stigning"))

# To find out change of categories this should consist of 3 weeks before this week
lin_reg_landsdel_3_last_week <- lin_reg_landsdel_3 %>%
  mutate(week = ISOweek(new_date)) %>%
  filter(week >= ISOweek(today() - days(28)),
         week <= ISOweek(today() - days(14))) %>%
  pivot_wider(names_from = measure, values_from = value)%>%
  group_by(landsdel) %>%
  nest() %>%
  mutate(model = map(data, ~ lm(log_rna_mean_faeces ~ new_date, data =.))) %>%
  mutate(tidy = map(model, broom::tidy)) %>%
  unnest(tidy) %>%
  filter(term != "(Intercept)") %>%
  mutate(perc_change_21 = ((10^estimate)-1)*100) %>%
  mutate(category = case_when(perc_change_21 <= -20 ~ "Meget kraftigt fald",
                              perc_change_21 <=  -12.5 ~ "Kraftigt fald",
                              perc_change_21 <=  -7.5 ~ "Moderat fald",
                              perc_change_21 <=  -2.5 ~ "Let fald",
                              perc_change_21 <=  2.5 ~ "Stabilt niveau",
                              perc_change_21 <=  7.5 ~ "Let stigning",
                              perc_change_21 <=  12.5 ~ "Moderat stigning",
                              perc_change_21 <=  20 ~ "Kraftig stigning",
                              perc_change_21 > 20 ~ "Meget kraftig stigning"))


lin_reg_landsdel_this_week <- lin_reg_landsdel_3_this_week %>%
  select(landsdel, category) %>%
  group_by(category) %>%
  summarise(n = n()) %>%
  full_join(this_week) %>%
  mutate(n = replace_na(n,0)) %>%
  arrange(category)
  

lin_reg_landsdel_previous_week <- lin_reg_landsdel_3_last_week %>%
  select(landsdel, category) %>%
  group_by(category) %>%
  summarise(n = n()) %>%
  full_join(previous_week) %>%
  mutate(n = replace_na(n,0)) %>%
  arrange(category)
  
lin_reg_two_weeks_landsdel <- lin_reg_landsdel_previous_week %>%
  bind_rows(lin_reg_landsdel_this_week) %>%
  arrange(category, week) %>%
    mutate(cat_perc_change = round(((lead(n) - n) / n *100),1)) %>%
    filter(week == ISOweek(today() - days(14))) %>%
    mutate(cat_perc_change = case_when(cat_perc_change == Inf ~ 100,
                                       cat_perc_change == "NaN" ~ 0,
                                       TRUE ~ cat_perc_change)) %>%
    select(-n,-week)%>%
  full_join(lin_reg_landsdel_this_week) %>%
  select(-week) %>%
    mutate(perc_sites = round((n / sum(n)*100),1))%>%
    relocate(cat_perc_change, .after = last_col()) %>%
  mutate(category = fct_relevel(as.factor(category), c(    "Meget kraftigt fald",
                                                           "Kraftigt fald",
                                                           "Moderat fald",
                                                           "Let fald",
                                                           "Stabilt niveau",
                                                           "Let stigning",
                                                           "Moderat stigning",
                                                           "Kraftig stigning",
                                                           "Meget kraftig stigning")))%>%
  arrange(category)%>%
    as_tibble()


# For hver region

lin_reg_region_3_last_week <- lin_reg_region_3 %>%
  mutate(week = ISOweek(new_date)) %>%
  filter(week >= ISOweek(today() - days(28)),
         week <= ISOweek(today() - days(14))) %>%
  pivot_wider(names_from = measure, values_from = value)%>%
  group_by(region) %>%
  nest() %>%
  mutate(model = map(data, ~ lm(log_rna_mean_faeces ~ new_date, data =.))) %>%
  mutate(tidy = map(model, broom::tidy)) %>%
  unnest(tidy) %>%
  filter(term != "(Intercept)") %>%
  mutate(perc_change_21 = ((10^estimate)-1)*100) %>%
  mutate(category = case_when(perc_change_21 <= -20 ~ "Meget kraftigt fald",
                              perc_change_21 <=  -12.5 ~ "Kraftigt fald",
                              perc_change_21 <=  -7.5 ~ "Moderat fald",
                              perc_change_21 <=  -2.5 ~ "Let fald",
                              perc_change_21 <=  2.5 ~ "Stabilt niveau",
                              perc_change_21 <=  7.5 ~ "Let stigning",
                              perc_change_21 <=  12.5 ~ "Moderat stigning",
                              perc_change_21 <=  20 ~ "Kraftig stigning",
                              perc_change_21 > 20 ~ "Meget kraftig stigning"))


lin_reg_region_this_week <- lin_reg_region_3_this_week %>%
  select(region, category) %>%
  group_by(category) %>%
  summarise(n = n()) %>%
  full_join(this_week) %>%
  mutate(n = replace_na(n,0)) %>%
  arrange(category)


lin_reg_region_previous_week <- lin_reg_region_3_last_week %>%
  select(region, category) %>%
  group_by(category) %>%
  summarise(n = n()) %>%
  full_join(previous_week) %>%
  mutate(n = replace_na(n,0)) %>%
  arrange(category)

lin_reg_two_weeks_region <- lin_reg_region_previous_week %>%
  bind_rows(lin_reg_region_this_week) %>%
  arrange(category, week) %>%
  mutate(cat_perc_change = round(((lead(n) - n) / n *100),1)) %>%
  filter(week == ISOweek(today() - days(14))) %>%
  mutate(cat_perc_change = case_when(cat_perc_change == Inf ~ 100,
                                     cat_perc_change == "NaN" ~ 0,
                                     TRUE ~ cat_perc_change)) %>%
  select(-n,-week)%>%
  full_join(lin_reg_region_this_week) %>%
  select(-week) %>%
  mutate(perc_sites = round((n / sum(n)*100),1))%>%
  relocate(cat_perc_change, .after = last_col()) %>%
  mutate(category = fct_relevel(as.factor(category), c(    "Meget kraftigt fald",
                                                           "Kraftigt fald",
                                                           "Moderat fald",
                                                           "Let fald",
                                                           "Stabilt niveau",
                                                           "Let stigning",
                                                           "Moderat stigning",
                                                           "Kraftig stigning",
                                                           "Meget kraftig stigning")))%>%
  arrange(category)%>%
  as_tibble()

# For hvert anlæg

lin_reg_anlaeg_last_week <- ww_human %>%
  select(anlaeg_rando, date_receipt, week, log_rna_mean_faeces, landsdel, påvist_mean) %>%
  # Manuelt fjerne NaN værdier da de af en eller anden grund ikke blot er NA
  filter(log_rna_mean_faeces != "NaN", 
         week >= ISOweek(today() - days(28)),
         week <= ISOweek(today() - days(14))) %>%
  group_by(anlaeg_rando, week) %>%
  nest() %>%
  mutate(n = map_dbl(data, nrow)) %>%
  filter(n >= 1) %>%
  unnest(data) %>%
  ungroup() %>%
  group_by(anlaeg_rando)%>%
  nest() %>%
  # Laver data for hvert renseanlæg
  mutate(model = map(data, ~ lm(log_rna_mean_faeces ~ date_receipt, data =.))) %>%
  mutate(tidy = map(model, broom::tidy)) %>%
  unnest(tidy) %>%
  filter(term != "(Intercept)") %>%
  mutate(perc_change_21 = ((10^estimate)-1)*100) %>%
  # Her for antal rækker i hver data - vi vil gerne sikre 4 datapunkter (gerne 2 i hver uge mindst)
  mutate(
    n = map_dbl(data, nrow)) %>%
  filter(n >= 4) %>%
  arrange(anlaeg_rando) %>%
  ungroup() %>%
  mutate(category = case_when(perc_change_21 <= -20 ~ "Meget kraftigt fald",
                              perc_change_21 <=  -12.5 ~ "Kraftigt fald",
                              perc_change_21 <=  -7.5 ~ "Moderat fald",
                              perc_change_21 <=  -2.5 ~ "Let fald",
                              perc_change_21 <=  2.5 ~ "Stabilt niveau",
                              perc_change_21 <=  7.5 ~ "Let stigning",
                              perc_change_21 <=  12.5 ~ "Moderat stigning",
                              perc_change_21 <=  20 ~ "Kraftig stigning",
                              perc_change_21 > 20 ~ "Meget kraftig stigning"))


lin_reg_anlaeg_this_week <- lin_reg_change %>%
  select(anlaeg_rando, category) %>%
  group_by(category) %>%
  summarise(n = n()) %>%
  full_join(this_week) %>%
  mutate(n = replace_na(n,0)) %>%
  arrange(category)


lin_reg_anlaeg_previous_week <- lin_reg_anlaeg_last_week %>%
  select(anlaeg_rando, category) %>%
  group_by(category) %>%
  summarise(n = n()) %>%
  full_join(previous_week) %>%
  mutate(n = replace_na(n,0)) %>%
  arrange(category)

lin_reg_two_weeks_anlaeg <- lin_reg_anlaeg_previous_week %>%
  bind_rows(lin_reg_anlaeg_this_week) %>%
  arrange(category, week) %>%
  mutate(cat_perc_change = round(((lead(n) - n) / n *100),1)) %>%
  filter(week == ISOweek(today() - days(14))) %>%
  mutate(cat_perc_change = case_when(cat_perc_change == Inf ~ 100,
                                     cat_perc_change == "NaN" ~ 0,
                                     TRUE ~ cat_perc_change)) %>%
  select(-n,-week)%>%
  full_join(lin_reg_anlaeg_this_week) %>%
  select(-week) %>%
  mutate(perc_sites = round((n / sum(n)*100),1))%>%
  relocate(cat_perc_change, .after = last_col()) %>%
  mutate(category = fct_relevel(as.factor(category), c(    "Meget kraftigt fald",
                                                           "Kraftigt fald",
                                                           "Moderat fald",
                                                           "Let fald",
                                                           "Stabilt niveau",
                                                           "Let stigning",
                                                           "Moderat stigning",
                                                           "Kraftig stigning",
                                                           "Meget kraftig stigning")))%>%
  arrange(category)%>%
  as_tibble()




# PLOTS AF LINEÆR REGRESSION -----------------------------------


# lin_reg1 <- ww_human %>%
#   mutate(week = ISOweek(date_receipt)) %>%
#   select(anlaeg_rando, date_receipt, week, rna_normalised_faeces, landsdel, påvist_mean) %>%
#   # Manuelt fjerne NaN værdier da de af en eller anden grund ikke blot er NA
#   filter(rna_normalised_faeces != "NaN", 
#          week >= ISOweek(today() - days(21)),
#          landsdel == "København") %>%
#   mutate(rna_faeces_log = log10(rna_normalised_faeces))
# 
# 
# ggplot(data = lin_reg1,
#        aes(date_receipt, rna_faeces_log)) +
#   geom_point()+
#   geom_smooth(method = "lm", se = FALSE)
# 
# 
# fit <- lm(rna_faeces_log ~ date_receipt, data = lin_reg1)
# 
# summary(fit)
# broom::tidy(fit)
# par(mfrow = c(2, 2))
# plot(fit)

#LAV INTERAKTIVT MAP så man kan zoome ind og se hvilket anlæg det er. Som tab til de andre maps (måske kun for alle anlæg)
# Ændre procentopdelingen så den løber fra -50 til +50% ændring over 3 uger og se hvordan det ser ud. Eventuelt så opdele i 10% eller mere?

# TO CREATE WEBSITE
# rmarkdown::render(
#   "R/wip/test_CDC.R",
#   output_file = paste0(glue("S:/Spildevand/outputs/maps/week_{week}/", week = week(today()-days(7))), 
#                        Sys.Date(), "-CDC_report.html")
# )
