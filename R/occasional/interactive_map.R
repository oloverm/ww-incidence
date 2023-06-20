

shp_nicco <- shp_oplande %>%
  st_simplify(dTolerance = 5) %>%
  mutate(area = st_area(geometry),
         anlaeg = fct_reorder(anlaeg, area, .desc = TRUE)) %>%
  arrange(anlaeg) %>%
  st_transform('+proj=longlat +datum=WGS84') %>%
  print()

library(leaflet)

map_nicco <- leaflet() %>%
  addProviderTiles(providers$OpenStreetMap.Mapnik) %>%
  addPolygons(
    data = shp_nicco,
    # fillColor = ~ pal_signals(grading),
    fillOpacity = 0.3,
    stroke = TRUE,
    weight = 0.2,
    color = "black",
    highlightOptions = highlightOptions(color = "black",
                                        stroke = TRUE, weight = 2,
                                        bringToFront = FALSE),
    label = ~ anlaeg
  )



# Save the leaflet signal map
htmlwidgets::saveWidget(
  map_nicco,
  glue(here("Oplande"), "/{today()}_kort-oplande.html"),
  selfcontained = TRUE
)

