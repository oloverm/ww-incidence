# Combine anlæg shapefiles ------------------------------------------------

# Read/clean lookup
source("R/02-1_read-clean-lookup.R", encoding = 'UTF-8')
source("R/00-2_functions-shp.R", encoding = "utf-8")

dk_kommuner <- st_read(here("Oplande/dk_kommuner.geojson"), quiet = TRUE)


individual_shapes <- list.files("Oplande/cleaned", pattern = "geojson$",
                                recursive = FALSE, full.names = TRUE) %>%
  set_names(str_remove_all, ".+/|\\.geojson$") %>%
  map2(names(.),
       ~ st_read(.x, quiet = TRUE) %>% mutate(company = .y)) 



shp_anlaeg <- individual_shapes %>%
  bind_rows() %>%
  relocate(company) %>%
  mutate(anlaeg = fct_shuffle(anlaeg)) %>%
  arrange(anlaeg) %>%
  st_simplify() %>%
  st_zm() %>%
  st_make_valid() %>%
  as_tibble() %>%
  st_as_sf()



# Write combined shape ----------------------------------------------------

shp_anlaeg %>%
  arrange(company, anlaeg) %>%
  st_write(glue("Oplande/all-anlaeg_{today()}.geojson"),
           delete_dsn = TRUE)

# Also save to the DIAS drive for Anders to get populations
shp_anlaeg %>%
  arrange(company, anlaeg) %>%
  st_write(glue("Q:/SSI/Spildevand/Populations from shapes/input/all-anlaeg_{today()}.geojson"),
           delete_dsn = TRUE)


# Simplified shape for quicker mapping
shp_simple <- shp_anlaeg %>%
  st_simplify(dTolerance = 5) %>%
  st_buffer(50) %>%
  st_simplify(dTolerance = 100)

object.size(shp_anlaeg)
object.size(shp_simple)

st_write(shp_simple,
         "Oplande/all-anlaeg-simple.geojson",
         delete_dsn = TRUE)
st_write(shp_simple,
         "Oplande/all-anlaeg-simple-mikkel.geojson",
         delete_dsn = TRUE)


# And write Excel list of oplande
shp_anlaeg %>%
  as_tibble() %>%
  select(-geometry) %>%
  mutate(anlaeg = as.character(anlaeg)) %>%
  arrange(company, anlaeg) %>%
  write_xlsx("Oplande/forsyninger_anlaeg.xlsx")





## EXTRA MANUAL WORK ---------------------------------------------------------

# If you've just added new shapes, you need to add them to the lookup so they
# are included in the plots. Do this by running through lookup_help.R, and
# manually adding to the lookup according to the output you get there.


# Then reread the updated lookup:
source("R/02-1_read-clean-lookup.R",
       encoding = 'UTF-8')



## Only used shapes --------------------------------------------------------

shp_simple %>%
  inner_join(lookup_oplande_names %>%
               filter(!is.na(anlaeg_eurofins),
                      in_use == TRUE | is.na(in_use)) %>%
               select(anlaeg_shp, anlaeg_display),
             by = c("anlaeg" = "anlaeg_shp")) %>%
  st_write("Oplande/used-anlaeg-simple.geojson",
         delete_dsn = TRUE)



shp_simple_inlookup <- st_read("Oplande/used-anlaeg-simple.geojson",
                               quiet = TRUE) %>%
  mutate(area = st_area(geometry),
         anlaeg = fct_reorder(anlaeg, sample(1:nrow(.), replace = FALSE))) %>%
  select(-area)


shp_simple_inlookup %>%
  mutate(geometry = st_centroid(geometry)) %>%
  select(anlaeg_display) %>%
  st_write("Oplande/used-anlaeg-points.geojson",
           delete_dsn = TRUE)


# Plots -------------------------------------------------------------------



crop_coords_dk <- st_sfc(
  # Bottom-left
  st_point(c(8, 54.5)),
  # Top-right
  st_point(c(15.8, 57.7)),
  # Default coordinate system
  crs = 4326
) %>%
  st_transform(25832) %>%
  st_coordinates() %>%
  as_tibble()



## Everything --------------------------------------------------------------




shp_simple_inlookup %>%
  map_anlaeg_busy(dk_kommuner, crop_coords_dk) %>%
  ggsave("Oplande/all_anlaeg.png",
         .,
         width = 6, height = 4.5, scale = 3, dpi = 300)







## Maps for display --------------------------------------------------------


### DK map no labels ---------------------------------------------------




shp_simple_inlookup %>%
  map_anlaeg_nolabels(dk_kommuner = dk_kommuner,
               crop_coords_dk = crop_coords_dk) %>%
  ggsave("Oplande/all_anlaeg_nonames.png",
         .,
         width = 6, height = 4.5)








### Landsdele ---------------------------------------------------------------

shp_for_landsdele <- shp_anlaeg %>%
  st_simplify(dTolerance = 20) %>%
  inner_join(lookup_oplande_names %>%
               filter(!is.na(anlaeg_eurofins)) %>%
               select(anlaeg_display, anlaeg_shp, overlap_level, landsdel),
             by = c("anlaeg" = "anlaeg_shp")) %>%
  mutate(area = st_area(geometry),
         anlaeg_display = fct_reorder(anlaeg_display, area, .desc = TRUE))

shp_landsdele <- st_read("Oplande/landsdele-simple.geojson")

dk_kommuner %>%
  st_transform(st_crs(shp_landsdele)) %>%
  st_join(shp_landsdele %>%
            filter(navn == "Sydjylland")) %>%
  filter(!is.na(navn))


shp_kommuner <- dk_kommuner %>%
  st_transform(st_crs(shp_landsdele)) %>%
  mutate(geometry = st_point_on_surface(geometry)) %>%
  select(LAU_NAME, geometry) %>%
  st_join(shp_landsdele) %>%
  as_tibble() %>%
  select(-geometry) %>%
  left_join(dk_kommuner,
            by = "LAU_NAME") %>%
  transmute(kommune = LAU_NAME,
            landsdel = navn %>%
              str_remove("^Byen "),
            geometry) %>%
  st_as_sf()



for (curr_landsdel in geogs$landsdele) {
  bbox <- shp_for_landsdele %>%
    filter(landsdel == curr_landsdel) %>%
    st_buffer(2000, nQuadSegs = 0) %>%
    st_bbox()

  n_anlaeg <- shp_for_landsdele %>%
    filter(landsdel == curr_landsdel) %>%
    pull(anlaeg_display) %>%
    n_distinct()

  shp_for_landsdele %>%
    filter(landsdel == curr_landsdel) %>%
    ggplot(aes(fill = anlaeg_display,
               colour = anlaeg_display)) +
    geom_sf(data = shp_kommuner,
            inherit.aes = FALSE,
            colour = "grey85", fill = "grey90") +
    geom_sf() +
    coord_sf(xlim = c(bbox$xmin, bbox$xmax),
             ylim = c(bbox$ymin, bbox$ymax),
             crs = 25832,
             datum = 25832,
             expand = TRUE) +
    scale_fill_manual(aesthetics = c("fill", "colour"),
                      values = viridis::turbo(n_anlaeg)) +
    geom_sf_label(aes(label = anlaeg_display),
                  size = 1.7, colour = "white", alpha = 0.8,
                  label.padding = unit(0.1, "lines"),
                  label.r = unit(0, "lines")) +
    guides(colour = "none",
           fill = "none") +
    theme_void() +
    theme(plot.background = element_rect(
      fill = "white",
      colour = "transparent")
    )

  # print(last_plot())

  ggsave(glue("Oplande/maps/landsdel-anlaeg-{curr_landsdel}.png",
              curr_landsdel = curr_landsdel %>%
                stringi::stri_trans_general("Latin-ASCII")),
         width = 4.5, height = 4.5)
}









### Biofos ------------------------------------------------------------------

crop_coords_biofos <- st_sfc(
  # Bottom-left
  st_point(c(12.13, 55.58)),
  # Top-right
  st_point(c(12.72, 55.81)),
  # Default coordinate system
  crs = 4326
) %>%
  st_transform(25832) %>%
  st_coordinates() %>%
  as_tibble()


ggplot() +
  # Denmark background
  geom_sf(data = dk_kommuner,
          colour = "grey80", fill = "grey90") +
  # Pumpestationer
  geom_sf(data = shp_simple_inlookup %>%
            filter(company == "biofos",
                   !anlaeg_display %in% c("Hvidovre (Avedøre)",
                                          "København (Lynetten)",
                                          "Lynetten (Strandvænget)",
                                          "Lynetten (Kløvermarken)",
                                          "København (Damhusåen)")),
          aes(fill = anlaeg_display), colour = "white",
          alpha = 0.6,
          show.legend = FALSE) +
  # Anlæg
  geom_sf(data = shp_simple_inlookup %>%
            filter(anlaeg_display %in% c("Hvidovre (Avedøre)",
                                         "København (Lynetten)",
                                         "København (Damhusåen)")),
          aes(colour = anlaeg_display), fill = "transparent",
          size = 1,
          show.legend = FALSE) +
  # Pumpestationer labels
  geom_sf_label(
    data = shp_simple_inlookup %>%
      filter(company == "biofos",
             !anlaeg_display %in% c("Hvidovre (Avedøre)",
                                    "København (Lynetten)",
                                    "Lynetten (Strandvænget)",
                                    "Lynetten (Kløvermarken)",
                                    "København (Damhusåen)")),
    aes(geometry = geometry,
        fill = anlaeg_display,
        label = anlaeg_display),
    nudge_x = 800,
    nudge_y = -500,
    colour = "white",
    alpha = 0.8,
    size = 3,
    # label.size = 0,
    show.legend = FALSE) +
  # Anlæg labels
  geom_sf_label(
    data = shp_simple_inlookup %>%
      filter(anlaeg_display %in% c("Hvidovre (Avedøre)",
                                   "København (Lynetten)",
                                   "København (Damhusåen)")),
    aes(geometry = geometry,
        colour = anlaeg_display,
        label = anlaeg_display),
    nudge_x = -2300,
    nudge_y = 9000,
    label.size = 0.7,
    fill = "grey90",
    alpha = 0.6,
    size = 3,
    show.legend = FALSE) +
  scale_colour_manual(values = c("royalblue", "orange", "forestgreen")) +
  theme_void() +
  theme(plot.background = element_rect(
    fill = "#ebf9ff",
    colour = "transparent")
  ) +
  coord_sf(xlim = crop_coords_biofos$X,
           ylim = crop_coords_biofos$Y,
           crs = 25832,
           datum = 25832,
           expand = FALSE)

ggsave("Oplande/anlaeg_biofos.png",
       width = 6, height = 4.61)






