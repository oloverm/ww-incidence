# Test shapefiles

# Setup -------------------------------------------------------------------
pacman::p_install(smoothr, force = FALSE)
pacman::p_install(here, force = FALSE)

here::i_am("R/occasional/check_shapes.R")

pacman::p_load(
  here,
  dplyr,
  sf,
  ggplot2,
  stringr,
  purrr,
  forcats,
  glue,
  cli,
  writexl,
  lubridate,
  conflicted
)
conflict_prefer("filter", "dplyr", quiet = TRUE)
theme_set(theme_void())

source("R/00-2_functions-shp.R", encoding = "utf-8")




# Aarhus ------------------------------------------------------------------
aarhus <- auto_read_shp(
  "Oplande/raw/Aarhus_vand",
  "Aarhusvand_oplande_20210610",
  anlaeg_col = NAVN
) %>%
  mutate(anlaeg = glue("Aarhus ({anlaeg})",
                       anlaeg = str_to_title(anlaeg)))

plot_anlaeg(aarhus)

st_save_cleaned(aarhus)



## Aarhus decentral --------------------------------------------------------

aarhus_decentral_gellerup <- auto_read_shp(
  "S:/Spildevand/National overvågning/Modtagne_GIS-filer/Aarhus/Decentral prøvetagning_Aarhus",
  "Gellerup_sogn",
  collapse = "Gellerup"
)

aarhus_decentral_helligånds <- auto_read_shp(
  "S:/Spildevand/National overvågning/Modtagne_GIS-filer/Aarhus/Decentral prøvetagning_Aarhus",
  "Helligaands_sogn",
  collapse = "Helligånds"
)

aarhus_decentral_rosenhøj <- auto_read_shp(
  "S:/Spildevand/National overvågning/Modtagne_GIS-filer/Aarhus/Decentral prøvetagning_Aarhus",
  "Rosenhoej",
  collapse = "Rosenhøj"
)


aarhus_decentral <- bind_rows(aarhus_decentral_gellerup,
                              aarhus_decentral_helligånds,
                              aarhus_decentral_rosenhøj)


plot_anlaeg(aarhus_decentral)

st_save_cleaned(aarhus_decentral)


# DIN Forsyning ------ ----------------------------------------------------

## Esbjerg Kommune ---------------------------------------------------------

# This is the one where water from BrammingNord sometimes gets pumped into
# EsbjergØst. But now, after pilot phase, BrammingNord itself is a measuring
# place.

auto_esbjerg_kom <- auto_read_shp(
  "S:/Spildevand/National overvågning/Modtagne_GIS-filer/Dinforsyning/Tilmikkel",
  "OplandTilRenseanleag",
  anlaeg_col = Renseanlae,
  holes_threshold_m2 = NULL
)

plot_anlaeg(auto_esbjerg_kom)


## Varde Kommune -----------------------------------------------------------

auto_varde_kom <- auto_read_shp(
  "S:/Spildevand/National overvågning/Modtagne_GIS-filer/Dinforsyning/Tilmikkel",
  "Oplande_TilRenseanlæg_Varde",
  anlaeg_col = Renseanlae,
  holes_threshold_m2 = NULL
)


## Combine DIN Forsyning -----------------------------------------------------
auto_both_dinforsyning <- bind_rows(auto_esbjerg_kom,
                                    auto_varde_kom) %>%
  mutate(anlaeg = str_rm_renseanlaeg(anlaeg))

plot_anlaeg(auto_both_dinforsyning)

auto_both_dinforsyning %>%
  as_tibble() %>%
  pull(anlaeg) %>%
  unique()

dinforsyning <- auto_both_dinforsyning %>%
  mutate(anlaeg = recode(anlaeg,
                         "Oest" = "Esbjerg Øst",
                         "Vest" = "Esbjerg Vest")) %>%
  filter(anlaeg %in% c("Bramming Nord",
                       "Esbjerg Øst",
                       "Esbjerg Vest",
                       "Varde",
                       "Nr. Nebel",
                       "Outrup",
                       "Ribe",
                       "Skovlund")) %>%
  st_buffer(25, nQuadSegs = 2) %>%
  st_simplify(dTolerance = 20) %>%
  st_fill_holes(10000)


plot_anlaeg(dinforsyning)

object.size(dinforsyning)


st_save_cleaned(dinforsyning)




## Jerne (Esbjerg decentral) -----------------------------------------------

esbjerg_decentral <- auto_read_shp("S:/Spildevand/National overvågning/Modtagne_GIS-filer/Dinforsyning/Shapefile_graense_og_oplande_F61S010",
                                   "Graense_for_oplande_til_broend_F61S010",
                                   collapse = "Jerne")

plot_anlaeg(esbjerg_decentral)

st_save_cleaned(esbjerg_decentral)



# Horsens -----------------------------------------------------------------


horsens <- auto_read_shp("Oplande/raw/Horsens",
                         "Opland til Horsens Central Rens",
                         collapse = "Horsens")

plot_anlaeg(horsens)

st_save_cleaned(horsens)



# Sønderborg --------------------------------------------------------------

st_layers("Oplande/raw/sonfor_spildevandsoplande")


raw_sønderborg <- st_read("Oplande/raw/sonfor_spildevandsoplande",
                          "sonfor_spildevandsoplande", quiet = TRUE)
sønderborg <- auto_read_shp("Oplande/raw/sonfor_spildevandsoplande",
                            "sonfor_spildevandsoplande") %>%
  mutate(anlaeg = glue("Sønderborg ({anlaeg})",
                       anlaeg = anlaeg %>%
                         str_remove("RENSEANLÆG ") %>%
                         str_to_title()))

plot_anlaeg(sønderborg)

st_save_cleaned(sønderborg)



# Thisted ----------------------------------------------------------------


thisted <- auto_read_shp("Oplande/raw/Thisted Renseanlæg_ESRI_140621",
                         "Thisted Renseanlæg_oplande_region",
                         collapse = "Thisted")

plot_anlaeg(thisted)

st_save_cleaned(thisted)

# BIOFOS/HOFOR -------------------------------------------------------------

st_layers("Oplande/raw/BIOFOS/BIOFOS_opland_renseanlæg_Avedøre og Lynetten")


## Avedøre and pumpestationer ----------------------------------------------

raw_avedøre <- st_read(
  "Oplande/raw/BIOFOS/BIOFOS_opland_renseanlæg_Avedøre og Lynetten",
  "BIOFOS_opland_renseanlæg_Avedøre"
)

raw_avedøre %>% check_crs()

avedøre <- raw_avedøre %>%
  correct_crs() %>%
  st_collapse_features("Avedøre")


# The sub-areas for the Avedøre pumpestationer were already in the file
avedøre_pumpestation <- raw_avedøre %>%
  filter(!is.na(Pumpest)) %>%
  transmute(anlaeg = paste(Pumpest, "(Avedøre)"))




## Lynetten and pumpestationer ---------------------------------------------

raw_lynetten <- st_read(
  "Oplande/raw/BIOFOS/BIOFOS_opland_renseanlæg_Avedøre og Lynetten",
  "BIOFOS_opland_renseanlæg_Lynetten"
)

check_crs(raw_lynetten)

lynetten <- raw_lynetten %>%
  correct_crs() %>%
  select(anlaeg = anlæg) %>%
  as_tibble() %>%
  st_as_sf()


# Avedøre and Lynetten are the two treatment plants. But for the ØU-sag we're
# also looking at two intermediate pumping stations each. The Lynetten ones are
# in the file we got from Ida.
raw_kløvermarken <- st_read("S:/Spildevand/Københavns Kommune/Oplande_og_maalerbroende/2021-06-25_oplande-f2.geojson", quiet = TRUE)

check_crs(raw_kløvermarken)

kløvermarken <- raw_kløvermarken %>%
  mutate(anlaeg = recode(name,
                         "KBH_Centrum" = "Kløvermarken (Lynetten)")) %>%
  filter(anlaeg == "Kløvermarken (Lynetten)") %>%
  select(anlaeg)


# Strandvænget is bigger than KBH_Nord, which Ida said originally was the same
st_layers("Oplande/raw/BIOFOS/kk2_2021-07-02_ida")
raw_strandvænget <- st_read("Oplande/raw/BIOFOS/kk2_2021-07-02_ida",
                            "Opland_Strandvænget", quiet = TRUE)

strandvænget <- raw_strandvænget %>%
  st_collapse_features(anlaeg = "Strandvænget (Lynetten)")

lynetten_pumpestation <- bind_rows(
  kløvermarken,
  strandvænget
)





### Nordre tilløb ----------------------------------------------------------

# Oops! Turns out they were actually taking the sample from Nordre tilløb rather
# than Strandvænget. Nordre tilløb also includes Gentofte. This is for the
# ØU-sag at least, but maybe also Copenhagen? Anyway, new shapes from Barbara

lynetten_tilløb <- auto_read_shp(
  "Oplande/raw/Lynetten_tilløb_2021-09-08",
  anlaeg_col = anlæg
) %>%
  mutate(anlaeg = recode(
    anlaeg,
    "Lynetten- nordre tilløb" = "Lynetten (nordre tilløb)",
    "Lynetten- søndre tilløb" = "Lynetten (søndre tilløb)"
  ))

plot_anlaeg(lynetten_tilløb)




## Damhusåen ---------------------------------------------------------------

damhusåen <- auto_read_shp("S:/Spildevand/National overvågning/Modtagne_GIS-filer/BIOFOS/Opland_renseanlaeg_Damhuaen",
                           anlaeg_col = Navn)

plot_anlaeg(damhusåen)




# Dragør ------------------------------------------------------------------

dragør <- auto_read_shp(
  "S:/Spildevand/National overvågning/Modtagne_GIS-filer/BIOFOS/Dragør",
  collapse = "Dragør"
)





## Combine BIOFOS ----------------------------------------------------------

biofos <- bind_rows(
  avedøre,
  lynetten,
  # lynetten_pumpestation,
  lynetten_tilløb,
  avedøre_pumpestation,
  damhusåen,
  dragør
) %>%
  mutate(area = st_area(geometry)) %>%
  arrange(desc(area)) %>%
  mutate(anlaeg = fct_inorder(anlaeg)) %>%
  select(-area)

ggplot(biofos, aes(fill = anlaeg)) +
  geom_sf(colour = "transparent", alpha = 0.6) +
  geom_sf_label(aes(label = anlaeg), show.legend = FALSE, label.size = 0) +
  guides(fill = "none") +
  labs(
    title = "BIOFOS oplande"
  )


st_save_cleaned(biofos, name = "biofos")





## HOFOR pumpestationer ----------------------------------------------------

raw_hofor_decentral <- list.files(
  "S:/Spildevand/National overvågning/Modtagne_GIS-filer/HOFOR_Provetagning_nov",
  pattern = "\\.shp$",
  full.names = TRUE,
  recursive = TRUE
) %>%
  set_names() %>%
  map_dfr(st_read, quiet = TRUE, .id = "file") %>%
  st_zm() %>%
  as_tibble() %>%
  st_as_sf()

raw_hofor_decentral %>%
  glimpse()

hofor_decentral <- raw_hofor_decentral %>%
  mutate(
    anlaeg = file %>%
      str_remove("S:/.*HOFOR_Provetagning_nov/") %>%
      str_remove("(/dataset)?.shp") %>%
      str_remove("pstOplande/") %>%
      str_remove("Sogne_") %>%
      str_squish()) %>%
  group_by(anlaeg) %>%
  summarise() %>%
  st_make_valid() %>%
  correct_crs() %>%
  st_fill_holes(200)

plot_anlaeg(hofor_decentral)

st_save_cleaned(hofor_decentral)




# Hunseby -----------------------------------------------------------------

# This anlæg spans two kommuner, so they gave us two different shapefiles.


## Guldborgsund kommune ----------------------------------------------------

raw_hunseby_guldborgsund <- st_read("Oplande/raw/Hunseby/Hunseby Rens_Lolland",
                                    "Guldborgsund")

check_crs(raw_hunseby_guldborgsund)

hunseby_guldborgsund <- raw_hunseby_guldborgsund %>%
  correct_crs() %>%
  as_tibble() %>%
  st_as_sf() %>%
  select() %>%
  mutate(kommune = "Goldburgsund")






## Lolland kommune ---------------------------------------------------------

# st_layers("Oplande/raw/Hunseby/Hunseby Lolland oplande")
raw_hunseby_lolland <- st_read("Oplande/raw/Hunseby/Hunseby Lolland oplande",
                               "OpusOpland_Hunseby_region")


check_crs(raw_hunseby_lolland)

hunseby_lolland <- raw_hunseby_lolland %>%
  summarise(kommune = "Lolland")

hunseby_lolland %>%
  ggplot() +
  geom_sf()


hunseby_combined <- bind_rows(hunseby_guldborgsund,
                              hunseby_lolland)

hunseby_combined %>%
  ggplot() +
  geom_sf(aes(fill = kommune), colour = "transparent")


hunseby <- hunseby_combined %>%
  summarise(anlaeg = "Hunseby") %>%
  # I checked in QGIS that none of these filled holes were important.
  st_fill_holes(500)

plot_anlaeg(hunseby)

st_save_cleaned(hunseby)






# Skagen ------------------------------------------------------------------

skagen <- auto_read_shp("Oplande/raw/Skagen/Skagen_spildevandsoplande",
                        "Skagen_spildevandsoplande",
                        collapse = "Skagen")

plot_anlaeg(skagen)

st_save_cleaned(skagen)






# HJVand (Hjørring, Løkken/Nr. Lyngby, Hirtshals, Sindal) -----------------

hjørring <- auto_read_shp("S:/Spildevand/National overvågning/Modtagne_GIS-filer/Hjørring, Sindal og Hirtshals",
                          "Opland_Hjørring",
                          collapse = "Hjørring")

løkken_nr_lyngby <- auto_read_shp("Oplande/raw/Opland_NrLyngby_hjvand_løkken/SHP",
                                  "Opland_Nr_Lyngby",
                                  collapse = "Løkken/Nr. Lyngby")

hirtshals <- auto_read_shp("S:/Spildevand/National overvågning/Modtagne_GIS-filer/Hjørring, Sindal og Hirtshals",
                           "Opland_Hirtshals",
                           collapse = "Hirtshals")


sindal <- auto_read_shp("S:/Spildevand/National overvågning/Modtagne_GIS-filer/Hjørring, Sindal og Hirtshals",
                        "Opland_Sindal",
                        collapse = "Sindal")


# This one is quite detailed so I'm simplifying it a bit more
hjvand <- bind_rows(hjørring,
                    løkken_nr_lyngby,
                    hirtshals,
                    sindal) %>%
  st_buffer(15, nQuadSegs = 1) %>%
  st_simplify(dTolerance = 1) %>%
  st_fill_holes(1000)

object.size(hjvand)

plot_anlaeg(hjvand)

st_save_cleaned(hjvand)





# Hillerød ----------------------------------------------------------------

hilleroed <- auto_read_shp("Oplande/raw/HilleroedForsyning_21062021",
                           "Hilleroed")

plot_anlaeg(hilleroed)

st_save_cleaned(hilleroed, "hilleroed")





# Fredericia --------------------------------------------------------------

# They can't do shapefiles, but they say they cover the whole municipality. So I
# downloaded the municipalities from Eurostat
# (https://ec.europa.eu/eurostat/web/gisco/geodata/reference-data/administrative-units-statistical-units/lau),
# and we'll just use the Fredericia one.


raw_lau <- st_read("Oplande/raw/LAU_RG_01M_2019_4326.geojson")

check_crs(raw_lau)

fredericia <- raw_lau %>%
  filter(CNTR_CODE == "DK",
         LAU_NAME == "Fredericia") %>%
  transmute(anlaeg = LAU_NAME) %>%
  correct_crs()


fredericia %>%
  ggplot() +
  geom_sf(fill = "red") +
  labs(title = "Fredericia (municipality boundary from Eurostat 2019)")

st_save_cleaned(fredericia)



# By the way, this is where I saved DK's kommuner from. Just wanted to keep a
# record somewhere

# raw_lau %>%
#   filter(CNTR_CODE == "DK") %>%
#   st_write("oplande/dk_kommuner.geojson")





# BlueKolding (Kolding, Christiansfeld, Vamdrup) --------------------------

kolding <- auto_read_shp("Oplande/raw/BlueKolding.geojson",
                         collapse = "Kolding")

plot_anlaeg(kolding)

christiansfeld <- auto_read_shp(
  "S:/Spildevand/National overvågning/Modtagne_GIS-filer/BlueKolding/Christiansfeld.geojson",
  anlaeg_col = Anlaeg
)

vamdrup <- auto_read_shp(
  "S:/Spildevand/National overvågning/Modtagne_GIS-filer/BlueKolding/Vamdrup.geojson",
  anlaeg_col = Anlaeg
)

bluekolding <- bind_rows(kolding, christiansfeld, vamdrup)

plot_anlaeg(bluekolding)

# TODO: once they send the correct Vamdrup file, export these all together
# (currently Vamdrup contains Christiansfeld opland (only))
st_save_cleaned(bluekolding)






# Brædstrup ---------------------------------------------------------------

brædstrup <- auto_read_shp("S:/Spildevand/National overvågning/Modtagne_GIS-filer/Forbrugsteder og oplande til Brædstrup rens",
                           "Oplande_til_Brædstrup_rens",
                           anlaeg_col = Anlæg)

plot_anlaeg(brædstrup)

st_save_cleaned(brædstrup)



# Odder -------------------------------------------------------------------

odder <- auto_read_shp(
  "S:/Spildevand/National overvågning/Modtagne_GIS-filer/Forbrugsteder og oplande til Odder renseanlæg",
  "Odder renseanlæg oplande",
  anlaeg_col = Anlaeg,
  collapse = "Odder"
)



plot_anlaeg(odder)

st_save_cleaned(odder)





# Viborg ------------------------------------------------------------------
viborg <- auto_read_shp(
  "S:/Spildevand/National overvågning/Modtagne_GIS-filer/GISdata Energi Viborg Vand"
)

plot_anlaeg(viborg)

st_save_cleaned(viborg)



# Frederikshavn -----------------------------------------------------------

# We got this twice. The other folder is called TBL.

frederikshavn <- auto_read_shp(
  "S:/Spildevand/National overvågning/Modtagne_GIS-filer/Frederikshavn",
  "TBLopland_Covid19_V1_region",
  anlaeg_col = Renseanlae
) %>%
  filter(anlaeg != "Anlæg")

plot_anlaeg(frederikshavn)

st_save_cleaned(frederikshavn)



# Hanstholm ---------------------------------------------------------------

hanstholm <- auto_read_shp(
  "S:/Spildevand/National overvågning/Modtagne_GIS-filer/Hanstholm_V_T_kloakoplande_adresser",
  layer = "H_V_T_kloakoplande_region",
  anlaeg_col = Renseanlæg
) %>%
  filter(!is.na(anlaeg))

plot_anlaeg(hanstholm)

st_save_cleaned(hanstholm)







# Mariager Fjord ----------------------------------------------------------

mariagerfjord <- auto_read_shp(
  "S:/Spildevand/National overvågning/Modtagne_GIS-filer/Mariager Fjord"
) %>%
  # Remove the oplande where they just dump it into the ground?
  filter(anlaeg != "Nedsivning til jorden")

plot_anlaeg(mariagerfjord)

st_save_cleaned(mariagerfjord)



# Nakskov -----------------------------------------------------------------

nakskov <- auto_read_shp(
  "S:/Spildevand/National overvågning/Modtagne_GIS-filer/Nakskov Renseanlæg",
  layer = "Nakskov opland_region",
  collapse = "Nakskov"
)

plot_anlaeg(nakskov)

st_save_cleaned(nakskov)






# Randers -----------------------------------------------------------------

# Apparently their file is wrong for now





# Rødbyhavn ---------------------------------------------------------------

# Like Herning, this read in as GEOMETRYCOLLECTION. Extracting polygons.

rødbyhavn <- auto_read_shp(
  "S:/Spildevand/National overvågning/Modtagne_GIS-filer/Rødbyhavn Renseanlæg",
  layer = "Rødbyhavn opland_region",
  anlaeg_col = Renseanlæg
) %>%
  st_collection_extract("POLYGON") %>%
  st_collapse_features("Rødbyhavn")

plot_anlaeg(rødbyhavn)

st_save_cleaned(rødbyhavn)



# Faxe, Dalby, Haslev -----------------------------------------------------

# Three areas in three layers. They're all part of Faxe forsyning. They don't
# have a CRS, but they look like they're on the right place on the map when you
# assume the standard one.

st_layers(
  "S:/Spildevand/National overvågning/Modtagne_GIS-filer/Faxe, Dalby, Haslev",
)

dalby <- auto_read_shp(
  "S:/Spildevand/National overvågning/Modtagne_GIS-filer/Faxe, Dalby, Haslev",
  layer = "Dalby",
  collapse = "Dalby",
  overwrite_crs = TRUE
)

fakse <- auto_read_shp(
  "S:/Spildevand/National overvågning/Modtagne_GIS-filer/Faxe, Dalby, Haslev",
  layer = "Fakse",
  collapse = "Faxe",
  overwrite_crs = TRUE
)

haslev <- auto_read_shp(
  "S:/Spildevand/National overvågning/Modtagne_GIS-filer/Faxe, Dalby, Haslev",
  layer = "Haslev C",
  collapse = "Haslev",
  overwrite_crs = TRUE
)


faxe_dalby_haslev <- bind_rows(dalby, fakse, haslev)

faxe_dalby_haslev %>%
  plot_anlaeg()

st_save_cleaned(faxe_dalby_haslev)







# Tårnby -----------------------------------------------------------------

# This one also had no CRS. It's on Amager, just below Lynetten's søndre tilløb.
# So I plotted those on the same map with forced CRS and it looks good.

tårnby <- auto_read_shp(
  "S:/Spildevand/National overvågning/Modtagne_GIS-filer/Taarnby_Covid19",
  layer = "Taarnby_Polygon_Covid19",
  overwrite_crs = TRUE,
  collapse = "Tårnby"
)

bind_rows(tårnby, biofos) %>%
  plot_anlaeg()

st_save_cleaned(tårnby)




# Vejle -------------------------------------------------------------------

# This is from the guy who phoned me and at first I thought he was from BIOFOS.
# I made the names a bit nicer.

vejle <- auto_read_shp(
  "S:/Spildevand/National overvågning/Modtagne_GIS-filer/Vejle"
) %>%
  mutate(anlaeg = glue(
    "Vejle ({one_anlaeg})",
    one_anlaeg = str_to_title(word(anlaeg))
  ))

vejle %>%
  plot_anlaeg()

st_save_cleaned(vejle)




# Vejle decentral (Nørremarken) -------------------------------------------

vejle_decentral <- auto_read_shp(
  "S:/Spildevand/Lille OEU-sag/Oplande/raw/Vejle/Vejle decentral",
  collapse = "Nørremarken (Vejle)"
)

plot_anlaeg(vejle_decentral)

st_save_cleaned(vejle_decentral)




# Hofmansgave -------------------------------------------------------------

hofmansgave <- auto_read_shp(
  "S:/Spildevand/National overvågning/Modtagne_GIS-filer/Hofmansgave Renseanlæg"
) %>%
  mutate(anlaeg = "Hofmansgave")

plot_anlaeg(hofmansgave)

st_save_cleaned(hofmansgave)




# Provas (Haderslev/Gram/Vojens) ---------------------------------------------

gram <- auto_read_shp(
  "S:/Spildevand/National overvågning/Modtagne_GIS-filer/Provas anlaeg SSI/Gram_anlaeg",
  layer = "Gram_renseanlaeg_opland",
  collapse = "Gram"
)

haderslev <- auto_read_shp(
  "S:/Spildevand/National overvågning/Modtagne_GIS-filer/Provas anlaeg SSI/Haderslev_anlaeg",
  layer = "Haderslev_renseanlaeg_opland",
  collapse = "Haderslev"
)

vojens <- auto_read_shp(
  "S:/Spildevand/National overvågning/Modtagne_GIS-filer/Provas anlaeg SSI/Vojens_anlaeg",
  layer = "Vojens_renseanlaeg_opland",
  collapse = "Vojens"
)


provas <- bind_rows(gram, haderslev, vojens)

plot_anlaeg(provas)


st_save_cleaned(provas, name = "provas_haderslev_vojens_gram")





# Silkeborg ---------------------------------------------------------------

# Months after they gave us those huge rough shapes, they sent us some stuff
# made out of lines. I had to do some manual processing and buffer it a bit to
# make them seem reasonable.

raw_silkeborg_kjellerup <- st_read(
  "S:/Spildevand/National overvågning/Modtagne_GIS-filer/Silkeborg Forsyning/Silkeborg_Opland_Silkeborg KOmmune_Renseanlæg",
  "Silkeborg_Opland_Kjellerup_Renseanlæg"
)

silkeborg_kjellerup <- raw_silkeborg_kjellerup %>%
  as_tibble() %>%
  st_as_sf() %>%
  st_polygonize() %>%
  filter(!st_is_empty(geometry)) %>%
  st_make_valid() %>%
  filter(!st_is_empty(geometry)) %>%
  # st_is_valid() %>% all() %>%
  summarise(anlaeg = "Silkeborg (Kjellerup)") %>%
  st_make_valid() %>%
  print()

plot_anlaeg(silkeborg_kjellerup)


raw_silkeborg_søholt <- st_read(
  "S:/Spildevand/National overvågning/Modtagne_GIS-filer/Silkeborg Forsyning/Silkeborg_Opland_Silkeborg KOmmune_Renseanlæg",
  "Silkeborg_Opland_Søholt_Renselæg"
)

silkeborg_søholt <- raw_silkeborg_søholt %>%
  as_tibble() %>%
  st_as_sf() %>%
  st_polygonize() %>%
  filter(!st_is_empty(geometry)) %>%
  st_make_valid() %>%
  filter(!st_is_empty(geometry)) %>%
  # st_is_valid() %>% all() %>%
  summarise(anlaeg = "Silkeborg (Søholt)") %>%
  st_make_valid() %>%
  print()


plot_anlaeg(silkeborg_søholt)



raw_silkeborg_them <- st_read(
  "S:/Spildevand/National overvågning/Modtagne_GIS-filer/Silkeborg Forsyning/Silkeborg_Opland_Silkeborg KOmmune_Renseanlæg",
  "Silkeborg_Opland_Them_Rensenlæg"
)

silkeborg_them <- raw_silkeborg_them %>%
  as_tibble() %>%
  st_as_sf() %>%
  st_polygonize() %>%
  filter(!st_is_empty(geometry)) %>%
  st_make_valid() %>%
  filter(!st_is_empty(geometry)) %>%
  # st_is_valid() %>% all() %>%
  summarise(anlaeg = "Silkeborg (Them)") %>%
  st_make_valid() %>%
  print()

plot_anlaeg(silkeborg_them)




raw_silkeborg_truust <- st_read(
  "S:/Spildevand/National overvågning/Modtagne_GIS-filer/Silkeborg Forsyning/Silkeborg_Opland_Silkeborg KOmmune_Renseanlæg",
  "Silkeborg_Opland_Truust_Renseanlæg"
)

silkeborg_truust <- raw_silkeborg_truust %>%
  as_tibble() %>%
  st_as_sf() %>%
  st_polygonize() %>%
  filter(!st_is_empty(geometry)) %>%
  st_make_valid() %>%
  filter(!st_is_empty(geometry)) %>%
  # st_is_valid() %>% all() %>%
  summarise(anlaeg = "Silkeborg (Truust)") %>%
  st_make_valid() %>%
  print()

plot_anlaeg(silkeborg_truust)


silkeborg <- bind_rows(silkeborg_kjellerup, silkeborg_søholt,
                       silkeborg_them, silkeborg_truust) %>%
  st_set_crs(25832) %>%
  st_simplify(dTolerance = 1) %>%
  st_buffer(20, nQuadSegs = 5) %>%
  st_simplify(dTolerance = 5) %>%
  st_fill_holes(5000)

plot_anlaeg(silkeborg)

st_save_cleaned(silkeborg)



# Holstebro & Vinderup (Vestforsyning) --------------------------------------

# This one was a .json file, I just had to change it to .geojson

holstebro_vinderup <- auto_read_shp(
  "S:/Spildevand/National overvågning/Modtagne_GIS-filer/Vestforsyning Holstebro Vinderup/Vestforsyning_Holstebro_Vinderup.geojson"
)

plot_anlaeg(holstebro_vinderup)

st_save_cleaned(holstebro_vinderup)



# Holbæk Lejre Roskilde --------------------------------------------------

# This contains oplande from different areas, don't think they should all be
# called Holbæk.

holbæk_lejre_roskilde <- auto_read_shp(
  "S:/Spildevand/National overvågning/Modtagne_GIS-filer/Holbæk_Lejre_Roskilde.geojson",
  anlaeg_col = Renseanlæg
) %>%
  mutate(anlaeg = word(anlaeg, 1))

plot_anlaeg(holbæk_lejre_roskilde)

st_save_cleaned(holbæk_lejre_roskilde)



# Vesthimmerland ----------------------------------------------------------

# File is called 820_Kloakoplande_Vedtaget_07.09.2021.geojson

vesthimmerland <- auto_read_shp(
  "S:/Spildevand/National overvågning/Modtagne_GIS-filer/820_Kloakoplande_Vedtaget_07.09.2021.geojson"
) %>%
  mutate(anlaeg = str_to_title(anlaeg))


plot_anlaeg(vesthimmerland)

st_save_cleaned(vesthimmerland)





# Svendborg ---------------------------------------------------------------

# The first file they sent had invalid/corrupt data. So did the second one, but
# I tried dealing with it in QGIS. I ran "Check validity" and "Fix geometries".
# There was partial overlap between what was already valid and what was valid
# after the fix, but both had extra bits. So now need to merge them, and "Union"
# didn't work in QGIS. So let's try it here.
raw_svendborg_qgis_fixed <- st_read(
  "S:/Spildevand/National overvågning/Modtagne_GIS-filer/Svendborg 2021-09-21/svendborg_qgis_fixed.geojson",
)
raw_svendborg_qgis_valid <- st_read(
  "S:/Spildevand/National overvågning/Modtagne_GIS-filer/Svendborg 2021-09-21/svendborg_qgis_valid.geojson"
)

# Still has multiple smaller oplande for each anlaeg. Combining.
svendborg_qgis_fixed <- raw_svendborg_qgis_fixed %>%
  group_by(Renseanlae) %>%
  summarise()

# The bits that were supposedly already valid have problems. Maybe it's
# open-ended polygons? So I'm converting them to individual lines, then turning
# the lines back into polygons. Then also collapsing them by anlaeg.
svendborg_qgis_valid <- raw_svendborg_qgis_valid %>%
  st_cast("MULTILINESTRING") %>%
  st_cast("LINESTRING") %>%
  st_make_valid() %>%
  filter(st_is(., "LINESTRING")) %>%
  st_make_valid() %>%
  st_polygonize() %>%
  filter(!st_is_empty(geometry)) %>%
  group_by(Renseanlae) %>%
  summarise()

# Now they're both valid
svendborg_qgis_fixed %>% st_is_valid() %>% all()
svendborg_qgis_valid %>% st_is_valid() %>% all()

# Combine the two sets of shapes
svendborg_inc_na <- bind_rows(svendborg_qgis_fixed,
                              svendborg_qgis_valid) %>%
  group_by(anlaeg = Renseanlae) %>%
  summarise() %>%
  # Fix encoding issue
  mutate(anlaeg = if_else(anlaeg == "StrandgÃ¥rden", "Strandgården", anlaeg))

# There are some with NA in the anlaeg column. What's with those?
svendborg_inc_na %>% plot_anlaeg() +
  geom_sf(data = ~ filter(., is.na(anlaeg)) %>%
            st_buffer(300),
          fill = "transparent")

# Looks like the NAs clearly belonged to Egsmade, so I'm assigning them there.
svendborg_na_filled <- svendborg_inc_na %>%
  mutate(anlaeg = if_else(is.na(anlaeg), "Egsmade", anlaeg)) %>%
  group_by(anlaeg) %>%
  summarise()

# This is what it looks like in the end, seems good to me
plot_anlaeg(svendborg_na_filled)

# Checks
st_is_valid(svendborg_na_filled)
check_crs(svendborg_na_filled)
st_z_range(svendborg_na_filled)


# Output in cleaned/ subfolder, but moving it back to
# S:/Spildevand/National overvågning/Modtagne_GIS-filer/Svendborg 2021-09-21
st_save_cleaned(svendborg_na_filled, name = "svendborg_to_check")

# Plotted it back in QGIS, and saw there were still some parts of the "valid"
# layer that peek out behind my version. Sent it back to Svendborg to fix.






# OK, in the end I just drew the missing parts of the polygons myself in QGIS.
# This is the result:
svendborg <- auto_read_shp(
  "S:/Spildevand/National overvågning/Modtagne_GIS-filer/Svendborg 2021-09-21/svendborg_manual_fix_2021-10-07.geojson"
)

plot_anlaeg(svendborg)

st_save_cleaned(svendborg)



# auto_read_shp(
#   "S:/Spildevand/National overvågning/Modtagne_GIS-filer/Svendborg 2021-10-05"
# )
#
#
# sv3 <- st_read("S:/Spildevand/National overvågning/Modtagne_GIS-filer/Svendborg 2021-10-05")
#
# sv3 %>%
#   as_tibble() %>%
#   st_as_sf() %>%
#   st_is_valid()


# Lemvig ------------------------------------------------------------------

lemvig_harboøre <- auto_read_shp(
  "S:/Spildevand/National overvågning/Modtagne_GIS-filer/Lemvig.geojson"
) %>%
  mutate(anlaeg = recode(anlaeg, "Harbooere" = "Harboøre"))

plot_anlaeg(lemvig_harboøre)

st_save_cleaned(lemvig_harboøre)



# Arwos (Stegholt, Gårdeby, Bov, Kollund) ---------------------------------

arwos <- auto_read_shp(
  "S:/Spildevand/National overvågning/Modtagne_GIS-filer/Arwos anlaeg.geojson"
) %>%
  mutate(anlaeg = anlaeg %>%
           word(1) %>%
           str_to_title())

arwos %>%
  plot_anlaeg()

st_save_cleaned(arwos)




# Favrskov (Drøsbro, Hadsten, Hammel, Hinnerup) ---------------------------

favrskov <- auto_read_shp(
  "S:/Spildevand/National overvågning/Modtagne_GIS-filer/Favrskov",
  anlaeg_col = Renseanl_1
) %>%
  mutate(anlaeg = anlaeg %>% word(1))

plot_anlaeg(favrskov)

st_save_cleaned(favrskov)



# Helsingør ---------------------------------------------------------------

helsingør <- auto_read_shp(
  "S:/Spildevand/National overvågning/Modtagne_GIS-filer/Helsingør"
)

plot_anlaeg(helsingør)

st_save_cleaned(helsingør)




# Rinkjøbing --------------------------------------------------------------

# Didn't have CRS, but looked good when plotted with Arwos

ringkjøbing <- auto_read_shp(
  "S:/Spildevand/National overvågning/Modtagne_GIS-filer/Ringkjøbing-Skjern",
  layer = "RSF_renseanlaeg",
  overwrite_crs = TRUE,
  anlaeg_col = Layer
) %>%
  mutate(anlaeg = anlaeg %>% str_remove(" renseanlæg"))

plot_anlaeg(ringkjøbing)

st_save_cleaned(ringkjøbing) %>% has_holes()





# Aquadjurs (Anholt, Fornæs, Randers) -----------------------------------


aquadjurs <- auto_read_shp(
  "S:/Spildevand/National overvågning/Modtagne_GIS-filer/Aquadjurs",
  anlaeg_col = renseanlag
)

# Anholt is minuscule
plot_anlaeg(aquadjurs)

st_save_cleaned(aquadjurs)



# Sorø --------------------------------------------------------------------

sorø <- auto_read_shp(
  "S:/Spildevand/National overvågning/Modtagne_GIS-filer/Sorø",
  anlaeg_col = Renseanlaeg
)

plot_anlaeg(sorø)

st_save_cleaned(sorø)



# Fredensborg -------------------------------------------------------------

# Humlebæk/Nivå, Fredensborg/lønholt, Kokkedal, Karlebo

fredensborg <- auto_read_shp(
  "S:/Spildevand/National overvågning/Modtagne_GIS-filer/Fredensborg"
)

plot_anlaeg(fredensborg)

st_save_cleaned(fredensborg)



# Jammerbugt --------------------------------------------------------------

# Sigsgaard, Fjerritslev, Attrup

jammerbugt <- auto_read_shp(
  "S:/Spildevand/National overvågning/Modtagne_GIS-filer/Jammerbugt/Oplande.geojson",
  anlaeg_col = renseanlaeg_navn
) %>%
  mutate(anlaeg = word(anlaeg, 1))

plot_anlaeg(jammerbugt)

st_save_cleaned(jammerbugt)




# Skive -------------------------------------------------------------------

# 2 anlæg: Skive and Harre-Vejle. Skive they sent a proper opland. Harre-Vejle I
# cleaned partly in QGIS, and partly here in R.

skive <- auto_read_shp("S:/Spildevand/Lille OEU-sag/Oplande/raw/Skive/ANLAEG_text",
              "ANLAEG_area",
              collapse = "Skive")



# They sent a million layers, most of them points. I merged a bunch of those
# point layers in QGIS, and now I'm going to buffer those into polygons here.
# The manual Harre-Vejle stuff overlaps with Skive. Since I trust Skive more,
# I'm just cutting the Skive part out of Harre-Vejle, with a 500-m buffer.

hv_points <- st_read("S:/Spildevand/Lille OEU-sag/Oplande/raw/Skive/merged-points-harre-vejle.geojson")

harre_vejle <- hv_points %>%
  correct_crs() %>%
  st_zm() %>%
  st_buffer(300, nQuadSegs = 4) %>%
  st_simplify(dTolerance = 40) %>%
  summarise(anlaeg = "Harre-Vejle") %>%
  st_simplify(dTolerance = 40) %>%
  st_fill_holes(10000) %>%
  # Fix overlap with Skive
  st_difference(skive %>%
                  st_buffer(500,nQuadSegs = 1)) %>%
  select(-anlaeg.1)



skive_harre_vejle <- bind_rows(skive, harre_vejle)


plot_anlaeg(skive_harre_vejle)

st_save_cleaned(skive_harre_vejle)




# Langeland (Rudkøbing) ---------------------------------------------------

# Langeland is the island (between Lolland and Fyn), Rodkøbing is the town There
# are other smaller towns on the island, presumably they have their own anlæg
# where we're not measuring.

langeland_rudkøbing <- auto_read_shp(
  "S:/Spildevand/National overvågning/Modtagne_GIS-filer/Langeland/SSI/SHP",
  layer = "Rudkøbing_Renseanlæg_Opland_region",
  anlaeg_col = "ANLAEG"
) %>%
  mutate(anlaeg = word(anlaeg, 1))

plot_anlaeg(langeland_rudkøbing)

st_save_cleaned(langeland_rudkøbing)



# Skanderborg -------------------------------------------------------------

# Weird encoding thing: I think it was read in as utf-8, but actually it was
# latin1 (whatever that is)? Also, again didn't have CRS. I compared to
# aquadjurs to confirm the location was ok.

# Skanderborg, Skovby, Ry, Hørning

skanderborg <- auto_read_shp(
  "S:/Spildevand/National overvågning/Modtagne_GIS-filer/Skanderborg",
  overwrite_crs = TRUE
) %>%
  mutate(anlaeg = anlaeg %>%
           `Encoding<-`("latin1") %>%
           word(1))


plot_anlaeg(skanderborg)

st_save_cleaned(skanderborg)





# Tønder & Skærbæk --------------------------------------------------------

# These look like the pipes to me, rather than the oplande. So I'm putting a big
# buffer around everything, because I don't think it will really capture the
# population otherwise.
auto_tønder_skærbæk <- auto_read_shp(
  "S:/Spildevand/National overvågning/Modtagne_GIS-filer/Tønder Forsyning",
  layer = "COVID19TF"
)


tønder_skærbæk <- auto_tønder_skærbæk %>%
  st_simplify(dTolerance = 10) %>%
  st_buffer(100) %>%
  st_fill_holes(2000) %>%
  st_simplify(dTolerance = 20)

plot_anlaeg(tønder_skærbæk)

object.size(auto_tønder_skærbæk)
object.size(tønder_skærbæk)

st_save_cleaned(tønder_skærbæk)




# Struer ------------------------------------------------------------------

struer <- auto_read_shp(
  "S:/Spildevand/National overvågning/Modtagne_GIS-filer/Struer",
  collapse = "Struer"
) %>%
  st_fill_holes(200)

plot_anlaeg(struer)

st_save_cleaned(struer)



# Randers -----------------------------------------------------------------

# Randers is included in Aquadjurs too, but this has the separate Indløb. Also
# has Langå.

randers <- auto_read_shp(
  "S:/Spildevand/National overvågning/Modtagne_GIS-filer/Randers"
) %>%
  mutate(anlaeg = anlaeg %>%
           str_remove(" Centralrenseanlæg – Indløb| Renseanlæg")) %>%
  st_fill_holes(200)

plot_anlaeg(randers)

st_save_cleaned(randers)



# Herning -----------------------------------------------------------------

# Herning, Trehøje Øst, Aulum, Sunds

# No CRS, but looks good next to Struer and Herning. Also, for some reason part
# of this was a GEOMETRYCOLLECTION rather than a MULTIPOLYGON, so I had to
# extract the polygon parts of that. Maybe there was just a stray point or line
# somewhere?
herning <- auto_read_shp(
  "S:/Spildevand/National overvågning/Modtagne_GIS-filer/Herning",
  overwrite_crs = TRUE,
  anlaeg_col = Anlaeg
) %>%
  st_collection_extract("POLYGON") %>%
  group_by(anlaeg) %>%
  summarise()

# No holes :)
has_holes(herning)

plot_anlaeg(herning)

st_save_cleaned(herning)


# Aalborg -----------------------------------------------------------------

aalborg <- auto_read_shp(
  "S:/Spildevand/National overvågning/Modtagne_GIS-filer/Aalborg"
) %>%
  mutate(anlaeg = anlaeg %>% str_replace("Renseanlæg", "Aalborg")) %>%
  st_fill_holes(200)

plot_anlaeg(aalborg)

st_save_cleaned(aalborg)


## Aalborg decentral -------------------------------------------------------


aalborg_decentral <- auto_read_shp(
  "S:/Spildevand/National overvågning/Modtagne_GIS-filer/Aalborg/Covid19_broende_aalborg2.geojson",
  anlaeg_col = OplandTilBroend
) %>%
  mutate(anlaeg = recode(anlaeg,
                         "4150600" = "Nørre Tranders Nord",
                         "R416650" = "Nørre Tranders Syd"))

plot_anlaeg(aalborg_decentral)

st_save_cleaned(aalborg_decentral)




# Syddjurs (Boeslum, Mørke) --------------------------------------------

syddjurs <- auto_read_shp(
  "S:/Spildevand/National overvågning/Modtagne_GIS-filer/Syddjurs"
) %>%
  st_fill_holes(200)

plot_anlaeg(syddjurs)

st_save_cleaned(syddjurs)


# Vordingborg (+ Stege, Prestø) -------------------------------------------

vordingborg_stege_præstø <- auto_read_shp(
  "S:/Spildevand/National overvågning/Modtagne_GIS-filer/vordingborgforsyning.geojson"
) %>%
  mutate(anlaeg = str_remove(anlaeg, " renseanlæg"))

plot_anlaeg(vordingborg_stege_præstø)

st_save_cleaned(vordingborg_stege_præstø)




# Halsnæs (Melby) ---------------------------------------------------------

halsnæs_melby <- auto_read_shp(
  "S:/Spildevand/National overvågning/Modtagne_GIS-filer/halsnæs.geojson"
) %>%
  mutate(anlaeg = "Halsnæs (Melby)") %>%
  smoothr::fill_holes(units::set_units(200, "m^2"))

plot_anlaeg(halsnæs_melby)


st_save_cleaned(halsnæs_melby)




# Kalundborg --------------------------------------------------------------

kalundborg <- auto_read_shp(
  "S:/Spildevand/National overvågning/Modtagne_GIS-filer/Kalundborg",
  layer = "Kalundborg Renseanlæg Spildevandsoplande",
  collapse = "Kalundborg"
)


ornum <- auto_read_shp(
  "S:/Spildevand/National overvågning/Modtagne_GIS-filer/Kalundborg",
  layer = "Ornum Renseanlæg Spildevandsoplande",
  collapse = "Ornum"
)

kalundborg_ornum <- bind_rows(kalundborg, ornum)

plot_anlaeg(kalundborg_ornum)

st_save_cleaned(kalundborg_ornum)













# Ikast, Brande, Nørre Snede ----------------------------------------------

# I could fix Nørre Snede and Ikast, but Brande seems to be empty?


raw_nørresnede <- st_read("S:/Spildevand/National overvågning/Modtagne_GIS-filer/Ikast_Brande_NørreSnede/Nr Snede.dxf")

nørresnede <- raw_nørresnede %>%
  st_polygonize() %>%
  st_make_valid() %>%
  as_tibble() %>%
  st_as_sf() %>%
  select() %>%
  st_cast("MULTIPOLYGON") %>%
  st_cast("POLYGON") %>%
  mutate(area = st_area(geometry)) %>%
  filter(area > 1) %>%
  transmute(anlaeg = "Nørre Snede") %>%
  `st_crs<-`(st_crs(25832))


plot_anlaeg(nørresnede)




raw_ikast <- st_read("S:/Spildevand/National overvågning/Modtagne_GIS-filer/Ikast_Brande_NørreSnede/Ikast.dxf")

ikast <- raw_ikast %>%
  st_polygonize() %>%
  st_make_valid() %>%
  as_tibble() %>%
  st_as_sf() %>%
  select() %>%
  st_cast("MULTIPOLYGON") %>%
  st_cast("POLYGON") %>%
  mutate(area = st_area(geometry)) %>%
  filter(area > 1) %>%
  transmute(anlaeg = "Ikast") %>%
  `st_crs<-`(st_crs(25832))

plot_anlaeg(ikast)



brande_poly <- st_read(
  "S:/Spildevand/National overvågning/Modtagne_GIS-filer/Ikast_Brande_NørreSnede/brande_qgis_polygonised.geojson"
)

brande_poly %>%
  mutate(anlaeg = "Brande") %>%
  plot_anlaeg()

brande <- brande_poly %>%
  `st_crs<-`(st_crs(25832)) %>%
  mutate(anlaeg = "Brande") %>%
  st_make_valid()

brande %>%
  plot_anlaeg()


ikast_brande_nørresnede <- bind_rows(brande, ikast, nørresnede)

plot_anlaeg(ikast_brande_nørresnede)

st_save_cleaned(ikast_brande_nørresnede)


# Ærøskøbing --------------------------------------------------------------

raw_ærøskøbing <- st_read(
  "S:/Spildevand/National overvågning/Modtagne_GIS-filer/Ærøskøbing"
)

ærøskøbing <- raw_ærøskøbing %>%
  as_tibble() %>%
  st_as_sf() %>%
  st_zm() %>%
  st_cast("POLYGON") %>%
  st_make_valid() %>%
  st_cast("POLYGON") %>%
  st_make_valid() %>%
  group_by(anlaeg) %>%
  summarise() %>%
  correct_crs() %>%
  st_fill_holes(200)

plot_anlaeg(ærøskøbing)

st_save_cleaned(ærøskøbing)




# Vejen -------------------------------------------------------------------

vejen <- auto_read_shp(
  "S:/Spildevand/National overvågning/Modtagne_GIS-filer/Vejen"
) %>%
  mutate(anlaeg = anlaeg %>% str_to_sentence())

plot_anlaeg(vejen)

st_save_cleaned(vejen)





# Middelfart, Nørre Aaby --------------------------------------------------

# This shape is very detailed so I did some extra simplification on it

middelfart_nørre_aaby_auto <- auto_read_shp(
  "S:/Spildevand/National overvågning/Modtagne_GIS-filer/Middelfart - Nørre Aaby/Middelfart - Nørre Aaby.geojson",
  holes_threshold_m2 = NULL
)


middelfart_nørre_aaby <- middelfart_nørre_aaby_auto %>%
  mutate(anlaeg = anlaeg %>%
           str_remove(regex("(central)?renseanlæg", ignore_case = TRUE)) %>%
           str_squish()) %>%
  st_simplify(dTolerance = 1) %>%
  st_buffer(15) %>%
  st_fill_holes(1000) %>%
  st_simplify(dTolerance = 2)

object.size(middelfart_nørre_aaby_auto)
object.size(middelfart_nørre_aaby)

middelfart_nørre_aaby %>%
  plot_anlaeg()

st_save_cleaned(middelfart_nørre_aaby)




# Faaborg -----------------------------------------------------------------

faaborg <- auto_read_shp(
  "S:/Spildevand/National overvågning/Modtagne_GIS-filer/Faaborg/Coronaprøver_Faaborg.geojson"
) %>%
  mutate(anlaeg = anlaeg %>%
           str_remove(regex("renseanlæg", ignore_case = TRUE)) %>%
           str_squish())

plot_anlaeg(faaborg)

st_save_cleaned(faaborg)









# Hedensted, Juelsminde -------------------------------------------------

hedensted_juelsminde <- auto_read_shp(
  "S:/Spildevand/National overvågning/Modtagne_GIS-filer/Hedensted/Hedensted_oplande.geojson",
  anlaeg_col = text7
) %>%
  mutate(anlaeg = anlaeg %>% str_rm_renseanlaeg())


plot_anlaeg(hedensted_juelsminde)

st_save_cleaned(hedensted_juelsminde)




# Gilleleje, Helsinge (Gribvand) ------------------------------------------

gribvand_gilleleje_helsinge <- auto_read_shp(
  "S:/Spildevand/National overvågning/Modtagne_GIS-filer/Gribvand"
)

plot_anlaeg(gribvand_gilleleje_helsinge)

st_save_cleaned(gribvand_gilleleje_helsinge)






# Morsø -------------------------------------------------------------------

karby <- auto_read_shp(
  "S:/Spildevand/National overvågning/Modtagne_GIS-filer/Morsø",
  "Karby_Oplande_region",
  collapse = "Karby"
)

langtoftegård <- auto_read_shp(
  "S:/Spildevand/National overvågning/Modtagne_GIS-filer/Morsø",
  "Langtoftegård_Oplande_region",
  collapse = "Sundby (Langtoftegård)"
)

østrestrand <- auto_read_shp(
  "S:/Spildevand/National overvågning/Modtagne_GIS-filer/Morsø",
  "ØstreStrand_Oplande_region",
  collapse = "Nykøbing Mors (Østre Strand)"
)


# I'm adding a buffer because this shape looks very specific, possibly about
# individual properties. If the CPR register has the coordinates slightly off, I
# don't want them to be missed. Simplifying after to not make the shape to
# complicated.
morsø <- bind_rows(karby, langtoftegård, østrestrand) %>%
  st_buffer(10) %>%
  st_simplify(dTolerance = 5) %>%
  st_fill_holes(200)

morsø %>%
  plot_anlaeg()


st_save_cleaned(morsø)







# Grindsted -------------------------------------------------------

billund_grindsted <- auto_read_shp(
  "S:/Spildevand/National overvågning/Modtagne_GIS-filer/Billund Grindsted"
) %>%
  mutate(anlaeg = "Grindsted/Billund")

plot_anlaeg(billund_grindsted)

st_save_cleaned(billund_grindsted)






# DIN Forsyning -----------------------------------------------------------

# They sent a MXD file, which we can't use. Asked for shapes.






# KLAR --------------------------------------------------------------------

# Greve, Køge, Solrød, Stevns
greve <- auto_read_shp(
  "S:/Spildevand/National overvågning/Modtagne_GIS-filer/KLAR forsyning",
  "Greve"
)
køge <- auto_read_shp(
  "S:/Spildevand/National overvågning/Modtagne_GIS-filer/KLAR forsyning",
  "Koege"
)
solrød <- auto_read_shp(
  "S:/Spildevand/National overvågning/Modtagne_GIS-filer/KLAR forsyning",
  "Solroed"
)
stevns <- auto_read_shp(
  "S:/Spildevand/National overvågning/Modtagne_GIS-filer/KLAR forsyning",
  "Stevns"
)

klar_greve_køge_solrød_stevns <- bind_rows(
  greve,
  køge,
  solrød,
  stevns
) %>%
  mutate(anlaeg = recode(anlaeg,
                         "KoegeEgnens" = "Køge",
                         "Solroed" = "Solrød",
                         "StroebyLadeplads" = "Strøby",
                         "StoreHeddinge" = "Store Heddinge"))

plot_anlaeg(klar_greve_køge_solrød_stevns)

st_save_cleaned(klar_greve_køge_solrød_stevns)








# Vandcenter Syd (Odense etc) ---------------------------------------------

## Odense VCS (Ejby Mølle) -------------------------------------------------

# The plant (and original folder) is called Ejby Mølle, but it's Odense

st_layers("Oplande/raw/Ejby_Mølle_VCS_Shp_filer")

odense_ejby_moelle_vcs <- auto_read_shp("Oplande/raw/Ejby_Mølle_VCS_Shp_filer",
                                        "VandCenter_Syd")



odense_nordøst <- auto_read_shp(
  "S:/Spildevand/National overvågning/Modtagne_GIS-filer/Nordøst Renseanlæg"
) %>%
  mutate(anlaeg = "Odense (Nordøst)")

odense_nordvest <- auto_read_shp(
  "S:/Spildevand/National overvågning/Modtagne_GIS-filer/Nordvest Renseanlæg"
) %>%
  mutate(anlaeg = "Odense (Nordvest)")

bogense <- auto_read_shp(
  "S:/Spildevand/National overvågning/Modtagne_GIS-filer/Bogense Renseanlæg"
) %>%
  mutate(anlaeg = "Bogense")

søndersø <- auto_read_shp(
  "S:/Spildevand/National overvågning/Modtagne_GIS-filer/Søndersø By Renseanlæg"
) %>%
  mutate(anlaeg = "Søndersø")

otterup <- auto_read_shp(
  "S:/Spildevand/National overvågning/Modtagne_GIS-filer/Otterup Renseanlæg"
) %>%
  mutate(anlaeg = "Otterup")

hofmansgave <- auto_read_shp(
  "S:/Spildevand/National overvågning/Modtagne_GIS-filer/Hofmansgave Renseanlæg"
) %>%
  mutate(anlaeg = "Hofmansgave")


vcs_odense <- bind_rows(
  odense_ejby_moelle_vcs,
  odense_nordøst,
  odense_nordvest,
  bogense,
  søndersø,
  otterup,
  hofmansgave
)

plot_anlaeg(vcs_odense)

st_save_cleaned(vcs_odense)



## Vollsmose (Odense decentral) --------------------------------------------

odense_vollsmose <- auto_read_shp("S:/Spildevand/National overvågning/Modtagne_GIS-filer/Vandcentersyd/Vollsmose_opland_graense.shp",
                                  collapse = "Vollsmose")

plot_anlaeg(odense_vollsmose)

st_save_cleaned(odense_vollsmose)


# Bornholm ----------------------------------------------------------------
# Finally

bornholm <- auto_read_shp(
  "S:/Spildevand/National overvågning/Modtagne_GIS-filer/bornholm",
  anlaeg_col = Bem
)

plot_anlaeg(bornholm)

st_save_cleaned(bornholm)











# Kerteminde/Nyborg kommuner --------------------------------------------------

# This is a complicated situation.
#
# Kerteminde has 2 anlæg: Hidsholm and Vest. They measure at both, but mix the
# samples and send them to Eurofins as 1 sample.
#
# The bottom part of Kerteminde Vest (Langeskov By), actually pumps most of its
# wastewater (80%) to Nyborg anlæg, which belongs to NFS forsyning across the
# kommune border.
#
# So I've got to delete the Langeskov part from the Kerteminde Vest opland, and
# add it to the Nyborg opland.

# And then I also need to group the remaining two Kerteminde oplande together,
# and connect them in the lookup to to the single Kerteminde sample we get from
# Eurofins.


## Read all Kerteminde/Nyborg data -----------------------------------------



### Kerteminde (Hidsholm, Nyborg?) ----------------------------------

raw_kerteminde <- auto_read_shp(
  "S:/Spildevand/National overvågning/Modtagne_GIS-filer/Kerteminde",
  layer = "OPLANDE"
) %>%
  mutate(anlaeg = glue("Kerteminde ({indløb})",
                       indløb = word(anlaeg, 2)))



### Langeskov by ------------------------------------------------------------

langeskov <- auto_read_shp(
  "S:/Spildevand/National overvågning/Modtagne_GIS-filer/OPLAND_LANGESKOV",
  layer = "OPLAND_LANGESKOV"
) %>%
  mutate(anlaeg = "Langeskov")


### Nyborg & Ørbæk ----------------------------------------------------------

# Haven't saved this one yet. They also get wastewater from a neighbouring
# kommune, which they don't have the shapefiles for. We need them if they always
# get that wastewater, but not if they only get it in overflow situations.
# Waiting to hear back

raw_nyborg <- auto_read_shp(
  "S:/Spildevand/National overvågning/Modtagne_GIS-filer/NyborgØrbæk",
  "Kloakoplande til Nyborg final",
  collapse = "Nyborg"
)

ørbæk <- auto_read_shp(
  "S:/Spildevand/National overvågning/Modtagne_GIS-filer/NyborgØrbæk",
  "Kloakoplande til Ørbæk final",
  collapse = "Ørbæk"
)




## Clean/separate/combine ----------------------------------------------------

# Extract Langeskov from Kerteminde
kerteminde <- raw_kerteminde %>%
  st_difference(langeskov %>% select()) %>%
  st_collapse_features(anlaeg = "Kerteminde")

plot_anlaeg(raw_kerteminde) +
  labs(title = "Raw Kerteminde shape")
plot_anlaeg(kerteminde %>% bind_rows(langeskov)) +
  labs(title = "Langeskov separated out")


# Add Langeskov to Nyborg
nyborg <- bind_rows(raw_nyborg, langeskov) %>%
  st_collapse_features(anlaeg = "Nyborg")

nyborg %>%
  plot_anlaeg()

# Combine Nyborg and Ørbæk
nyborg_ørbæk <- bind_rows(nyborg, ørbæk)



# Check final shapes
bind_rows(
  raw_kerteminde,
  raw_nyborg,
  ørbæk,
  langeskov
) %>%
  plot_anlaeg() +
  labs(title = "All Kerteminde/Nyborg oplande (original-ish)",
       subtitle = str_wrap("Langeskov originally belonged to Kerteminde (vest), but adding to Nyborg because that's where most of their WW goes", 70))

bind_rows(
  kerteminde,
  nyborg_ørbæk
) %>%
  plot_anlaeg() +
  labs(title = "All Kerteminde/Nyborg oplande (final)",
       subtitle = str_wrap("Langeskov originally belonged to Kerteminde (vest), but adding to Nyborg because that's where most of their WW goes", 70))


st_save_cleaned(kerteminde)
st_save_cleaned(nyborg_ørbæk)








# Assens ------------------------------------------------------------------

raw_assens <- st_read("S:/Spildevand/National overvågning/Modtagne_GIS-filer/Assens")

assens <- raw_assens %>%
  rename(anlaeg = Anlaeg) %>%
  as_tibble() %>%
  st_as_sf() %>%
  st_zm() %>%
  st_simplify() %>%
  st_cast("POLYGON") %>%
  st_make_valid() %>%
  st_fill_holes(200) %>%
  group_by(anlaeg) %>%
  summarise() %>%
  st_simplify() %>%
  st_fill_holes(200) %>%
  st_simplify() %>%
  mutate(anlaeg = recode(anlaeg,
                         "Haarby" = "Hårby",
                         "Aa" = "Å Strand",
                         "Aarup" = "Årup")) %>%
  print()

plot_anlaeg(assens)

st_save_cleaned(assens)






# Odsherred (Fårevejle and Nykøbing) ------------------------------------

odsherred_nykøbing_fårevejle <- auto_read_shp(
  "S:/Spildevand/National overvågning/Modtagne_GIS-filer/Odsherred/oplande.geojson",
  anlaeg_col = anlaegnavn
) %>%
  mutate(anlaeg = str_rm_renseanlaeg(anlaeg))

plot_anlaeg(odsherred_nykøbing_fårevejle)

st_save_cleaned(odsherred_nykøbing_fårevejle)






# Novafos -----------------------------------------------------------------

novafos <- auto_read_shp(
  "S:/Spildevand/National overvågning/Modtagne_GIS-filer/Novafos/SSI Novafos renseanlægsoplande 1.0.geojson"
) %>%
  mutate(anlaeg = str_rm_renseanlaeg(anlaeg) %>%
           str_remove(" Rens$") %>%
           str_squish())


plot_anlaeg(novafos)

st_save_cleaned(novafos)



## Novafos decentral -------------------------------------------------------

# These are some options for where to put samplers. Send to Anders to get
# populations and vax coverage.

st_read(
  "S:/Spildevand/National overvågning/Modtagne_GIS-filer/Novafos/SSI"
) %>%
  st_write(
    "S:/Spildevand/National overvågning/Modtagne_GIS-filer/Novafos/SSI/Mørkhøj options.geojson",
    delete_dsn = TRUE
  )



# Lyngby/Taarbæk ----------------------------------------------------------

# The kommune and forsyning is Lyngby-Taarbæk, but in the master data it's
# called Mølleværket, which is the name of the anlæg (or Mølleåen?). They also
# sent two other tiny shapes on the coast, but they're covered by
# Lynetten/Rudersdal.

raw_lyngby_taarbæk <- st_read("S:/Spildevand/National overvågning/Modtagne_GIS-filer/Lyngby-Taarbæk/01 LTF_til_MØV/LTF_SP_opland_v01.dbf")

# This one is very detailed so I'm doing a lot of heavy simplification
lyngby_taarbæk <- raw_lyngby_taarbæk %>%
  correct_crs() %>%
  st_make_valid() %>%
  st_zm() %>%
  st_simplify(dTolerance = 5) %>%
  st_buffer(25, nQuadSegs = 1) %>%
  summarise(anlaeg = "Lyngby-Taarbæk (Mølleåen)") %>%
  st_simplify(dTolerance = 10) %>%
  st_fill_holes(10000)

object.size(lyngby_taarbæk)

lyngby_taarbæk %>%
  plot_anlaeg()

st_save_cleaned(lyngby_taarbæk)



# Brønderslev -------------------------------------------------------------

# This doesn't include Aså and Hjallerup renseanlæg, which are from the same forsyning. Asked for them 2021-11-22.

brønderslev <- auto_read_shp(
  "S:/Spildevand/National overvågning/Modtagne_GIS-filer/Brønderslev/Brønderslev_oplande/Brønderslev_oplande.geojson"
) %>%
  mutate(anlaeg = str_rm_renseanlaeg(anlaeg))

plot_anlaeg(brønderslev)

st_save_cleaned(brønderslev)






# NK-Spildevand -----------------------------------------------------------

nk_spildevand <- auto_read_shp(
  "S:/Spildevand/National overvågning/Modtagne_GIS-filer/NK-Spildevand",
  anlaeg_col = "Renseanlæ") %>%
  mutate(anlaeg = anlaeg %>% str_rm_renseanlaeg() %>%
           str_replace_all("_", " ") %>%
           str_squish()) %>%
  # We're only measuring from three of htese sites
  filter(anlaeg %in% c("Næstved", "Fuglebjerg", "Holme Olstrup")) %>%
  print()

plot_anlaeg(nk_spildevand)

st_save_cleaned(nk_spildevand)





# Ringsted ----------------------------------------------------------------

ringsted <- auto_read_shp(
  "S:/Spildevand/National overvågning/Modtagne_GIS-filer/Ringsted/Opland M_llevej  til SSI",
  overwrite_crs = TRUE,
  collapse = "Ringsted"
)

plot_anlaeg(ringsted)

st_save_cleaned(ringsted)


# Guldborgsund ------------------------------------------------------------

marielyst <- auto_read_shp(
  "S:/Spildevand/National overvågning/Modtagne_GIS-filer/Guldborgsund/Marielyst_final_Oplande_GF_region",
  collapse = "Marielyst"
)


nykøbing_falster <- auto_read_shp(
  "S:/Spildevand/National overvågning/Modtagne_GIS-filer/Guldborgsund/Nykøbing_final_Oplande_GF_region",
  collapse = "Nykøbing Falster"
)

guldborgsund <- bind_rows(nykøbing_falster,
                          marielyst) %>%
  st_buffer(10, nQuadSegs = 2) %>%
  st_simplify(dTolerance = 10)

plot_anlaeg(guldborgsund)

object.size(guldborgsund)


st_save_cleaned(guldborgsund)






# Slagelse Kommune (SK Spildevand) ----------------------------------------

sk_spildevand <- auto_read_shp(
  "S:/Spildevand/National overvågning/Modtagne_GIS-filer/SK Spildevand/Slagelse kommune"
)

plot_anlaeg(sk_spildevand)

st_save_cleaned(sk_spildevand)

# shapes for Ishoej------
#used the Fredericia way to get this geoJSon
library(sp)
library(rgeos)
library(sf)
raw_lau <- st_read("S:/Spildevand/Lille OEU-sag/Oplande/raw/LAU_RG_01M_2019_4326.geojson")

check_crs(raw_lau)

ishoj <- raw_lau %>%
  filter(CNTR_CODE == "DK",
         LAU_NAME == "Ish?j") %>%
  transmute(anlaeg = LAU_NAME) %>%
  correct_crs()
proj4string(ishoj) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")


ishoj %>%
  ggplot() +
  geom_sf(fill = "magenta") +
  labs(title = "Ish?j (municipality boundary from Eurostat 2019)")
ishoj <- st_transform(ishoj, crs = 25832)

ishoj1x <- c(55.615985, 55.611962, 55.609320, 55.605805, 55.611405, 55.617585)
ishoj1y <- c(12.351638, 12.346316, 12.352711, 12.346617, 12.334686, 12.346531)
ishoj1xy <- cbind(ishoj1y, ishoj1x)
ishoj1xy <- st_coordinates(ishoj1xy)
ishoj1p <- Polygon(ishoj1xy)
ishoj1ps <- Polygons(list(ishoj1p),1)
ishoj1 <- SpatialPolygons(list(ishoj1ps))
proj4string(ishoj1) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
datahoj1 <- data.frame(f = 99.9)
ishoj1f <- SpatialPolygonsDataFrame(ishoj1, datahoj1)
ishoj1sf <- st_as_sf(ishoj1f)
ishoj1sf <- st_transform(ishoj1sf, crs = 25832)
ishoj1sf %>%
  ggplot()+
  geom_sf(fill = "dark red")+
  labs(title = "Ish?j 1")
ishoj1sf <- ishoj1sf %>%
  mutate(anlaeg = "Is01") %>%
  select(anlaeg, geometry)


ishoj2x <- c(55.616228, 55.617512, 55.616688, 55.618506, 55.619911, 55.619451, 55.619815, 55.620493, 55.620929, 55.621244, 55.622941, 55.622044, 55.622721, 55.622319, 55.620845, 55.618669)
ishoj2y <- c(12.360951, 12.362453, 12.364684, 12.369019, 12.367388, 12.366143, 12.364513, 12.365156, 12.364513, 12.364813, 12.359878, 12.358419, 12.357235, 12.356052, 12.360209, 12.356796)
ishoj2xy <- cbind(ishoj2y, ishoj2x)
ishoj2xy <- st_coordinates(ishoj2xy)
ishoj2p <- Polygon(ishoj2xy)
ishoj2ps <- Polygons(list(ishoj2p),1)
ishoj2 <- SpatialPolygons(list(ishoj2ps))
proj4string(ishoj2) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
datahoj2 <- data.frame(f = 99.9)
ishoj2f <- SpatialPolygonsDataFrame(ishoj2, datahoj2)
ishoj2sf <- st_as_sf(ishoj2f)
ishoj2sf <- st_transform(ishoj2sf, crs = 25832)
ishoj2sf %>%
  ggplot()+
  geom_sf(fill = "dark green")+
  labs(title = "Ish?j 2")
ishoj2sf <- ishoj2sf %>%
  mutate(anlaeg = "Is02") %>%
  select(anlaeg, geometry)

ishoj3x <- c(55.618499, 55.617231, 55.615192, 55.610840, 55.605445, 55.602325, 55.604657, 55.604925, 55.609336, 55.611872, 55.616911, 55.618512, 55.616173, 55.617389, 55.616455, 55.617629)
ishoj3y <- c(12.369057, 12.370089, 12.369179, 12.374330, 12.365623, 12.358704, 12.354334, 12.348462, 12.354011, 12.346481, 12.352983, 12.356500, 12.361036, 12.362462, 12.364602, 12.366650)
ishoj3xy <- cbind(ishoj3y, ishoj3x)
ishoj3xy <- st_coordinates(ishoj3xy)
ishoj3p <- Polygon(ishoj3xy)
ishoj3ps <- Polygons(list(ishoj3p),1)
ishoj3 <- SpatialPolygons(list(ishoj3ps))
proj4string(ishoj3) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
datahoj3 <- data.frame(f = 99.9)
ishoj3f <- SpatialPolygonsDataFrame(ishoj3, datahoj3)
ishoj3sf <- st_as_sf(ishoj3f)
ishoj3sf <- st_transform(ishoj3sf, crs = 25832)
ishoj3sf %>%
  ggplot()+
  geom_sf(fill = "dark blue")+
  labs(title = "Ish?j 3")
ishoj3sf <- ishoj3sf %>%
  mutate(anlaeg = "Is03") %>%
  select(anlaeg, geometry)

st_write(ishoj, "S:/Spildevand/Lille OEU-sag/Oplande/cleaned/ishoejkommune.geojson")
st_write(ishoj1sf, "S:/Spildevand/Lille OEU-sag/Oplande/cleaned/ishoej1.geojson", append = FALSE)
st_write(ishoj2sf, "S:/Spildevand/Lille OEU-sag/Oplande/cleaned/ishoej2.geojson", append = FALSE)
st_write(ishoj3sf, "S:/Spildevand/Lille OEU-sag/Oplande/cleaned/ishoej3.geojson", append = FALSE)

st_crs(ishoj1sf)
st_crs(skovvejen)

# Skovvejen and Skovparken --------------------------


skovvejen <- auto_read_shp(
  "S:/Spildevand/Lille OEU-sag/Oplande/raw/Skovvejen/Skovvejen.geojson",
  anlaeg_col = Anlaeg
)
skovvejen %>%
  ggplot()+
  geom_sf(fill = "dark blue")+
  labs(title = "skovvejen")
st_write(skovvejen, "S:/Spildevand/Lille OEU-sag/Oplande/cleaned/skovvejen.geojson")

skovparken <- auto_read_shp(
  "S:/Spildevand/Lille OEU-sag/Oplande/raw/Skovparken/Skovparken.geojson",
  anlaeg_col = Anlaeg
)
skovparken %>%
  ggplot()+
  geom_sf(fill = "dark blue")+
  labs(title = "skovparken")
st_write(skovparken, "S:/Spildevand/Lille OEU-sag/Oplande/cleaned/skovparken.geojson")

# Albertslund ---------------
albertslund <- st_read("S:/Spildevand/Lille OEU-sag/Oplande/raw/Albertslund")

albertslund <- albertslund %>%
  mutate(anlaeg = "Albertslund") %>%
  select(anlaeg, geometry)

albertslund %>%
  ggplot()+
  geom_sf(fill = "dark blue")+
  labs(title = "albertslund")

st_write(albertslund, "S:/Spildevand/Lille OEU-sag/Oplande/cleaned/Albertslund.geojson")

# Br?ndby strand ---------

bsx <- c(12.392, 12.4115, 12.4117, 12.4246, 12.4253, 12.4253, 12.4246, 12.400, 12.397)
bsy <- c(55.6279, 55.629, 55.628, 55.630, 55.6285, 55.626, 55.6228, 55.6246, 55.6247)
bsxy <- cbind(bsx, bsy)
bsxy <- st_coordinates(bsxy)
bsp <- Polygon(bsxy)
bsps <- Polygons(list(bsp),1)
bs <- SpatialPolygons(list(bsps))
proj4string(bs) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
databs <- data.frame(f = 99.9)
bsf <- SpatialPolygonsDataFrame(bs, databs)
bssf <- st_as_sf(bsf)
bssf <- st_transform(bssf, crs = 25832)
bssf %>%
  ggplot()+
  geom_sf(fill = "dark blue")+
  labs(title = "Br?ndby Strand")
bssf <- bssf %>%
  mutate(anlaeg = "Br?ndby Strand") %>%
  select(anlaeg, geometry)

st_write(bssf, "S:/Spildevand/Lille OEU-sag/Oplande/cleaned/Br?ndby_Strand.geojson")






# HOFOR 2022-08 -----------------------------------------------------------


kbh_nord <- auto_read_shp("S:/Spildevand/Lille OEU-sag/Oplande/raw/BIOFOS/Shapefiler_prøvetagningsteder_SSI",
                          "KBH_Nord",
                          collapse = "København Nord (HOFOR 2022-08)")

østamager <- auto_read_shp("S:/Spildevand/Lille OEU-sag/Oplande/raw/BIOFOS/Shapefiler_prøvetagningsteder_SSI",
                          "Oestamager",
                          collapse = "Østamager (HOFOR 2022-08)")

pstSTRV_hel <- auto_read_shp("S:/Spildevand/Lille OEU-sag/Oplande/raw/BIOFOS/Shapefiler_prøvetagningsteder_SSI",
                          "pstSTRV_hel",
                          collapse = "pstSTRV_hel (HOFOR 2022-08)",
                          holes_threshold_m2 = 2000)

valby_frederiksberg <- auto_read_shp("S:/Spildevand/Lille OEU-sag/Oplande/raw/BIOFOS/Shapefiler_prøvetagningsteder_SSI",
  "Valby_frederiksberg",
  collapse = "Valby/Frederiksberg (HOFOR 2022-08)")

vestamager <- auto_read_shp("S:/Spildevand/Lille OEU-sag/Oplande/raw/BIOFOS/Shapefiler_prøvetagningsteder_SSI",
                            "Vestamager",
                            collapse = "Vestamager (HOFOR 2022-08)")

vesterbro_nørrebro <- auto_read_shp("S:/Spildevand/Lille OEU-sag/Oplande/raw/BIOFOS/Shapefiler_prøvetagningsteder_SSI",
                          "Vesterbro_Noerrebro",
                          collapse = "Vesterbro/Nørrebro (HOFOR 2022-08)")


hofor_2022_08 <- bind_rows(
  kbh_nord,
  østamager,
  pstSTRV_hel,
  valby_frederiksberg,
  vestamager,
  vesterbro_nørrebro
) %>%
  st_simplify(dTolerance = 20) %>%
  st_fill_holes(2000)


ggplot(hofor_2022_08) +
  geom_sf(aes(fill = anlaeg,
              colour = anlaeg),
          alpha = 0.5)



st_save_cleaned(hofor_2022_08)
