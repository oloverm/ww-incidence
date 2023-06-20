#'
#' # Read data
#'

# Read data ---------------------------------------------------------------


# Shapes for oplande
cli_alert("Reading opland shapes")
shp_oplande <- st_read(
  fp_latest(here("Oplande"), "all-anlaeg_[0-9-]{10}\\.geojson"),
  quiet = TRUE
)


# Populations
cli_alert("Reading opland populations")
pops <- read_xls(fp_latest("Q:/SSI/Spildevand/Populations from shapes/output",
                           "Leverance oplande .+xls$"))


# Lookup
cli_alert("Reading anlæg name lookup")
source(here("R/02-1_read-clean-lookup.R"),
       encoding = 'UTF-8')




cli_alert("Reading OWID data")
# OWID hospitalisation data
raw_owid <- read_csv(
  "https://covid.ourworldindata.org/data/owid-covid-data.csv",
  col_types = cols(
    .default = col_skip(),
    location = col_character(),
    date = col_date(format = ""),
    icu_patients = col_double(),
    # icu_patients_per_million = col_double(),
    hosp_patients = col_double(),
    # hosp_patients_per_million = col_double(),
    # weekly_icu_admissions = col_double(),
    # weekly_icu_admissions_per_million = col_double(),
    weekly_hosp_admissions = col_double(),
    # weekly_hosp_admissions_per_million = col_double(),
    #new_tests_per_thousand = col_double(),
    #new_cases = col_double()
  )
)

cli_alert("Reading test and case data")

raw_test_historical <- read_xlsx("S:/Spildevand/Lille OEU-sag/data/Testede_og_positive_pr_opland_pr_dato_NY_METODE.xlsx") 

# Få sti til alle daglige filer filerne
file_list_test <- list.files("Q:/SSI/Spildevand/tests_opland",
                             full.names = TRUE)

# Ændre navne på filerne til datoer fra filnavnet
file.list2 <- str_remove_all(file_list_test, "[^\\d-]") 

# Overskriv navnene
file_list_test <- setNames(file_list_test, file.list2) # only needed when you need an id-column with the file-names

raw_test_daily <- map_df(file_list_test, read_excel, .id = "dato")



# Read human --------------------------------------------------------------

# cli_alert("Reading human data")
# # Human data is fragmented in many daily files. Takes a long time to read, so we
# # save the cumulative data in the folder. This checks whether the latest data
# # has already been written in there. Reads it if so, otherwise gets it the slow
# # way and saves the cumulative. Only updated around 14:30, so will look at
# # yesterday's data if you're running it before then.
# 
# days_ago_human_target <- if_else(
#   hour(now()) > 14 | (hour(now()) == 14 & minute(now()) >= 40),
#   2,
#   3
# )
# 
# fp_human_target <- here(glue("data/human/{target_date}_human.csv",
#                              target_date = today() - days(days_ago_human_target)))
# 
# if (file.exists(fp_human_target)) {
#   # If the file exists, just use that
#   cli_alert_info("Using cached human data: {.file {fp_human_target}}")
#   human <- read_csv(fp_human_target)
# 
#   shp_human <- human %>%
#     filter(!is.na(x_koordinat), !is.na(y_koordinat)) %>%
#     st_as_sf(coords = c("x_koordinat", "y_koordinat"),
#              crs = st_crs(25832)) %>%
#     mutate(date_test = as.Date(date_test))
# } else {
#   # If the file doesn't exist, read in the slow way and then write the file
#   cli_alert_info("Reading new human data the slow way")
#   source(here("R/01-1_read-data-human.R"))
# 
#   human %>%
#     write_csv(here(glue("data/human/{latest_test}_human.csv",
#                          latest_test = max(.$date_test))))
# }







# Read WW -----------------------------------------------------------------
cli_alert("Reading WW data")
# Like the human data, reading all the small files becomes slow after a while,
# so we have a crude caching system.

# List of files from Eurofins
fps_ww <- list.files(here("data/spildevand"),
                     pattern = "\\.csv$",
                     full.names = TRUE,
                     recursive = FALSE)


# cli_alert_warning("Manually reading new data format file")
# fps_ww <- "S:/Spildevand/Lille OEU-sag/data/spildevand/Covid-19-SSI historisk 18-01-2022 new datasetup.csv"


files_list_ww <- fps_ww %>%
  set_names(basename) %>%
  # set_names(str_extract, "\\d{2}-?\\d{2}-?\\d{2,4}") %>%
  map(
    function(x) {

      read_csv2(
        x,
        skip = 2,
        locale = locale(encoding = "windows-1252",
                        decimal_mark = ",",
                        grouping_mark = "."),
        col_names = c(
          "batchcode",
          "lab_proeve_nr",
          "status",
          "rapportnr",
          "client_name",
          "matrice",
          "proevetagnings_dato",
          "proevetager",
          "initialer",
          "analyse_start",
          "rapporteringsdato",
          "modagelses_dato",
          "proevetagnings_metode",
          "nedboer_mm",
          "temperatur_c",
          "vandmaengde_l",
          "proevemaerke",
          "proevested",
          "volumen_proevemaengde_ml",
          "volumen_rna_ekstrakt_rdrp_gen_microl",
          "volumen_rna_ekstrakt_n_gen_microl",
          "sarscov2_kvalitativ",
          "trendvalue_rd_rp_cq",
          "sarscov2_rdrp_gen_kopier_ml",
          "sarscov2_rdrp_gen_kopier_l",
          "loq_rdrp_gen_kopier_l",
          "trendvalue_n_gen_cq",
          "sarscov2_n_gen_kopier_ml",
          "sarscov2_n_gen_kopier_l",
          "loq_n_gen_kopier_l",
          "pladeposition_1",
          "pladeposition_2",
          "waste_water_control_mnv_cq",
          "pladeplacering",
          "inhiberingskontrol",
          "pcr_rdrp_gen_kontrol_pos",
          "pcr_rdrp_gen_kontrol_neg",
          "pcr_n_gen_kontrol_pos",
          "pcr_n_gen_kontrol_neg",
          "mnv_e_pos_positiv_pcr_kontrol",
          "lod_rdrp",
          "lod_n",
          "k417n_wt",
          "k417n_mu",
          "l452r_wt",
          "l452r_mu",
          "batchkommentar",
          "proevekommentar",
          "n1_kvantitativ",
          "n1_trend_vaerdi",
          "n1_kopier_l",  # use
          "raw_loq_n1",  # use
          "raw_lod_n1",  # use
          "n1_c_pos",
          "n1_c_neg",
          "n2_trend_vaerdi",
          "n2_kvantitativ",
          "n2_kopier_l",  # use
          "raw_loq_n2",  # use
          "raw_lod_n2",  # use
          "n2_c",
          "n2_c_2",
          "pm_mo_v_trend_vaerdi",
          "pmmov_kvantitativ",
          "pmmov_kopier_l",  # use
          "raw_loq_pmmov",  # use
          "raw_lod_pmmov",  # use
          "pmmov_c_pos",
          "pmmov_c_neg",
          "cr_ass_p_trend_vaerdi",
          "crass_kvantitativ",
          "crass_kopier_l",  # use
          "raw_loq_crass",  # use
          "raw_lod_crass",  # use
          "crass_c_pos",
          "crass_c_neg",
          "mnv_trendvaerdi",
          "ipd",
          "rdrp_trend_vaerdi_2022",
          "rdrp_kvantitativ_2022",
          "rdrp_kopier_l_2022",  # use
          "raw_loq_rdrp_2022",  # use
          "raw_lod_rdrp_2022",  # use
          "rdrp_c_pos_2022",
          "rdrp_c_neg_2022",
          "comment_amount",
          "comment_cooling",
          "comment_overflow",
          "comment_grabsample",
          "comment_failed",
          "comment_other",
          "barcode"
        ),
        col_types = cols(
          .default = col_character(),
          proevetagnings_dato = col_datetime(format = ""),
          analyse_start       = col_datetime(format = ""),
          rapporteringsdato   = col_datetime(format = ""),
          modagelses_dato     = col_datetime(format = "")
        ),
      )
    }
  )


combined_ww <- bind_rows(files_list_ww, .id = "file")





cli_alert_success("Read in all data")


# # Read WW from database --------------------------------------------------------
# 
# cli_alert("Reading WW data from database")
# 
# 
# # Setup ------------------------------------------------------------------------
# here::i_am("R/01_read-data.R")
# 
# pacman::p_load(dplyr,
#                dbplyr,  # the package that translates dplyr code to SQL for you
#                readr,
#                tidyr,
#                here)
# 
# # Need odbc to connect to the database
# pacman::p_install(odbc, force = FALSE)
# 
# 
# 
# # This creates the connection to the database.
# con <- DBI::dbConnect(
#   odbc::odbc(),
#   Driver             = "ODBC Driver 17 for SQL Server",  # NOT "SQL Server"
#   Server             = "s-dnbdb02-p",
#   Database           = "tcdk_spildevand",
#   Trusted_Connection = "yes"  # This was very hard to find
# )
# 
# 
# # This creates the connection to the specific view they created for us, to
# # replicate the CSV.
# db_eurofins <- tbl(con, in_schema("dbo", "EuroFinsCsvFileReplacementView"))
# 
# 
# # If you want to just get the data onto your computer, you can use collect().
# # This is probably where you'll want to start, because then you can treat it as
# # normal data in R.
# df_eurofins <- db_eurofins %>%
#   collect()
# 
# # Have to manually change the column names to the ones we use 
# 
# colnames(df_eurofins) <-
#   c(
#     "batchcode",
#     "lab_proeve_nr",
#     "status",
#     "rapportnr",
#     "client_name",
#     "matrice",
#     "proevetagnings_dato",
#     "proevetager",
#     "initialer",
#     "analyse_start",
#     "rapporteringsdato",
#     "modagelses_dato",
#     "proevetagnings_metode",
#     "nedboer_mm",
#     "temperatur_c",
#     "vandmaengde_l",
#     "proevemaerke",
#     "proevested",
#     "volumen_proevemaengde_ml",
#     "volumen_rna_ekstrakt_rdrp_gen_microl",
#     "volumen_rna_ekstrakt_n_gen_microl",
#     "sarscov2_kvalitativ",
#     "trendvalue_rd_rp_cq",
#     "sarscov2_rdrp_gen_kopier_ml",
#     "sarscov2_rdrp_gen_kopier_l",
#     "loq_rdrp_gen_kopier_l",
#     "trendvalue_n_gen_cq",
#     "sarscov2_n_gen_kopier_ml",
#     "sarscov2_n_gen_kopier_l",
#     "loq_n_gen_kopier_l",
#     "pladeposition_1",
#     "pladeposition_2",
#     "waste_water_control_mnv_cq",
#     "pladeplacering",
#     "inhiberingskontrol",
#     "pcr_rdrp_gen_kontrol_pos",
#     "pcr_rdrp_gen_kontrol_neg",
#     "pcr_n_gen_kontrol_pos",
#     "pcr_n_gen_kontrol_neg",
#     "mnv_e_pos_positiv_pcr_kontrol",
#     "lod_rdrp",
#     "lod_n",
#     "k417n_wt",
#     "k417n_mu",
#     "l452r_wt",
#     "l452r_mu",
#     "batchkommentar",
#     "proevekommentar",
#     "n1_kvantitativ",
#     "n1_trend_vaerdi",
#     "n1_kopier_l",  # use
#     "raw_loq_n1",  # use
#     "raw_lod_n1",  # use
#     "n1_c_pos",
#     "n1_c_neg",
#     "n2_trend_vaerdi",
#     "n2_kvantitativ",
#     "n2_kopier_l",  # use
#     "raw_loq_n2",  # use
#     "raw_lod_n2",  # use
#     "n2_c",
#     "n2_c_2",
#     "pm_mo_v_trend_vaerdi",
#     "pmmov_kvantitativ",
#     "pmmov_kopier_l",  # use
#     "raw_loq_pmmov",  # use
#     "raw_lod_pmmov",  # use
#     "pmmov_c_pos",
#     "pmmov_c_neg",
#     "cr_ass_p_trend_vaerdi",
#     "crass_kvantitativ",
#     "crass_kopier_l",  # use
#     "raw_loq_crass",  # use
#     "raw_lod_crass",  # use
#     "crass_c_pos",
#     "crass_c_neg",
#     "mnv_trendvaerdi",
#     "ipd",
#     "rdrp_trend_vaerdi_2022",
#     "rdrp_kvantitativ_2022",
#     "rdrp_kopier_l_2022",  # use
#     "raw_loq_rdrp_2022",  # use
#     "raw_lod_rdrp_2022",  # use
#     "rdrp_c_pos_2022",
#     "rdrp_c_neg_2022",
#     "comment_amount",
#     "comment_cooling",
#     "comment_overflow",
#     "comment_grabsample",
#     "comment_failed",
#     "comment_other")
# 
# # Convert everything to characters except the dates - making the date in correct format.
# 
# df_eurofins <- df_eurofins %>%
#   mutate(across(!proevetagnings_dato & !analyse_start & !rapporteringsdato & !modagelses_dato, as.character)) %>%
#   mutate(proevetagnings_dato = as.Date(proevetagnings_dato, "%Y-%m-%d"), 
#          analyse_start = as.Date(analyse_start, "%d-%m-%y"),
#          rapporteringsdato = as.Date(rapporteringsdato,"%d-%m-%y"),
#          modagelses_dato = as.Date(modagelses_dato, "%d-%m-%y"))
# 
# cli_alert_success("Read in all data - including ww data from database")
