

#source("R/00_setup.R", encoding = "utf-8")

# Read data (not necessary when implemented in "00-0_run.R)

# Read lookup
source("R/02-1_read-clean-lookup.R", encoding = 'UTF-8')


# List of anlæg and their forsyninger from the shapes
forsyninger_anlaeg_shp <- read_xlsx("Oplande/forsyninger_anlaeg.xlsx")

# List of anlæg from Eurofins
anlaeg_eurofins <- fp_latest("data/ww-human",
                             "[0-9]{4}-[0-9]{2}-[0-9]{2}_ww-human\\.xlsx") %>%
  read_excel() %>%
  select(proevested) %>%
  filter(!is.na(proevested)) %>%
  distinct(proevested)


cli_alert_success("Finished reading anlæg and shapes")

cli_alert("Checking if shapes are present in the look up file")

# Shapes that aren't represented in the lookup yet. See if they fit with any of
# the Eurofins names, otherwise add them on the end of the anlaeg_shp column.
shapes <- forsyninger_anlaeg_shp %>%
  anti_join(lookup_oplande_names, by = c("anlaeg" = "anlaeg_shp")) %>%
  compact()

# Tell which shapes are not present
if (is_empty(shapes) == TRUE) {
  cli_div(theme = list(span.bold = list(color = "red")))
  cli_alert_success("Shapes are up to date")
} else if (is_empty(shapes) == FALSE) {
  cli_alert_danger("{.bold Important!} Check/add following to anlaeg_shp in {.file Data/lookup_oplande_names.xlsx}: {shapes$anlaeg}")
}

# Eurofins data points that aren't represented in the lookup yet. See if they
# fit with any of the shp names, otherwise add them to the end of the
# anlaeg_eurofins column.


cli_alert("Checking if Eurofins datapoints are updated in look up file ")

a_eurofins <- anlaeg_eurofins %>%
  anti_join(lookup_oplande_names,
            by = c("proevested" = "anlaeg_eurofins")) %>%
  arrange(proevested) %>%
  compact()


# Creating statement about either having no missing in lookup 

if (is_empty(a_eurofins) == TRUE) {
  cli_div(theme = list(span.bold = list(color = "red")))
  cli_alert_success("Eurofins datapoints are up to date")
} else if (is_empty(a_eurofins) == FALSE) {
    cli_alert_danger("{.bold Important!} Check/add following to anlaeg_eurofins in {.file Data/lookup_oplande_names.xlsx}: {a_eurofins$proevested}")
  }



# Copy the values here, then I can paste it in the company column
# lookup_oplande_names %>%
#   select(anlaeg_shp) %>%
#   tidylog::left_join(forsyninger_anlaeg_shp,
#                      by = c("anlaeg_shp" = "anlaeg")) %>%
#   select(company)







# Re-read lookup
#source("R/02-1_read-clean-lookup.R", encoding = 'UTF-8')
