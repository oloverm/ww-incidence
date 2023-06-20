## Lookup ------------------------------------------------------------------
raw_lookup_oplande_names <- readxl::read_xlsx(
  here("Data/lookup_oplande_names.xlsx")
)

lookup_oplande_names <- raw_lookup_oplande_names %>%
  mutate(anlaeg_display = anlaeg_display %>%
           coalesce(anlaeg_eurofins,
                    anlaeg_shp) %>%
           str_remove(regex("renseanl(ae|æ)g", ignore_case = TRUE)) %>%
           str_squish())

lookup_oplande_names_all_list <- lookup_oplande_names %>%
  filter(in_use == "TRUE") %>%
  as.list()


## Lookup for anlaeg included after july 2022 (89 places in total) ------------------------------------------------------------------
raw_lookup_oplande_names_new <- readxl::read_xlsx(
  here("Data/lookup_oplande_names_july22.xlsx")
)

# Just going to need it as a list since all will be included until the 11/7-22
lookup_oplande_names_new <- raw_lookup_oplande_names_new %>%
  mutate(anlaeg_display = anlaeg_display %>%
           coalesce(anlaeg_eurofins,
                    anlaeg_shp) %>%
           str_remove(regex("renseanl(ae|æ)g", ignore_case = TRUE)) %>%
           str_squish()) %>%
  filter(in_use == "TRUE") %>%
  as.list()

