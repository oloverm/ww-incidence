
# Data checks -------------------------------------------------------------


# Missing from lookup
ww_human %>%
  anti_join(lookup_oplande_names,
            by = c("anlaeg_rando" = "anlaeg_display")) %>%
  distinct(anlaeg_rando, proevested)


# Missing region/landsdel
ww_human %>%
  filter(is.na(region) | is.na(landsdel)) %>%
  distinct(anlaeg_rando, proevested)



# Unknown in_use
lookup_oplande_names %>%
  filter(is.na(in_use))


# Dates in the future???
ww_human %>%
  filter(!is.na(sample),
         date_receipt > today()) %>%
  select(anlaeg_rando, proevested, date_receipt, sample)





