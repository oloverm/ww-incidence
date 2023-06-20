lookup_oplande_names %>%
  filter(in_use,
         missing_shp == TRUE) %>%
  nrow()

list(
  never_sent_sample = never_sent_sample,
  no_recent_samples = no_recent_samples
)



# He also needs (from other sources so far):
# - Number of signals reported this week, by category
# - % increase per region and nationally
# - Oplande where we're missing shapes: 28
# - Oplande where we’re missing samples: 7
