# Quick start: if you just want to run everything, click "Source" in the
# top-right corner of this window



# Setup ------------------------------------------------------------------------
# You have to run this part every time you open RStudio


# This sets your working directory to the right folder
setwd("S:/Spildevand/Lille OEU-sag")
if (!"here" %in% installed.packages()) install.packages("here")
here::i_am("R/00-0_run.R", uuid = "36e8c2a8-e55d-4a3e-8ab2-d76f0cb7edbf")


# This runs a setup script. It should automatically install any extra packages
# you need.
source("R/00_setup.R", encoding = 'UTF-8')

# Read data
read_ww(cache_limit = hours(1000))

# Check if lookup_oplande_names file is up to date
source("R/occasional/lookup_help.R", encoding = 'UTF-8')

# Extra cleaning
# TODO: add this to read_ww(), and add it to cache too
source("R/02-2_clean-signals.R", encoding = 'UTF-8')
source("R/02-3_clean-diagnostic.R", encoding = 'UTF-8')



# Outputs
source("R/03-1_figs-website.R", encoding = 'UTF-8')
source("R/03-2_figs-aggregated.R", encoding = 'UTF-8')
source("R/03-3_figs-signaler.R", encoding = 'UTF-8')
source("R/03-4_figs-maps.R", encoding = 'UTF-8')

# Report with interesting outputs
rmarkdown::render(
  "R/report.R",
  output_file = paste0("../outputs/", Sys.Date(), "_report.html"),
)


# Report with maps
rmarkdown::render(
  "R/maps_report.R",
  output_file = paste0(glue("S:/Spildevand/outputs/maps/week_{week}/",
                            week = week(today() - days(7))),
                       Sys.Date(), "_maps_report.html")
)


# Website graphs
rmarkdown::render(
  "R/website-graphs.R",
  output_file = paste0("../outputs/website/",
                       format(Sys.time(), "%Y-%m-%d-%H%M"),
                       "_website-graphs.docx")
)


# # Historical signals for STPS
# rmarkdown::render(
#   "R/historical-signals.R",
#   output_file = paste0("../outputs/signaler/",
#                        format(Sys.time(), "%Y-%m-%d-%H%M"),
#                        "_historical-signals.html")
# )






