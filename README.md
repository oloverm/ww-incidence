
# Predicting COVID-19 incidence using wastewater surveillance data in Denmark, October 2021 – June 2022

This repository contains most of the code and part of the data underlying the paper. The data is restricted because not all of it is allowed to be in the public domain. Some of the code is not included because it's not relevant.

The key code for analysis is `R/tsa/tsa.Rmd`. A lot of that can be run if you read in `data/dk_restricted.csv` first, but not the parts referring to care personnel.

The `dk` dataset is already processed according to all the data cleaning steps described in the paper. Those steps are performed in the function `read_ww()`, which runs the scripts `R/01_read-data.R` and `R/02_clean-data.R`.
