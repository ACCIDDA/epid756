## Code to prepare `simdata` dataset

simdata <- readRDS("data-raw/simdata.rds")

usethis::use_data(simdata, overwrite = TRUE)
