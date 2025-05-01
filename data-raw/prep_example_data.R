## code to prepare `example` dataset goes here

samp_sch_data <- read.csv("data-raw/cdeTools_example.csv")

usethis::use_data(samp_sch_data, overwrite = TRUE)
