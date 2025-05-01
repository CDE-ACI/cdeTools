library(readxl)
library(usethis)

cutscores_default <- list(
  traditional = readxl::read_excel("data-raw/fwk_cutscores_2024.xlsx", sheet = "traditional"),
  aec = readxl::read_excel("data-raw/fwk_cutscores_2024.xlsx", sheet = "aec")
)

usethis::use_data(cutscores_default, internal = TRUE, overwrite = TRUE)
