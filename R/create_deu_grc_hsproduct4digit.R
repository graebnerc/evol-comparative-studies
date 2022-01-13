#' @data{country_hsproduct4digit_year.csv,
#'   author = {The Growth Lab at Harvard University},
#'   publisher = {Harvard Dataverse},
#'   title = {{International Trade Data (HS, 92)}},
#'   year = {2019},
#'   version = {V4},
#'   doi = {10.7910/DVN/T4CHWJ},
#'   url = {https://doi.org/10.7910/DVN/T4CHWJ}
#' }

library(data.table)
library(here)
library(dplyr)

raw_data_new <- data.table::fread(
  here("data/country_hsproduct4digit_year.csv"), 
  select = c("year"="double", "pci"="double", 
             "location_code"="character", 
             "export_value"="double", 
             "hs_product_code"="character")
) %>%
  dplyr::filter(location_code %in% c("GRC", "DEU"))

data.table::fwrite(
  raw_data_new, file = here("data/deu_grc_hsproduct4digit.csv"))
