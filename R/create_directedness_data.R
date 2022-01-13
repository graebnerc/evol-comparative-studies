#' @data{country_hsproduct4digit_year.csv,
#'   author = {The Growth Lab at Harvard University},
#'   publisher = {Harvard Dataverse},
#'   title = {{International Trade Data (HS, 92)}},
#'   year = {2019},
#'   version = {V4},
#'   doi = {10.7910/DVN/T4CHWJ},
#'   url = {https://doi.org/10.7910/DVN/T4CHWJ}
#' }

library(here)
library(data.table)
library(countrycode)
source(here("R/country-setup.R"))
all_countries <- countrycode(unlist(countries_interest), "iso2c", "iso3c")

atlas_data <- fread(
  here("data/country_hsproduct4digit_year.csv"),
  select = c(
    "year"="double", "pci"="double", 
    "location_code"="character", 
    "export_value"="double", 
    "hs_product_code"="character",
    "hs_eci"="double"))
atlas_data <- atlas_data[location_code %in% all_countries]

fwrite(atlas_data, here("data/directedness_data.csv"))
