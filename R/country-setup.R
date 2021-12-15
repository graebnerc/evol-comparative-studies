point_size <- 3.5
shape_values <- c("Core"=0, "East"=4, "Periphery"=8, "Finance"=13)
shape_values2 <- c("Center"=0, "East"=4, "South"=8, "Finance"=13)

greys <- grey.colors(n = 4, start = 0.2, end = 0.8)
color_values <- c(
  "Core"=greys[1], 
  "East"=greys[2], 
  "Periphery"=greys[3], 
  "Finance"=greys[4])
color_values2 <- c(
  "Center"=greys[1], 
  "East"=greys[2], 
  "South"=greys[3], 
  "Finance"=greys[4])

# Country groups

countries_interest <- list()
countries_interest[["Germany"]] <- countrycode(
  "Germany", "country.name", "iso2c")
countries_interest[["France"]] <- countrycode(
  "France", "country.name", "iso2c")
countries_interest[["Finance"]] <- countrycode(
  c("Cyprus", "Luxembourg", "Netherlands", "Malta", "Ireland"), 
  "country.name", "iso2c")
countries_interest[["Core"]] <- countrycode(
  c("Austria", "Belgium", "Denmark", "Finland", "Sweden"), 
  "country.name", "iso2c")
countries_interest[["Catchup"]] <- countrycode(
  c("Bulgaria", "Romania", "Czech Republic", "Estonia", "Latvia", "Lithuania", 
    "Hungary", "Poland", "Slovenia", "Slovakia", "Croatia"), 
  "country.name", "iso2c")
countries_interest[["Periphery"]] <- countrycode(
  c("Greece", "Italy", "Portugal", "Spain"), "country.name", "iso2c")
