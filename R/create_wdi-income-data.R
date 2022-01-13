library(WDI)
library(data.table)
library(here)

income_growth <- WDI(
  indicator = c("growth"="NY.GDP.MKTP.KD.ZG", 
                "growth_pc"="NY.GDP.PCAP.KD.ZG", 
                "gdp_ppp"="NY.GDP.MKTP.PP.KD",
                "gdp_ppp_pc"="NY.GDP.PCAP.PP.KD",
                "unemployment"="SL.UEM.TOTL.ZS",
                "population"="SP.POP.TOTL"), 
  country = unlist(countries_interest), start = 1962
)
fwrite(income_growth, file = here("data/wdi-inc-growth.csv"))
