library(dplyr)
library(tidyr)
library(countrycode)
library(ggplot2)
library(here)
library(data.table)
library(ggpubr)
library(WDI)
library(scales)
library(ggthemr)
ggthemr('greyscale')
source(here("R/country-setup.R"))

start_year <- 2000
end_year <- 2020
plots_title_size <- 13
plots_axis_title_size <- 10
plots_axis_ticks_size <- 9
legend_font_size <- 11

x_axis_breaks <- c(2000, 2005, 2007, 2010, 2015, 2020)

countries <- list()
countries[["Core"]] <- countrycode(c("Austria", "Belgium", "Denmark", 
                                     "Finland", "Germany", "Sweden"), 
                                   "country.name", "iso3c")
countries[["Catchup"]] <- countrycode(c("Bulgaria", "Romania", 
                                        "Czech Republic", "Estonia", 
                                        "Latvia", "Lithuania", 
                                        "Hungary", "Poland", "Slovenia", 
                                        "Slovakia", "Croatia"), 
                                      "country.name", "iso3c")
countries[["Finance"]] <- countrycode(c("Luxembourg", "Netherlands", 
                                        "Malta", "Ireland"), 
                                      "country.name", "iso3c")
countries[["Periphery"]] <- countrycode(c("Cyprus", "France", "Greece", 
                                          "Italy", "Portugal", "Spain"), 
                                        "country.name", "iso3c")
countries_all <- unlist(countries)


macro_data <- fread("data/macro_data_ppp.csv") %>%
  dplyr::select(-ID) %>%
  dplyr::mutate(is.north=ifelse(iso3c %in% countries[["Core"]],
                                "Core countries", ifelse(
                                  iso3c %in% countries[["Catchup"]], 
                                  "Catch-up countries", ifelse(
                                    iso3c %in% countries[["Finance"]], 
                                    "Finance hubs", ifelse(
                                      iso3c %in% countries[["Periphery"]], 
                                      "Periphery", NA
                                    )
                                  )))
  )  %>%
  dplyr::filter(!is.na(is.north)
  ) %>%
  dplyr::mutate(is.north=as.factor(is.north)
  )

unemp_plot <- macro_data %>%
  dplyr::select(dplyr::one_of(
    "year", "iso3c", "unemp_rate", "population", "is.north")
  ) %>%
  dplyr::rename(population=population) %>%
  dplyr::filter(year>=start_year & year <= end_year) %>%
  dplyr::group_by(year, is.north) %>%
  dplyr::mutate(population_group=sum(population)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(pop_rel_group=population / population_group) %>%
  dplyr::group_by(year, is.north) %>%
  dplyr::mutate(test_pop=sum(pop_rel_group)) %>%
  dplyr::ungroup() %>%
  dplyr::group_by(year, is.north) %>%
  dplyr::summarise(
    unemp_rate_mean=weighted.mean(unemp_rate, 
                                  pop_rel_group),
    unemp_rate_sd=sd(unemp_rate*pop_rel_group), .groups = "drop"
  ) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(
    is.north = ifelse(is.north=="Catch-up countries", "East", ifelse(
      is.north == "Core countries", "Core", ifelse(
        is.north=="Periphery", "Periphery", "Finance")))
  ) %>%
  ggplot(., aes(x=year, y=unemp_rate_mean, color=is.north, shape=is.north)) +
  geom_point(size=point_size) + 
  geom_line() +
  ylab("% of active population") +
  ggtitle(paste0("Unemployment rates") ) + 
  scale_x_continuous(
    breaks=x_axis_breaks, 
    expand = expansion(
      mult = c(0, 0), add = c(0, 0.5)
    )
  ) +
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1, scale = 1)
  ) +
  scale_shape_manual(
    name = "Group",
    values = shape_values
  ) +
  scale_color_manual(
    name = "Group", values = color_values) +
  theme(legend.text=element_text(size=legend_font_size)) +
  theme(axis.title = element_text(color="black", size=plots_axis_title_size),
        axis.title.x = element_blank(),
        legend.title = element_blank(),
        plot.title = element_text(color="black", size=plots_title_size),
        axis.text = element_text(color="black", size=plots_axis_ticks_size))
unemp_plot 

publicdebt_plot <- macro_data %>%
  dplyr::select(dplyr::one_of(
    "year", "iso3c", "publicdebt", "population", "is.north")
  ) %>%
  dplyr::rename(population=population) %>%
  dplyr::filter(year>=start_year & year <= end_year) %>%
  dplyr::group_by(year, is.north) %>%
  dplyr::mutate(population_group=sum(population)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(pop_rel_group=population / population_group) %>%
  dplyr::group_by(year, is.north) %>%
  dplyr::mutate(test_pop=sum(pop_rel_group)) %>%
  dplyr::ungroup() %>%
  dplyr::group_by(year, is.north) %>%
  dplyr::summarise(
    publicdebt_mean=weighted.mean(publicdebt, 
                                  pop_rel_group),
    publicdebt_sd=sd(publicdebt*pop_rel_group), .groups = "drop"
  ) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(
    is.north = ifelse(is.north=="Catch-up countries", "East", ifelse(
      is.north == "Core countries", "Core", ifelse(
        is.north=="Periphery", "Periphery", "Finance")))
  ) %>%
  ggplot(., aes(x=year, y=publicdebt_mean, color=is.north, shape=is.north)) +
  geom_point(size=point_size) + 
  geom_line() +
  ylab("% of GDP") +
  ggtitle(paste0("Public debt to GDP")) + 
  scale_x_continuous(
    breaks=x_axis_breaks, 
    expand = expansion(
      mult = c(0, 0), add = c(0, 0.5)
    )
  ) +
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1, scale = 1)
  ) +
  scale_shape_manual(
    name = "Group",
    values = shape_values
  ) +
  scale_color_manual(
    name = "Group", values = color_values) +
  theme(legend.text=element_text(size=legend_font_size)) +
  theme(axis.title = element_text(color="black", size=plots_axis_title_size),
        axis.title.x = element_blank(),
        legend.title = element_blank(),
        plot.title = element_text(color="black", size=plots_title_size),
        axis.text = element_text(color="black", size=plots_axis_ticks_size))
publicdebt_plot

fig_1 <- ggarrange(unemp_plot, publicdebt_plot, 
                   ncol=2, nrow=1, common.legend = T,
                   legend = "bottom", 
                   labels = paste0(LETTERS[1:2], ")"))

fig_1 <- annotate_figure(
  fig_1,
  bottom = text_grob("Source: AMECO (Spring 2020 forecast), own calculations.",
                     color = "black", hjust = 1, x = 1, 
                     face = "italic", size = 10))

ggsave(plot = fig_1, 
       filename = here("figures/Figure 3.7.pdf"),
       width = 9, height = 3)
