library(dplyr)
library(tidyr)
library(countrycode)
library(ggplot2)
library(here)
library(data.table)
library(ggpubr)
library(scales)
library(ggthemr)
ggthemr('greyscale')

source(here("R/country-setup.R"))

# Setup data---------------------------

income_growth <- fread(here("data/wdi-inc-growth.csv"))

filter_y_min <- 1995
filter_y_max <- 2020
ger_group <- "Core" # Germany
fra_group <- "Periphery" # France

income_growth_groups <- income_growth %>%
  dplyr::filter(year>=filter_y_min, year<=filter_y_max) %>%
  mutate(
    ccode = iso2c,
    c_group = ifelse(
      ccode %in% countries_interest[["Germany"]], ger_group, ifelse(
        ccode %in% countries_interest[["France"]], fra_group, ifelse(
          ccode %in% countries_interest[["Core"]], "Core", ifelse(
            ccode %in% countries_interest[["Finance"]], "Finance", ifelse(
              ccode %in% countries_interest[["UK"]], "GBR", ifelse(
                ccode %in% countries_interest[["Catchup"]], "East", ifelse(
                  ccode %in% countries_interest[["Periphery"]], "Periphery", NA
                )))))))) 

min_year <- min(income_growth_groups$year)
max_year <- max(income_growth_groups$year)
gdp_minyear <- paste0("gdp_ppp_pc_", min_year)
gdp_maxyear <- paste0("gdp_ppp_pc_", max_year)
pop_minyear <- paste0("population_", min_year)
pop_maxyear <- paste0("population_", max_year)

# Cumulative growth---------------------
income_growth_cumg <- income_growth_groups %>%
  filter(year %in% c(min_year, max_year)) %>%
  select(all_of(c("iso2c", "c_group", "gdp_ppp_pc", "year", "population"))) %>%
  pivot_wider(
    id_cols = all_of(c("iso2c", "c_group")), 
    names_from = "year", 
    values_from = all_of(c("gdp_ppp_pc", "population"))
    ) %>%
  mutate(
    cum_growth_pc = (
      (!!rlang::sym(gdp_maxyear) -!!rlang::sym(gdp_minyear)
       )/!!rlang::sym(gdp_minyear))*100,
    avg_pop = ((!!rlang::sym(pop_minyear) + !!rlang::sym(pop_maxyear))/2)
    ) %>%
  select(all_of(c("iso2c", "c_group", "avg_pop", "cum_growth_pc")))
  
growth_cumg_plt <- income_growth_cumg %>%
  mutate(iso2c=countrycode(iso2c, "iso2c", "country.name")) %>%
  ggplot(data = ., 
         aes(x=reorder(iso2c, -cum_growth_pc), 
             y=cum_growth_pc)
         ) +
  geom_bar(stat = "identity") +
  labs(
    title = paste0("Cumulated growth (", min_year, "-", max_year, ")" ),
    y = "Cumulated growth of GDP p.c. (PPP)"
    ) +
  scale_y_continuous(labels = percent_format(scale = 1), 
                     expand = expansion(add = c(0, 10))) +
  theme(panel.grid.major.x = element_blank(), 
        plot.title = element_text(size = 15),
        legend.text = element_text(size = 13),
        axis.title.x = element_blank(), 
        axis.text.x = element_text(angle = 45, hjust = 1))

# Deviation from mean income---------------------
mean_gdp <- income_growth_groups %>%
  group_by(year) %>%
  summarise(
    mean_gdp_ppp_pc = mean(gdp_ppp_pc), 
    mean_gdp_ppp_pc_w = weighted.mean(gdp_ppp_pc, population)
  )

income_groups <- income_growth_groups %>%
  group_by(year, c_group) %>%
  summarise(
    g_avg_gdp_ppp_pc = mean(gdp_ppp_pc), 
    g_avg_gdp_ppp_pc_w = weighted.mean(gdp_ppp_pc, population), 
    .groups = "drop"
  ) %>%
  left_join(mean_gdp, by = "year") %>%
  mutate(
    gdp_dev = g_avg_gdp_ppp_pc - mean_gdp_ppp_pc,
    gdp_dev_w = g_avg_gdp_ppp_pc_w - mean_gdp_ppp_pc_w
  )

income_groups_plt <- ggplot(
  data = income_groups, 
  mapping = aes(x=year, y=gdp_dev, color=c_group, shape=c_group)) +
  geom_point(size=point_size) + geom_line() +
  scale_y_continuous(labels = number_format(scale = 0.001, suffix = "k")) +
  scale_shape_manual(
    name = "Group",
    values = shape_values,
    labels = label_vec,
    ) +
  scale_color_manual(
    name = "Group", values = color_values, labels = label_vec) +
  labs(
    title = paste0("Deviation from average income (", 
                   min_year, "-", max_year, ")" ),
    y = "Deviation from average GDP p.c. (PPP)") +
  theme(legend.title = element_blank(),
        axis.title.x = element_blank(),
        plot.title = element_text(size = 15),
        legend.text = element_text(size = 13))
income_groups_plt

# Full plot------

ineq_plot <- ggarrange(
  growth_cumg_plt, income_groups_plt, ncol = 2, 
  labels = c("a)", "b)"))
ineq_plot <- annotate_figure(
  ineq_plot, 
  bottom = text_grob("Data: World Bank; own calculation.", hjust = -1.1))

ggsave(plot = ineq_plot, 
       filename = here("figures/Figure 3.1.pdf"), width = 11, height = 4)
