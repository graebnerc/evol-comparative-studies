library(dplyr)
library(tidyr)
library(countrycode)
library(ggplot2)
library(here)
library(data.table)
library(ggpubr)
library(ggrepel)
library(scales)
library(ggthemr)
library(haven)
ggthemr('greyscale')
download_data <- FALSE

source(here("R/country-setup.R"))

eci_data <- read_dta(here("data/rankings.dta")) %>%
  select(all_of(c("year", "hs_eci", "code"))) %>%
  dplyr::mutate(
    code = countrycode(code, "iso3c", "iso2c")
  ) 

# Data source: https://doi.org/10.7910/DVN/XTAQMC

income_growth <- fread(here("data/wdi-inc-growth.csv"))
# Data: see R file

# Cumulative growth

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
        ccode %in% countries_interest[["France"]],fra_group, ifelse(
          ccode %in% countries_interest[["Core"]], "Core", ifelse(
            ccode %in% countries_interest[["Finance"]], "Finance", ifelse(
              ccode %in% countries_interest[["UK"]], "GBR", ifelse(
                ccode %in% countries_interest[["Catchup"]], "East", ifelse(
                  ccode %in% countries_interest[["Periphery"]], "Periphery", NA
                )))))))) %>%
  left_join(eci_data, by = c("year", "ccode"="code"))

# Correlation figure
font_color <- c(
  "Core"="white", 
  "East"="black", 
  "Periphery"="black", 
  "Finance"="black"
  )

income_eci <- income_growth_groups %>%
  dplyr::filter(!is.na(hs_eci)) %>%
  group_by(iso2c, c_group) %>%
  summarise(
    eci = mean(hs_eci),
    gdp = mean(gdp_ppp_pc), 
    .groups = "drop"
  ) %>%
  ggplot(data = ., aes(x=eci, y=gdp)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, formula = 'y ~ x', show.legend = FALSE) +
  geom_label_repel(
    mapping = aes(label = iso2c, fill=c_group, color=c_group), 
    key_glyph=draw_key_rect, segment.color="black") +
  scale_fill_grey(aesthetics = c("fill")) +
  scale_color_manual(
    values = font_color, aesthetics = c("color")) +
  guides(color="none") +
  labs(title = "Complexity and income", x="ECI", y="GDP per capita (PPP)") +
  scale_y_continuous(labels = number_format(scale = 0.001, suffix = "k")) +
  theme(legend.title = element_blank(), 
        plot.title = element_text(size = 15),
        legend.text = element_text(size = 13),
        legend.position = "bottom")

# ECI over time
eci_dyn_data <- income_growth_groups %>%
  group_by(year, c_group) %>%
  summarise(eci=mean(hs_eci, na.rm=TRUE), .groups = "drop") %>%
  dplyr::mutate(
    c_group = ifelse(c_group == "Core", "Center", 
                     ifelse(c_group=="Periphery", "South", c_group))
  )

eci_dyn_plot <- ggplot(
  data = eci_dyn_data, aes(x=year, y=eci, color=c_group, shape=c_group)
  ) +
  geom_point() + geom_line() +
  scale_shape_manual(
    name = "Group",
    values = shape_values2,
    labels = label_vec,
  ) +
  labs(title = "Economic complexity over time", y="ECI") +
  scale_color_manual(
    name = "Group", values = color_values2, labels = label_vec) +
  geom_text_repel(
    data = dplyr::filter(eci_dyn_data, year==2017),
    mapping = aes(label = c_group), show.legend = FALSE, 
    color="black", nudge_x = 2) +
  theme(legend.title = element_blank(), 
        legend.text = element_text(size = 13),
        plot.title = element_text(size = 15),
        legend.position = "bottom", 
        axis.title.x = element_blank()) 

# Full plot------

eci_plot <- ggarrange(
  income_eci, eci_dyn_plot, ncol = 2, 
  labels = c("a)", "b)"))
eci_plot <- annotate_figure(
  eci_plot, bottom = text_grob(
    "Data: WID, CID Atlas of Economic Complexity; own calculation.", 
    hjust = -0.15))

ggsave(plot = eci_plot, 
       filename = here("figures/Figure 3.2.pdf"), width = 11, height = 4)