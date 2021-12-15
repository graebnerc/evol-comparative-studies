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

macro_data_raw <- fread("data/macro_data.csv")


vars_of_interest <- c("Country", "Year",
                      "complexity_HH", "population", "GDP_pc_PPP", 
                      "ind_output_meur", "Unemployment_rate",
                      "Current_account_balance_to_GDP", "exp_to_gdp",
                      "Public_debt_to_GDP", "Adjusted_wage_share", 
                      "employm_indus", "fdi_in_gdp", "Private_sector_debt_to_GDP",
                      "Tax_Wealth", "finance_share_GO",
                      "Capital_accumulation", "GDP_growth", "Gini_net", 
                      "size_of_finance", "FDI_assets_stock", "FDI_liabilities_stock")


make_single_plot <- function(plot_data, plot_variable, plot_title = NA,
                             y_limit = NA, leg_pos = "bottom", y_label = "LABEL") {
  basic_plot <- ggplot(
    filter(
      plot_data,
      variable == plot_variable
    ),
    aes(x = "0", y = mean_value, fill = c_group)
  ) +
    geom_bar(stat = "identity", position = position_dodge(0.9), color=NA) +
    geom_errorbar(aes(
      ymin = mean_value - sd_value,
      ymax = mean_value + sd_value, group = c_group
    ),
    show.legend = FALSE,
    position = position_dodge(0.9), width = .2, colour = "#00001a"
    ) +
    scale_x_discrete(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0)) +
    geom_text(aes(y = mean_value + sd_value * 1.1, label = ""), 
              show.legend = FALSE) + # Simple hack to make spots at top of y axis
    theme_bw() +
    labs(y=y_label) +
    theme(
      legend.position = leg_pos,
      legend.title = element_blank(),
      legend.text = element_text(size = 14), 
      axis.title.x = element_blank(),
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      plot.title = element_text(hjust = 0.5, size = 12),
      axis.text.y = element_text(size = 12),
      axis.title.y = element_text(size = 14),
      panel.border = element_blank(),
      axis.line = element_line()
    )
  if (!is.na(y_limit[1])) {
    basic_plot <- basic_plot + coord_cartesian(ylim = y_limit)
  }
  if (!is.na(plot_title)) {
    basic_plot <- basic_plot + ggtitle(paste(plot_title))
  } else {
    basic_plot <- basic_plot + ggtitle(paste(plot_variable))
  }
  return(basic_plot)
}



macro_data <- macro_data_raw %>%
  dplyr::filter(
    Country %in% countrycode(unlist(countries_interest), "iso2c", "iso3c"),
    Year >= 2000 & Year < 2016
  ) %>%
  select(one_of(vars_of_interest)) %>%
  mutate(
    Country = countrycode(Country, "iso3c", "iso2c"),
    c_group = ifelse(
      Country=="DE", "core", ifelse(Country=="FR", "periphery", ifelse(
      Country %in% countries_interest[["Core"]], "core", ifelse(
        Country %in% countries_interest[["Catchup"]], "catchup", ifelse(
          Country %in% countries_interest[["Finance"]], "finance", ifelse(
            Country %in% countries_interest[["Periphery"]], "periphery", NA
          )
        )
      )
    )
  ))) %>%
  mutate(foreign_ownership = ((FDI_assets_stock) / (FDI_liabilities_stock)) - 1) # foreign_assests_to_liabilities


# The core=====================================================================
macro_data_agg_core <- macro_data %>%
  mutate(c_group = ifelse(c_group == "core", "core", "other")) %>%
  group_by(c_group, Year) %>%
  summarise_if(is.numeric, mean, na.rm=TRUE) %>%
  ungroup()

macro_data_agg_core_mean <- macro_data_agg_core %>%
  group_by(c_group) %>%
  summarise(
    GDP = mean(GDP_pc_PPP, na.rm=TRUE),
    `Industrial product.` = mean(ind_output_meur, na.rm=TRUE),
    Complexity = mean(complexity_HH, na.rm=TRUE),
    Unemployment = mean(Unemployment_rate, na.rm=TRUE)
  ) %>%
  ungroup() %>%
  gather(variable, mean_value, -c_group)
head(macro_data_agg_core_mean)

macro_data_agg_core_sd <- macro_data_agg_core %>%
  group_by(c_group) %>%
  summarise(
    GDP = sd(GDP_pc_PPP, na.rm=TRUE),
    `Industrial product.` = sd(ind_output_meur, na.rm=TRUE),
    Complexity = sd(complexity_HH, na.rm=TRUE),
    Unemployment = sd(Unemployment_rate, na.rm=TRUE)
  ) %>%
  ungroup() %>%
  gather(variable, sd_value, -c_group)
head(macro_data_agg_core_sd)

macro_data_core_peri_sum <- left_join(
  macro_data_agg_core_mean, macro_data_agg_core_sd, 
  by = c("c_group", "variable"))

complexity <- make_single_plot(macro_data_core_peri_sum,
                               "Complexity",
                               leg_pos = "none",
                               y_label = "Economic complexity index"
) +
  scale_fill_grey(labels = c("core"="Core", "other"="Other")) 
complexity + theme(legend.position = "bottom")

ind_prod <- make_single_plot(macro_data_core_peri_sum,
                             "Industrial product.", 
                             plot_title = "Industrial\n output",
                             leg_pos = "none",
                             y_label = "EUR (current prices)"
) + 
  scale_y_continuous(
    labels = scales::number_format(scale=0.001, suffix = "b"),
    expand = c(0, 0)) +
  scale_fill_grey(labels = c("core"="Core", "other"="Other")) 
ind_prod

unemp <- make_single_plot(
  macro_data_core_peri_sum,
  "Unemployment",
  plot_title = "Unempl.",
  leg_pos = "none", 
  y_label = "Percentage of active population"
) + 
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1, scale = 1),
    expand = c(0, 0)
  ) +
  scale_fill_grey(labels = c("core"="Core", "other"="Other")) 
unemp 

gdp_pc <- make_single_plot(macro_data_core_peri_sum,
                           "GDP",
                           "GDP p.c.",
                           leg_pos = "none",
                           y_label = "GDP per capita (PPP)"
) + scale_y_continuous(
  labels = scales::number_format(scale=0.001, suffix = "k"),
  expand = c(0, 0)) +
  scale_fill_grey(labels = c("core"="Core", "other"="Other")) 
gdp_pc

core_plot <- ggarrange(
  complexity, gdp_pc, ind_prod, unemp,ncol = 4,
  common.legend = TRUE, legend = "bottom")


# The periphery================================================================

macro_data_agg_peri <- macro_data %>%
  mutate(c_group = ifelse(c_group == "periphery", "periphery", "other")) %>%
  group_by(c_group, Year) %>%
  summarise_if(is.numeric, mean, na.rm=TRUE) %>%
  ungroup()


macro_data_agg_peri_mean <- macro_data_agg_peri %>%
  group_by(c_group) %>%
  summarise(
    `Exports/GDP` = mean(exp_to_gdp, na.rm=TRUE),
    `Current account` = mean(Current_account_balance_to_GDP, na.rm=TRUE),
    Unemployment = mean(Unemployment_rate, na.rm=TRUE),
    `Public debt` = mean(Public_debt_to_GDP, na.rm=TRUE)
  ) %>%
  ungroup() %>%
  gather(variable, mean_value, -c_group)
head(macro_data_agg_peri_mean)

macro_data_agg_peri_sd <- macro_data_agg_peri %>%
  group_by(c_group) %>%
  summarise(
    `Exports/GDP` = sd(exp_to_gdp, na.rm=TRUE),
    `Current account` = sd(Current_account_balance_to_GDP, na.rm=TRUE),
    Unemployment = sd(Unemployment_rate, na.rm=TRUE),
    `Public debt` = sd(Public_debt_to_GDP, na.rm=TRUE)
  ) %>%
  ungroup() %>%
  gather(variable, sd_value, -c_group)
head(macro_data_agg_peri_sd)

macro_data_agg_peri_sum <- left_join(macro_data_agg_peri_mean, 
                                     macro_data_agg_peri_sd, 
                                     by=c("c_group", "variable"))
macro_data_agg_peri_sum <- macro_data_agg_peri_sum %>%
  mutate(c_group = ifelse(c_group == "periphery", "aperiphery", c_group))

curr_acc <- make_single_plot(macro_data_agg_peri_sum,
                             "Current account",
                             plot_title = "Current\n account",
                             leg_pos = "none",
                             y_label = "Percentage of GDP",
                             y_limit = c(-10, 2)
)  + 
  scale_y_continuous(
    breaks = seq(-10, 2, by=1.0),
    labels = scales::percent_format(scale = 1, accuracy = 1),
    expand = c(0, 0)
  ) +
  theme(panel.grid.minor.y = element_blank()) +
  scale_fill_grey(labels = c("aperiphery"="Periphery", "other"="Other")) 
curr_acc +theme(legend.position = "bottom")

exp_gdp <- make_single_plot(macro_data_agg_peri_sum,
                            "Exports/GDP",
                            "Exports to GDP",
                            leg_pos = "none",
                            y_label = "Percentage of GDP"
) + 
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1, scale = 1),
    expand = c(0, 0)
  ) +
  scale_fill_grey(labels = c("aperiphery"="Periphery", "other"="Other")) 
exp_gdp

pub_debt <- make_single_plot(macro_data_agg_peri_sum,
                             "Public debt",
                             leg_pos = "none",
                             y_label = "Percentage of GDP"
) + 
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1, scale = 1),
    expand = c(0, 0)
  ) +
  scale_fill_grey(labels = c("aperiphery"="Periphery", "other"="Other")) 
pub_debt

unemp <- make_single_plot(macro_data_agg_peri_sum,
                          "Unemployment",
                          plot_title = "Unempl.",
                          leg_pos = "none",
                          y_label = "Percentage of active population"
) + 
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1, scale = 1),
    expand = c(0, 0)
  ) +
  scale_fill_grey(labels = c("aperiphery"="Periphery", "other"="Other")) 
unemp

peri_plot <- ggarrange(
  curr_acc, exp_gdp, pub_debt, unemp, ncol = 4,
  common.legend = TRUE, legend = "bottom"
)

# Catch-up=====================================================================
macro_data_agg_catchup <- macro_data %>%
  mutate(c_group = ifelse(c_group == "catchup", "catchup", "other")) %>%
  group_by(c_group, Year) %>%
  summarise_if(is.numeric, mean, na.rm=TRUE) %>%
  ungroup()


macro_data_agg_catchup_mean <- macro_data_agg_catchup %>%
  group_by(c_group) %>%
  summarise(
    `Wage share` = mean(Adjusted_wage_share, na.rm=TRUE),
    `GDP pc (PPP)` = mean(GDP_pc_PPP, na.rm=TRUE),
    `Foreign ownership` = mean(foreign_ownership, na.rm=TRUE),
    `Industial empl.` = mean(employm_indus, na.rm=TRUE)
  ) %>%
  ungroup() %>%
  gather(variable, mean_value, -c_group)
head(macro_data_agg_catchup_mean)

macro_data_agg_catchup_sd <- macro_data_agg_catchup %>%
  group_by(c_group) %>%
  summarise(
    `Wage share` = sd(Adjusted_wage_share, na.rm=TRUE),
    `GDP pc (PPP)` = sd(GDP_pc_PPP, na.rm=TRUE),
    `Foreign ownership` = sd(foreign_ownership, na.rm=TRUE),
    `Industial empl.` = sd(employm_indus, na.rm=TRUE)
  ) %>%
  ungroup() %>%
  gather(variable, sd_value, -c_group)
head(macro_data_agg_catchup_sd)

macro_data_agg_catchup_sum <- left_join(
  macro_data_agg_catchup_mean, 
  macro_data_agg_catchup_sd, 
  by=c("c_group", "variable"))

wage_share <- make_single_plot(macro_data_agg_catchup_sum,
                               "Wage share",
                               y_limit = c(40, 65),
                               leg_pos = "none",
                               y_label = "Adj. wage share in % of GDP"
) + 
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1, scale = 1),
    expand = c(0, 0)
  ) +
  scale_fill_grey(labels = c("catchup"="Catch-up", "other"="Other")) 
wage_share

ind_emp <- make_single_plot(macro_data_agg_catchup_sum,
                            "Industial empl.",
                            plot_title = "Industial\n employment",
                            leg_pos = "none",
                            y_label = "Percentage of total employment"
) + 
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1, scale = 1),
    expand = c(0, 0)
  ) +
  scale_fill_grey(labels = c("catchup"="Catch-up", "other"="Other")) 
ind_emp

for_own <- make_single_plot(macro_data_agg_catchup_sum,
                            "Foreign ownership",
                            "Foreign\n ownership",
                            y_limit = c(-1.0, 0.1),
                            leg_pos = "none",
                            y_label = "Foreign assets to foreign liabilities"
) + 
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1, scale = 100),
    expand = c(0, 0)
  ) +
  scale_fill_grey(labels = c("catchup"="Catch-up", "other"="Other")) 
for_own

gdp_pc <- make_single_plot(macro_data_agg_catchup_sum,
                           "GDP pc (PPP)", 
                           "GDP p.c.",
                           leg_pos = "none",
                           y_label = "GDP per capita (PPP)"
) + scale_y_continuous(
  labels = scales::number_format(scale=0.001, suffix = "k"),
  expand = c(0, 0)) +
  scale_fill_grey(labels = c("catchup"="Catch-up", "other"="Other")) 
gdp_pc

cp_plot <- ggarrange(
  for_own, gdp_pc, ind_emp, wage_share, ncol = 4,
  common.legend = TRUE, legend = "bottom"
)

# Finance======================================================================

macro_data_agg_finance <- macro_data %>%
  mutate(c_group = ifelse(c_group == "finance", "finance", "other")) %>%
  group_by(c_group, Year) %>%
  summarise_if(is.numeric, mean, na.rm=TRUE) %>%
  ungroup()


macro_data_agg_finance_mean <- macro_data_agg_finance %>%
  group_by(c_group) %>%
  summarise(
    `Private debt` = mean(Private_sector_debt_to_GDP, na.rm=TRUE),
    FDI = mean(fdi_in_gdp, na.rm=TRUE),
    `Share finance (GO)` = mean(finance_share_GO, na.rm=TRUE),
    `Wealth tax` = mean(Tax_Wealth, na.rm=TRUE)
  ) %>%
  ungroup() %>%
  gather(variable, mean_value, -c_group)
head(macro_data_agg_finance_mean)

macro_data_agg_finance_sd <- macro_data_agg_finance  %>%
  group_by(c_group) %>%
  summarise(
    `Private debt` = sd(Private_sector_debt_to_GDP, na.rm=TRUE),
    FDI = sd(fdi_in_gdp, na.rm=TRUE),
    `Share finance (GO)` = sd(finance_share_GO, na.rm=TRUE),
    `Wealth tax` = sd(Tax_Wealth, na.rm=TRUE)
  ) %>%
  ungroup() %>%
  gather(variable, sd_value, -c_group)
head(macro_data_agg_finance_sd)

macro_data_agg_finance_sum <- left_join(macro_data_agg_finance_mean, 
                                        macro_data_agg_finance_sd, 
                                        by=c("c_group", "variable"))

fdi <- make_single_plot(
  macro_data_agg_finance_sum, "FDI", leg_pos = "none",
  y_label = "Percentage of GDP"
) +  
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1, scale = 1),
    expand = c(0, 0)
  ) +
  scale_fill_grey(labels = c("finance"="Financial hubs", "other"="Other")) 
fdi

priv_debt <- make_single_plot(macro_data_agg_finance_sum,
                              "Private debt",
                              leg_pos = "none",
                              y_label = "Private sector debt to GDP"
)  + 
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1, scale = 1),
    expand = c(0, 0)
  ) +
  scale_fill_grey(labels = c("finance"="Financial hubs", "other"="Other")) 
priv_debt

finance_go <- make_single_plot(macro_data_agg_finance_sum,
                               "Share finance (GO)",
                               leg_pos = "none",
                               y_label = "Share in gross output of all sectors"
) + 
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1, scale = 100),
    expand = c(0, 0)
  ) + ggtitle("Share finance") +
  scale_fill_grey(labels = c("finance"="Financial hubs", "other"="Other")) 
finance_go

wealth_tax <- make_single_plot(macro_data_agg_finance_sum,
                               "Wealth tax",
                               leg_pos = "none",
                               y_label = "Tax revenue as % of GDP"
) + 
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 0.1, scale = 1),
    expand = c(0, 0)
  ) +
  scale_fill_grey(labels = c("finance"="Financial hubs", "other"="Other")) 
wealth_tax

finance_plot <- ggarrange(
  fdi, finance_go, priv_debt, wealth_tax, ncol = 4,
  common.legend = TRUE, legend = "bottom"
)

# full plot

full_plot <- ggarrange(
  core_plot, peri_plot, cp_plot, finance_plot, 
  ncol = 2, nrow = 2,
  labels = paste0(letters, ")"),
  font.label = list(size=18)
)

ggsave(plot = full_plot, 
       filename = here("figures/Figure 3.6.pdf"),
       width = 13, height = 9)


