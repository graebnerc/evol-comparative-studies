library(data.table)
library(here)
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggthemr)
library(ggpubr)
library(countrycode)
library(purrr)
library(latex2exp)
ggthemr('greyscale')

source(here("R/directedness-functions.R"))
source(here("R/country-setup.R"))

depend_var <- "diff_exp_val_total_log"
indep_vars <- c("av_pci_w")
weight_var <- c("late_exp_share_mean")
reg_formula <- set_up_reg_formula(depend_var, indep_vars)

base_data <- data.table::fread(
  file = here("data/directedness_data.csv"), 
  select = c("year"="double", "pci"="double", 
             "location_code"="character", 
             "export_value"="double", 
             "hs_product_code"="character",
             "hs_eci"="double")) %>%
  rename(# to make it consistent with original study
    exporter = location_code,
    commoditycode = hs_product_code,     
    exp_val = export_value
  ) %>%
  dplyr::filter(
    !commoditycode %in% c(
      "financial", "ict", "transport", "travel", "unspecified", "XXXX")
    )

# base_data <- data.table::fread(
#   file = here("data/deu_grc_hsproduct4digit.csv"), 
#   select = c("year"="double", "pci"="double", 
#              "location_code"="character", 
#              "export_value"="double", 
#              "hs_product_code"="character")) %>%
#   rename(# to make it consistent with original study
#     exporter = location_code,
#     commoditycode = hs_product_code,     
#     exp_val = export_value
#   )

# crisis_data <- make_plot_data(base_data, period_considered = "crisis")

# I. Set up the difference data--------
# 0. Set parameters
threshold_low <- 1995
threshold_mid_lower <- 2005
threshold_mid_upper <- 2010
threshold_upper <- 2020
weight_threshold <- 2017
label_years <- paste0(
  threshold_low, "-", threshold_mid_lower, " vs. ",
  threshold_mid_upper, "-", threshold_upper)

# 1. Add periods

base_data_final <- base_data %>%
  dplyr::filter(
    year>=threshold_low, year<=threshold_upper
  ) %>%
  dplyr::mutate(
    period=ifelse(
      year<=threshold_mid_lower, "early", 
      ifelse(
        year>=threshold_mid_upper, "late", "inter"))
  )

# 2. Get product-level summary stats for both periods

base_data_final_v2 <- base_data_final %>%
  dplyr::filter(period %in% c("early", "late")) %>%
  dplyr::group_by(year, exporter) %>%
  dplyr::mutate(
    total_exp = sum(exp_val, na.rm = TRUE)
  ) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(
    exp_share = exp_val / total_exp
  ) %>%
  dplyr::group_by(exporter, year) %>%
  dplyr::mutate(test_val=sum(exp_share)) %>%
  dplyr::ungroup()

if (sum(base_data_final_v2$test_val) != nrow(base_data_final_v2)) {
  warning(
    "Export basket shares do not sum to unity for each country in each year!")
}

base_data_final_v3 <- base_data_final_v2 %>%
  group_by(exporter, period, commoditycode) %>%
  summarise(
    av_pci=mean(pci, na.rm=TRUE),
    av_pci_w=weighted.mean(pci, exp_val, na.rm=TRUE),
    exp_val_total=sum(exp_val, na.rm=TRUE),
    exp_val_mean=mean(exp_val, na.rm=TRUE), 
    exp_share_mean=mean(exp_share, na.rm=TRUE),
    .groups = "drop"
  ) %>%
  ungroup()

# 3. Compute export basket composition in the late period
# These will be used as weights in the WLS estimation below
late_exp_shares <- base_data_final_v2 %>%
  dplyr::filter(year>weight_threshold) %>%
  dplyr::group_by(exporter, commoditycode) %>%
  dplyr::summarise(# TODO das gibt in der Summe nicht 1/besser: summe ausrechnen und dann teilen? oder das hier eigentlich besser passend?
    late_exp_share_mean=mean(exp_share, na.rm=TRUE), 
    .groups = "drop") %>%
  dplyr::ungroup()

late_exp_shares %>% group_by(exporter) %>% 
  summarise(sum_share=sum(late_exp_share_mean))

# 4. Compute full time descriptives
fulltime_data <- base_data_final_v2 %>%
  dplyr::group_by(exporter, commoditycode) %>%
  dplyr::summarise(
    av_pci=mean(pci, na.rm=TRUE),
    av_pci_w=weighted.mean(pci, exp_val, na.rm=TRUE),
    av_exp_share=mean(exp_share, na.rm=TRUE),
    .groups = "drop"
  ) %>%
  dplyr::ungroup() %>% 
  dplyr::left_join(late_exp_shares, by=c("exporter", "commoditycode"))

# 5. Get difference between total exports in late and early period

diff_data_diffs <- base_data_final_v3 %>%
  dplyr::select(-all_of(c("av_pci", "av_pci_w"))) %>%
  tidyr::pivot_longer(
    cols = all_of(c("exp_val_total", "exp_val_mean", "exp_share_mean")),
    names_to = "kind"
  ) %>%
  tidyr::pivot_wider(names_from = "period", values_from = "value") %>%
  dplyr::mutate(
    diff_late_early=late-early
  ) %>%
  dplyr::select(-all_of(c("late", "early"))) %>%
  tidyr::pivot_wider(
    names_from = "kind", 
    values_from = "diff_late_early") %>%
  dplyr::rename(
    diff_exp_share=exp_share_mean,
    diff_exp_val_total=exp_val_total,
    diff_exp_val_mean=exp_val_mean
  )

# 6. Merge data
diff_data_final <- left_join(
  fulltime_data, diff_data_diffs, by=c("exporter", "commoditycode")) 

# var_label(data_final) <- list(
#   exporter = "The exporting region according to Jakob's classification.", 
#   commoditycode = "HS92 commoditycode", 
#   av_pci = "Average complexity of the product over the full time considered", 
#   av_pci_w = "Weighted average complexity of the product over the full time considered. Weights are yearly total exports, so it differs among countries.", 
#   av_exp_share = "Average export share of the product in it's regions' total exports.", 
#   late_exp_share_mean = "Average export share of the product in it's regions' total exports for the ultimate years of the sample (used for WLS).", 
#   diff_exp_share_reg = "Difference between average export shares in early and late period.", 
#   diff_exp_val_mean = "Difference between average exports in early and late period.",
#   diff_exp_val_total = "Difference between the sums of export value in early and late period."
#   )


# II.Compute changes in exports------------
# Create a list with two data frames: one with the positive changes, 
# one with the negative changes
# In both cases, logs are taken from the changes, and the abs values of the 
# negative changes

# 0. Get original name of the dependent variable
dep_var_unlog <- gsub('.{4}$', '', depend_var) # Get the original name of the dep var (i.e. removing '_log')

# 1. Get reduced diff data
inter_dat <- diff_data_final %>%
  dplyr::select(
    one_of("exporter", "commoditycode", # Identifier
           dep_var_unlog, # Dependent variable
           indep_vars, # Independent variables
           weight_var) # Weighting variable
  ) 

# 2. Compute directedness
ger_group <- "Core"
fra_group <- "Periphery"
countries <- unique(inter_dat$exporter)
beta_frames <- purrr::map(.x = countries, .f = ~get_beta(inter_dat, .))
tech_directedness <- purrr::reduce(beta_frames, .f = rbind)

# III. Make plot----
# 1. Add country groups
tech_directedness <- tech_directedness %>%
  dplyr::mutate(
    ccode = countrycode(country, "iso3c", "iso2c"),
    c_group = ifelse(
      ccode %in% countries_interest[["Germany"]], ger_group, ifelse(
        ccode %in% countries_interest[["France"]], fra_group, ifelse(
          ccode %in% countries_interest[["Core"]], "Core", ifelse(
            ccode %in% countries_interest[["Finance"]], "Finance", ifelse(
              ccode %in% countries_interest[["UK"]], "GBR", ifelse(
                ccode %in% countries_interest[["Catchup"]], "East", ifelse(
                  ccode %in% countries_interest[["Periphery"]], "Periphery", NA
                )))))))) %>%
  select(-ccode)

# 2. Add ECI of 1995
eci_base_year <- 1995

eci_data_red <- unique(base_data[year==eci_base_year, .(exporter, hs_eci)])

tech_directedness_data <- tech_directedness %>%
  left_join(., eci_data_red, by=c("country"="exporter"))
tech_directedness_data

# 3. Make plot

reg_model <- lm(tech_direct~hs_eci, data = tech_directedness_data)
reg_model_east <- lm(tech_direct~hs_eci, 
                     data = dplyr::filter(tech_directedness_data, c_group=="East"))
reg_model_rest <- lm(tech_direct~hs_eci, 
                     data = dplyr::filter(tech_directedness_data, !c_group %in% c("East")))

beta0_east <- round(coef(reg_model_east)[["(Intercept)"]], 2)
beta1_east <- round(coef(reg_model_east)[["hs_eci"]], 2)
r2east <- round(summary(reg_model_east)[["r.squared"]], 2)
label_east <- paste0("Eastern countries:\n", beta0_east, " + ", beta1_east, "\\\cdot ECI95, R^2=", r2east)


beta0_rest <- round(coef(reg_model_rest)[["(Intercept)"]], 2)
beta1_rest <- round(coef(reg_model_rest)[["hs_eci"]], 2)
r2rest <- round(summary(reg_model_rest)[["r.squared"]], 2)

fig_path_dep <- ggplot(
  data = tech_directedness_data, 
  mapping = aes(x = hs_eci, y=tech_direct)
  ) +
  geom_point(aes(shape=c_group)) +
  geom_abline(intercept = beta0_east, slope = beta1_east, color="#606060") +
  geom_abline(intercept = beta0_rest, slope = beta1_rest, color="#C0C0C0") +
  annotate(
    geom = "text", x=1.6, y=1.2, hjust=0, color="#606060",
    label = "Eastern countries: "
    ) +
  annotate(
    geom = "text", x=1.6, y=1.1, hjust=0, color="#606060",
    label = TeX(r'($-0.69 + 1.27\cdot ECI95,$  $R^2=0.69$)')
    ) +
  annotate(
    geom = "text", x=1.5, y=-.15, hjust=0, color="#606060",
    label = "Remaining countries: "
  ) +
  annotate(
    geom = "text", x=1.5, y=-0.25, hjust=0, color="#606060",
    label = TeX(r'($-0.4 + 0.26\cdot ECI95,$  $R^2=0.16$)')
  ) +
  labs(
    title = "Path dependent development and economic complexity in Europe",
    x = "Economic Complexity in 1995",
    y = paste0(
      "Technological diectedness \n(", label_years, ")")
  ) +
  theme(legend.title = element_blank(), 
        plot.title = element_text(hjust = 0.5))
fig_path_dep

ggsave(plot = fig_path_dep, 
       filename = here("figures/Figure 3.5.pdf"), 
                       width = 8, height = 4)
