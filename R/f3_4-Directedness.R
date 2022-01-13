library(data.table)
library(here)
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggthemr)
library(ggpubr)
ggthemr('greyscale')

source(here("R/directedness-functions.R"))

depend_var <- "diff_exp_val_total_log"
indep_vars <- c("av_pci_w")
weight_var <- c("late_exp_share_mean")
reg_formula <- set_up_reg_formula(depend_var, indep_vars)

base_data <- data.table::fread(
  file = here("data/deu_grc_hsproduct4digit.csv"), 
  select = c("year"="double", "pci"="double", 
             "location_code"="character", 
             "export_value"="double", 
             "hs_product_code"="character")) %>%
  rename(# to make it consistent with original study
    exporter = location_code,
    commoditycode = hs_product_code,     
    exp_val = export_value
  )

# crisis_data <- make_plot_data(base_data, period_considered = "crisis")

# I. Set up the difference data--------
# 0. Set parameters
threshold_low <- 1995
threshold_mid_lower <- 2005
threshold_mid_upper <- 2010
threshold_upper <- 2020
weight_threshold <- 2017

# 1. Add periods

base_data_final <- base_data %>%
  dplyr::filter(
    year>=threshold_low, year<=threshold_upper
    ) %>%
  dplyr::mutate(
    period=ifelse(
      year<=threshold_mid_lower, "early", 
      ifelse(year>=threshold_mid_upper, "late", "inter"))
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

# The basics were specified at the beginning of the script:
  # depend_var <- "diff_exp_val_total_log"
  # indep_vars <- c("av_pci_w")
  # weight_var <- c("late_exp_share_reg")
  # reg_formula <- set_up_reg_formula(depend_var, indep_vars)

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

# 2. Get the lists with changes
# reg_dat_crisis <- make_regression_data(depend_var, c(indep_vars, weight_var), crisis_data)

# Return two data frames: 
# one with positive changes and one with negative changes (both in logs)

reg_data <- list()
reg_data[["pos"]] <- inter_dat %>%
  dplyr::filter(!!(as.name(dep_var_unlog))>0) %>%
  dplyr::mutate(!!(as.name(depend_var)) := log(!!(as.name(dep_var_unlog))))

reg_data[["neg"]] <- inter_dat %>%
  dplyr::filter(!!(as.name(dep_var_unlog))<0) %>%
  dplyr::mutate(
    !!(as.name(dep_var_unlog)) := abs(!!(as.name(dep_var_unlog))),
    !!(as.name(depend_var)) := log(!!(as.name(dep_var_unlog)))
    ) 

# III. Make the plots----------------------------
low_col <- grey.colors(50)[35]
high_col <- grey.colors(50)[1]
grc_reg_plot <- make_wls_plot(
  reg_data, 
  country_name = "GRC", 
  x_var = "av_pci_w",
  y_var = "diff_exp_val_total_log", 
  size_var = "late_exp_share_mean", 
  change_type = "positive",
  interval_val = "conf")

grc_plot <- grc_reg_plot$plot + 
  scale_colour_gradient(
    low = low_col, high = high_col, 
    name="Share in recent exports", 
    limit = c(0.0, 0.28),
    breaks = c(seq(0.0, 0.25, 0.1)),
    label=scales::percent_format(scale = 100, accuracy = 1)
  ) +
  scale_x_continuous(limits = c(-3,2.3)) +
  labs(title = "Greece") + theme(plot.title = element_text(hjust = 0.5))

deu_reg_plot <- make_wls_plot(
  reg_data, 
  country_name = "DEU", 
  x_var = "av_pci_w",
  y_var = "diff_exp_val_total_log", 
  size_var = "late_exp_share_mean", 
  change_type = "positive",
  interval_val = "conf", 
  wls_reg=TRUE) 

deu_plot <- deu_reg_plot$plot + 
  scale_colour_gradient(
    low = low_col, high = high_col, 
    name="Share in recent exports", 
    limit = c(0.01, 0.09),
    breaks = seq(0.02, 0.08, 0.02),
    label=scales::percent_format(scale = 100, accuracy = 1)
    ) +
  scale_x_continuous(limits = c(-3,2.3)) +
  labs(title = "Germany") + theme(plot.title = element_text(hjust = 0.5))

deu_grc_plot <- ggpubr::ggarrange(
  deu_plot, grc_plot, ncol = 2, legend = "bottom", common.legend = FALSE, 
  labels = c("a)", "b)")
)
deu_grc_plot <- ggpubr::annotate_figure(
  p = deu_grc_plot,
  top = text_grob("Direction of technological change", hjust = 0.5, size=15), 
  bottom = text_grob(
    "Data: CID Atlas of Economic Complexity; own calculations.", 
    hjust = 0)
)
ggsave(plot = deu_grc_plot, filename = here("figures/Figure 3.4.pdf"), 
       width = 10, height = 4)
