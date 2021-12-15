# Data wrangling functions ----

#' ADD
add_periods <- function(init_data, period="long", gap=FALSE){
  if (gap==FALSE){
    if (period=="long"){
      y_threshold_low <- 1979
      y_threshold_middle <- 2000
    } else if (period=="crisis"){
      y_threshold_low <- 1999
      y_threshold_middle <- 2008
    } else {
      stop("Wrong period given: long or crisis")
    }
    data_work <- data.table(init_data)
    data_work <- data_work[year > y_threshold_low]
    data_work <- data_work %>%
      mutate(period=ifelse(year<y_threshold_middle, "early", "late"))
  }
  else {
    if (period=="long"){
      y_threshold_low <- 1994# 1979
      y_threshold_middle <- 2000# 2000 - gap
      y_threshold_up <- 2010 #2000 + gap
    } else if (period=="crisis"){
      y_threshold_low <- 1999
      y_threshold_middle <- 2008 - gap
      y_threshold_up <- 2000 + gap
    } else {
      stop("Wrong period given: long or crisis")
    }
    data_work <- data.table(init_data)
    data_work <- data_work[year > y_threshold_low]
    data_work <- data_work %>%
      mutate(period=ifelse(year<y_threshold_middle, "early", 
                           ifelse(year>y_threshold_up, "late", "inter")))
  }
  
  return(data_work)
}

#' ADD
make_plot_data <- function(raw_dat, period_considered="long", gap_used=FALSE) {
  stopifnot(period_considered %in% c("long", "crisis"))
  
  work_data_p <- add_periods(raw_dat, period = period_considered, gap = gap_used) 
  work_data <- work_data_p %>%
    filter(period %in% c("early", "late")) %>%
    group_by(exporter, period, commoditycode) %>%
    summarise(av_pci=mean(pci, na.rm=TRUE),
              av_pci_w=weighted.mean(pci, exp_val, na.rm=TRUE),
              exp_val_total=sum(exp_val, na.rm=TRUE),
              exp_val_mean=mean(exp_val, na.rm=TRUE),
              exp_share_wt_mean=mean(exp_share_sect, na.rm=TRUE),
              exp_share_reg_mean=mean(exp_share_sect_reg, na.rm=TRUE)) %>%
    ungroup()
  # print(1)
  late_exp_share_reg <- work_data_p %>%
    filter(year>2012) %>%
    group_by(exporter, commoditycode) %>%
    summarise(late_exp_share_reg=mean(exp_share_sect_reg, na.rm=TRUE)) %>%
    ungroup()
  # print(2)
  fulltime_data <- work_data_p %>%
    group_by(exporter, commoditycode) %>%
    summarise(av_pci=mean(pci, na.rm=TRUE),
              av_pci_w=weighted.mean(pci, exp_val, na.rm=TRUE),
              av_exp_share_reg=mean(exp_share_sect_reg, na.rm=TRUE)) %>%
    ungroup() %>% 
    left_join(late_exp_share_reg, by=c("exporter", "commoditycode"))
  # print(3)
  
  diff_data_diffs <- work_data %>%
    select(exporter, period, commoditycode, exp_val_total, exp_share_wt_mean, exp_share_reg_mean) %>%
    gather(measure, value, -exporter, -period, -commoditycode) %>%
    spread(period, value) %>%
    mutate(diff_late_early=late-early) %>%
    select(-early, -late) %>%
    spread(measure, diff_late_early) %>%
    rename(diff_exp_share_reg=exp_share_reg_mean,
           diff_exp_share_wt=exp_share_wt_mean,
           diff_exp_val_total=exp_val_total)
  head(diff_data_diffs)
  
  # print(4)
  diff_data_ratios <- work_data %>%
    select(exporter, period, commoditycode, exp_val_total, exp_share_wt_mean, exp_share_reg_mean) %>%
    gather(measure, value, -exporter, -period, -commoditycode) %>%
    spread(period, value) %>%
    mutate(ratio_late_early=late/early) %>%
    select(-early, -late) %>%
    spread(measure, ratio_late_early) %>%
    rename(ratio_exp_share_reg=exp_share_reg_mean, # late/early
           ratio_exp_share_wt=exp_share_wt_mean,
           ratio_exp_val_total=exp_val_total)
  head(diff_data_ratios)
  
  # print(5)
  
  # diff_data heisst jetzt diff_data_diffs
  data_final <- left_join(fulltime_data, diff_data_diffs, by=c("exporter", "commoditycode")) %>%
    left_join(diff_data_ratios, by=c("exporter", "commoditycode"))
  
  var_label(data_final) <- list(exporter = "The exporting region according to Jakob's classification.", 
                                commoditycode = "SITC V2 commoditycode", 
                                av_pci = "Average complexity of the product over the full time considered", 
                                av_pci_w = "Weighted average complexity of the product over the full time considered. Weights are yearly total exports, so it differs among countries.", 
                                av_exp_share_reg = "Average export share of the product in it's regions' total exports.", 
                                late_exp_share_reg = "Average export share of the product in it's regions' total exports for the last 3 years of the sample.", 
                                diff_exp_share_reg = "Difference between average export share in region's total exports in early and late period.", 
                                diff_exp_share_wt = "Difference between average export share in world total exports in early and late period.", 
                                diff_exp_val_total = "Difference between the sums of export value in early and late period.",
                                ratio_exp_share_reg = "Ratio of average export share in region's total exports in early and late period (late/early).",
                                ratio_exp_share_wt = "Ratio of average export share in world total exports in early and late period (late/early).",
                                ratio_exp_val_total = "Ratio of the sums of export value in early and late period (late/early).")
  return(data_final)
}

#' ADD
make_regression_data <- function(dep_var, # The dependent variable, in its log formulation
                                 idep_vars, # Vector with the independent variables
                                 raw_dat){ # The raw data set
  # Returns a list with two data frames: one with the positive changes, one with the negative changes
  # In both cases, logs are taken from the changes, and the abs values of the negative changes
  if (!grepl("_log", dep_var)){
    # Assume that the dependent variable is given as later used in formula
    stop("Please provide dependent variable in log form (i.e. with '_log' at the end!")
  }
  
  dep_var_unlog <- gsub('.{4}$', '', dep_var) # Get the original name of the dep var (i.e. removing '_log')
  
  inter_dat <- raw_dat %>% # long_reg_data_v1
    select(one_of("exporter", "commoditycode", # Identifier
                  dep_var_unlog, # Dependent var
                  idep_vars)) # Independent vars
  
  return_list <- list()
  # Return two data frames: one with positive changes and one with negative changes
  
  return_list[["pos"]] <- inter_dat %>%
    filter(UQ(as.name(dep_var_unlog))>0) %>%
    mutate(UQ(as.name(dep_var)) := log(UQ(as.name(dep_var_unlog))))
  
  return_list[["neg"]] <- inter_dat %>%
    filter(UQ(as.name(dep_var_unlog))<0) %>%
    mutate(UQ(as.name(dep_var_unlog)) := abs(UQ(as.name(dep_var_unlog))) ) %>%
    mutate(UQ(as.name(dep_var)) := log(UQ(as.name(dep_var_unlog))))
  
  return(return_list)
}


# Plot functions ----
# 
# make_single_cntry_plot <- function(cntry, period="long", 
#                                    x_val="av_pci_w", 
#                                    y_val="diff_exp_val_total", 
#                                    fill_val="late_exp_share_reg",
#                                    share_on_y=FALSE, 
#                                    x_limit=c(-4,5), 
#                                    trans_dat=FALSE,
#                                    spec_dat=FALSE,
#                                    log_y_trans=TRUE,
#                                    log_x_trans=FALSE,
#                                    method_geom_smooth="loess"){
#   if (spec_dat==FALSE){
#     if (trans_dat==FALSE){
#       if (period=="long"){
#         expl_dat <- filter(long_data, exporter==cntry)
#       } else if (period=="crisis"){
#         expl_dat <- filter(crisis_data, exporter==cntry)
#       } else {
#         stop("Wrong period given, allowed are 'long' and 'crisis'!")
#       }
#     } else {
#       if (period=="long"){
#         expl_dat <- filter(long_plot_data_trans, exporter==cntry)
#       } else if (period=="crisis"){
#         expl_dat <- filter(crisis_plot_data_trans, exporter==cntry)
#       } else {
#         stop("Wrong period given, allowed are 'long' and 'crisis'!")
#       }
#     }
#   } else {
#     expl_dat <- filter(spec_dat, exporter==cntry)
#   }
#   
#   # print("Considering only positive changes!")
#   expl_dat_pos <- expl_dat %>%
#     filter(UQ(as.name(y_val))>0)
#   plot_title_pos <- paste0(cntry, " (", period, ", positive changes") # x_val, " ~ ", y_val, " for ", 
#   
#   # print("Considering only negative changes!")
#   expl_dat_neg <- expl_dat %>%
#     filter(UQ(as.name(y_val))<0) %>%
#     mutate(UQ(as.name(y_val)) := abs(UQ(as.name(y_val))) )
#   plot_title_neg <- paste0(cntry, " (", period, ", negative changes") # x_val, " ~ ", y_val, " for ", 
#   
#   share_pos_changes <- round((nrow(expl_dat_pos[y_val]) / nrow(expl_dat[y_val])*100), 1)
#   share_neg_changes <- round((nrow(expl_dat_neg[y_val]) / nrow(expl_dat[y_val])*100), 1)
#   # print(paste(cntry, "pos changes:", share_pos_changes))
#   # print(paste(cntry, "neg changes:", share_neg_changes))
#   
#   plot_title_pos <- paste0(plot_title_pos, " [", share_pos_changes, "% of all])")
#   plot_title_neg <- paste0(plot_title_neg, " [", share_neg_changes, "% of all])")
#   
#   
#   if (share_on_y==TRUE){
#     expl_plot_pos <- ggplot(expl_dat_pos) + 
#       aes_string(x=x_val, y=fill_val, colour=y_val) +
#       geom_point(aes_string(size=fill_val), alpha=0.5) +
#       ggtitle(plot_title_pos) +
#       scale_color_viridis() + 
#       scale_size(guide=FALSE) +
#       theme(legend.position = "bottom", 
#             legend.title = element_text(size=7),
#             legend.key = element_rect(size = 7),
#             legend.key.size = unit(2, "mm"),
#             legend.key.width = unit(8, "mm"),
#             plot.title = element_text(size=8),
#             axis.title = element_text(size=8))
#     
#     expl_plot_neg <- ggplot(expl_dat_neg) + 
#       aes_string(x=x_val, y=fill_val, colour=y_val) + 
#       geom_point(aes_string(size=fill_val), alpha=0.5) +
#       ggtitle(plot_title_neg) +
#       scale_color_viridis() +
#       scale_size(guide=FALSE) +
#       theme(legend.position = "bottom", 
#             legend.title = element_text(size=7),
#             legend.key = element_rect(size = 7),
#             legend.key.size = unit(2, "mm"),
#             legend.key.width = unit(8, "mm"),
#             plot.title = element_text(size=8),
#             axis.title = element_text(size=8))
#   } else{
#     expl_plot_pos <- ggplot(expl_dat_pos) + 
#       aes_string(x=x_val, y=y_val, colour=fill_val) + 
#       geom_point(aes_string(size=fill_val), alpha=0.5) +
#       ggtitle(plot_title_pos) +
#       scale_color_viridis() +
#       scale_size(guide=FALSE) +
#       theme(legend.position = "bottom", 
#             legend.title = element_text(size=7),
#             legend.key = element_rect(size = 7),
#             legend.key.size = unit(2, "mm"),
#             legend.key.width = unit(8, "mm"),
#             plot.title = element_text(size=8),
#             axis.title = element_text(size=8))
#     
#     expl_plot_neg <- ggplot(expl_dat_neg) + 
#       aes_string(x=x_val, y=y_val, colour=fill_val) + 
#       geom_point(aes_string(size=fill_val), alpha=0.5) +
#       ggtitle(plot_title_neg) +
#       scale_color_viridis() + 
#       scale_size(guide=FALSE) +
#       theme(legend.position = "bottom", 
#             legend.title = element_text(size=7),
#             legend.key = element_rect(size = 7),
#             legend.key.size = unit(2, "mm"),
#             legend.key.width = unit(8, "mm"),
#             plot.title = element_text(size=8),
#             axis.title = element_text(size=8))
#   }
#   
#   if (is.na(x_limit)==FALSE){
#     expl_plot_pos <- expl_plot_pos +xlim(x_limit)
#     expl_plot_neg <- expl_plot_neg + xlim(x_limit)
#   }
#   
#   ret_list <- list()
#   if (log_y_trans==TRUE){
#     ret_list[["pos_plot"]] <- expl_plot_pos + scale_y_continuous(trans='log10') + ylab(paste0("log(", y_val, ")"))
#     ret_list[["neg_plot"]] <- expl_plot_neg + scale_y_continuous(trans='log10') + ylab(paste0("log(", y_val, ")"))
#   } else{
#     ret_list[["pos_plot"]] <- expl_plot_pos
#     ret_list[["neg_plot"]] <- expl_plot_neg
#   }
#   if (log_x_trans==TRUE){
#     ret_list[["pos_plot"]] <- ret_list[["pos_plot"]] + scale_x_continuous(trans='log10') + xlab(paste0("log(", x_val, ")"))
#     ret_list[["neg_plot"]] <- ret_list[["neg_plot"]] + scale_x_continuous(trans='log10') + xlab(paste0("log(", x_val, ")"))
#   }
#   
#   
#   ret_list[["pos_plot"]] <- ret_list[["pos_plot"]] + geom_smooth(method = method_geom_smooth)
#   ret_list[["neg_plot"]] <- ret_list[["neg_plot"]] + geom_smooth(method = method_geom_smooth)
#   
#   
#   ret_list[["share_pos"]] <- share_pos_changes
#   ret_list[["share_neg"]] <- share_neg_changes
#   
#   return(ret_list)
# }

#' Creates a plot for the WLS regression on tech directed.
make_wls_plot <- function(regression_data, # nimmt long or crisis data
                          country_name,
                          x_var, 
                          y_var, 
                          size_var, # aktuell auch weight var
                          change_type="positive",
                          interval_val="conf") {
  if (change_type=="positive"){
    reg_dat_used <- dplyr::filter(regression_data[["pos"]], 
                                  exporter == country_name)
  } else if (change_type=="negative"){
    reg_dat_used <- dplyr::filter(regression_data[["neg"]], 
                                  exporter == country_name)
  } else {
    stop("Wrong change_type given:positive or negative.")
  }
  reg_formula <- set_up_reg_formula(y_var, x_var)
  
  reg_model <- lm(reg_formula, 
                  data = reg_dat_used,
                  weights = late_exp_share_reg)
  
  predicted_df <- data.frame(predict(reg_model, 
                                     reg_dat_used, 
                                     interval = "conf"), 
                             complexity=reg_dat_used$av_pci_w) 
  # browser()
  exp_plot_re_coef <- ggplot(reg_dat_used,   
                             aes_string(x=x_var,
                                        y=y_var,
                                        color=size_var)) +
    geom_point(aes_string(size=size_var), alpha=0.5) +
    geom_line(color='#00394d', 
              data = predicted_df, 
              aes(x=complexity, y=fit), 
              linetype="solid") +
    geom_line(color='#0086b3', 
              data = predicted_df, 
              aes(x=complexity, y=upr), 
              linetype="dashed") +
    geom_line(color='#0086b3', 
              data = predicted_df, 
              aes(x=complexity, y=lwr), 
              linetype="dashed") +
    ggtitle(paste0(country_name, 
                   ": WLS with recent export shares as weights (", 
                   change_type, " change)")) +
    scale_size(guide=FALSE) + 
    scale_color_viridis() + 
    theme_bw() +
    theme(legend.position = "bottom",
          legend.title = element_text(size=7),
          legend.key = element_rect(size = 10),
          legend.key.size = unit(2, "mm"),
          legend.key.width = unit(8, "mm"),
          plot.title = element_text(size=8),
          axis.title = element_text(size=6),
          panel.border = element_blank(),
          axis.line = element_line(colour = "black"))
  
  return(list(plot=exp_plot_re_coef, reg_model=reg_model))
}

# Regression functions ----

#' #' Braucht es für make_reg_frame
#' set_up_reg_list <- function(dep_var, 
#'                             period_considered,
#'                             changes_considered, 
#'                             length_of_list){
#'   reg_list <- list()
#'   reg_list[["country"]] <- rep(NA, length_of_list)
#'   reg_list[["dep_var"]] <- rep(dep_var, length_of_list)
#'   reg_list[["change_sign"]] <- rep(changes_considered, length_of_list)
#'   reg_list[["period"]] <- rep(period_considered, length_of_list)
#'   reg_list[["rsq_adj"]] <- rep(NA, length_of_list)
#'   
#'   reg_list[["int_coef"]] <- rep(NA, length_of_list)
#'   reg_list[["int_pval"]] <- rep(NA, length_of_list)
#'   reg_list[["int_sig"]] <- rep(NA, length_of_list)
#'   
#'   reg_list[["pci_w_coef"]] <- rep(NA, length_of_list)
#'   reg_list[["pci_w_pval"]] <- rep(NA, length_of_list)
#'   reg_list[["pci_w_sig"]] <- rep(NA, length_of_list)
#'   
#'   reg_list[["late_share_coef"]] <- rep(NA, length_of_list)
#'   reg_list[["late_share_pval"]] <- rep(NA, length_of_list)
#'   reg_list[["late_share_sig"]] <- rep(NA, length_of_list)
#'   return(reg_list)
#' }

#' ADD
set_up_reg_formula <- function(dependent_variable, independent_variables){
  regression_formula <- paste0(dependent_variable, " ~ ")
  for(i in 1:length(independent_variables)) {regression_formula <-paste0(regression_formula, independent_variables[i], " + ")}
  regression_formula <- as.formula(gsub('.{3}$', '', regression_formula))
  return(regression_formula)
}

#' #' Braucht es in make_reg_frame
#' get_the_stars <- function(p_val){
#'   stopifnot(typeof(p_val)=="double")
#'   if (is.na(p_val)){
#'     stars <- NA
#'   } else if(p_val<0.01){
#'     stars <- "***"
#'   } else if(0.01<=p_val & p_val<0.05){
#'     stars <- "**"
#'   } else if(0.05<=p_val & p_val<0.1){
#'     stars <- "*"
#'   } else if (p_val>=0.1){
#'     stars <- "0"
#'   } else {
#'     stars <- NA
#'   }
#'   return(stars)
#' }

# # Braucht es für make_reg_frame
# get_dist_changes <- function(raw_dat, # Either long_data or crisis_data
#                              cntry, # "The name of country/region considered
#                              period_cons="long", # Which period distinction was used for raw_dat? long or crisis
#                              gap="No", # Was a gap used for creation of raw_dat? "Yes" or "No"
#                              var_of_interest){ # The variable considered, takes the dependent variable in reg_frame_...
#   # Takes the long_data or crisis_data as input for raw data and calculates the values, numbers and shares of positive and negative changes
#   if (substr(var_of_interest, nchar(var_of_interest)-2, nchar(var_of_interest)) == "log"){
#     var_con <- substr(var_of_interest, 1, nchar(var_of_interest)-4)
#   } else {
#     var_con <- var_of_interest
#   }
#   
#   work_data_v1 <- raw_dat %>% 
#     select(one_of("exporter", var_con)) %>%
#     filter(exporter==cntry)
#   # browser()
#   stats_list <- list()
#   stats_list[["country"]] <- cntry
#   stats_list[["period"]] <- period_cons
#   stats_list[["gap"]] <- gap
#   stats_list[["var"]] <- NA
#   stats_list[["positive_v"]] <- NA
#   stats_list[["negative_v"]] <- NA
#   stats_list[["positive_n"]] <- NA
#   stats_list[["negative_n"]] <- NA
#   
#   stats_list[["var"]] <- var_con 
#   stats_list[["positive_v"]] <- sum(filter(work_data_v1, UQ(as.name(var_con))>0)[var_con]) # The amount of positive changes
#   stats_list[["negative_v"]] <- sum(filter(work_data_v1, UQ(as.name(var_con))<0)[var_con]) # The amount of negative changes
#   stats_list[["positive_n"]] <- sum(!is.na(filter(work_data_v1, UQ(as.name(var_con))>0)[var_con]))  # The number of positive changes
#   stats_list[["negative_n"]] <- sum(!is.na(filter(work_data_v1, UQ(as.name(var_con))<0)[var_con]))  # The number of negative changes
#   # browser()
#   stats_frame <- as.data.frame(stats_list)
#   # browser()
#   stats_frame <- stats_frame %>%
#     mutate(positive_v_sh=positive_v / (positive_v + abs(negative_v)),
#            negative_v_sh=abs(negative_v) / (positive_v + abs(negative_v)),
#            positive_n_sh=positive_n / (positive_n + negative_n),
#            negative_n_sh=negative_n / (positive_n + negative_n))
#   
#   return(stats_frame)
# }

# 
# make_reg_frame <- function(regression_data, 
#                            dist_change_data, # long_data, crisis_data, or similar
#                            period_cons,
#                            change_cons, 
#                            dep_var, 
#                            idep_vars, 
#                            countries_cons,
#                            gap_cons, # Either "Yes" or "No"
#                            weights=FALSE
# ){
#   
#   stopifnot(change_cons %in% c("positive", "negative"))
#   stopifnot(period_cons %in% c("long", "crisis"))
#   stopifnot(gap_cons %in% c("Yes", "No"))
#   
#   reg_formula <- set_up_reg_formula(dep_var, idep_vars)
#   
#   reg_list <- set_up_reg_list(dep_var = dep_var, 
#                               changes_considered = change_cons,
#                               period_considered = period_cons,
#                               length_of_list = length(countries_cons))
#   if (weights==FALSE){
#     for(i in 1:length(countries_cons)){
#       current_region <- countries_cons[i]
#       print(paste0("Dep var: ", dep_var, " ; changes:", change_cons, "; country: ", current_region))
#       reg_sum <- summary(lm(reg_formula, 
#                             data = filter(regression_data,
#                                           exporter==current_region)))
#       intercept_estimate <- reg_sum$coefficients[1]
#       intercept_pval <- reg_sum$coefficients[10]
#       dep1_estimtate  <- reg_sum$coefficients[2]
#       dep1_pval  <- reg_sum$coefficients[11]
#       dep2_estimate  <- reg_sum$coefficients[3]
#       dep2_pval  <- reg_sum$coefficients[12]
#       
#       reg_list[["country"]][i] <- current_region
#       reg_list[["rsq_adj"]][i] <- reg_sum$adj.r.squared
#       
#       reg_list[["int_coef"]][i] <- intercept_estimate
#       reg_list[["int_pval"]][i] <- intercept_pval
#       reg_list[["int_sig"]][i] <- get_the_stars(intercept_pval)
#       
#       reg_list[["pci_w_coef"]][i] <- dep1_estimtate
#       reg_list[["pci_w_pval"]][i] <- dep1_pval
#       reg_list[["pci_w_sig"]][i] <- get_the_stars(dep1_pval)
#       
#       reg_list[["late_share_coef"]][i] <- dep2_estimate
#       reg_list[["late_share_pval"]][i] <- dep2_pval
#       reg_list[["late_share_sig"]][i] <- get_the_stars(dep2_pval)
#       
#     }
#   } else {
#     if(!(weights %in% names(regression_data)))
#       stop("Weights must be the name of a variable in the regression frame!")
#     
#     for(i in 1:length(countries_cons)){
#       current_region <- countries_cons[i]
#       print(paste0("Dep var: ", dep_var, "weights: ", weight_var, "; changes:", change_cons, "; country: ", current_region))
#       # browser()
#       
#       reg_dat <-  filter(regression_data, 
#                          exporter==current_region) %>%
#         mutate(wgh_var= UQ(as.name(weight_var)))
#       
#       reg_model <- lm(reg_formula, 
#                       data = reg_dat,
#                       weights = wgh_var)
#       reg_sum <- summary(reg_model)
#       
#       intercept_estimate <- reg_sum$coefficients[1]
#       intercept_pval <- reg_sum$coefficients[7]
#       dep1_estimtate  <- reg_sum$coefficients[2]
#       dep1_pval  <- reg_sum$coefficients[8]
#       
#       reg_list[["country"]][i] <- current_region
#       reg_list[["rsq_adj"]][i] <- reg_sum$r.squared
#       # browser()
#       reg_list[["int_coef"]][i] <- intercept_estimate
#       reg_list[["int_pval"]][i] <- intercept_pval
#       reg_list[["int_sig"]][i] <- get_the_stars(intercept_pval)
#       
#       reg_list[["pci_w_coef"]][i] <- dep1_estimtate
#       reg_list[["pci_w_pval"]][i] <- dep1_pval
#       reg_list[["pci_w_sig"]][i] <- get_the_stars(dep1_pval)
#     }
#   }
#   #browser()
#   dist_changes_frame <- get_dist_changes(dist_change_data, countries_cons[1], period_cons = period_cons, gap = gap_cons, var_of_interest = dep_var) 
#   for (i in 2:length(countries_cons)){
#     dist_changes_frame <- rbind(dist_changes_frame, get_dist_changes(dist_change_data, countries_cons[i], period_cons = period_cons, gap = gap_cons, var_of_interest = dep_var)) 
#   }
#   # browser()
#   final_frame <- left_join(data.frame(reg_list), dist_changes_frame, by=c("country", "period"))
#   
#   return(final_frame)
# }

#' Create a WLS plot
#' @param base_data The base data: a list with logs of positive 
#'  and negative changes over the periods considered
#' @param country_name The iso3c code for the country considered
#' @param x_var The variable for the x-axis, usually PCI-related
#' @param y_var The variable for the y-axis, usually diff-related
#' @param size_var, The variable specifying the size of the bubbles and the
#'  weights in the WLS regression
#' @param change_type="positive" Should positive of negative changes be
#'  considered? Usually, 'positive' is chosen
#' @param interval_val="conf" Kind of prediction; "conf" is used as a 
#'  default
#' @param wls_reg Should a WLS reg be used?
make_wls_plot <- function(base_data, country_name, x_var, y_var, size_var, 
                          change_type="positive", interval_val="conf", 
                          wls_reg=TRUE) {
  if (change_type=="positive"){
    reg_dat_used <- dplyr::filter(
      base_data[["pos"]], exporter == country_name)
  } else if (change_type=="negative"){
    reg_dat_used <- dplyr::filter(
      base_data[["neg"]], exporter == country_name)
  } else {
    stop("Wrong change_type given:positive or negative.")
  }
  reg_formula <- set_up_reg_formula(y_var, x_var)
  
  if (wls_reg){
    reg_model <- lm(formula = reg_formula, 
                    data = reg_dat_used,
                    weights = late_exp_share_mean) # TODO: make programmable
  } else{
    reg_model <- lm(formula = reg_formula, 
                    data = reg_dat_used) # TODO: make programmable
  }

  predicted_df <- data.frame(
    predict(reg_model, reg_dat_used, interval = "conf"), 
    complexity=reg_dat_used$av_pci_w) # TODO: make flexible?

  exp_plot_re_coef <- ggplot(
    data = reg_dat_used, 
    mapping = aes_string(x=x_var, y=y_var, color=size_var)) +
    geom_point(aes_string(size=size_var), alpha=0.5) +
    geom_line(
      color='black', 
      data = predicted_df, 
      aes(x=complexity, y=fit), 
      linetype="solid"
      ) +
    geom_line(
      color='black', 
      data = predicted_df, 
      aes(x=complexity, y=upr), 
      linetype="dashed"
      ) +
    geom_line(
      color='black', 
      data = predicted_df, 
      aes(x=complexity, y=lwr), 
      linetype="dashed"
      ) +
    scale_size(guide="none") + 
    labs(x = "Weighted average PCI", y = "Expansions in exports (log)") +
    theme(
      legend.position = "bottom"
      )
    #   legend.title = element_text(size=7),
    #   legend.key = element_rect(size = 10),
    #   legend.key.size = unit(2, "mm"),
    #   legend.key.width = unit(8, "mm"),
    #   plot.title = element_text(size=8),
    #   axis.title = element_text(size=6),
    #   panel.border = element_blank(),
    #   axis.line = element_line(colour = "black")
    #   )
  
  return(list(plot=exp_plot_re_coef, reg_model=reg_model))
}
