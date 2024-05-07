#MARKETING MIX MODELLING TOOL

#------------------------------------------------------------------------------
#--------------------Read in all necessary functions & files-------------------
#------------------------------------------------------------------------------

library(here)
source('functions/source_all_functions.R')

# Specify the directory path
setwd(here())
directory_path <- getwd()

Sys.setlocale("LC_TIME", "C") # I need this in order for the dates to work

# Read the import file
import_file <- read.csv(file.path(directory_path, "master_file.csv"))
import_file$Date <- as.Date(import_file$Date, format = "%d-%b-%y")

# Read the dates file
dates_file <- read.csv(file.path(directory_path, "dates_lookup.csv"))
dates_file$Date <- as.Date(dates_file$Date, format = "%d-%b-%y")

import_file <- merge(import_file, dates_file, by = "Date", all.x = TRUE)

# Read the taxonomy file
taxonomy <- read_excel(file.path(directory_path, "taxonomy.xlsx"), sheet = "taxonomy")



#------------------------------------------------------------------------------
#-----------------------Variable creation--------------------------------------
#------------------------------------------------------------------------------



# Create new dummy variable with 1s: specify name and range
#import_file <- add_new_variable(import_file, new_var_name = "new_variable", start_date = "2023-02-10", end_date = "2024-02-15")

# Split variables
#import_file$test_var_new_var <- import_file$gt_peroni * import_file$new_variable

# Create lag /lead vars: 1. specify variable name, 2. specify variable to lag/lead
import_file$s_christmas_lead1 <- lead(import_file$s_christmas,1) %>% replace(is.na(.), 0)
#import_file$s_christmas_lead2 <- lead(import_file$s_christmas,2) %>% replace(is.na(.), 0)
#import_file$gt_peroni_lag1 <- lag(import_file$gt_peroni,1) %>% replace(is.na(.), 0)

# create relative pricing
#import_file$rel_price_multiples_can_330ml_6_10pack_1 <- import_file$mod_bp_multiples_pna_can_330ml_6_10pack/import_file$c_bp_multiples_birra_moretti_can_330_ml_6_pack
import_file$rel_price_multiples_can_330ml_6_10pack_2 <- import_file$mod_bp_multiples_pna_can_330ml_6_10pack/import_file$c_bp_multiples_total_can_330_6_pack

# Create moving average variables
window_sizes <- c(3, 5, 7, 9, 11, 13) #specify week ranges you want to create moving averages
import_file <- calculate_rolling_averages(import_file, "bt_peroni_consideration", window_sizes)


# Add custom variables to taxonomy file (decomping purpose)
taxonomy <- dplyr::bind_rows(
  taxonomy,
  taxonomy %>% filter(variable_name == 'bt_peroni_consideration') %>% mutate(variable_name = 'bt_peroni_consideration_5ma'),
  taxonomy %>% filter(variable_name == 's_christmas') %>% mutate(variable_name = 's_christmas_lead1'),
  taxonomy %>% filter(variable_name == 'c_bp_multiples_total_can_330_6_pack') %>% mutate(variable_name = 'rel_price_multiples_can_330ml_6_10pack_2')
)




#------------------------------------------------------------------------------
#------------------------MODEL-------------------------------------------------
#------------------------------------------------------------------------------

#### formula definition ####
formula.01 = mod_vol_multiples_pna_can_330ml_6_10pack~ #dependent variable
  mod_dist_multiples_pna_can_330ml_6_10pack+
  #mod_bp_multiples_pna_can_330ml_6_10pack+
  #own_bp_multiples_peroni_nastro_azzurro_can_330_ml_6_pack+
  mod_discount_multiples_pna_can_330ml_6_10pack+
  #mod_disp_multiples_pna_can_330ml_6_10pack+
  mod_featdisp_multiples_pna_can_330ml_6_10pack+
  #gt_lager+
  s_christmas+
  s_christmas_lead1+
  s_spring_bank_holiday+
  #s_all_school_holidays+
  s_new_years_day+
  #w_deviation_max_temp_c+
  #w_deviation_min_temp_c+
  w_hourly_temperature_dev_dt+
  w_sunhour_smoothed+
  #w_wtd_sunhour+
  #w_wtd_max_temp_c+
  #e_unemployment+
  #e_rpi+
  #e_cci+
  #bt_brandvue_peroni_love+
  #bt_peroni_awareness_5ma+
  bt_peroni_consideration_5ma+
  #bt_peroni_consideration+
  #bt_peroni_consideration_5ma+
  #bt_peroni_consideration_13ma+
  events_peroni_rugby_world_cup_23+
  events_rugby_wc_final+
  events_peroni_royal_ascot+
  #events_rugby_wc_argentina+
  #events_peroni_race_wknd+
  #events_peroni_bst+
  #covid_new_daily_deaths+
  #covid_hospital_cases+
  #covid_third_lockdown_decay+
  #dummy_month_jan+
  #dummy_month_feb+
  #dummy_month_mar+
  #dummy_month_apr+
  #dummy_month_may+
  #dummy_month_jun+
  dummy_month_jul+
  dummy_month_aug+
  #dummy_month_feb
  dummy_month_dec+
  #c_bp_multiples_stella_artois_can_440_ml_10_pack+
  #c_bp_multiples_birra_moretti_can_330_ml_6_pack+
  #c_bp_multiples_budweiser_can_440_ml_10_pack+
  #c_bp_multiples_san_miguel_can_568_ml_4_pack+
  #c_bp_multiples_birra_moretti_btl_330_ml_4_pack+
  #rel_price_multiples_can_330ml_6_10pack_1+
  #c_bp_multiples_total_can_330_6_pack+
  rel_price_multiples_can_330ml_6_10pack_2+
  #own_bp_multiples_peroni_nastro_azzurro_btl_330_ml_10_pack+
  #c_discount_multiples_stella_artois_can_440_ml_18_pack+
  #c_discount_multiples_san_miguel_can_330_ml_6_pack+
  c_discount_multiples_corona_can_330_ml_6_pack+
  c_discount_multiples_stella_artois_can_568_ml_4_pack+
  #c_discount_multiples_stella_artois_btl_330_ml_12_pack+
  #c_discount_multiples_madri_exceptional_can_440_ml_10_pack+
  #c_discount_multiples_san_miguel_can_440_ml_10_pack
  atan(m_tv_peroni_total_tvr_adstock60/70)+
  atan(m_ooh_peroni_total_imp_adstock50/120000000)+
  #atan(m_vod_peroni_im_adstock30/1200000)+
  atan(m_social_peroni_total_im_adstock10/10000000)+
  #atan(m_cinema_peroni_ad_adstock60/1500000)+
  #atan(m_digital_peroni_total_im_adstock60/18000)+
  #atan(m_yt_peroni_im_adstock30/7000000)+ #correlates with social and too strong? - CHECK
  #atan(m_spotify_peroni_im_adstock20/300000)+ #creates high VIF with many other variables
  atan(cm_total_heineken_sp_adstock10/7000)+
  atan(cm_total_stella_unf_sp_adstock40/380000)
  #dummy_20220605+
  #dummy_20210829+
  #dummy_20210207
  #dummy_20220717+
  #dummy_20230521
  #dummy_20231029+
  #dummy_20230611
  
  

#### end of formula def ####

#----------------------- Model results-----------------------------------------

#use the same name as in dependent variable without "mod_vol_"
multiples_pna_can_330ml_6_10pack <- lm(formula = formula.01, data = import_file)
model_stats(multiples_pna_can_330ml_6_10pack, date_var = import_file$Date)



#------------------------------------------------------------------------------
#----------------------Functions-----------------------------------------------
#------------------------------------------------------------------------------

# Actual vs. predicted chart vs. variable. Use "" to see just actual vs. predicted
actual_vs_fitted_plot(multiples_pna_can_330ml_6_10pack, import_file, "")

# Automatic variable selection
auto_variable_selection(multiples_pna_can_330ml_6_10pack, import_file, "m_ooh_peroni_total")

# adstock & dr heatmap
heatmap(
  dataset = import_file,
  formula.input = paste(formula.01[2], formula.01[1], formula.01[3], sep = " "), # please remember that the formula should not include analysed expense channel
  expense_channel = "m_cinema_peroni_ad",
  adstocks = c(0, .1, .2, .3, .4, .5, .6, .7, .8, .9), #c(.1)
  dr_type = "atan",
  dr_divisors = c(.4, .5, .6, .7, .8, .9, 1), # c(.4)
  criteria = c("t-stat")) # "R2", "t-stat"

# Chart variables
plot_line1(import_file$mod_bp_multiples_pna_can_330ml_6_10pack, import_file)
plot_line1((atan(import_file$m_tv_peroni_total_tvr/50)), import_file)
plot_line2("mod_vol_multiples_pna_can_330ml_6_10pack", "gt_lager", import_file)


# Residual plot
residuals_vs_variable_plot(multiples_pna_can_330ml_6_10pack, import_file, "dummy_month_jan")

create_residuals_histogram(multiples_pna_can_330ml_6_10pack, import_file)

# Price elasticity
calculate_price_elasticity(multiples_pna_can_330ml_6_10pack, "mod_vol_multiples_pna_can_330ml_6_10pack", "mod_bp_multiples_pna_can_330ml_6_10pack", import_file)

# Plot media curve
plot_media_curve(import_file, media_var = "m_tv_peroni_total_tvr", dim_ret = 30)




#------------------------------------------------------------------------------
#----------------------Decomposition-------------------------------------------
#------------------------------------------------------------------------------

model_decomp(multiples_pna_can_330ml_6_10pack)

final_decomp_export <- model_decomp(multiples_pna_can_330ml_6_10pack)
write.csv(final_decomp_export, file = file.path(directory_path, "/decomps/decomp_multiples_pna_can_330ml_6_10pack_smoothweather.csv"), row.names = FALSE)

generate_roi_table()
