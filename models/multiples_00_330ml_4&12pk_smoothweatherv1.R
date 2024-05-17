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
#import_file$s_christmas_lead1 <- lead(import_file$s_christmas,1) %>% replace(is.na(.), 0)
#import_file$s_christmas_lead2 <- lead(import_file$s_christmas,2) %>% replace(is.na(.), 0)
#import_file$gt_peroni_lag1 <- lag(import_file$gt_peroni,1) %>% replace(is.na(.), 0)

# create relative pricing
import_file$rel_price_multiples_pna00_glass_330ml_4_12pack_1 <- import_file$mod_bp_multiples_pna00_glass_330ml_4_12pack/import_file$c_bp_multiples_total00_btl_330_12_pack

# Create moving average variables
window_sizes <- c(3, 5, 7, 9, 11, 13) #specify week ranges you want to create moving averages
import_file <- calculate_rolling_averages(import_file, "bt_peroni_consideration", window_sizes)


# Add custom variables to taxonomy file (decomping purpose)
taxonomy <- dplyr::bind_rows(
  taxonomy,
  taxonomy %>% filter(variable_name == 'bt_peroni_consideration') %>% mutate(variable_name = 'bt_peroni_consideration_13ma'),
  taxonomy %>% filter(variable_name == 'c_bp_multiples_total00_btl_330_12_pack') %>% mutate(variable_name = 'rel_price_multiples_pna00_glass_330ml_4_12pack_1')
)




#------------------------------------------------------------------------------
#------------------------MODEL-------------------------------------------------
#------------------------------------------------------------------------------

###formula definition ####
formula.01 = mod_vol_multiples_pna00_glass_330ml_4_12pack~ #dependent variable
  mod_dist_multiples_pna00_glass_330ml_4_12pack+
  #own_dist_multiples_peroni_nastro_azzurro_0_0_btl_330_ml_12_pack+
  #own_dist_multiples_peroni_nastro_azzurro_0_0_btl_330_ml_4_pack+
  #mod_bp_multiples_pna00_glass_330ml_4_12pack+
  #own_bp_multiples_peroni_nastro_azzurro_0_0_btl_330_ml_4_pack+
  #own_bp_multiples_peroni_nastro_azzurro_0_0_btl_330_ml_12_pack+
  mod_discount_multiples_pna00_glass_330ml_4_12pack+
  mod_featdisp_multiples_pna0_glass_330ml_4_12pack+
  #s_christmas+
  s_new_years_day+
  s_spring_bank_holiday+
  #s_all_bank_holiday+
  dummy_month_jan+
  dummy_month_feb+
  #dummy_month_mar+
  #dummy_month_apr+
  #dummy_month_may+
  dummy_month_jun+
  dummy_month_jul+
  #dummy_month_aug+
  dummy_month_sep+
  dummy_month_oct+
  #dummy_month_nov+
  dummy_month_dec+
  w_hourly_temperature_dev_dt+
  w_hourly_temperature_smoothed+
  cat_smooth_vol00_multiples_glass_4_12_v1+
  #market_multiples_total_distribution+
  e_cci+
  #e_rpi+
  #bt_peroni_consideration_13ma+
  #covid_hospital_cases+
  covid_third_lockdown_decay+
  events_peroni_howdens_xmas_raceday+
  #events_peroni_rugby_world_cup_23+
  #events_peroni_uefa_21+
  dummy_20231217+
  #dummy_trend+
  #c_bp_multiples_total00_btl_330_12_pack+
  #rel_price_multiples_pna00_glass_330ml_4_12pack_1+ #acting as a trend, CHECK
  #c_discount_multiples_corona_cero_btl_330_ml_4_pack+
  c_discount_multiples_heineken_0_0_alcohol_free_btl_330_ml_12_pack+
  #atan(m_tv_peroni0_total_tvr_adstock70/50)+
  atan(m_sponsor_peroni0_firstdate_im_adstock10/2500000)+
  #atan(m_vod_peroni0_total_im_adstock60/2000000)+
  atan(m_social_peroni0_total_im_adstock10/1300000)
  #atan(m_yt_peroni0_im_adstock40/150000)
  #dummy_pna0_launch



#### end of formula def ####

#----------------------- Model results-----------------------------------------

#use the same name as in dependent variable without "mod_vol_"
multiples_pna00_glass_330ml_4_12pack <- lm(formula = formula.01, data = import_file)
model_stats(multiples_pna00_glass_330ml_4_12pack, date_var = import_file$Date)



#------------------------------------------------------------------------------
#----------------------Functions-----------------------------------------------
#------------------------------------------------------------------------------

# Actual vs. predicted chart vs. variable. Use "" to see just actual vs. predicted
actual_vs_fitted_plot(multiples_pna00_glass_330ml_4_12pack, import_file, "")

# Automatic variable selection
auto_variable_selection(multiples_pna00_glass_330ml_4_12pack, import_file, "dummy_month")

# adstock & dr heatmap
heatmap(
  dataset = import_file,
  formula.input = paste(formula.01[2], formula.01[1], formula.01[3], sep = " "), # please remember that the formula should not include analysed expense channel
  expense_channel = "m_yt_peroni0_im",
  adstocks = c(0, .1, .2, .3, .4, .5, .6, .7, .8, .9), #c(.1)
  dr_type = "atan",
  dr_divisors = c(.4, .5, .6, .7, .8, .9, 1), # c(.4)
  criteria = c("t-stat")) # "R2", "t-stat"

# Chart variables
plot_line1(import_file$mod_bp_multiples_pna00_glass_330ml_4_12pack, import_file)
plot_line1((atan(import_file$m_tv_peroni_total_tvr/50)), import_file)
plot_line2("c_bp_multiples_heineken_0_0_alcohol_free_btl_330_ml_12_pack", "c_avp_multiples_heineken_0_0_alcohol_free_btl_330_ml_12_pack", import_file)


# Residual plot
residuals_vs_variable_plot(multiples_pna00_glass_330ml_4_12pack, import_file, "mod_discount_multiples_pna00_glass_330ml_4_12pack")

create_residuals_histogram(multiples_pna00_glass_330ml_4_12pack, import_file)

# Price elasticity
calculate_price_elasticity(multiples_pna00_glass_330ml_4_12pack, "mod_vol_multiples_pna00_glass_330ml_4_12pack", "mod_bp_multiples_pna00_glass_330ml_4_12pack", import_file)

# Plot media curve
plot_media_curve(import_file, media_var = "m_tv_peroni_total_tvr", dim_ret = 30)




#------------------------------------------------------------------------------
#----------------------Decomposition-------------------------------------------
#------------------------------------------------------------------------------

model_decomp(multiples_pna00_glass_330ml_4_12pack)

final_decomp_export <- model_decomp(multiples_pna00_glass_330ml_4_12pack)
write.csv(final_decomp_export, file = file.path(directory_path, "/decomps/decomp_multiples_pna00_glass_330ml_4_12pack_smoothweather.csv"), row.names = FALSE)

generate_roi_table("smoothweather")
