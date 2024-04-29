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
#import_file$rel_price_multiples_glass_330ml_10pack_1 <- import_file$mod_bp_multiples_pna_glass_330ml_10pack/import_file$c_bp_multiples_corona_btl_330_ml_10_pack

# Create moving average variables
window_sizes <- c(3, 5, 7, 9, 11, 13) #specify week ranges you want to create moving averages
import_file <- calculate_rolling_averages(import_file, "bt_peroni_consideration", window_sizes)


# Add custom variables to taxonomy file (decomping purpose)
taxonomy <- dplyr::bind_rows(
  taxonomy,
  taxonomy %>% filter(variable_name == 'bt_peroni_consideration') %>% mutate(variable_name = 'bt_peroni_consideration_9ma')
  #taxonomy %>% filter(variable_name == 's_christmas') %>% mutate(variable_name = 's_christmas_lead2')
)




#------------------------------------------------------------------------------
#------------------------MODEL-------------------------------------------------
#------------------------------------------------------------------------------

#### formula definition ####
formula.01 = mod_vol_impulse_pna_glass_330ml_1_4pack~ #dependent variable
  mod_dist_impulse_pna_glass_330ml_1_4pack+
  #own_dist_impulse_peroni_nastro_azzurro_btl_330_ml_single+
  #own_dist_impulse_peroni_nastro_azzurro_btl_330_ml_4_pack+
  mod_bp_impulse_pna_glass_330ml_1_4pack+
  mod_discount_impulse_pna_glass_330ml_1_4pack+
  #dummy_month_jan+
  #dummy_month_feb+
  #dummy_month_mar+
  dummy_month_apr+
  dummy_month_may+
  dummy_month_jun+
  dummy_month_jul+
  dummy_month_aug+
  #dummy_month_sep+
  #dummy_month_oct+
  dummy_month_nov+
  #dummy_month_dec+
  #s_all_bank_holiday+
  s_good_friday+
  s_spring_bank_holiday+
  w_hourly_temperature_dev_dt+
  w_hourly_wind_speed_dev_dt+
  #e_cci+
  covid_hospital_cases+
  bt_peroni_consideration_9ma+
  events_peroni_howdens_xmas_raceday+
  events_peroni_uefa_21+
  c_discount_impulse_corona_btl_330_ml_single+
  c_discount_impulse_stella_artois_btl_330_ml_4_pack+
  atan(m_tv_peroni_total_tvr_adstock60/70)+
  #atan(m_ooh_peroni_total_imp_adstock10/140000000)+
  #atan(m_sponsor_peroni_now_im_adstock20/8000000)+
  atan(m_social_peroni_total_im_adstock10/10000000)+
  atan(m_spotify_peroni_im_adstock10/300000)+
  atan(cm_total_stella_unf_sp_adstock40/300000)+
  atan(cm_total_budweiser_sp_adstock40/250000)



#### end of formula def ####

#----------------------- Model results-----------------------------------------

#use the same name as in dependent variable without "mod_vol_"
impulse_pna_glass_330ml_1_4pack <- lm(formula = formula.01, data = import_file)
model_stats(impulse_pna_glass_330ml_1_4pack, date_var = import_file$Date)



#------------------------------------------------------------------------------
#----------------------Functions-----------------------------------------------
#------------------------------------------------------------------------------

# Actual vs. predicted chart vs. variable. Use "" to see just actual vs. predicted
actual_vs_fitted_plot(impulse_pna_glass_330ml_1_4pack, import_file, "mod_bp_impulse_pna_glass_330ml_1_4pack")

# Automatic variable selection
auto_variable_selection(impulse_pna_glass_330ml_1_4pack, import_file, "own_dist_impulse")

# adstock & dr heatmap
heatmap(
  dataset = import_file,
  formula.input = paste(formula.01[2], formula.01[1], formula.01[3], sep = " "), # please remember that the formula should not include analysed expense channel
  expense_channel = "cm_total_budweiser_sp",
  adstocks = c(0, .1, .2, .3, .4, .5, .6, .7, .8, .9), #c(.1)
  dr_type = "atan",
  dr_divisors = c(.4, .5, .6, .7, .8, .9, 1), # c(.4)
  criteria = c("t-stat")) # "R2", "t-stat"

# Chart variables
plot_line1(import_file$mod_discount_impulse_pna_glass_330ml_1_4pack, import_file)
plot_line1((atan(import_file$m_tv_peroni_total_tvr/50)), import_file)
plot_line2("mod_vol_impulse_pna_glass_330ml_1_4pack", "	gt_lager", import_file)


# Residual plot
residuals_vs_variable_plot(impulse_pna_glass_330ml_1_4pack, import_file, "mod_discount_impulse_pna_glass_330ml_1_4pack")

create_residuals_histogram(impulse_pna_glass_330ml_1_4pack, import_file)

# Price elasticity
calculate_price_elasticity(impulse_pna_glass_330ml_1_4pack, "mod_vol_impulse_pna_glass_330ml_1_4pack", "mod_bp_impulse_pna_glass_330ml_1_4pack", import_file)

# Plot media curve
plot_media_curve(import_file, media_var = "m_tv_peroni_total_tvr", dim_ret = 30)




#------------------------------------------------------------------------------
#----------------------Decomposition-------------------------------------------
#------------------------------------------------------------------------------

model_decomp(impulse_pna_glass_330ml_1_4pack)

final_decomp_export <- model_decomp(impulse_pna_glass_330ml_1_4pack)
write.csv(final_decomp_export, file = file.path(directory_path, "/decomps/decomp_impulse_pna_glass_330ml_1_4pack.csv"), row.names = FALSE)

generate_roi_table()
