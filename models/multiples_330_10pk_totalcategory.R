#MARKETING MIX MODELLING TOOL
library(here)

#-----------------------------------------------------------------
#--------------------Read in all necessary functions & files------
#-----------------------------------------------------------------

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


# Read the spends file




#-----------------------------------------------------------------
#-----------------------Variable creation-------------------------
#-----------------------------------------------------------------



# Create new dummy variable with 1s: specify name and range
#import_file <- add_new_variable(import_file, new_var_name = "new_variable", start_date = "2023-02-10", end_date = "2024-02-15")

# Split variables
#import_file$test_var_new_var <- import_file$gt_peroni * import_file$new_variable


# Create lag /lead vars: 1. specify variable name, 2. specify variable to lag/lead
import_file$s_christmas_lead1 <- lead(import_file$s_christmas,1) %>% replace(is.na(.), 0)
#import_file$s_christmas_lead2 <- lead(import_file$s_christmas,2) %>% replace(is.na(.), 0)
#import_file$gt_peroni_lag1 <- lag(import_file$gt_peroni,1) %>% replace(is.na(.), 0)

# create relative pricing
import_file$rel_price_multiples_glass_330ml_10pack_1 <- import_file$mod_bp_multiples_pna_glass_330ml_10pack/import_file$c_bp_multiples_total_btl_330_10_pack

 
# Create moving average variables
window_sizes <- c(3, 5, 7, 9, 11, 13) #specify week ranges you want to create moving averages
import_file <- calculate_rolling_averages(import_file, "bt_peroni_consideration", window_sizes)

# Add custom variables to taxonomy file (decomping purpose)
taxonomy <- dplyr::bind_rows(
  taxonomy,
  taxonomy %>% filter(variable_name == 's_christmas') %>% mutate(variable_name = 's_christmas_lead1'),
  taxonomy %>% filter(variable_name == 'bt_peroni_consideration') %>% mutate(variable_name = 'bt_peroni_consideration_3ma'),
  taxonomy %>% filter(variable_name == 'c_bp_multiples_total_btl_330_10_pack') %>% mutate(variable_name = 'rel_price_multiples_glass_330ml_10pack_1')
)





#-----------------------------------------------------------------
#------------------------MODEL------------------------------------
#-----------------------------------------------------------------

#### formula definition ####
formula.01 = mod_vol_multiples_pna_glass_330ml_10pack~ #dependent variable
  mod_dist_multiples_pna_glass_330ml_10pack+
  mod_bp_multiples_pna_glass_330ml_10pack+
  mod_discount_multiples_pna_glass_330ml_10pack+
  mod_featdisp_multiples_pna_glass_330ml_10pack+
  cat_vol_multiples_glass_10_pack+
  #s_christmas+
  #s_christmas_lead1+
  #s_boxing_day+
  #s_christmas_lead2+
  #s_spring_bank_holiday+
  #s_good_friday+
  #dummy_month_may+
  #dummy_month_apr+
  #dummy_month_mar+
  #dummy_month_feb+
  #dummy_month_jul+
  #dummy_month_dec+
  #dummy_month_jun+
  #dummy_month_aug+
  #dummy_month_nov+
  #dummy_month_dec+
  #w_wtd_max_temp_c+
  #w_hourly_temperature_dev_dt+
  #w_sunhour_smoothed+
  #gt_peroni+
  #covid_mobility_residential+
  #covid_third_lockdown_decay+
  events_peroni_uefa_21+
  events_peroni_all_racing+
  events_peroni_fifa_world_cup_22+
  #events_peroni_rugby_world_cup_23+
  #bt_peroni_consideration_11ma+
  #c_bp_impulse_stella_artois_can_440_ml_10_pack+
  #c_bp_multiples_corona_btl_330_ml_10_pack+
  #rel_price_multiples_glass_330ml_10pack_1+ #takes away effect from distribution and probably it's coincidence that there's drop in relative pricing during same time while distribution increases significantly
  #c_bp_multiples_budweiser_btl_300_ml_6_pack+
  #c_bp_multiples_stella_artois_btl_284_ml_18_pack+
  #c_bp_multiples_total_btl_330_10_pack+
  #c_discount_multiples_stella_artois_btl_284_ml_12_pack+
  #c_discount_multiples_corona_btl_330_ml_24_pack+
  #e_cci+
  #e_rpi+
  #atan(m_yt_peroni_im_adstock10/5000000)+
  #atan(m_ooh_peroni_total_imp_adstock50/140000000)+
  atan(m_oohunscored_peroni_total_sp_adstock70/75820)+
  #atan(m_sponsor_peroni_now_im_adstock10/14500000)+
  #atan(m_vod_peroni_im_adstock60/1000000)+
  atan(m_digital_peroni_total_im_adstock50/2000000)+
  atan(m_influencers_peroni_sp_adstock50/38000)+
  atan(cm_total_stella_unf_sp_adstock20/300000)
  #dummy_20221218+
  #dummy_20210606
  #dummy_20220424


#### end of formula def ####
#use the same name as in dependent variable without "mod_vol_"
multiples_pna_glass_330ml_10pack <- lm(formula = formula.01, data = import_file)

# Model results
model_stats(multiples_pna_glass_330ml_10pack, date_var = import_file$Date)


#-----------------------------------------------------------------



#-----------------------------------------------------------------
#----------------------Functions----------------------------
#-----------------------------------------------------------------

# adstock & dr heatmap

heatmap(
  dataset = import_file,
  formula.input = paste(formula.01[2], formula.01[1], formula.01[3], sep = " "), # please remember that the formula should not include analysed expense channel
  expense_channel = "cm_total_stella_unf_sp",
  adstocks = c(0, .1, .2, .3, .4, .5, .6, .7, .8, .9), #c(.1)
  dr_type = "atan",
  dr_divisors = c(.4, .5, .6, .7, .8, .9, 1), # c(.4)
  criteria = c("t-stat")) # "R2", "t-stat"

# Automatic variable selection
auto_variable_selection(multiples_pna_glass_330ml_10pack, import_file, "cat_vol_multiples")

# Chart variables
plot_line1((atan(import_file$m_youtube_peroni_sp_adstock10/30000)), import_file)
plot_line2("c_bp_multiples_total_btl_330_10_pack", "mod_bp_multiples_pna_glass_330ml_10pack", import_file)

# Actual vs. predicted chart vs. variable. Use "" to see just actual vs. predicted
actual_vs_fitted_plot(multiples_pna_glass_330ml_10pack, import_file, "cat_vol_multiples_glass_10_pack")

# Residual plot
residuals_vs_variable_plot(multiples_pna_glass_330ml_10pack, import_file, "gt_peroni")

create_residuals_histogram(multiples_pna_glass_330ml_10pack, import_file)

# Price elasticity
calculate_price_elasticity(multiples_pna_glass_330ml_10pack, "mod_vol_multiples_pna_glass_330ml_10pack", "mod_bp_multiples_pna_glass_330ml_10pack", import_file)

# Plot media curve
plot_media_curve(import_file, media_var = "m_youtube_peroni_sp_adstock10", dim_ret = 2000)




#-----------------------------------------------------------------
#----------------------Decomposition------------------------------
#-----------------------------------------------------------------

model_decomp(multiples_pna_glass_330ml_10pack)

final_decomp_export <- model_decomp(multiples_pna_glass_330ml_10pack)
write.csv(final_decomp_export, file = file.path(directory_path, "/decomps/decomp_multiples_pna_glass_330ml_10pack_smoothweather.csv"), row.names = FALSE)

generate_roi_table("smoothweather")
