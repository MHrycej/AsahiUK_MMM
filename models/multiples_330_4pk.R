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
import_file <- add_new_variable(import_file, new_var_name = "new_variable", start_date = "2023-02-10", end_date = "2024-02-15")

# Split variables
import_file$test_var_new_var <- import_file$gt_peroni * import_file$new_variable


# Create lag /lead vars: 1. specify variable name, 2. specify variable to lag/lead
import_file$s_christmas_lead1 <- lead(import_file$s_christmas,1) %>% replace(is.na(.), 0)
import_file$s_christmas_lead2 <- lead(import_file$s_christmas,2) %>% replace(is.na(.), 0)
#import_file$gt_peroni_lag1 <- lag(import_file$gt_peroni,1) %>% replace(is.na(.), 0)

# Create moving average variables
window_sizes <- c(3, 5, 7, 9, 11, 13) #specify week ranges you want to create moving averages
import_file <- calculate_rolling_averages(import_file, "bt_brandvue_peroni_consideration", window_sizes)


# Add custom variables to taxonomy file (decomping purpose)
taxonomy <- dplyr::bind_rows(
  taxonomy,
  taxonomy %>% filter(variable_name == 'bt_brandvue_peroni_consideration') %>% mutate(variable_name = 'bt_brandvue_peroni_consideration_3ma')
)




#-----------------------------------------------------------------
#------------------------MODEL------------------------------------
#-----------------------------------------------------------------

#### formula definition ####
formula.01 = mod_vol_multiples_pna_glass_330ml_4pack~ #dependent variable
  #mod_dist_multiples_pna_glass_330ml_4pack+
  mod_bp_multiples_pna_glass_330ml_4pack+ #change price manually in master file
  mod_discount_multiples_pna_glass_330ml_4pack+
  mod_featdisp_multiples_pna_glass_330ml_4pack+
  gt_peroni+
  #dummy_month_sep+
  #dummy_month_oct+
  #dummy_month_nov+
  #dummy_month_jun+
  #dummy_month_jul+
  #dummy_month_apr+
  #dummy_month_dec+
  s_christmas+
  #s_christmas_lead1
  #s_new_years_day+
  s_spring_bank_holiday+
  s_good_friday+
  #s_fathers_day+
  w_wtd_avg_temp_c+
  e_rpi+
  #e_cci+
  dummy_month_jan+
  #bt_brandvue_peroni_consideration+
  bt_brandvue_peroni_consideration_3ma+
  events_peroni_uefa_21+
  events_peroni_howdens_xmas_raceday+
  covid_new_daily_deaths+
  #c_bp_multiples_budweiser_can_568_ml_4_pack+
  #c_bp_multiples_stella_artois_btl_330_ml_6_pack+
  #c_bp_multiples_stella_artois_btl_330_ml_4_pack+
  c_discount_multiples_stella_artois_btl_330_ml_12_pack+
  c_discount_multiples_stella_artois_unfiltered_can_440_ml_4_pack+
  #c_discount_multiples_heineken_silver_btl_330_ml_12_pack
  atan(m_tv_peroni_total_tvr_adstock60/70)+
  atan(m_ooh_peroni_total_imp_adstock30/70000000)+
  atan(m_oohunscored_peroni_total_sp_adstock30/170000)+
  atan(m_sponsor_peroni_now_sp/40000)+
  atan(m_vod_peroni_im_adstock30/1900000)+
  atan(m_cinema_peroni_ad/2700000)+
  atan(m_social_peroni_total_im_adstock20/6500000)+
  atan(m_digital_peroni_total_sp_adstock30/18000)+
  atan(m_influencers_peroni_sp_adstock40/10000)+
  atan(cm_total_san_miguel_sp_adstock60/300000)



#### end of formula def ####
#use the same name as in dependent variable without "mod_vol_"
multiples_pna_glass_330ml_4pack <- lm(formula = formula.01, data = import_file)

# Model results
model_stats(multiples_pna_glass_330ml_4pack, date_var = import_file$Date)


#-----------------------------------------------------------------



#-----------------------------------------------------------------
#----------------------Functions----------------------------
#-----------------------------------------------------------------

# Actual vs. predicted chart vs. variable. Use "" to see just actual vs. predicted
actual_vs_fitted_plot(multiples_pna_glass_330ml_4pack, import_file, "")


# Automatic variable selection
auto_variable_selection(multiples_pna_glass_330ml_4pack, import_file, "dummy_month")


# adstock & dr heatmap

heatmap(
  dataset = import_file,
  formula.input = paste(formula.01[2], formula.01[1], formula.01[3], sep = " "), # please remember that the formula should not include analysed expense channel
  expense_channel = "m_influencers_peroni_sp",
  adstocks = c(0, .1, .2, .3, .4, .5, .6, .7, .8, .9), #c(.1)
  dr_type = "atan",
  dr_divisors = c(.4, .5, .6, .7, .8, .9, 1), # c(.4)
  criteria = c("t-stat")) # "R2", "t-stat"



# Chart variables
plot_line1((import_file$e_rpi), import_file)
plot_line2("bt_brandvue_peroni_consideration", "bt_brandvue_peroni_consideration_13ma", import_file)



# Residual plot
residuals_vs_variable_plot(multiples_pna_glass_330ml_10pack, import_file, "gt_peroni")

create_residuals_histogram(multiples_pna_glass_330ml_4pack, import_file)

# Price elasticity
calculate_price_elasticity(multiples_pna_glass_330ml_4pack, "mod_vol_multiples_pna_glass_330ml_4pack", "mod_bp_multiples_pna_glass_330ml_4pack", import_file)

# Plot media curve
plot_media_curve(import_file, media_var = "m_youtube_peroni_sp_adstock10", dim_ret = 2000)




#-----------------------------------------------------------------
#----------------------Decomposition------------------------------
#-----------------------------------------------------------------

model_decomp(multiples_pna_glass_330ml_4pack)

final_decomp_export <- model_decomp(multiples_pna_glass_330ml_4pack)
write.csv(final_decomp_export, file = file.path(directory_path, "/decomps/decomp_multiples_pna_glass_330ml_4pack.csv"), row.names = FALSE)

generate_roi_table()
