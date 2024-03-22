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
taxonomy <- read_excel(file.path(directory_path, "taxonomy.xlsx"))


# Read the spends file




#-----------------------------------------------------------------
#-----------------------Variable creation-------------------------
#-----------------------------------------------------------------



# Create new dummy variable with 1s: specify name and range
import_file <- add_new_variable(import_file, new_var_name = "new_variable", start_date = "2023-02-10", end_date = "2024-02-15")

# Split variables
import_file$test_var_new_var <- import_file$gt_peroni * import_file$new_variable


# Create lag /lead vars: 1. specify variable name, 2. specify variable to lag/lead
import_file$gt_peroni_lead1 <- lead(import_file$gt_peroni,1) %>% replace(is.na(.), 0)
import_file$gt_peroni_lag1 <- lag(import_file$gt_peroni,1) %>% replace(is.na(.), 0)



#----------------------------------------------------------------
# Add custom variables to taxonomy file (decomping purpose)
taxonomy <- dplyr::bind_rows(
  taxonomy,
  taxonomy %>% filter(variable_name == 'gt_peroni') %>% mutate(variable_name = 'gt_peroni_lag1'),
  taxonomy %>% filter(variable_name == 'gt_peroni') %>% mutate(variable_name = 'gt_peroni_lead1')
)





#-----------------------------------------------------------------
#------------------------MODEL------------------------------------
#-----------------------------------------------------------------

#### formula definition ####
formula.01 = mod_vol_multiples_pna_glass_330ml_10pack~ #dependent variable
  #mod_dist_multiples_pna_glass_330ml_10pack+
  mod_bp_multiples_pna_glass_330ml_10pack+
  mod_discount_multiples_pna_glass_330ml_10pack+
  mod_featdisp_multiples_pna_glass_330ml_10pack+
  s_christmas+
  s_spring_bank_holiday+
  s_good_friday+
  # dummy_month_may+
  # dummy_month_apr+
  # dummy_month_dec+
  #w_wtd_max_temp_c+
  gt_peroni+
  covid_mobility_residential+
  events_peroni_uefa_21+
  events_peroni_all_racing+
  events_peroni_fifa_world_cup_22+
  #c_bp_multiples_corona_btl_330_ml_10_pack+
  #c_bp_multiples_budweiser_btl_300_ml_6_pack+
  #c_bp_multiples_stella_artois_btl_284_ml_18_pack+
  c_discount_multiples_stella_artois_btl_284_ml_12_pack+
  c_discount_multiples_corona_btl_330_ml_24_pack+
  #e_cci
  #s_fathers_day+
  #s_school_christmas_holidays+
  #w_deviation_max_temp_c+
  atan(m_vod_peroni_first_dates_sponsor_sp_adstock50/70000)+
  atan(m_youtube_peroni_sp_adstock10/10000)


#### end of formula def ####

#Name the model correctly here: e.g. peroni_multiples_330_12pk
model1 <- lm(formula = formula.01, data = import_file)

# Model results
model_stats(model1, date_var = import_file$Date)


#-----------------------------------------------------------------



#-----------------------------------------------------------------
#----------------------Functions----------------------------
#-----------------------------------------------------------------

# adstock & dr heatmap

heatmap(
  dataset = import_file,
  formula.input = paste(formula.01[2], formula.01[1], formula.01[3], sep = " "), # please remember that the formula should not include analysed expense channel
  expense_channel = "m_ooh_peroni_digital_imp",
  adstocks = c(0, .1, .2, .3, .4, .5, .6, .7, .8, .9), #c(.1)
  dr_type = "atan",
  dr_divisors = c(.4, .5, .6, .7, .8, .9, 1, 1.1, 1.3, 1.5, 1.7, 1.9, 2.1), # c(.4)
  criteria = c("t-stat")) # "R2", "t-stat"

# Automatic variable selection
auto_variable_selection(model1, import_file, "m_ooh_peroni")

# Chart variables
plot_line1((atan(import_file$m_press_spend30/30000)), import_file)
plot_line2("mod_vol_multiples_pna_glass_330ml_10pack", "m_vod_peroni_first_dates_sponsor_sp_adstock50", import_file)

# Actual vs. predicted chart vs. variable. Use "" to see just actual vs. predicted
actual_vs_fitted_plot(model1, import_file, "")

# Residual plot
residuals_vs_variable_plot(model1, import_file, "gt_peroni")

create_residuals_histogram(model1, import_file)

# Price elasticity
calculate_price_elasticity(model1, "mod_vol_multiples_pna_glass_330ml_10pack", "mod_bp_multiples_pna_glass_330ml_10pack", import_file)

# Plot media curve
plot_media_curve(import_file, media_var = "m_press_spend30", dim_ret = 2000)




#-----------------------------------------------------------------
#----------------------Decomposition------------------------------
#-----------------------------------------------------------------

model_decomp(model1)


