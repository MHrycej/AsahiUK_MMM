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
import_file <- add_new_variable(import_file, new_var_name = "new_variable", start_date = "2023-02-10", end_date = "2024-02-15")

# Split variables
import_file$test_var_new_var <- import_file$gt_peroni * import_file$new_variable

# Create lag /lead vars: 1. specify variable name, 2. specify variable to lag/lead
import_file$s_christmas_lead1 <- lead(import_file$s_christmas,1) %>% replace(is.na(.), 0)
import_file$s_christmas_lead2 <- lead(import_file$s_christmas,2) %>% replace(is.na(.), 0)
import_file$gt_peroni_lag1 <- lag(import_file$gt_peroni,1) %>% replace(is.na(.), 0)

# Create moving average variables
window_sizes <- c(3, 5, 7, 9, 11, 13) #specify week ranges you want to create moving averages
import_file <- calculate_rolling_averages(import_file, "bt_brandvue_peroni_consideration", window_sizes)


# Add custom variables to taxonomy file (decomping purpose)
taxonomy <- dplyr::bind_rows(
  taxonomy,
  taxonomy %>% filter(variable_name == 's_christmas') %>% mutate(variable_name = 's_christmas_lead1'),
  taxonomy %>% filter(variable_name == 's_christmas') %>% mutate(variable_name = 's_christmas_lead2')
)




#------------------------------------------------------------------------------
#------------------------MODEL-------------------------------------------------
#------------------------------------------------------------------------------
#### 2sls ####
formula.2sls = gt_peroni~ # mod_vol_multiples_pna_glass_330ml_18_24pack~ #dependent variable
  # mod_bp_multiples_pna_glass_330ml_18_24pack+
  mod_discount_multiples_pna_glass_330ml_18_24pack+
  mod_featdisp_multiples_pna_glass_330ml_18_24pack+
  dummy_month_jan+
  dummy_month_feb+
  dummy_month_mar+
  dummy_month_apr+
  dummy_month_may+
  dummy_month_jun+
  dummy_month_jul+
  dummy_month_aug+
  dummy_month_sep+
  dummy_month_oct+
  dummy_month_nov+
  s_christmas+
  s_christmas_lead1+
  s_spring_bank_holiday+
  s_good_friday+
  covid_mobility_residential+
  events_peroni_uefa_21+
  events_peroni_all_racing+
  events_peroni_fifa_world_cup_22+
  c_discount_multiples_birra_moretti_btl_330_ml_18_pack+
  atan(m_tv_peroni_eurosfinal_tvr_adstock60/3.32)

import_file$gt_trend_2sls_residuals = lm(formula = formula.2sls, data = import_file)$residuals


#### formula definition ####
formula.01 = mod_vol_multiples_pna_glass_330ml_18_24pack~ #dependent variable
  # mod_bp_multiples_pna_glass_330ml_18_24pack+
  mod_discount_multiples_pna_glass_330ml_18_24pack+
  mod_featdisp_multiples_pna_glass_330ml_18_24pack+
  dummy_month_jan+
  dummy_month_feb+
  dummy_month_mar+
  dummy_month_apr+
  dummy_month_may+
  dummy_month_jun+
  dummy_month_jul+
  dummy_month_aug+
  dummy_month_sep+
  dummy_month_oct+
  dummy_month_nov+
  s_christmas+
  s_christmas_lead1+
  s_spring_bank_holiday+
  s_good_friday+
  covid_mobility_residential+
  events_peroni_uefa_21+
  events_peroni_all_racing+
  events_peroni_fifa_world_cup_22+
  c_discount_multiples_birra_moretti_btl_330_ml_18_pack+
  atan(m_tv_peroni_eurosfinal_tvr_adstock60/3.32)+
  gt_trend_2sls_residuals

summary(lm(formula = formula.01, data = import_file))
plot(import_file$gt_peroni, import_file$gt_trend_2sls_residuals, pch = 20)

#### correlation ####
cor_dat = 
  import_file[, c(
  "mod_vol_multiples_pna_glass_330ml_18_24pack", 
  "gt_peroni", 
  "mod_discount_multiples_pna_glass_330ml_18_24pack", 
  "mod_featdisp_multiples_pna_glass_330ml_18_24pack", 
  "dummy_month_jan", 
  "dummy_month_feb", 
  "dummy_month_mar", 
  "dummy_month_apr", 
  "dummy_month_may", 
  "dummy_month_jun", 
  "dummy_month_jul", 
  "dummy_month_aug", 
  "dummy_month_sep", 
  "dummy_month_oct", 
  "dummy_month_nov", 
  "s_christmas", 
  "s_christmas_lead1", 
  "s_spring_bank_holiday", 
  "s_good_friday", 
  "covid_mobility_residential", 
  "events_peroni_uefa_21", 
  "events_peroni_all_racing", 
  "events_peroni_fifa_world_cup_22", 
  "c_discount_multiples_birra_moretti_btl_330_ml_18_pack"
)]

cor(cor_dat)


####  Model results ####

#use the same name as in dependent variable without "mod_vol_"
multiples_pna_glass_330ml_18_24pack <- lm(formula = formula.01, data = import_file)
model_stats(multiples_pna_glass_330ml_18_24pack, date_var = import_file$Date)
print(summary(multiples_pna_glass_330ml_18_24pack))
plot(x = import_file$mod_vol_multiples_pna_glass_330ml_18_24pack, y = import_file$gt_peroni, pch = 20)

#------------------------------------------------------------------------------
#----------------------Functions-----------------------------------------------
#------------------------------------------------------------------------------

# Actual vs. predicted chart vs. variable. Use "" to see just actual vs. predicted
actual_vs_fitted_plot(multiples_pna_glass_330ml_18_24pack, import_file, "")

# Automatic variable selection
auto_variable_selection(multiples_pna_glass_330ml_18_24pack, import_file, "m_tv_peroni")

# adstock & dr heatmap
heatmap(
  dataset = import_file,
  formula.input = paste(formula.01[2], formula.01[1], formula.01[3], sep = " "), # please remember that the formula should not include analysed expense channel
  expense_channel = "m_tv_peroni_eurosfinal_tvr",
  adstocks = c(0, .1, .2, .3, .4, .5, .6, .7, .8, .9), #c(.1)
  dr_type = "atan",
  dr_divisors = c(.4, .5, .6, .7, .8, .9, 1), # c(.4)
  criteria = c("R2")) # "R2", "t-stat"

# Chart variables
plot_line1(import_file$multiples_pna_glass_330ml_18_24pack, import_file)
plot_line1((atan(import_file$m_tv_peroni_total_tvr/50)), import_file)
plot_line2("mod_vol_multiples_pna_glass_330ml_18_24pack", "	gt_lager", import_file)


# Residual plot
residuals_vs_variable_plot(multiples_pna_glass_330ml_18_24pack, import_file, "multiples_pna_glass_330ml_18_24pack")

create_residuals_histogram(multiples_pna_glass_330ml_18_24pack, import_file)

# Price elasticity
calculate_price_elasticity(multiples_pna_glass_330ml_18_24pack, "mod_vol_multiples_pna_glass_330ml_18_24pack", "mod_bp_multiples_pna_glass_330ml_18_24pack", import_file)

# Plot media curve
plot_media_curve(import_file, media_var = "m_tv_peroni_total_tvr", dim_ret = 30)




#------------------------------------------------------------------------------
#----------------------Decomposition-------------------------------------------
#------------------------------------------------------------------------------

model_decomp(multiples_pna_glass_330ml_18_24pack)

final_decomp_export <- model_decomp(multiples_pna_glass_330ml_18_24pack)
write.csv(final_decomp_export, file = file.path(directory_path, "/decomps/decomp_multiples_pna_glass_330ml_18_24pack.csv"), row.names = FALSE)

generate_roi_table()
