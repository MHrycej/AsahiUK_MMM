#MARKETING MIX MODELLING TOOL
library(readxl)
library(here)

#-----------------------------------------------------------------
#--------------------Read in all necessary functions & files------
#-----------------------------------------------------------------

source('functions/source_all_functions.R')


#comment comment comment_mhr

# Specify the directory path
setwd(here())
directory_path <- getwd()

  # Read the import file
import_file <- read.csv(file.path(directory_path, "import_file_test_v1.csv"))
import_file$Date <- as.Date(import_file$Date, format = "%d-%b-%y")

# Read the dates file
dates_file <- read.csv(file.path(directory_path, "dates_lookup.csv"))
dates_file$Date <- as.Date(dates_file$Date, format = "%d-%b-%y")

# Read the taxonomy file
taxonomy <- read_excel(file.path(directory_path, "taxonomy_test.xlsx"))


# MARTIN: new function to read sellout data
source(paste(directory_path, "functions/sellout_data_read_v01.R", sep = "/")) # look into specific file for more details

nielsen = nielsen.creation(
  "C:/Users/MHrycej/OneDrive - ABEG/Martin/Projects/MMM/R GIT/AsahiUK_MMM", 
  "PNA_MULTIPLES_GLASS_330ML_12PACK",  # selection of model
  c("peroni"), # BRAND aggregation brand strings to search for
  c("peroni"), # SKU aggregation brand strings to limit our SKUs
  "Btl 330 Ml 12 Pack"                 # SKU aggregation SKU strings to search for
)

# Extracting the columns with "_Volume" in their names
selected_columns <- grep("_Volume$", colnames(nielsen), value = TRUE)
# Selecting Year and Week columns as the first columns
selected_columns <- c("Year", "Week", selected_columns)
# Creating the new data frame 'nielsen_volume' with the selected columns
nielsen_volume <- nielsen[selected_columns]
# Convert 'Week' column to numeric
nielsen_volume$Week <- as.numeric(nielsen_volume$Week)

# Format the week number with two digits
nielsen_volume$Date <- as.Date(ISOweek::ISOweek2date(paste(nielsen_volume$Year, sprintf("W%02d", nielsen_volume$Week), "1", sep="-")))
nielsen_volume <- nielsen_volume %>%
  select(-matches("dep_|c_brand_"), -Year, -Week)



import_file1 <- left_join(import_file, nielsen_volume, by = "Date")


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

model1 <- lm(
  `c_sku_Peroni Nastro Azzurro Btl 330 Ml 12 pack_Volume` ~ #dependent variable
    dis_peroni_btl_12p_330_wtd_avg+
    p_peroni_btl_12p_330_bp+
    p_peroni_btl_12p_330_dis+
    s_fathers_day+
    s_ny_day+
    atan(m_press_spend30/140000)+
    s_xmas_day+
    sh_xmas_hol
  
  ,data = import_file1)



# Model results
model_stats(model1, date_var = import_file1$Date)


#-----------------------------------------------------------------



#-----------------------------------------------------------------
#----------------------Visual analysis----------------------------
#-----------------------------------------------------------------

# Chart variables
plot_line1((atan(import_file$m_press_spend30/30000)), import_file)
plot_line2("c_sku_Peroni Nastro Azzurro Btl 330 Ml 12 pack_Volume", "p_peroni_btl_12p_330_bp", import_file1)

# Actual vs. predicted chart vs. variable. Use "" to see just actual vs. predicted
actual_vs_fitted_plot(model1, import_file1, "")

# Residual plot
residuals_vs_variable_plot(model1, import_file, "gt_peroni")

# Price elasticity
calculate_price_elasticity(model1, "c_sku_Peroni Nastro Azzurro Btl 330 Ml 12 pack_Volume", "p_peroni_btl_12p_330_bp", import_file1)

# Plot media curve
plot_media_curve(import_file, media_var = "m_press_spend30", dim_ret = 2000)

# Automatic variable selection
auto_variable_selection(model1, import_file1, "c_sku_")

# Media heatmap
## Martin - here is the aesthetics part of the heat map code 
### Let me know if you need more info about the data feeding into the plot
# pick a nice colour scheme
#col <- colorRampPalette(rev(rgb(c(231,117,27),c(41,112,158),c(138,179,119),
#                              max=255)))(100)
# plot the data
# p <- ggplot(DATA)
# p <- p + geom_tile(aes(x=x.pos, y=y.pos, fill=density, height=1000, width=1000))+
#   scale_fill_gradientn(colours=col, space="Lab", na.value="grey50",
#                        guide="colourbar")
# p + theme_bw() + coord_equal()

#-----------------------------------------------------------------
#----------------------Decomposition------------------------------
#-----------------------------------------------------------------

model_decomp(model1)







