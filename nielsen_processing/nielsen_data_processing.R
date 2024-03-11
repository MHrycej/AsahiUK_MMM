#Process Nielsen data: pull needed skus and metrics we need for modelling

library(writexl)
library(readxl)
library(here)
library(ISOweek)

setwd(here())
directory_path <- getwd()

# Read the dates file
dates_file <- read.csv(file.path(directory_path, "dates_lookup_real.csv"))
dates_file$Date <- as.Date(dates_file$Date, format = "%d-%b-%y")


#new function to read sellout data
source(paste(directory_path, "functions/sellout_data_read_v01.R", sep = "/")) # look into specific file for more details

nielsen = nielsen.creation(
  "C:/Users/MHrycej/OneDrive - ABEG/Martin/Projects/MMM/R GIT/AsahiUK_MMM", 
  "PNA_MULTIPLES_GLASS_330ML_4PACK",  # selection of model
  c("peroni"), # BRAND aggregation brand strings to search for
  c(""), # SKU aggregation brand strings to limit our SKUs
  c("Birra Moretti Btl 660 Ml single", "Stella Artois Btl 660 Ml single", "Corona Btl 620 Ml single")                 # SKU aggregation SKU strings to search for

)


#exploring nielsen

# Extracting needed columns
selected_columns <- grep("_distribution_w$|Price", colnames(nielsen), value = TRUE)

# Selecting Year and Week columns as the first columns
selected_columns <- c("Year", "Week", selected_columns)

# Creating the new data frame 'nielsen_data' with the selected columns
nielsen_data <- nielsen[selected_columns]
# Convert 'Week' column to numeric
nielsen_data$Week <- as.numeric(nielsen_data$Week)

# Format the week number with two digits
nielsen_data$Date <- as.Date(ISOweek::ISOweek2date(paste(nielsen_data$Year, sprintf("W%02d", nielsen_data$Week), "1", sep="-")))
nielsen_data <- nielsen_data %>%
  select(-matches("dep_|c_brand_"), -Year, -Week)

# Trim the dates range by joining it with dates_file
result <- left_join(dates_file, nielsen_data, by = "Date")
result <- result %>%
  select(-year) %>%
  mutate_all(~replace_na(., 0))

# Write result_transposed to an Excel file in the specified directory
#write_xlsx(result, path = file.path(directory_path, "nielsen_data.xlsx"))




##################################################################
# FOR TAXONOMY

sku_names_long <- gather(result[-1], key = "Variable") %>%
  distinct(Variable) %>%
  mutate(
    abbreviation = gsub("[ %()\\.-]", "_", tolower(Variable)), #add "_", remove symbols and put to lower case
    decomp = case_when(
      grepl("distribution", abbreviation) ~ "distribution", #map to decomps
      grepl("price", abbreviation) ~ "price", #map to decomps
      grepl("promo", abbreviation) ~ "promotions", #map to decomps
      TRUE ~ "other"
    )
  )


