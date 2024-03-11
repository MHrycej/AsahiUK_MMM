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
  c("peroni", "moretti", "madri", "estrella", "asd", "san miguel", "heineken", "cruzcampo", "corona", "stella","budweiser"), # SKU aggregation brand strings to limit our SKUs
  ""                 # SKU aggregation SKU strings to search for

)


#exploring nielsen

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



result <- left_join(dates_file, nielsen_volume, by = "Date")

# Remove rows with NA values in the "Year" column
result <- result[complete.cases(result$year), ]

# Replace all NAs with 0s
result[is.na(result)] <- 0



# Group by "Year", summarize, transpose, and sort
result_transposed <- result %>%
  select(-Date) %>%
  group_by(year) %>%
  summarise(across(everything(), sum)) %>%
  pivot_longer(cols = -year, names_to = "SKU", values_to = "Volume") %>%
  pivot_wider(names_from = year, values_from = Volume, values_fill = 0) %>%
  arrange(desc(`2023`)) 

# Write result_transposed to an Excel file in the specified directory
write_xlsx(result_transposed, path = file.path(directory_path, "result_transposed_v1.xlsx"))
