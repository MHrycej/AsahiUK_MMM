library(readxl)
library(openxlsx)
library(dplyr)
library(purrr)
library(here)
library(writexl)

# Specify the directory path
setwd(here())
directory_path <- getwd()

# Read the import file
media_data <- read_excel(file.path(directory_path, "/adstocked_media/media_data.xlsx"))
media_data$Date <- as.Date(media_data$Date, format = "%d-%b-%y")

# Define adstock coefficients list
adstock_coefficients <- c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7)

# Create a new data frame for adstocked values
transformed_data <- media_data %>% select(Date)

# Iterate over each channel
for (channel in colnames(media_data)[2:ncol(media_data)]) {
  
  # Iterate over each adstock coefficient
  for (adstock_coefficient in adstock_coefficients) {
    
    # Calculate adstocked values directly in the loop
    media_values <- as.numeric(media_data[[channel]])
    adstocked_values <- accumulate(media_values, ~ round(.x * adstock_coefficient + .y, digits = 4))
    
    # Create column name with channel and adstock information
    adstock_col_name <- paste0(channel, "_adstock", adstock_coefficient * 100) # Multiply by 100 for naming
    
    # Assign adstocked values to the new data frame, including the last value
    transformed_data[[adstock_col_name]] <- c(0, adstocked_values)[1:length(transformed_data$Date)]
  }
}

# Keep original media_data columns in transformed_data
transformed_data <- left_join(media_data, transformed_data, by = "Date")

transformed_data <- transformed_data %>% select(where(~all(!is.na(.))))

# Save transformed_data to Excel
write_xlsx(transformed_data, path = file.path(directory_path, "/adstocked_media/adstocked_media.xlsx"))
