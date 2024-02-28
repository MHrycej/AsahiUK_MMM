# Create dummy range variables
#Updated: 27/02/2024

add_new_variable <- function(data, new_var_name, start_date, end_date) {
  # Convert the 'Date' column to Date type if it's not already
  data$Date <- as.Date(data$Date)
  
  # Create the new variable with all zeros
  data[[new_var_name]] <- 0
  
  # Set value to 1 within the specified date range
  data[data$Date >= as.Date(start_date) & data$Date <= as.Date(end_date), new_var_name] <- 1
  
  return(data)
}