#Calculate moving averages
#Updated: 12/04/2024

calculate_rolling_averages <- function(data, variable_name, window_sizes) {
  # Find the column index of the variable
  var_index <- match(variable_name, colnames(data))
  
  if (is.na(var_index)) {
    stop("Variable not found in the data frame")
  }
  
  # Extract the variable
  variable <- data[, var_index]
  
  # Initialize matrix to store rolling averages
  rolling_averages <- matrix(NA, nrow = nrow(data), ncol = length(window_sizes))
  
  # Calculate rolling averages for each window size
  for (i in seq_along(window_sizes)) {
    window_size <- window_sizes[i]
    for (j in seq_len(nrow(data))) {
      start_index <- max(1, j - (window_size - 1))
      end_index <- j
      rolling_averages[j, i] <- mean(variable[start_index:end_index], na.rm = TRUE)
    }
  }
  
  # Add rolling averages to data frame
  for (i in seq_along(window_sizes)) {
    new_variable_name <- paste0(variable_name, "_", window_sizes[i], "ma")
    data[new_variable_name] <- rolling_averages[, i]
  }
  
  return(data)
}