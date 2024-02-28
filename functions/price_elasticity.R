#Own price elasticity
#Updated: 27/02/2024

calculate_price_elasticity <- function(model, dependent_variable, price_variable, data_table) {
  # Get the coefficients
  coefficients <- coef(model)
  
  # Get the coefficient for the specified price variable
  elasticity_coefficient <- coefficients[price_variable]
  
  # Calculate mean values for price variable and dependent variable
  mean_price <- mean(data_table[[price_variable]])
  mean_dependent <- mean(data_table[[dependent_variable]])
  
  # Calculate price elasticity
  price_elasticity <- elasticity_coefficient * (mean_price / mean_dependent)
  
  # Print the result
  cat("Own Price Elasticity:", price_elasticity, "\n")
  
  
}