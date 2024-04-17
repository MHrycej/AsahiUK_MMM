#Residuals Histogram
#Updated: 17/04/2024

create_residuals_histogram <- function(model, import_file) {
  # Getting residuals from the model
  model_augmented <- broom::augment(model)
  
  residuals <- model_augmented %>%
    dplyr::select(residuals = .resid)
  
  # Check if 'Date' is already present in import_file
  if ("Date" %in% colnames(import_file)) {
    merged_table <- dplyr::bind_cols(residuals, import_file)
  } else {
    stop("Error: 'Date' column not found in import_file.")
  }
  
  # Create histogram of residuals
  hist(merged_table$residuals, breaks = 30, freq = FALSE, col = "skyblue", 
       main = "Residuals Histogram", xlab = "Residual Value")
  
  # Overlay with density curve of normal distribution
  curve(dnorm(x, mean = mean(merged_table$residuals), sd = sd(merged_table$residuals)), 
        add = TRUE, col = "red", lwd = 2)
  
  # Get predicted values and residuals
  predicted <- fitted(model)
  residuals <- residuals(model)
  
  # Create scatterplot of residuals vs predicted values
  plot(predicted, residuals,
       xlab = "Predicted Values",
       ylab = "Residuals",
       main = "Residuals vs Predicted Values Plot",
       pch = 16,  # point shape
       col = "blue",  # point color
       cex = 1.2)  # point size
  
  # Add a horizontal line at y = 0 for reference
  abline(h = 0, col = "red")
  
  # Add a lowess smooth line to visually assess the pattern
  #lines(lowess(predicted, residuals), col = "green")
  
  # Add a legend
  legend("topright", legend = c("Residuals"), col = c("blue"), lty = 1, cex = 0.8)
}
