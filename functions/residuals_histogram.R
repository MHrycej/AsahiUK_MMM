#Residuals Histogram
#Updated: 19/03/2024

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
}