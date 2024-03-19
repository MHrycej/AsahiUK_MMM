# Residuals vs. variable chart
# Updated: 27/02/2024


residuals_vs_variable_plot <- function(model, import_file, second_variable) {
  # getting residuals from the model
  model_augmented <- augment(model)
  
  residuals <- model_augmented %>%
    select(residuals = .resid)
  
  # Check if 'Date' is already present in import_file
  if ("Date" %in% colnames(import_file)) {
    merged_table <- bind_cols(residuals, import_file)
  } else {
    stop("Error: 'Date' column not found in import_file.")
  }
  
  # Bar chart for residuals with dual Y-axes
  plot <- plot_ly(merged_table, x = ~Date) %>%
    add_trace(y = ~residuals, type = 'bar', name = 'Residuals', yaxis = 'y1') %>%
    layout(title = paste("Residuals vs.", second_variable),
           xaxis = list(title = "Date"),
           yaxis = list(title = "Residuals", side = 'left', showgrid = FALSE)) 
  
  # Add second variable as a line on the right Y-axis
  plot <- plot %>% 
    add_trace(y = as.formula(paste0("~", second_variable)),
              type = 'scatter', mode = 'lines', name = second_variable, yaxis = 'y2') %>% 
    layout(yaxis2 = list(title = second_variable, side = 'right', overlaying = "y", showgrid = FALSE))
  
  # Display the plot
  plot
}