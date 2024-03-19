# Actual vs. Predicted
#Updated: 27/02/2024


actual_vs_fitted_plot <- function(model, import_file, third_variable_name) {
  # getting variables used in the model
  model_augmented <- augment(model)
  
  model_augmented1 <- model_augmented %>%
    select(actual = 1, predicted = .fitted)
  
  # Check if 'Date' is already present in import_file
  if ("Date" %in% colnames(import_file)) {
    merged_table <- bind_cols(model_augmented1, import_file)
  } else {
    stop("Error: 'Date' column not found in import_file.")
  }
  
  # Line chart with dual Y-axes
  plot <- plot_ly(merged_table, x = ~Date) %>%
    add_trace(y = ~actual, type = 'scatter', mode = 'lines+markers', name = 'Actual', yaxis = 'y1') %>%
    add_trace(y = ~predicted, type = 'scatter', mode = 'lines+markers', name = 'Predicted', yaxis = 'y1') %>%
    layout(title = if (third_variable_name != "") paste("Actual vs. Predicted vs.", third_variable_name) else "Actual vs. Predicted",
           xaxis = list(title = "Date"),
           yaxis = list(title = "Actual & Predicted", side = 'left', showgrid = FALSE))
  
  # Add third variable trace if provided
  if (third_variable_name != "") {
    plot <- plot %>% 
      add_trace(y = as.formula(paste0("~", third_variable_name)),
                type = 'scatter', mode = 'lines+markers', name = third_variable_name, yaxis = 'y2') %>% 
      layout(yaxis2 = list(title = third_variable_name, side = 'right', overlaying = "y", showgrid = FALSE))
  }
  
  # Display the plot
  plot
}