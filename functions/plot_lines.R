### Variables charting
### plot_line1
#Updated: 27/02/2024



plot_line1 <- function(formula, data) {
  # Capture the formula expression as a string and extract the part after the $
  var_full_name <- deparse(substitute(formula))
  var_name <- sub(".*\\$", "", var_full_name)
  
  # Apply the formula to create a new variable
  data$custom_var <- eval(formula, envir = data)
  
  data$Date <- as.Date(data$Date, format = "%d-%b-%y")
  
  # Create a ggplot line plot
  gg <- ggplot(data, aes(x = Date)) +
    geom_line(aes(y = custom_var, color = var_name)) + # Use var_name here
    scale_x_date(date_labels = "%b-%y", date_breaks = "4 months") +
    scale_y_continuous(
      name = 'Variable',
      labels = comma
    ) +
    labs(title = paste('Line Plot of', var_name),
         x = 'Weeks',
         color = 'Legend') +
    theme_minimal() +
    theme(legend.position = "top", axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
  
  # Convert ggplot to plotly
  plot <- ggplotly(gg, tooltip = c("Date", var_name)) # Use var_name here
  
  # Adjust the width and move the legend inside the chart
  plot <- plot %>% layout(width = 700, legend = list(x = 0.0, y = 1.05, orientation = 'h'))
  
  # Display the plot
  print(plot)
}


# Example usage:
# plot_line1(~ atan(m_Press_Total_Spend_eur_adstock0.3 / 140000), import_file)


#####################################################################
#plot_line2
library(ggplot2)
library(plotly)
library(dygraphs)

plot_line2 <- function(var1, var2, data) {
  data$Date <- as.Date(data$Date, format = "%d-%b-%y")
  
  # Create a plotly line chart
  plot <- plot_ly(data, x = ~Date, type = "scatter", mode = "lines", y = ~get(var1), name = var1, line = list(color = 'blue')) %>%
    add_trace(y = ~get(var2), name = var2, line = list(color = 'red'), yaxis = "y2") %>%
    layout(title = "Interactive Line Plot of Two Variables",
           xaxis = list(title = "Date"),
           yaxis = list(title = paste0(var1)),
           yaxis2 = list(title = paste0(var2), overlaying = "y", side = "right"))
  
  # Display the plot
  print(plot)
}


###############################################################
#plot curve
plot_media_curve <- function(data, media_var, dim_ret) {
  max_x <- max(data[[media_var]])
  
  plot_ly(data, x = ~get(media_var), y = ~atan(get(media_var) / dim_ret),
          type = "scatter", mode = "markers") %>%
    add_trace(x = max_x, y = atan(max_x / dim_ret),
              type = "scatter", mode = "text", text = paste("dim_ret =", dim_ret),
              showlegend = FALSE) %>%
    layout(xaxis = list(title = media_var), yaxis = list(title = "atan(Media Variable / dim_ret)"))
}
