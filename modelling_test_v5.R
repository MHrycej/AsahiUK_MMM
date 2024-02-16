#MARKETING MIX MODELLING TOOL
library(readxl)

#-----------------------------------------------------------------
#--------------------Read in all necessary files------------------
#-----------------------------------------------------------------

#comment comment

# Specify the directory path
directory_path <- getwd()

  # Read the import file
import_file <- read.csv(file.path(directory_path, "import_file_test.csv"))
import_file$Date <- as.Date(import_file$Date, format = "%d-%b-%y")

# Read the dates file
dates_file <- read.csv(file.path(directory_path, "dates_lookup.csv"))
dates_file$Date <- as.Date(dates_file$Date, format = "%d-%b-%y")

# Read the taxonomy file
taxonomy <- read_excel(file.path(directory_path, "taxonomy_test.xlsx"))

# Read the spends file





#-----------------------------------------------------------------
#-----------------------Variable creation-------------------------
#-----------------------------------------------------------------



# Create new dummy variable with 1s: specify name and range
import_file <- add_new_variable(import_file, new_var_name = "new_variable", start_date = "2023-02-10", end_date = "2024-02-15")

# Split variables
import_file$test_var_new_var <- import_file$gt_peroni * import_file$new_variable


# Create lag /lead vars: 1. specify variable name, 2. specify variable to lag/lead
import_file$gt_peroni_lead1 <- lead(import_file$gt_peroni,1) %>% replace(is.na(.), 0)
import_file$gt_peroni_lag1 <- lag(import_file$gt_peroni,1) %>% replace(is.na(.), 0)



#----------------------------------------------------------------
# Add custom variables to taxonomy file (decomping purpose)
taxonomy <- dplyr::bind_rows(
  taxonomy,
  taxonomy %>% filter(variable_name == 'gt_peroni') %>% mutate(variable_name = 'gt_peroni_lag1'),
  taxonomy %>% filter(variable_name == 'gt_peroni') %>% mutate(variable_name = 'gt_peroni_lead1')
)





#-----------------------------------------------------------------
#------------------------MODEL------------------------------------
#-----------------------------------------------------------------

model1 <- lm(
  dep_peroni_btl_12p_330_vol ~ #dependent variable
    dis_peroni_btl_12p_330_wtd_avg+
    p_peroni_btl_12p_330_bp+
    p_peroni_btl_12p_330_dis+
    s_fathers_day+
    s_ny_day+
    atan(m_press_spend30/140000)+
    s_xmas_day+
    sh_xmas_hol
  
  ,data = import_file)



# Model results
model_stats(model1, date_var = import_file$Date)


#-----------------------------------------------------------------



#-----------------------------------------------------------------
#----------------------Visual analysis----------------------------
#-----------------------------------------------------------------

# Chart variables
plot_line1((atan(import_file$m_press_spend30/30000)), import_file)
plot_line2("p_peroni_btl_12p_330_ap", "p_peroni_btl_12p_330_bp", import_file)

# Actual vs. predicted chart vs. variable. Use "" to see just actual vs. predicted
actual_vs_fitted_plot(model1, import_file, "")

# Residual plot
residuals_vs_variable_plot(model1, import_file, "gt_peroni")

# Price elasticity
calculate_price_elasticity(model1, "dep_peroni_btl_12p_330_vol", "p_peroni_btl_12p_330_bp", import_file)

# Plot media curve
plot_media_curve(import_file, media_var = "m_press_spend30", dim_ret = 2000)

# Automatic variable selection
auto_variable_selection(model1, import_file, "x_")

# Media heatmap


#-----------------------------------------------------------------
#----------------------Decomposition------------------------------
#-----------------------------------------------------------------

model_decomp(model1)













#----------------------------------------------------------------
#--------------------------Functions-----------------------------
#----------------------------------------------------------------

##############  ###################################################
#model stats
library(dplyr)
library(car)
library(DT)
library(skedastic)
library(tidyr)
library(stringr)

model_stats <- function(model, date_var = NULL) {
  
  # Check if there is a previous model to compare with
  if (exists("prev_model", envir = .GlobalEnv)) {
    # Calculate percentage change in coefficients
    prev_coeffs <- tidy(prev_model)
    current_coeffs <- tidy(model)  # Move this line here
    
    # Ensure both data frames have the same terms
    common_terms <- intersect(prev_coeffs$term, current_coeffs$term)
    
    # Initialize coeff_change with 0 for all terms in current_coeffs
    coeff_change <- rep(0, nrow(current_coeffs))
    
    # Find matching indices for common terms
    prev_indices <- match(common_terms, prev_coeffs$term)
    current_indices <- match(common_terms, current_coeffs$term)
    
    # Update coeff_change only for the common terms
    coeff_change[current_indices] <- (current_coeffs$estimate[current_indices] - prev_coeffs$estimate[prev_indices]) /
      prev_coeffs$estimate[prev_indices]
  } else {
    # If it's the first run, set coeff_change to 0
    current_coeffs <- tidy(model)  # Move this line here
    coeff_change <- rep(0, nrow(current_coeffs))
  }
  
  # Save the current model for the next run
  assign("prev_model", model, envir = .GlobalEnv)
  
  # Getting VIF codes for variables
  vif_df <- car::vif(model) %>% as.data.frame()
  vif_tidy <- tibble(term = rownames(vif_df), VIF = vif_df$.) #tidy up
  
  # Getting coeffs and all the other stats for the variables included in the model and joining with VIF codes
  mod_coeffs <- broom::tidy(model) %>%
    dplyr::left_join(vif_tidy, by = "term") %>%
    mutate(coeff_change = coeff_change[match(term, current_coeffs$term)]) %>%
    rename(t.stat = statistic)
  
  # Formatting the table for the variable stats (coeffs, t-stats etc)
  mod_coef_tbl <- datatable(mod_coeffs, class = "compact - hover",
                            colnames = c('term', 'estimate', 'std.error', 't.stat', 'p.value', 'VIF', 'coeff_change'),
                            options = list(
                              dom = 't',
                              scrolly = FALSE,
                              pageLength = 500,
                              initComplete = JS(
                                "function(settings, json) {",
                                "$(this.api().table().container()).css({'font-size': '14px'});",
                                "}"
                              )
                            )) %>%
    formatRound(c('estimate', 'std.error', 't.stat', 'p.value', 'VIF'), 2) %>%
    formatPercentage('coeff_change', 0) %>%
    formatStyle('estimate',
                color = styleInterval(c(0),
                                      c("#FF0000B3", "#00A600B3")))
  
  # Calculating durbin watson for the model and white test
  DW <- car::durbinWatsonTest(model)$dw
  white_test <- skedastic::white(model, interactions = FALSE, statonly = FALSE) %>%
    dplyr::pull(p.value)
  
  # Getting all different model stats and adding DW and white test
  model_stats <- broom::glance(model) %>%
    bind_cols(tibble(`Durbin-Watson` = DW,
                     `White test` = white_test))
  
  # Creating overall model stats, putting into long format, tidying up
  model_stats_tbl <- model_stats %>%
    pivot_longer(cols = everything(), names_to = "Statistic", values_to = "Value") %>%
    mutate(Value = round(Value, 2))
  
  # Formatting the table for overall model stats
  mod_stat_tbl <- datatable(model_stats_tbl, 
                            class = "compact - hover",
                            colnames = c('Statistic', 'Value'),
                            options = list(
                              dom = 't',
                              scrolly = FALSE,
                              pageLength = 500,
                              initComplete = JS(
                                "function(settings, json) {",
                                "$(this.api().table().container()).css({'font-size': '14px'});",
                                "}"
                              )
                            )) %>%
    formatRound('Value', 2) %>%
    formatStyle('Value',
                color = styleInterval(c(0),
                                      c("#FF0000B3", "#00A600B3")))
  
  print(mod_coef_tbl)
  print(mod_stat_tbl)
}





###############################################################
#####-----Model decomposition-----###
# Need to create functionality that let's us save media variables into taxonomy file, so it will be mapped against media channel
# modify mod_coeffs file so that it will take slope number off from the label and in taxonomy we map all the media channels with adstocks, slopes aren't necessary in this case

library(janitor)
library(scales)
library(DT)
library(broom)
library(forcats)


model_decomp <- function(model) {

#creating model coefficients table
mod_coeffs <- broom::tidy(model) %>%
    select(term, estimate) %>%
    mutate(term = case_when(
      term == "(Intercept)" ~ "intercept",
      TRUE ~ term
    ))




# Melt import_file to long format
import_file_long <- import_file %>%
  gather(term, value, -Date)

# Merge melted import_file_long with mod_coeffs on term
merged_df <- import_file_long %>%
  left_join(mod_coeffs %>% select(term, estimate), by = "term") %>%
  mutate(value = value * estimate) %>% #create new column where coeffs are multiplied by raw values
  select(-estimate)  # Drop the 'estimate' column

# Pivot the DataFrame back to wide format and remove all unnecessary columns (not used from raw data) | this step could be made better, so we don't have to take all raw variables into calculation
decomp_df <- merged_df %>%
  group_by(Date, term) %>%
  summarise(value = sum(value)) %>%
  spread(term, value) %>%
  select(-which(colSums(is.na(.)) > 0))

#show(decomp_df)


#Media decomping
# getting variables used in the model
model_augmented <- augment(model)

# Select the columns with atan transformations
atan_transformed_table <- model_augmented %>%
  select(matches("atan"))

#adding Date column
merged_atan_table <- bind_cols(atan_transformed_table, dates_file) %>%
  select(-year)

#creating long fromat for transformed data  
long_atan_transformed_table <- merged_atan_table %>%
  gather(term, value, -Date)

# Merge melted import_file_long with mod_coeffs on term
merged_media_df <- long_atan_transformed_table %>%
  left_join(mod_coeffs %>% select(term, estimate), by = "term") %>%
  mutate(value = value * estimate) %>% #create new column where coeffs are multiplied by raw values
  select(-estimate)  # Drop the 'estimate' column

# Pivot the DataFrame back to wide format and remove all unnecessary columns (not used from raw data) | this step could be made better, so we don't have to take all raw variables into calculation
decomp_media_df <- merged_media_df %>%
  group_by(Date, term) %>%
  summarise(value = sum(value)) %>%
  spread(term, value) %>%
  select(-which(colSums(is.na(.)) > 0))


# remove atan label and slope from media variables
decomp_media_df <- decomp_media_df %>%
  rename_all(~ str_replace(., ".*atan\\((.*?)/.*?\\).*", "\\1"))

total_decomp_df <- merge(decomp_df, decomp_media_df, by = "Date")

######################################################33
##-------reference points

# Melt decomp_df to long format
decomp_long_df <- total_decomp_df %>%
  pivot_longer(cols = -Date, names_to = "variable_name", values_to = "value")

# Merge decomp_long with taxonomy on variable_name
merged_decomp_df <- left_join(decomp_long_df, taxonomy, by = "variable_name") %>%
  select(Date, variable_name, value, reference_point)

# Create newvalue column and initialize it with original values
merged_decomp_df$newvalue <- merged_decomp_df$value

# Update values for max reference points
max_condition <- merged_decomp_df$reference_point == "max"
#finding reference points that have "max" label
max_values <- merged_decomp_df[max_condition,] %>%
  group_by(variable_name) %>%
  summarise(max_value = max(value)) %>%
  drop_na() %>%
  ungroup()

#calculating newvalue column, where max is taken off from the value (actual contirbution)
merged_decomp_df <- merged_decomp_df %>%
  left_join(max_values, by = "variable_name") %>%
  group_by(Date) %>%
  mutate(
    referenced_feature = ifelse(reference_point == "max", max_value, NA_real_),
    newvalue = ifelse(reference_point == "max", value - max_value, newvalue)
  ) %>%
  ungroup()

# Update values for min reference points
min_condition <- merged_decomp_df$reference_point == "min"
#finding reference points that have "min" label
min_values <- merged_decomp_df[min_condition,] %>%
  group_by(variable_name) %>%
  summarise(min_value = min(value)) %>%
  ungroup()

#merge max ref with min df
merged_decomp_df <- merged_decomp_df %>%
  left_join(min_values, by = "variable_name") %>%
  mutate(
    referenced_feature = ifelse(min_condition, min_value, referenced_feature),
    newvalue = ifelse(min_condition, value - min_value, newvalue)
  )


# calculate sub_base column (min and max values summed together)
sub_base_df <- merged_decomp_df %>%
  select(Date, referenced_feature) %>%
  drop_na() %>%
  group_by(Date) %>%
  summarise(sub_base = sum(referenced_feature, na.rm = TRUE))

#creating new column with referenced values and non referenced values
merged_decomp_df1 <- merged_decomp_df %>%
  select(Date, variable_name, value, newvalue) %>%
  mutate(final_value = ifelse(is.na(newvalue), value, newvalue)) %>%
  select(Date, variable_name, final_value)

#convert referenced and non referenced values to wide format
wide_decomp <- merged_decomp_df1 %>%
  pivot_wider(names_from = variable_name, values_from = final_value)

wide_decomp_w_subbase <- merge(sub_base_df, wide_decomp, by = "Date")

####################################################################
###----grouping variables into decomp groups
#converting decomped variables into long format
decomp_final_long_df <- wide_decomp_w_subbase %>%
  pivot_longer(cols = -Date, names_to = "variable_name", values_to = "value")

#grouping by decomp group
merged_decomp_final <- merge(decomp_final_long_df, taxonomy, by = "variable_name") %>%
  select(Date, decomp_group, value) %>%
  group_by(Date, decomp_group) %>%
  summarise(final_value = sum(value, na.rm = TRUE))
#converting to wide format
wide_final_decomp <- merged_decomp_final %>%
  pivot_wider(names_from = decomp_group, values_from = final_value)

#######################################################################
#-create stacked decomp chart


# Reverse the order of decomp_group levels
merged_decomp_final$decomp_group <- forcats::fct_rev(merged_decomp_final$decomp_group)

# Create the ggplot with reversed decomp_group
p <- ggplot(merged_decomp_final, aes(x = Date, y = final_value, fill = decomp_group)) +
  geom_area() +
  labs(title = "Decomposition Chart",
       x = "Date",
       y = "Values (000s)") +
  scale_x_date(date_breaks = "4 months", date_labels = "%b %Y") +
  scale_y_continuous(labels = scales::comma_format(scale = 1e-3, big.mark = ",")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

# Print the ggplotly chart
print(ggplotly(p))

###################################################################
#-create decomps table with % of total for full period
#grouping and calculating decomp group contributions on total level, calculating shares
summed_decomps <- merged_decomp_final %>%
  ungroup() %>%
  select(-Date) %>%
  group_by(decomp_group) %>%
  summarise(total_value = sum(final_value, na.rm = TRUE)) %>%
  mutate(percent_share = total_value / sum(total_value) * 100) %>%
  mutate(across(c(total_value, percent_share), ~round(., 1)))

# Create an interactive DataTable with rounded values
datatable(summed_decomps, options = list(dom = 't', pageLength = nrow(summed_decomps))) %>%
  formatStyle(c("total_value", "percent_share"), `border-radius` = '8px')


}

#####################################################################
#plot_line1

library(ggplot2)
library(plotly)

plot_line1 <- function(formula, data) {
  # Apply the formula to create a new variable
  data$custom_var <- eval(formula, envir = data)
  
  data$Date <- as.Date(data$Date, format = "%d-%b-%y")
  
  # Create a ggplot line plot
  gg <- ggplot(data, aes(x = Date)) +
    geom_line(aes(y = custom_var / 1, color = "Custom Variable")) +
    scale_x_date(date_labels = "%b-%y", date_breaks = "4 months") +
    scale_y_continuous(
      name = 'Variable'
    ) +
    labs(title = 'Line Plot of Variable',
         x = 'Weeks',
         color = 'Legend') +
    theme_minimal() +
    theme(legend.position = "top", axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
  
  # Convert ggplot to plotly
  plot <- ggplotly(gg, tooltip = c("Date", "Variable"))
  
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
  plot <- plot_ly(data, x = ~Date, type = "scatter", mode = "lines", y = ~get(var1) / 1000, name = var1, line = list(color = 'blue')) %>%
    add_trace(y = ~get(var2) / 1000, name = var2, line = list(color = 'red'), yaxis = "y2") %>%
    layout(title = "Interactive Line Plot of Two Variables",
           xaxis = list(title = "Date"),
           yaxis = list(title = paste0(var1, " (in thousands)")),
           yaxis2 = list(title = paste0(var2, " (in thousands)"), overlaying = "y", side = "right"))
  
  # Display the plot
  print(plot)
}



# Example usage:
# plot_line("dep_brand_B_12P_330_sb", "m_Press_Total_Spend_eur_adstock0.3", import_file)


#########################################
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


#######################
#Own price elasticity
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


#######################################################
# Create dummy range variables
add_new_variable <- function(data, new_var_name, start_date, end_date) {
  # Convert the 'Date' column to Date type if it's not already
  data$Date <- as.Date(data$Date)
  
  # Create the new variable with all zeros
  data[[new_var_name]] <- 0
  
  # Set value to 1 within the specified date range
  data[data$Date >= as.Date(start_date) & data$Date <= as.Date(end_date), new_var_name] <- 1
  
  return(data)
}


########################################################
# Actual vs. Predicted
library(plotly)

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

# Example usage
# If you want to show only Actual vs. Predicted without the third variable:
# actual_vs_fitted_plot(model1, import_file, "")


##################################################################
# Residuals vs. variable
library(plotly)

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


##############################################################
# Automatic variable selection
auto_variable_selection <- function(model, data, search_term) {
  # Extract column names
  column_names <- colnames(data)
  
  # Create a data frame with column names
  column_names_df <- data.frame(Variable = column_names)
  
  # Put column names into long format
  long_format_column_names <- column_names_df %>%
    pivot_longer(cols = "Variable", names_to = "Variable") %>%
    select(variable = value)
  
  # Filter based on the search term
  filtered_column_names <- long_format_column_names %>%
    filter(str_detect(variable, search_term))
  
  # Get the existing independent variables from the model
  existing_variables <- names(model$model)[-1]
  
  # Get the dependent variable from the model
  dependent_variable <- as.character(formula(model)[[2]])
  
  # Create an empty data frame to store the results
  results_table <- data.frame(term = character(),
                              estimate = numeric(),
                              t.stat = numeric(),
                              p.value = numeric(),
                              stringsAsFactors = FALSE)
  
  # Iterate through each variable in filtered_column_names
  for (variable_name in filtered_column_names$variable) {
    
    # Skip if the variable is already in the model
    if (variable_name %in% existing_variables) {
      next
    }
    
    # Create a formula for the linear regression model with the current variable
    formula <- as.formula(paste(paste(dependent_variable, "~"), paste(existing_variables, collapse = " + "), " + ", variable_name))
    
    # Fit the linear regression model
    current_model <- lm(formula, data = data)
    
    # Extract the tidy results using broom::tidy
    tidy_results <- tidy(current_model)
    
    # Extract the row for the variable of interest
    result_row <- tidy_results[tidy_results$term == variable_name, ]
    
    # Check if the variable was statistically significant
    if (nrow(result_row) > 0) {
      # Store the results in the results_table
      result_row <- data.frame(
        term = result_row$term,
        estimate = result_row$estimate,
        t.stat = result_row$statistic,
        p.value = result_row$p.value
      )
      
      # Append the current result to the results_table
      results_table <- rbind(results_table, result_row)
    }
  }
  
  # Print the DataTable immediately after running the function
  datatable(results_table, class = "compact - hover",
            colnames = c('term', 'estimate', 't.stat', 'p.value'),
            options = list(
              dom = 't',
              scrolly = FALSE,
              pageLength = 500,
              initComplete = JS(
                "function(settings, json) {",
                "$(this.api().table().container()).css({'font-size': '14px'});",
                "}"
              )
            )) %>%
    formatRound(c('estimate', 't.stat', 'p.value'), 2) %>%
    formatStyle('estimate',
                color = styleInterval(c(0),
                                      c("red", "green")))
}



