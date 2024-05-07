# Automatic variable selection
# Updated: 02/05/2024


auto_variable_selection <- function(model, data, search_term) {
  #model <- multiples_pna00_glass_330ml_4_12pack
  #data <- import_file
  #search_term <- "dummy_month"
  
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
    filter(str_starts(variable, search_term)) 
  
  # Get the existing independent variables from the model
  existing_variables <- names(model$model)[-1]
  
  # Get the dependent variable from the model
  dependent_variable <- as.character(formula(model)[[2]])
  
  # Create an empty data frame to store the results
  results_table <- data.frame(term = character(),
                              estimate = numeric(),
                              t.stat = numeric(),
                              p.value = numeric(),
                              dw.value = numeric(),
                              stringsAsFactors = FALSE)
  
  # Iterate through each variable in filtered_column_names
  for (variable_name in filtered_column_names$variable) {
    #variable_name <- "dummy_month_jan"
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
    
    #calculate Durbin Watson
    DW <- car::durbinWatsonTest(model)$dw
    
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
      
      # Calculate Durbin-Watson
      DW <- durbinWatsonTest(current_model)$dw
      
      # Append Durbin-Watson value to the result_row
      result_row$dw.value <- DW
      
      # Append the current result to the results_table
      results_table <- rbind(results_table, result_row)
    }
  }
  
  # Print the DataTable immediately after running the function
  datatable(results_table, class = "compact - hover",
            colnames = c('term', 'estimate', 't.stat', 'p.value', 'dw.value'),
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
    formatRound(c('estimate', 't.stat', 'p.value', "dw.value"), 2) %>%
    formatStyle('estimate',
                color = styleInterval(c(0),
                                      c("red", "green")))
}
