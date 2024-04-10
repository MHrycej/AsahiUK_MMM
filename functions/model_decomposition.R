#####-----Model decomposition-----###
#Update date: 25/03/2024

library(janitor)
library(scales)
library(DT)
library(broom)
library(forcats)


#1. Get model coefficients and raw variables used in the model for non-media variables: multiply
#2. Get model coefficients and raw variables for media used in the model: multiply
#3. Calculate reference points and add into sub-base if there are any specified in taxonomy
#4. Group variables into decomp groups based on taxonomy mapping
#5. Generate charts and tables
#6. Calculate tails
#7. Create final decomp table to export

model_decomp <- function(model) {
  #######################################################
  #1. calculate non-media variable contributions
  
  #model <- multiples_pna_glass_330ml_12pack
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
  
  ###################################################################
  #2. Media decomping
  
  # getting variables used in the model
  model_augmented <- augment(model)
  
  # Select the columns with atan transformations
  atan_transformed_table <- model_augmented %>%
    select(matches("atan"))
  
  # Check if atan_transformed_table is empty
  if (ncol(atan_transformed_table) == 0) {
    # If empty, proceed with decomp_df directly
    total_decomp_df <- decomp_df
  } else {
    # If not empty, proceed with merging decomp_df and decomp_media_df
    # adding Date column
    merged_atan_table <- bind_cols(atan_transformed_table, dates_file) %>%
      select(-Year, -Week)
    
    # creating long format for transformed data  
    long_atan_transformed_table <- merged_atan_table %>%
      gather(term, value, -Date)
    
    # Merge melted import_file_long with mod_coeffs on term
    merged_media_df <- long_atan_transformed_table %>%
      left_join(mod_coeffs %>% select(term, estimate), by = "term") %>%
      mutate(value = value * estimate) %>% # create new column where coeffs are multiplied by raw values
      select(-estimate)  # Drop the 'estimate' column
    
    # Pivot the DataFrame back to wide format and remove all unnecessary columns
    decomp_media_df <- merged_media_df %>%
      group_by(Date, term) %>%
      summarise(value = sum(value)) %>%
      spread(term, value) %>%
      select(-which(colSums(is.na(.)) > 0))
    
    # remove atan label and slope from media variables
    decomp_media_df <- decomp_media_df %>%
      rename_all(~ str_replace(., ".*atan\\((.*?)/.*?\\).*", "\\1"))
    
    # Merge decomp_df and decomp_media_df
    total_decomp_df <- merge(decomp_df, decomp_media_df, by = "Date")
  }
  
  #######################################################
  #3. Reference points
  
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
  
  if(nrow(sub_base_df) > 0) {
    wide_decomp_w_subbase <- merge(sub_base_df, wide_decomp, by = "Date")
  } else {
    wide_decomp_w_subbase <- wide_decomp
  }
  
  ####################################################################
  #4. grouping variables into decomp groups

  
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
  #5. create stacked decomp chart
  
  # Reverse the order of decomp_group levels
  #merged_decomp_final$decomp_group <- forcats::fct_rev(merged_decomp_final$decomp_group)
  
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
  #5. create decomps table with % of total for full period
  
  #grouping and calculating decomp group contributions on total level, calculating shares
  summed_decomps <- merged_decomp_final %>%
    ungroup() %>%
    select(-Date) %>%
    group_by(decomp_group) %>%
    summarise(total_value = sum(final_value, na.rm = TRUE)) %>%
    mutate(percent_share = total_value / sum(total_value) * 100) %>%
    mutate(across(c(total_value, percent_share), ~round(., 1)))
  
  # Create an interactive DataTable with rounded values
  print(
    datatable(summed_decomps, options = list(dom = 't', pageLength = nrow(summed_decomps))) %>%
      formatStyle(c("total_value", "percent_share"), `border-radius` = '8px')
  )
  
  
  ################################################################
  #6. Calculate Tails
  
  #For tails we need:
  #1. raw media variables used in the model
  #2. media weekly decomps
  #3. adstocks for each media variable
  #4. create input table for tails function
  #5. tails calculation function
  
  if (exists("decomp_media_df")) {
    # Your existing code for working with decomp_media_df
    
    #6.1. Get raw media variables used in the model
    raw_media_colnames <- names(decomp_media_df) %>%
      # Remove "_adstock" label if present in the column names
      str_replace("_adstock.*$", "") %>%
      # If no "_adstock" label, keep the name the same
      ifelse(!grepl("_adstock", names(decomp_media_df)), names(decomp_media_df), .)
    
    raw_media_vars <- import_file %>%
      select(all_of(raw_media_colnames))
    
    
    #6.2. Media weekly decomps already calculated above in decomp_media_df but need to change var names to be consistent with raw variables
    media_decomp_for_tails <- decomp_media_df %>%
      rename_with(~ if_else(grepl("_adstock", .), str_replace(., "_adstock.*$", ""), .), .cols = everything())
    
    
    #6.3. Get adstocks for each media variable
    # Extract adstock values from column names of decomp_media_df
    adstock_values <- sub(".*_adstock(\\d+)$", "\\1", names(decomp_media_df)[-1])
    adstock_values[!grepl("_adstock", names(decomp_media_df)[-1])] <- 0
    
    # Extract column names from media_decomp_for_tails, skipping the Date column
    adstock_colnames <- names(media_decomp_for_tails)[-1]
    
    # Convert vectors to data frames
    adstock_colnames_df <- data.frame(adstock_colnames, stringsAsFactors = FALSE)
    adstock_values_df <- data.frame(adstock_values, stringsAsFactors = FALSE)
    
    # Combine column names and values column-wise
    adstocks <- cbind(adstock_colnames_df, adstock_values_df)
    adstocks$adstock_values <- as.numeric(adstocks$adstock_values)
    adstocks <- adstocks %>%
      mutate(adstock_values = adstock_values/100)
    
    adstocks <- pivot_wider(adstocks, 
                            names_from = adstock_colnames,
                            values_from = adstock_values)
    
    
    #6.4. Use all input tables in the tails function (run tails function for each media variable)
    
    # Initialize an empty results table
    decomp_tails <- data.frame(date = as.Date(character()))
    
    #specify column names that should be run through function
    media_decomp_cols <- names(media_decomp_for_tails)[-1] # Exclude the Date column
    raw_media_cols <- names(raw_media_vars)[-1] # Exclude the Date column
    
    # Iterate over the columns of media_decomp_for_tails and raw_media_vars
    for (i in seq_along(media_decomp_cols)) {
      # Extract the column name
      #i=4
      col_name <- media_decomp_cols[i]
      
      # Extract the corresponding columns from media_decomp_for_tails and raw_media_vars
      media_decomp_col <- media_decomp_for_tails[c('Date', col_name)]
      raw_media_col <- raw_media_vars[c('Date', raw_media_cols[i])]
      adstock <- adstocks[[i]]
      
      # Rename columns
      names(media_decomp_col)[2] <- 'decomp'
      names(raw_media_col)[2] <- 'raw_grp'
      
      # Merge the columns
      dataf <- merge(raw_media_col, media_decomp_col, by = 'Date')
      names(dataf)[1] <- 'date'
      
      # Call out the function
      tails_output <- get.tails(dataf, adstock)
      
      # Rename columns back to original names
      names(tails_output)[names(tails_output) == 'decomp_tails'] <- col_name
      
      # Store the result of the current iteration in the results table
      decomp_tails <- merge(decomp_tails, tails_output[c('date', col_name)], by = 'date', all = TRUE)
    }
    
    # Replace column names in results with the column names from media_decomp_for_tails
    colnames(decomp_tails)[-1] <- media_decomp_cols
    colnames(decomp_tails)[-1] <- colnames(decomp_media_df)[-1]
    
    decomp_tails_long <- decomp_tails %>%
      pivot_longer(cols = -date,
                   names_to = "variable_name",
                   values_to = "value_tails") %>%
      rename(Date = date)  # Rename date column to Date
    
    #################################################################
    #7. Create final decomp table
    
    model_name <- deparse(substitute(model))
    
    final_decomp_export <- merge(decomp_final_long_df, taxonomy, by = "variable_name") %>%
      left_join(decomp_tails_long, by = c("Date", "variable_name")) %>%
      mutate(value_tails = coalesce(value_tails, value)) %>%
      select(Date, variable_name, decomp_group, value, value_tails) %>%
      mutate(model_name = model_name) %>%
      select(model_name, variable_name, decomp_group, Date, value, value_tails)
  }
  
}
