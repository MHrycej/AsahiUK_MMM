################################################################
#########----------MODELLING----------#########################
# Modelling main function to get variables' and model's stats
###############################################################
#Updated: 27/02/2024


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