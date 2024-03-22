#Aggregate all individual model results together
#Calculate ROI table
#Updated: 22/03/2024


generate_roi_table <- function() {

directory_path <- getwd()

# # Read the dates file
# dates_file <- read.csv(file.path(directory_path, "dates_lookup.csv"))
# dates_file$Date <- as.Date(dates_file$Date, format = "%d-%b-%y")
# 
# # Read the taxonomy file
# taxonomy <- read_excel(file.path(directory_path, "taxonomy.xlsx"))
# 
# # Read the import file
# import_file <- read.csv(file.path(directory_path, "master_file.csv"))
# import_file$Date <- as.Date(import_file$Date, format = "%d-%b-%y")

# Read the media file
media_file <- read_excel(file.path(directory_path, "/adstocked_media/media_data.xlsx"))
media_file$Date <- as.Date(media_file$Date, format = "%d-%b-%y")

# Read the decomp files
subdirectory <- "decomps"
# Get a list of decomp files in the subdirectory
files <- list.files(path = file.path(directory_path, subdirectory), pattern = "\\.csv$", full.names = TRUE)
all_data <- lapply(files, read.csv)

##################################################


# Stack all the decomp dataframes together
stacked_data <- do.call(rbind, all_data)

# Merge 'stacked_data' with 'dates_file' based on the 'Date' column
stacked_data1 <- merge(stacked_data, dates_file, by = "Date", all.x = TRUE)

#filter just media variables
media_contributions <- stacked_data1 %>%
  filter(startsWith(variable_name, "m_"))

#split variable_name into separate columns
split_data <- media_contributions %>%
  separate(variable_name, c("prefix", "channel", "brand", "campaign", "metric", "adstock"), sep = "_")

# Define the patterns indicating the need for shifting
patterns <- c("sp", "im", "imp", "ad", "circ", "tvr", "vis")

# Use case_when() to conditionally shift data to the right by one column to align data with columns
final_media_contributions <- split_data %>%
  mutate(
    adstock = ifelse(campaign %in% patterns, metric, adstock),
    metric = ifelse(campaign %in% patterns, campaign, metric),
    campaign = ifelse(campaign %in% patterns, NA_character_, campaign),
  ) %>%
  select(-prefix, -adstock, -Week)
final_media_contributions$Date <- as.Date(final_media_contributions$Date)

#######################################

#create long media table
media_long <- gather(media_file, variable_name, spend, -Date)

#filter just spend variables
media_spend_long <- media_long %>%
  filter(endsWith(variable_name, "_sp"))

# Merge 'stacked_data' with 'dates_file' based on the 'Date' column
media_spend_long1 <- merge(media_spend_long, taxonomy, by = "variable_name", all.x = TRUE)
media_spend_long2 <- media_spend_long1 %>%
  select(variable_name, Date, spend)

#split variable name
split_data_spends <- media_spend_long2 %>%
  separate(variable_name, c("prefix", "channel", "brand", "campaign", "metric", "adstock"), sep = "_")

# Use case_when() to conditionally shift data to the right by one column
final_media_spends <- split_data_spends %>%
  mutate(
    adstock = ifelse(campaign %in% patterns, metric, adstock),
    metric = ifelse(campaign %in% patterns, campaign, metric),
    campaign = ifelse(campaign %in% patterns, NA_character_, campaign),
  ) %>%
  select(-prefix, -adstock, -metric)

######################################
#take pricing variables to calculate values
prices <- import_file %>%
  gather(model_name, price, -Date) %>%
  filter(startsWith(model_name, "mod_avp")) %>%
  mutate(model_name = gsub("^mod_avp_", "", model_name))

#####################################
#join contirbutions and spends together

final_data <- left_join(final_media_contributions, final_media_spends,
                         by = c("Date", "channel", "brand", "campaign"))
final_data <- final_data %>%
  select(model_name, brand, channel, campaign, decomp_group, Year, Date, volume = value, spend)

#join prices together with volume and spend table and calculate value
final_data_w_values <- left_join(final_data, prices,
                                 by = c("Date", "model_name")) %>%
  mutate(value = volume * price) %>%
  select(-volume, -price)


#####################################
#calculate ROIs

aggregated_data <- final_data_w_values %>%
  select(model_name, decomp_group, Year, value, spend) %>%
  group_by(model_name, decomp_group, Year) %>%
  summarize(total_value = sum(value, na.rm = TRUE),
            total_spend = sum(spend, na.rm = TRUE),
            roi = total_value / total_spend) %>%
  mutate(roi = ifelse(is.nan(roi) | is.infinite(roi), 0, roi))

#calculate total ROIs, sum of all the models values
aggregated_data_tot <- aggregated_data %>%
  group_by(decomp_group, Year) %>%
  summarize(total_value = sum(total_value),
            total_spend = unique(total_spend),
            roi = total_value / total_spend) %>%
  mutate(roi = ifelse(is.nan(roi) | is.infinite(roi), 0, roi)) %>%
  ungroup() %>%
  mutate(model_name = "total")

roi_table <- bind_rows(aggregated_data, aggregated_data_tot)

# Display roi_table in the R viewer with enhanced interactivity
print(datatable(roi_table, 
          options = list(dom = 't', pageLength = nrow(roi_table)),
          extensions = 'Buttons', 
          filter = 'top', 
          style = 'default',
          class = 'compact',
          rownames = FALSE) %>%
  formatRound(c(4, 5), digits = 0) %>%
  formatRound(c(6), digits = 2) %>%
  formatStyle(
    columns = names(roi_table),
    border = '1px solid #CCCCCC'
  ))


}
