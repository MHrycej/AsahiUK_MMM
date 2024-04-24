#Process Nielsen data: pull needed skus and metrics we need for modelling

library(writexl)
library(readxl)
library(here)
library(ISOweek)
library(dplyr)
library(tidyr)
library(stringr)

setwd(here())
directory_path <- getwd()


# Read the dates file
dates_file <- read.csv(file.path(directory_path, "dates_lookup.csv"))
dates_file$Date <- as.Date(dates_file$Date, format = "%d-%b-%y")


# read in Nielsen data
source(paste(directory_path, "functions/sellout_data_read_v02.R", sep = "/")) # look into specific file for more details

nielsen = nielsen.creation(
  "C:/Users/MHrycej/OneDrive - ABEG/Martin/Projects/MMM/R GIT/AsahiUK_MMM/nielsen_processing", 
  "all",  # selection of model
  c("ignore"), # BRAND aggregation brand strings to search for
  c("peroni", "moretti", "madri", "estrella", "asd", "san miguel", "heineken", "cruzcampo", "corona", "stella","budweiser"), # SKU aggregation brand strings to limit our SKUs
  c("Peroni Nastro Azzurro Btl 620 Ml single", "Peroni Nastro Azzurro Btl 500 Ml single", "Peroni Nastro Azzurro Btl 330 Ml 4 pack", "Peroni Nastro Azzurro Btl 330 Ml single", "Peroni Nastro Azzurro Btl 330 Ml 10 pack", "Peroni Nastro Azzurro Btl 330 Ml 12 pack", "Peroni Nastro Azzurro Btl 330 Ml 18 pack", "Peroni Nastro Azzurro Btl 330 Ml 24 pack", "Peroni Nastro Azzurro Can 330 Ml 10 pack", "Peroni Nastro Azzurro Can 330 Ml 6 pack", "Peroni Nastro Azzurro Can 330 Ml single", "Peroni Nastro Azzurro 0.0% Btl 330 Ml 4 pack", "Peroni Nastro Azzurro 0.0% Btl 330 Ml 12 pack", "Peroni Nastro Azzurro 0.0% Btl 330 Ml single", "Peroni Nastro Azzurro Can 440 Ml 4 pack", "Peroni Nastro Azzurro Btl 250 Ml 4 pack","Birra Moretti Btl 660 Ml single","Heineken (5%) Btl 650 Ml single", "Stella Artois Btl 660 Ml single","San Miguel Btl 660 Ml single", "Corona Btl 620 Ml single","Madri Exceptional Btl 660 Ml single","Madri Exceptional Btl 660 Ml single", "Budweiser Btl 660 Ml single", "Stella Artois Unfiltered Btl 620 Ml single","Estrella Damm Barcelona Btl 660 Ml single", "Stella Artois Can 568 Ml 4 pack", "San Miguel Can 440 Ml 4 pack","Budweiser Can 440 Ml 4 pack","Corona Btl 330 Ml 4 pack","Madri Exceptional Can 440 Ml 4 pack","San Miguel Can 568 Ml 4 pack","Budweiser Can 568 Ml 4 pack","Birra Moretti Btl 330 Ml 4 pack","Birra Moretti Can 440 Ml 4 pack","Stella Artois Can 440 Ml 4 pack","Corona Cero Btl 330 Ml 4 pack","San Miguel 0.0% Btl 330 Ml 4 pack","Birra Moretti Zero Btl 330 Ml 4 pack","Budweiser Btl 300 Ml 4 pack","San Miguel Btl 330 Ml 4 pack","Corona Can 440 Ml 4 pack","Heineken (5%) Can 440 Ml 4 pack","Stella Artois Alcohol Free 0.0% Btl 330 Ml 4 pack","Peroni Libera 0.0% Btl 330 Ml 4 pack","Heineken 0.0 Alcohol Free Btl 330 Ml 4 pack","Heineken 0.0 Alcohol Free Can 330 Ml 6 pack","Heineken (5%) Can 330 Ml 6 pack","Birra Moretti Can 330 Ml 6 pack","Estrella Damm Barcelona Can 330 Ml 6 pack","Budweiser Btl 300 Ml 6 pack","Stella Artois Btl 330 Ml 6 pack","Corona Can 330 Ml 6 pack","Budweiser Budvar Can 330 Ml 6 pack","San Miguel Can 330 Ml 6 pack","Stella Artois Can 440 Ml 10 pack","San Miguel Can 440 Ml 10 pack","Birra Moretti Can 330 Ml 10 pack","Budweiser Can 440 Ml 10 pack","Cruzcampo Can 440 Ml 10 pack","Madri Exceptional Can 440 Ml 10 pack","Corona Can 330 Ml 10 pack","Peroni Nastro Azzurro Stile Capri Btl 330 Ml 10 pack","Stella Artois Unfiltered Can 440 Ml 10 pack","Corona Btl 330 Ml 12 pack","Birra Moretti Btl 330 Ml 12 pack","San Miguel Btl 330 Ml 12 pack","Madri Exceptional Btl 330 Ml 12 pack","Estrella Damm Barcelona Btl 330 Ml 12 pack","Stella Artois Btl 330 Ml 12 pack","Heineken (5%) Btl 330 Ml 12 pack","Heineken 0.0 Alcohol Free Btl 330 Ml 12 pack","Budweiser Btl 300 Ml 12 pack","Stella Artois Unfiltered Btl 330 Ml 12 pack","Stella Artois Btl 284 Ml 12 pack","Stella Artois Can 440 Ml 18 pack","Corona Btl 330 Ml 18 pack","Budweiser Can 440 Ml 18 pack","Stella Artois Btl 284 Ml 18 pack","Birra Moretti Btl 330 Ml 18 pack","San Miguel Btl 330 Ml 18 pack","Corona Btl 330 Ml 24 pack","Stella Artois Can 568 Ml single","Budweiser Can 440 Ml single","Corona Btl 330 Ml single","Budweiser Can 568 Ml single","Budweiser Budvar Btl 500 Ml single","Peroni Red Btl 330 Ml single","Heineken Silver Btl 650 Ml single","Heineken (5%) Can 568 Ml 4 pack","San Miguel Can 500 Ml 4 pack","Stella Artois Unfiltered Can 440 Ml 4 pack","Estrella Damm Barcelona Btl 330 Ml 4 pack","Madri Exceptional Btl 330 Ml 4 pack","Stella Artois Btl 330 Ml 4 pack","Budweiser Zero Can 330 Ml 4 pack","Stella Artois Unfiltered Can 330 Ml 6 pack","Heineken (5%) Can 440 Ml 10 pack","Corona Btl 330 Ml 10 pack","Budweiser Btl 300 Ml 10 pack","Estrella Damm Barcelona Can 330 Ml 10 pack","Heineken Silver Btl 330 Ml 12 pack","Corona Cero Btl 330 Ml 12 pack","Cruzcampo Btl 330 Ml 12 pack","Budweiser Btl 300 Ml 24 pack","Birra Moretti Btl 330 Ml 24 pack","San Miguel 0.0% Btl 330 Ml 12 pack")                 # SKU aggregation SKU strings to search for
)

# calculating base prices and discounts
source(paste(directory_path, "functions/base_price_v01.R", sep = "/")) # look into specific file for more details

nielsen.bp = base_price(
  nielsen,     # input dataset
  "Price_pl",  # string defining (average) price variable
  0.001, 
  TRUE         # to plot or not to plot
)



# Extracting needed columns from one big table
nielsen_data <- nielsen.bp %>%
  select(matches("Year|Week|_distribution_w|Price_pl|baseprice|mod_Volume_|discount")) %>%
  select(-matches("OTHER")) %>%
  rename_all(
    ~ ifelse(grepl("Peroni Nastro", .), str_replace(., "^sku_", "own_"), .)) %>%   # change peroni sku level data prefix to "own_"
  rename_all(~str_replace(., "^sku_", "c_")) # everything else is competitors "c_"

# Convert 'Week' column to numeric, Format the week number with two digits; Trim the dates range by joining it with dates_file
nielsen_data$Week <- as.numeric(nielsen_data$Week)
nielsen_data$Date <- as.Date(ISOweek::ISOweek2date(paste(nielsen_data$Year, sprintf("W%02d", nielsen_data$Week), "1", sep="-")))-1

nielsen_data <- nielsen_data %>%
  select(-Year, -Week) %>%
  mutate_all(~replace_na(., 0))

nielsen_data <- left_join(dates_file, nielsen_data, by = "Date")








##################################################################
# FOR TAXONOMY

#Create long list of all the Nielsen variables:
# 1. trim sku name in abbreviation column
# 2. map all variable names into decomp groups

sku_names_long <- gather(nielsen_data[-1], key = "Variable") %>%
  distinct(Variable) %>%
  mutate(
    abbreviation = gsub("[ %()\\.-]", "_", tolower(Variable)), #add "_", remove symbols and put to lower case
    abbreviation = case_when(
      grepl("_volume", abbreviation) ~ gsub("_volume", "_vol", abbreviation),
      grepl("_distribution_w", abbreviation) ~ gsub("_distribution_w", "_dist", abbreviation),
      grepl("_baseprice", abbreviation) ~ gsub("_baseprice", "_bp", abbreviation),
      grepl("_price_pl", abbreviation) ~ gsub("_price_pl", "_avp", abbreviation),
      TRUE ~ abbreviation
    ),
    abbreviation = str_replace_all(abbreviation, "__", "_"),  # Remove duplicated underscores
    decomp = case_when(
      grepl("mod_dist", abbreviation) ~ "distribution", #map to decomps
      grepl("c_dist", abbreviation) ~ "comp_distribution", #map to decomps
      grepl("own_dist", abbreviation) ~ "cannibal_distribution", #map to decomps
      grepl("mod_bp_", abbreviation) ~ "price", #map to decomps
      grepl("mod_avp_", abbreviation) ~ "price", #map to decomps
      grepl("c_bp_", abbreviation) ~ "comp_price", #map to decomps
      grepl("c_avp_", abbreviation) ~ "comp_price", #map to decomps
      grepl("own_bp_", abbreviation) ~ "cannibal_price", #map to decomps
      grepl("own_avp_", abbreviation) ~ "cannibal_price", #map to decomps
      grepl("c_discount_", abbreviation) ~ "comp_promo", #map to decomps
      grepl("mod_discount", abbreviation) ~ "promo", #map to decomps
      grepl("own_discount", abbreviation) ~ "cannibal_promo", #map to decomps
      grepl("mod_vol", abbreviation) ~ "kpi", #map to decomps
      TRUE ~ "other"
    )
  )


# create Nielsen taxonomy that could be copy/pasted to main taxonomy file
taxonomy <- sku_names_long %>%
  rename(variable_name = abbreviation) %>%
  mutate(
    classification = case_when( #create classification column so they could be mapped against Martin's original sku names
      grepl("mod_Volume_", Variable) ~ gsub(".*mod_Volume_", "", Variable), #take text after specified prefixes
      grepl("mod_distribution_w_", Variable) ~ gsub(".*mod_distribution_w_", "", Variable),
      grepl("mod_baseprice_", Variable) ~ gsub(".*mod_baseprice_", "", Variable),
      grepl("mod_discount_", Variable) ~ gsub(".*mod_discount_", "", Variable),
      grepl("mod_Price_pl_", Variable) ~ gsub(".*mod_Price_pl_", "", Variable),
      grepl("c_Distribution_w_", Variable) ~ gsub(".*c_Distribution_w_", "", Variable),
      grepl("c_baseprice_", Variable) ~ gsub(".*c_baseprice_", "", Variable),
      grepl("c_discount_", Variable) ~ gsub(".*c_discount_", "", Variable),
      grepl("c_Price_pl_", Variable) ~ gsub(".*c_Price_pl_", "", Variable),
      grepl("own_Distribution_w_", Variable) ~ gsub(".*own_Distribution_w_", "", Variable),
      grepl("own_baseprice_", Variable) ~ gsub(".*own_baseprice_", "", Variable),
      grepl("own_Price_pl_", Variable) ~ gsub(".*own_Price_pl_", "", Variable),
      
      TRUE ~ NA_character_
    ),
    abbreviation1 = gsub("^(.*?_.*?_).*", "\\1", variable_name),
    abbreviation2 = gsub("^.*?_.*?_(.*)", "\\1", variable_name),
    category = case_when(
      grepl("mod_vol_", abbreviation1) ~ "sales",
      grepl("mod_dist_", abbreviation1) ~ "distribution",
      grepl("mod_bp_", abbreviation1) ~ "pricing",
      grepl("mod_avp_", abbreviation1) ~ "pricing",
      grepl("mod_discount_", abbreviation1) ~ "promo",
      grepl("c_dist_", abbreviation1) ~ "competitor - distribution",
      grepl("c_bp_", abbreviation1) ~ "competitor - pricing",
      grepl("c_avp_", abbreviation1) ~ "competitor - pricing",
      grepl("c_discount_", abbreviation1) ~ "competitor - promo",
      grepl("own_dist_", abbreviation1) ~ "cannibalisation - distribution",
      grepl("own_bp_", abbreviation1) ~ "cannibalisation - pricing",
      grepl("own_avp_", abbreviation1) ~ "cannibalisation - pricing",
      grepl("own_discount_", abbreviation1) ~ "cannibalisation - promo",
      TRUE ~ NA_character_
    ),
    metric = NA_character_,  # New empty column "metric"
    abbreviation3 = NA_character_  # New empty column "abbreviation3"
  ) %>%
  select(variable_name, category, abbreviation1, classification, abbreviation2, metric, abbreviation3, decomp_group = decomp, Variable) %>%
  arrange(variable_name)


# map Nielsen sku names to our variable names using created taxonomy table
nielsen_data_mapped <- nielsen_data %>%
  rename_with(~taxonomy$variable_name[match(., taxonomy$Variable)], -1) %>%
  select(Date, sort(names(.)[-1]))


write_xlsx(taxonomy, path = file.path(directory_path, "nielsen_taxonomy.xlsx"))
write_xlsx(nielsen_data_mapped, path = file.path(directory_path, "final_nielsen_data.xlsx"))


############################################################################
#aggregated competitor pricing

nielsen_data_long <- pivot_longer(nielsen_data_mapped,
                                  cols = -Date,
                                  names_to = "variable_name",
                                  values_to = "value")

nielsen_data_long <- nielsen_data_long %>%
  filter(grepl("c_bp_", variable_name))
         
         
nielsen_data_long1 <- nielsen_data_long %>%
  filter(!grepl("zero|cero|alcohol_free|0_0", variable_name))

nielsen_data_long_zero <- nielsen_data_long %>%
  filter(grepl("zero|cero|alcohol_free|0_0", variable_name))

nielsen_data_long2 <- nielsen_data_long1 %>%
  mutate(category = str_extract(variable_name, "^[^_]+_[^_]+_[^_]+"),
         pack_size = ifelse(str_extract(variable_name, "[^_]+$") == "single",
                            "single",
                            str_extract(variable_name, "[^_]+_[^_]+$")),
         btl_can = ifelse(str_extract(variable_name, "[^_]+$") == "single",
                          str_extract(variable_name, "(?<=_)[^_]+(?=_[^_]*_[^_]*_[^_]*$)"),
                          str_extract(variable_name, "(?<=_)[^_]+(?=_[^_]*_[^_]*_[^_]*_[^_]*$)")),
         size = ifelse(str_extract(variable_name, "[^_]+$") == "single",
                       str_extract(variable_name, "(?<=_)[^_]+(?=_[^_]*_[^_]*$)"),
                       str_extract(variable_name, "(?<=_)[^_]+(?=_[^_]*_[^_]*_[^_]*$)")),
         brand = str_extract(variable_name, paste0("(?<=", category, "_).*?(?=_", btl_can, "_)"))) %>%
  select(-variable_name) %>%
  group_by(Date, category, pack_size, btl_can, size) %>%
  summarize(average_value = mean(value[value != 0])) %>%
  mutate(average_value = ifelse(is.nan(average_value), 0, average_value)) %>%
  mutate(variable = paste(category, "total", btl_can, size, pack_size, sep = "_")) %>%
  ungroup() %>%
  select(Date, variable, average_value)

nielsen_data_long_zero2 <- nielsen_data_long_zero %>%
  mutate(category = str_extract(variable_name, "^[^_]+_[^_]+_[^_]+"),
         pack_size = ifelse(str_extract(variable_name, "[^_]+$") == "single",
                            "single",
                            str_extract(variable_name, "[^_]+_[^_]+$")),
         btl_can = ifelse(str_extract(variable_name, "[^_]+$") == "single",
                          str_extract(variable_name, "(?<=_)[^_]+(?=_[^_]*_[^_]*_[^_]*$)"),
                          str_extract(variable_name, "(?<=_)[^_]+(?=_[^_]*_[^_]*_[^_]*_[^_]*$)")),
         size = ifelse(str_extract(variable_name, "[^_]+$") == "single",
                       str_extract(variable_name, "(?<=_)[^_]+(?=_[^_]*_[^_]*$)"),
                       str_extract(variable_name, "(?<=_)[^_]+(?=_[^_]*_[^_]*_[^_]*$)")),
         brand = str_extract(variable_name, paste0("(?<=", category, "_).*?(?=_", btl_can, "_)"))) %>%
  select(-variable_name) %>%
  group_by(Date, category, pack_size, btl_can, size) %>%
  summarize(average_value = mean(value[value != 0])) %>%
  mutate(average_value = ifelse(is.nan(average_value), 0, average_value)) %>%
  mutate(variable = paste(category, "total00", btl_can, size, pack_size, sep = "_")) %>%
  ungroup() %>%
  select(Date, variable, average_value)

nielsen_data_wide <- nielsen_data_long2 %>%
  pivot_wider(names_from = variable, values_from = average_value)

nielsen_data_wide_zero <- nielsen_data_long_zero2 %>%
  pivot_wider(names_from = variable, values_from = average_value)

write_xlsx(nielsen_data_wide, path = file.path(directory_path, "total_competitor_price_data.xlsx"))
write_xlsx(nielsen_data_wide_zero, path = file.path(directory_path, "total_competitor_price_data_zero.xlsx"))


