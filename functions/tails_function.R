#Sys.setenv(TZ='GMT')
library(tidyverse)
library(lubridate)
library(magrittr)



#FUNCTION that takes dataframe and adstock as inputs

get.tails <- function(dataf, adstock) {
  # first add rows so we can adstock the last value
  dataf_ext <- dataf %>%
    bind_rows(tibble(date = seq(max(dataf$date) + weeks(1), max(dataf$date) + weeks(30), by = "week"))) %>%
    replace(is.na(.), 0)
  
  # adstock the last value 
  last_as <- dataf_ext %>%
    filter(date >= max(dataf$date)) %>%
    mutate(decomp = stats::filter(decomp, filter = adstock, method = "r")) %>%
    filter(date > min(date))
  
  # filter and bind_rows
  dataf_ext <- dataf_ext %>%
    filter(date <= max(dataf$date)) %>%
    bind_rows(last_as)
  
  df_tails <- dataf_ext %>%   
    select(-decomp) %>%
    mutate(rn = row_number()) %>% # create a column to capture tails called "rn"
    spread(date, raw_grp) %>% # go wide/pivot
    replace(is.na(.), 0) %>% # replace NAs
    select(-rn) %>%
    # Apply adstock across columns
    map_dfc(~stats::filter(.x, filter = adstock, method = "r")) %>%
    # group by the tail week identifier and calculate weekly shares across this
    mutate(date_tail = as.Date(names(.))) %>% # add date column to capture tails (get the names of the df which are the dates)
    gather(date, vol, -date_tail) %>% # make the data long
    group_by(date_tail) %>% # group by the date with the adstock in it (this would be the columns of the matrix)
    mutate(percent = vol / sum(vol), date = as.Date(date)) %>% # calculate shares
    mutate(percent = replace(percent, is.nan(percent), 0)) %>% # fix nan
    # select(-vol) %>%
    # spread(date, percent)
    # join in the original data by date_tail and apportion using shares
    left_join(dataf_ext, by = c("date_tail" = "date")) %>%
    mutate(decomp_f = percent * decomp) %>% # apportion
    select(date_tail, date, decomp_f) %>%
    spread(date, decomp_f) %>%
    # summarise columns 
    ungroup() %>%
    summarise(across(where(is.numeric), sum)) %>%
    gather(date, decomp_tails) %>%
    mutate(date = as.Date(date))
  
  # create final table
  df_tails <- dataf_ext %>%
    left_join(df_tails, by = "date")
  
  sum_check <- sum(df_tails$decomp) ==  sum(df_tails$decomp_tails) # check sums
  
  # this is a check to ensure the original and transformed are the same - others can be added
  # if(isFALSE(sum_check)) {stop("ERROR!!! Decomp sums and decomp tails are different")}
  
  return(df_tails) # return the outcome
}


# #create data for testing
# df <- tibble(date = seq(ymd('2021-01-03'), ymd('2023-12-24'), by = '1 week'),
#              raw_grp = rnorm(156, 100, 10), #raw grp
#              decomp = rnorm(156,1,0.2)*raw_grp %>%
#                stats::filter(filter = 0.6, method= "r"))
# 
# rows_to_add <- 4
# 
# df <- bind_rows(df, tibble(date = seq(max(df$date)+weeks(1),max(df$date)+weeks(rows_to_add), by="week"))) %>%
#   replace(is.na(.), 0)
# 
# #df needs to have "date", "raw_grp" and "decomp" fields
# 
# dataf <- df
# adstock <- 0.5


