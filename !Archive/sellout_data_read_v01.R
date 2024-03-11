#### - NIELSEN TABLE CREATION FUNCTION DEFINITION                              ####
#### 0 SETUP & FUNCTION INPUT DEFINITION                                        ####

# nielsen is created on our parquet files coming from AZURE SYNAPSE as of now
#
# 1 MODEL FILE containing our defined (18) data groups we are interested in
# 2 BRAND FILE containing brand aggregation of data
# 3 SKU FILE containing SKU level aggregation of data
#
# This function requires 4 specific inputs:
# 1 specific definition of our modelled group
# 2 selection of strings to define competing brands to be included within BRAND data appendices
#   - case insensitive, script takes care of duplicities
#   - sub-string based search through all brand names (i.e. 'pils' filters out both 'Pilsner U.' & 'wtf horrendous Pils')
# 3 selection of strings to define competing brands to be included within SKU data appendices
#   - works the same as above, just as a pre-filtering for our SKU selection
# 4 selection of strings to define competing SKUs to be included within SKU data-appendices
#   - works similarly as above, just this time we are filtering through BRAND-prefiltered SKU dataset from (3)
#   - ....and we are filtering solely on SKU basis :)
#

nielsen.creation = function(
    # SO PLEASE SET UP THE FOLLOWING PARAMETERS:
    # 0 PATH
    folder.path, 
    # 1 DEFINE MODEL
    def.model, # = "PNA_MULTIPLES_GLASS_330ML_10PACK"
    # CHOOSE FROM THE FOLLOWING:
    # "PNA_ONTRADE_DRAUGHT_PINT"            "PNA_ONTRADE_GLASS_330ML_1PACK"      "unspecified yet"                     "PNA00_IMPULSE_GLASS_330ML_1-12PACK"   
    # "PNA00_MULTIPLES_GLASS_330ML_4PACK"   "PNA_IMPULSE_CAN_330ML_1-10PACK"     "PNA_IMPULSE_GLASS_330ML_1-4PACK"     "PNA_IMPULSE_GLASS_330ML_10-12PACK"    
    # "PNA_IMPULSE_GLASS_330ML_18-24PACK"   "PNA_IMPULSE_GLASS_500-620ML_1PACK"  "PNA_MULTIPLES_CAN_330ML_6-10PACK"    "PNA_MULTIPLES_GLASS_250ML_4PACK"    
    # "PNA_MULTIPLES_GLASS_330ML_10PACK"    "PNA_MULTIPLES_GLASS_330ML_12PACK"   "PNA_MULTIPLES_GLASS_330ML_18-24PACK" "PNA_MULTIPLES_GLASS_330ML_4PACK"    
    # "PNA_MULTIPLES_GLASS_500ML_1PACK"     "PNA_MULTIPLES_GLASS_620ML_1PACK"    "PNA_IMPULSE_CAN_440ML_4PACK"         "PNA_MULTIPLES_CAN_440ML_4PACK"
    #
    # 2 SELECT BRANDS FOR BRAND LEVEL
    def.brand.list, # = c("heineken", "carlsberg", "grolsch")
    #
    # 3 SELECT BRANDS FOR SKU LEVEL  
    def.sku.brand.list, # = c("heineken", "carlsberg", "grolsch")
    #
    # 4 SELECT SKU SUB-STRING FOR SKU LEVEL
    def.sku.list # = c("Btl 330 Ml 4 pack")
    # 
    # Please adjust paths within the 1 READING PARQUET... part of the script !!!!
){
  
  #### - LOADING PACKAGES                                                         ####
  
  require(arrow)
  require(dplyr)
  require(ggplot2)
  
  #### 1 READING PARQUET DATA FILES FROM AZURE                                    ####
  # BRAND
  setwd(paste(folder.path, "uk_sellout_fact_brand.parquet", sep = ""))
  for(i in 2:length(list.files())){
    sub = read_parquet(list.files()[i])
    if(i == 2){
      brand = sub
    }else{
      brand = rbind(brand, sub)
    }
    print(i)
  }
  
  # SKU
  setwd(paste(folder.path, "uk_sellout_fact_sku.parquet", sep = ""))
  for(i in 2:length(list.files())){
    sub = read_parquet(list.files()[i])
    if(i == 2){
      sku = sub
    }else{
      sku = rbind(sku, sub)
    }
    print(i)
  }
  
  # MODEL
  setwd(paste(folder.path, "uk_sellout_fact_model.parquet", sep = ""))
  for(i in 2:length(list.files())){
    sub = read_parquet(list.files()[i])
    if(i == 2){
      model = sub
    }else{
      model = rbind(model, sub)
    }
    print(i)
  }
  
  #### 2 SUMMARY CHECKS                                                           ####
  
  # sum(sku$Value, na.rm = T)
  # sum(brand$Value, na.rm = T)
  # sum(model$Value, na.rm = T)
  # 
  # colnames(model)
  # colnames(sku)
  # colnames(brand)
  # 
  # for(i in 1:length(unique(model$model_agg))){
  #   print(
  #     paste(
  #       unique(model$model_agg)[i], 
  #       " - ", 
  #       nrow(model[model$model_agg == unique(model$model_agg)[i], ]), 
  #       " rows", 
  #       sep = ""
  #     )
  #   )
  # }
  # unique(model$model_agg)
  
  #### 3 Function to define and merge our dataset                                 ####
# first we define function to replace our colnames; it has three parameters:
# original column of names to be adjusted
# prefix to be used
# specific columns not to be adjusted (e.g. group by colums such as period/week...)

replace.colnames = function(colnames.in, prefix, columns.to.not.replace){
  col.out = rep(NA, length(colnames.in))
  if(length(columns.to.not.replace) > 0){
    col.out[match(columns.to.not.replace, colnames.in)] = columns.to.not.replace
  }
  
  other.cols = paste(rep(prefix, length(colnames.in)), colnames.in, sep = "")
  other.cols.to.replace = (!(colnames.in %in% columns.to.not.replace)) * c(1:length(colnames.in))
  other.cols.to.replace = other.cols.to.replace[other.cols.to.replace != 0]
  
  col.out[other.cols.to.replace] = other.cols[other.cols.to.replace]
  
  return(col.out)
}


  #### 4 Definition of our a) MODEL, b) BRANDs, c) SKUs                           ####
  #### 4.a selection of model is straightforward                                  ####
  
  # def.model = unique(model$model_agg)[5] # !!! PLEASE DEFINE THIS FIELD - MODEL GROUP SELECTION !!!
  
  nielsen.dev = model[model$model_agg == def.model, ]
  
  colnames(nielsen.dev) = replace.colnames(
    colnames(nielsen.dev), 
    "dep_", # !!! PREFIX CAN BE ADJUSTED HERE !!!
    c("_Key_Period", "market_agg", "Year", "Month", "Week")
  )
  
  # we also use market_agg to correctly filter other datasets
  current.market_agg = nielsen.dev$market_agg[1]
  brand = brand[brand$market_agg == current.market_agg, ]
  sku = sku[sku$market_agg == current.market_agg, ]
  
  # for brand & sku data frames we do not need some duplicate columns, which will be anyway obtained within core model df
  brand = brand[, -match(c("Channel", "Category", "Year", "Month", "Week"), colnames(brand))]
  sku = sku[, -match(c("Channel", "Category", "Year", "Month", "Week"), colnames(sku))]
  
  #### 4.b for brand we define list of strings to look for within BRAND column    ####
  #   then we take uniques and we loop through those & append resulting data tables
  # def.brand.list = c("heineken", "carlsberg", "grolsch") # !!! PLEASE DEFINE THIS FIELD - FULL BRAND AGGREGATION LIST (case insensitive) !!!
  
  for(comp.brand.i in 1:length(def.brand.list)){
    brand.str = def.brand.list[comp.brand.i]
    brand.list = unique(brand$Brand)
    brand.list = brand.list[grep(brand.str, brand.list, ignore.case = TRUE)]
    brand.list = unique(brand.list)
    
    if(comp.brand.i == 1){
      brand.list.out = c(NA)
    }
    brand.list.out = c(brand.list.out, brand.list)
    
    remove(brand.str, brand.list)
    brand.list.out = unique(brand.list.out)
  }
  
  brand.list.out = brand.list.out[is.na(brand.list.out) == F]
  # Now we have full list of unique actual BRAND names to use, so we will go through them one by one
  
  if(length(brand.list.out) > 0){
    for(bb in 1:length(brand.list.out)){
      current.brand = brand.list.out[bb]
      sub = brand[brand$Brand == current.brand, ]
      
      colnames(sub) = replace.colnames(
        colnames(sub), 
        paste("c_brand_", current.brand, "_", sep = ""), 
        c("market_agg", "_Key_Period")
      )
      
      nielsen.dev = merge(x = nielsen.dev, y = sub, by.x = c("market_agg", "_Key_Period"), by.y = c("market_agg", "_Key_Period"), 
                       all.x = T, all.y = F)
      
      remove(current.brand, sub)
    }
    remove(bb)
  }
  
  #### 4.c For SKU we define list of strings to look for within SKU column        ####
  #   first we have to define our main Brands and filter the data as in the case with Brands above
  #   then we take uniques and we loop through those & append resulting data tables
  # def.sku.list = c("Btl 330 Ml 4 pack") # !!! PLEASE DEFINE THIS FIELD - FULL SKU STRING LIST (case insensitive) !!!
  # def.sku.brand.list = c("heineken", "carlsberg", "grolsch") # !!! PLEASE DEFINE THIS FIELD - FULL SKU STRING LIST (case insensitive) !!!
  
  # First we find list of actual brands to filter our sku dataset
  for(comp.brand.i in 1:length(def.sku.brand.list)){
    brand.str = def.sku.brand.list[comp.brand.i]
    brand.list = unique(sku$Brand)
    brand.list = brand.list[grep(brand.str, brand.list, ignore.case = TRUE)]
    brand.list = unique(brand.list)
    
    if(comp.brand.i == 1){
      sku.brand.list.out = c(NA)
    }
    sku.brand.list.out = c(sku.brand.list.out, brand.list)
    
    remove(brand.str, brand.list)
    sku.brand.list.out = unique(sku.brand.list.out)
  }
  
  sku.brand.list.out = sku.brand.list.out[is.na(sku.brand.list.out) == F]
  
  # now we filter sku dataset by possible brands
  sku = sku[sku$Brand %in% sku.brand.list.out, ]
  
  for(comp.sku.i in 1:length(def.sku.list)){
    sku.str = def.sku.list[comp.sku.i]
    sku.list = unique(sku$SKU)
    sku.list = sku.list[grep(sku.str, sku.list, ignore.case = TRUE)]
    sku.list = unique(sku.list)
    
    if(comp.sku.i == 1){
      sku.list.out = c(NA)
    }
    sku.list.out = c(sku.list.out, sku.list)
    
    remove(sku.str, sku.list)
    sku.list.out = unique(sku.list.out)
  }
  
  sku.list.out = sku.list.out[is.na(sku.list.out) == F]
  # Now we have full list of unique actual sku names to use, so we will go through them one by one
  
  if(length(sku.list.out) > 0){
    for(bb in 1:length(sku.list.out)){
      current.sku = sku.list.out[bb]
      sub = sku[sku$SKU == current.sku, ]
      
      colnames(sub) = replace.colnames(
        colnames(sub), 
        paste("c_sku_", current.sku, "_", sep = ""), 
        c("market_agg", "_Key_Period")
      )
      
      nielsen.dev = merge(x = nielsen.dev, y = sub, by.x = c("market_agg", "_Key_Period"), by.y = c("market_agg", "_Key_Period"), 
                           all.x = T, all.y = F)
      
      remove(current.sku, sub)
    }
    remove(bb)
  }
  
  #### 5 Now we have (not so)wide nielsen table, containing all vars; cleanup    ####
  
  print(colnames(nielsen.dev))
  remove(brand, sku, model, 
         brand.list.out, sku.list.out, sku.brand.list.out, 
         comp.brand.i, comp.sku.i, 
         current.market_agg, 
         def.brand.list, def.sku.list, def.sku.brand.list, 
         i, def.model, replace.colnames)
  
  return(nielsen.dev)
  
  #### 6 END WITHIN FUNCTION                                                      ####
  
}

#### END OF FUNCTION DEFINITION                                                   ####

# function usage example:

# nielsen = nielsen.creation(
#   "PNA_MULTIPLES_GLASS_330ML_10PACK", 
#   c("grolsch", "carlsberg", "peroni"), 
#   c("grolsch", "carlsberg", "peroni"), 
#   "Btl 330 Ml 10 Pack"
# )


