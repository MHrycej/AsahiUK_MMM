setwd(paste(directory_path, "/sellout_temporary_flatifle"))

list.files(path = paste(directory_path, "", sep = ""))

require(arrow)


brand = read_parquet(file = "uk_sellout_fact_brand.parquet")
sku = read_parquet(file = "uk_sellout_fact_sku.parquet")
model = read_parquet(file = "uk_sellout_fact_model.parquet")



