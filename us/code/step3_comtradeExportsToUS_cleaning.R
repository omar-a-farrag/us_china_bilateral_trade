################################################################################
### Cleaning Partner Exports to US (Aggregates EA Countries)
################################################################################
rm(list = ls())
suppressMessages(source("/if/appl/R/Functions/IFfunctions.r"))
library(readr); library(dplyr); library(tidyr); library(lubridate); library(this.path)
setwd(dirname(this.path()))

tariffs_hs6 <- read_csv("../../../tariff_codes/master_hs6.csv", show_col_types = FALSE) %>%
  unique() %>% rename(commodity = hs6) %>% mutate(commodity = as.character(commodity))

# Added 'ea' and 'ge'
country_abbrs <- c("ar", "bz", "ca", "co", "cl", "ch", "ea", "ge", "hk", "id", "in", 
                   "jn", "ko", "ma", "mx", "ph", "si", "ta", "th", "uk", "vt")

for (abbr in country_abbrs) {
  input_file <- paste0("../data/raw/exports/monthly/comtrade_exports_to_us_", abbr, ".csv")
  if (!file.exists(input_file)) next
  
  cat("Processing:", abbr, "...\n")
  
  # Crucial aggregation added here to fuse EA countries into one
  hs6_master <- read_csv(input_file, show_col_types = FALSE, col_types = cols(.default = col_character())) %>%
    rename(commodity = cmdcode, date_str = period, usd = primaryvalue) %>%
    mutate(usd = as.numeric(usd), date = ym(date_str)) %>%
    group_by(date, commodity) %>% 
    summarise(usd = sum(usd, na.rm = TRUE), .groups = 'drop') 
  
  df_tariff_hs6 <- inner_join(hs6_master, tariffs_hs6, by = "commodity", relationship = "many-to-one")
  df_no_tariffs_hs6 <- filter(hs6_master, !(commodity %in% df_tariff_hs6$commodity))
  
  dir.create("../data/cleaned/exports/monthly/", recursive = TRUE, showWarnings = FALSE)
  base_path <- "../data/cleaned/exports/monthly/"
  
  write.csv(hs6_master %>% group_by(date) %>% summarise(total_count = n(), value = sum(usd, na.rm = TRUE)), 
            paste0(base_path, "total_hs6_monthly_us_", abbr, ".csv"), row.names = FALSE)
  write.csv(df_tariff_hs6 %>% group_by(date) %>% summarise(total_count = n(), value = sum(usd, na.rm = TRUE)), 
            paste0(base_path, "tariffs_hs6_monthly_us_", abbr, ".csv"), row.names = FALSE)
  write.csv(df_no_tariffs_hs6 %>% group_by(date) %>% summarise(total_count = n(), value = sum(usd, na.rm = TRUE)), 
            paste0(base_path, "no_tariffs_hs6_monthly_us_", abbr, ".csv"), row.names = FALSE)
}
cat("Batch processing complete.\n")