################################################################################
### Script 4: Clean Partner Imports FROM China
################################################################################
rm(list=ls())
suppressMessages(source("/if/appl/R/Functions/IFfunctions.r"))
library(readr); library(dplyr); library(lubridate); library(this.path)
setwd(dirname(this.path()))

tariffs_hs6 <- read_csv("../../../tariff_codes/master_hs6.csv", show_col_types = FALSE) %>%
  unique() %>% rename(commodity = hs6) %>% mutate(commodity = as.character(commodity))

country_abbrs <- c("ar", "bz", "ca", "co", "cl", "ch", "ge", "hk", "id", "in", 
                   "jn", "ko", "ma", "mx", "ph", "si", "ta", "th", "uk", "us", "vt")

cat("Starting Partner Imports Cleaning...\n")
for (abbr in country_abbrs) {
  if(abbr == "ch") next
  
  input_file <- paste0("../data/raw/imports/monthly/comtrade_imports_from_china_", abbr, ".csv")
  if (!file.exists(input_file)) { cat("Skipping", abbr, "(No file)\n"); next }
  
  cat("Processing:", abbr, "...\n")
  hs6_master <- read_csv(input_file, show_col_types = FALSE, col_types = cols(.default = col_character())) %>%
    rename(commodity = cmdcode, date_str = period, usd = primaryvalue) %>%
    mutate(usd = as.numeric(usd), date = ym(date_str)) %>%
    select(date, commodity, usd)
  
  df_tariff_hs6 <- inner_join(hs6_master, tariffs_hs6, by = "commodity")
  df_no_tariffs_hs6 <- filter(hs6_master, !(commodity %in% df_tariff_hs6$commodity))
  
  #dir.create("../data/cleaned/imports/monthly/", recursive = TRUE, showWarnings = FALSE)
  base_path <- paste0("../data/cleaned/imports/monthly/")
  
  write.csv(hs6_master %>% group_by(date) %>% summarise(total=n(), value=sum(usd, na.rm=T)), 
            paste0(base_path, "total_hs6_monthly_", abbr, "_from_ch.csv"), row.names = FALSE)
  
  write.csv(df_tariff_hs6 %>% group_by(date) %>% summarise(total=n(), value=sum(usd, na.rm=T)), 
            paste0(base_path, "tariffs_hs6_monthly_", abbr, "_from_ch.csv"), row.names = FALSE)
  
  write.csv(df_no_tariffs_hs6 %>% group_by(date) %>% summarise(total=n(), value=sum(usd, na.rm=T)), 
            paste0(base_path, "no_tariffs_hs6_monthly_", abbr, "_from_ch.csv"), row.names = FALSE)
}
cat("Script 4 Complete.\n")