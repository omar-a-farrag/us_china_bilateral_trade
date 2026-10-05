################################################################################
### EA Data Cleaning: Aggregating 20 Countries -> 1 EA Region
################################################################################
rm(list=ls())
suppressMessages(source("/if/appl/R/Functions/IFfunctions.r"))
library(readr); library(dplyr); library(lubridate); library(this.path)
setwd(dirname(this.path()))

tariffs_hs6 <- read_csv("../../../tariff_codes/master_hs6.csv", show_col_types = FALSE) %>%
  unique() %>% rename(commodity = hs6) %>% mutate(commodity = as.character(commodity))

partners <- c("ch", "id", "ma", "ph", "th", "vt")

clean_and_aggregate <- function(input_path, output_dir, prefix) {
  if (!file.exists(input_path)) return()
  
  # Restored to the simple, clean aggregation mirroring your US Exports script
  df_agg <- read_csv(input_path, show_col_types = FALSE, col_types = cols(.default = col_character())) %>%
    rename(commodity = cmdcode, date_str = period, usd = primaryvalue) %>%
    mutate(usd = as.numeric(usd), date = ym(date_str)) %>%
    group_by(date, commodity) %>%
    summarise(usd = sum(usd, na.rm=TRUE), .groups = 'drop') 
  
  # Tariff Split
  df_tariff <- inner_join(df_agg, tariffs_hs6, by = "commodity")
  df_notariff <- filter(df_agg, !(commodity %in% df_tariff$commodity))
  
  # Save Aggregates
  write.csv(df_agg %>% group_by(date) %>% summarise(value=sum(usd)), 
            paste0(output_dir, "total_hs6_", prefix, ".csv"), row.names=F)
  write.csv(df_tariff %>% group_by(date) %>% summarise(value=sum(usd)), 
            paste0(output_dir, "tariffs_hs6_", prefix, ".csv"), row.names=F)
  write.csv(df_notariff %>% group_by(date) %>% summarise(value=sum(usd)), 
            paste0(output_dir, "no_tariffs_hs6_", prefix, ".csv"), row.names=F)
}

dir.create("../data/cleaned/imports/monthly", recursive=T, showWarnings=F)
dir.create("../data/cleaned/exports/monthly", recursive=T, showWarnings=F)

cat("Starting EA Cleaning...\n")
for (p in partners) {
  clean_and_aggregate(paste0("../data/raw/imports/monthly/ea_imports_from_", p, ".csv"),
                      "../data/cleaned/imports/monthly/", paste0("ea_imp_from_", p))
  clean_and_aggregate(paste0("../data/raw/exports/monthly/partner_exports_to_ea_", p, ".csv"),
                      "../data/cleaned/exports/monthly/", paste0("partner_exp_to_ea_", p))
}
cat("EA Cleaning Complete.\n")
