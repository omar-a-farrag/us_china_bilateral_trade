################################################################################
### Cleaning & Tariff Split for Mexico Trade Data (HS 6-digit)
################################################################################
rm(list=ls())
suppressMessages(source("/if/appl/R/Functions/IFfunctions.r"))
library(readr); library(dplyr); library(lubridate); library(this.path)
setwd(dirname(this.path()))

# 1. Load Tariff Master List
tariffs_hs6 <- read_csv("../../../tariff_codes/master_hs6.csv", show_col_types = FALSE) %>%
  unique() %>% rename(commodity = hs6) %>% mutate(commodity = as.character(commodity))

# 2. Target Countries
country_abbrs <- c("ch", "id", "ma", "ph", "th", "vt")

# Helper function to clean a single file
clean_comtrade_file <- function(input_path, output_dir, file_prefix) {
  if (!file.exists(input_path)) return()
  
  df_raw <- read_csv(input_path, show_col_types = FALSE, col_types = cols(.default = col_character())) %>%
    rename(commodity = cmdcode, date_str = period, usd = primaryvalue) %>%
    mutate(usd = as.numeric(usd), date = ym(date_str)) %>%
    select(date, commodity, usd)
  
  # Tariff Splits
  df_tariff <- inner_join(df_raw, tariffs_hs6, by = "commodity", relationship = "many-to-one")
  df_notariff <- filter(df_raw, !(commodity %in% df_tariff$commodity))
  
  # Export Aggregates
  write.csv(df_raw %>% group_by(date) %>% summarise(value = sum(usd, na.rm=T)), 
            paste0(output_dir, "total_hs6_", file_prefix, ".csv"), row.names = FALSE)
  write.csv(df_tariff %>% group_by(date) %>% summarise(value = sum(usd, na.rm=T)), 
            paste0(output_dir, "tariffs_hs6_", file_prefix, ".csv"), row.names = FALSE)
  write.csv(df_notariff %>% group_by(date) %>% summarise(value = sum(usd, na.rm=T)), 
            paste0(output_dir, "no_tariffs_hs6_", file_prefix, ".csv"), row.names = FALSE)
}

# 3. Execution Loop
dir.create("../data/cleaned/mexico/imports/", recursive = TRUE, showWarnings = FALSE)
dir.create("../data/cleaned/mexico/exports/", recursive = TRUE, showWarnings = FALSE)

cat("Starting Mexico Data Cleaning...\n")

for (abbr in country_abbrs) {
  cat("Processing:", abbr, "...\n")
  
  # Clean Mexico Imports
  in_imp <- paste0("../data/raw/mexico/imports/monthly/mx_imports_from_", abbr, ".csv")
  out_imp_dir <- "../data/cleaned/mexico/imports/"
  clean_comtrade_file(in_imp, out_imp_dir, paste0("mx_imp_from_", abbr))
  
  # Clean Partner Exports
  in_exp <- paste0("../data/raw/mexico/exports/monthly/partner_exports_to_mx_", abbr, ".csv")
  out_exp_dir <- "../data/cleaned/mexico/exports/"
  clean_comtrade_file(in_exp, out_exp_dir, paste0("partner_exp_to_mx_", abbr))
}

cat("Mexico Cleaning Complete.\n")