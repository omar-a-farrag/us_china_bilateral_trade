################################################################################
### Cleaning HS 10-digit codes of US-reported Imports
## Omar Farrag, IF Division -- EME
################################################################################

rm(list = ls())
suppressMessages(source("/if/appl/R/Functions/IFfunctions.r"))

library(readxl)
library(dplyr)
library(zoo)
library(policyPlot)
library(tidyverse)
library(ggplot2)
library(pracma)
library(xts)
library(sjmisc)
library(grid)
library(colorblindcheck)
library(gplots) 
library(tidyr)
require(this.path)

setwd(dirname(this.path()))

# ==============================================================================
# 1. STATIC SETUP (Run once)
# ==============================================================================
cat("Loading and cleaning Tariff Master lists...\n")

tariffs_hs6_raw <- read_csv("../../../tariff_codes/master_hs6.csv", show_col_types = FALSE)
tariffs_hs8_raw <- read_csv("../../../tariff_codes/master_hs8.csv", show_col_types = FALSE)

tariffs_hs6 <- unique(tariffs_hs6_raw) %>%
  rename(commodity = hs6) %>%
  mutate(commodity = as.character(commodity)) 
sum_dup_6 <- sum(duplicated(tariffs_hs6))

tariffs_hs8 <- unique(tariffs_hs8_raw) %>%
  rename(commodity = hs8) %>%
  mutate(commodity = as.character(commodity))
sum_dup_8 <- sum(duplicated(tariffs_hs8))

country_abbrs <- c("ar", "bz", "ca", "co", "cl", "ch", "ea", "ge", "hk", "id", "in", 
                   "jn", "ko", "ma", "mx", "ph", "si", "ta", "th", "uk", "vt")
# ==============================================================================
# 2. THE LOOP
# ==============================================================================
cat("--------------------------------------------------\n")
cat("Starting batch processing for", length(country_abbrs), "countries.\n")

for (abbr in country_abbrs) {
  
  input_file <- paste0("../data/raw/imports/monthly/us_rep_hs10_values_", abbr, ".csv")
  
  if (!file.exists(input_file)) {
    cat("SKIPPING:", abbr, "(File not found: ", input_file, ")\n")
    next
  }
  
  cat(format(Sys.time(), "%H:%M:%S"), "- Processing:", abbr, "...\n")
  
  hs10_master <- read_csv(input_file, show_col_types = FALSE, 
                          col_types = cols(commodity = col_character()))
  
  hs10_master_6 <- hs10_master %>% mutate(commodity = substring(commodity, 1, 6))
  hs10_master_8 <- hs10_master %>% mutate(commodity = substring(commodity, 1, 8))
  
  df_tariff_hs6 <- inner_join(hs10_master_6, tariffs_hs6, by = "commodity", relationship = "many-to-one")
  df_tariff_hs8 <- inner_join(hs10_master_8, tariffs_hs8, by = "commodity", relationship = "many-to-one")
  
  tariff_codes_hs6 <- df_tariff_hs6$commodity 
  tariff_codes_hs8 <- df_tariff_hs8$commodity 
  
  df_no_tariffs_hs6 <- filter(hs10_master_6, !(commodity %in% tariff_codes_hs6))
  df_no_tariffs_hs8 <- filter(hs10_master_8, !(commodity %in% tariff_codes_hs8))
  
  # Aggregation & Export
  df_counts_hs6 <- hs10_master_6 %>% group_by(date) %>% summarise(total_count = n(), value = sum(usd, na.rm = TRUE))
  write.csv(df_counts_hs6, paste0("../data/cleaned/imports/monthly/total_hs6_monthly_us_", abbr, ".csv"), row.names = FALSE)
  
  df_counts_hs8 <- hs10_master_8 %>% group_by(date) %>% summarise(total_count = n(), value = sum(usd, na.rm = TRUE))
  write.csv(df_counts_hs8, paste0("../data/cleaned/imports/monthly/total_hs8_monthly_us_", abbr, ".csv"), row.names = FALSE)
  
  df_counts_tariffs_hs6 <- df_tariff_hs6 %>% group_by(date) %>% summarise(total_count = n(), value = sum(usd, na.rm = TRUE))
  write.csv(df_counts_tariffs_hs6, paste0("../data/cleaned/imports/monthly/tariffs_hs6_monthly_us_", abbr, ".csv"), row.names = FALSE)
  
  df_counts_tariffs_hs8 <- df_tariff_hs8 %>% group_by(date) %>% summarise(total_count = n(), value = sum(usd, na.rm = TRUE))
  write.csv(df_counts_tariffs_hs8, paste0("../data/cleaned/imports/monthly/tariffs_hs8_monthly_us_", abbr, ".csv"), row.names = FALSE)
  
  df_counts_no_tariffs_hs6 <- df_no_tariffs_hs6 %>% group_by(date) %>% summarise(total_count = n(), value = sum(usd, na.rm = TRUE))
  write.csv(df_counts_no_tariffs_hs6, paste0("../data/cleaned/imports/monthly/no_tariffs_hs6_monthly_us_", abbr, ".csv"), row.names = FALSE)
  
  df_counts_no_tariffs_hs8 <- df_no_tariffs_hs8 %>% group_by(date) %>% summarise(total_count = n(), value = sum(usd, na.rm = TRUE))
  write.csv(df_counts_no_tariffs_hs8, paste0("../data/cleaned/imports/monthly/no_tariffs_hs8_monthly_us_", abbr, ".csv"), row.names = FALSE)
}

cat("--------------------------------------------------\n")
cat("Batch processing complete.\n")