rm(list = ls())
suppressMessages(source("/if/appl/R/Functions/IFfunctions.r"))

library(readr)
library(dplyr)
library(tidyr)
library(lubridate)
library(this.path)
library(pracma) 

setwd(dirname(this.path()))

# ==============================================================================
# 1. STATIC SETUP (Run once)
# ==============================================================================
cat("Loading and cleaning Tariff Master lists...\n")

# Load Tariff List (HS6 Only)
# Adjust path levels (../../) if necessary depending on where this script lives
tariffs_hs6_raw <- read_csv("../../../tariff_codes/master_hs6.csv", show_col_types = FALSE)

# Clean HS6
tariffs_hs6 <- unique(tariffs_hs6_raw) %>%
  rename(commodity = hs6) %>%
  mutate(commodity = as.character(commodity)) 
sum_dup_6 <- sum(duplicated(tariffs_hs6))
disp(paste("HS6 Duplicates:", sum_dup_6))

# Define the list of country abbreviations 
# (Note: 'ta' will likely be skipped if the file wasn't generated in the previous step)
country_abbrs <- c("ar", "bz", "ca", "co", "cl", "ch", "ge", "hk", "id", "in", 
                   "jn", "ko", "ma", "mx", "ph", "si", "ta", "th", "uk", "vt")

# ==============================================================================
# 2. THE LOOP
# ==============================================================================
cat("--------------------------------------------------\n")
cat("Starting batch processing for HS6 Exports (UN Comtrade).\n")

for (abbr in country_abbrs) {
  
  # Dynamic Input Filename (Matches the file we created in the API pull step)
  input_file <- paste0("../data/raw/exports/monthly/comtrade_exports_to_us_", abbr, ".csv")
  
  # Check if file exists (Important for 'ta' or other failed downloads)
  if (!file.exists(input_file)) {
    cat("SKIPPING:", abbr, "(File not found)\n")
    next
  }
  
  cat(format(Sys.time(), "%H:%M:%S"), "- Processing:", abbr, "...\n")
  
  # Read Data
  # We read all columns as characters initially to be safe, then convert
  df_raw <- read_csv(input_file, show_col_types = FALSE, 
                     col_types = cols(.default = col_character()))
  
  # ---------------------------------------------------------
  # STANDARDIZE COLUMNS (UN Comtrade -> Internal Format)
  # ---------------------------------------------------------
  hs6_master <- df_raw %>%
    rename(
      commodity = cmdcode,      # UN name -> Our name
      date_str = period,        # Keep as string for parsing
      usd = primaryvalue        # UN name -> Our name
    ) %>%
    mutate(
      usd = as.numeric(usd),    # Convert value back to number
      date = ym(date_str)       # Convert "201001" to Date object
    ) %>%
    select(date, commodity, usd) # Keep only what we need
  
  # ---------------------------------------------------------
  # Join with Tariff Lists
  # ---------------------------------------------------------
  # Join
  df_tariff_hs6 <- inner_join(hs6_master, tariffs_hs6, by = "commodity", relationship = "many-to-one")
  
  # Filter Anti-Join
  tariff_codes_hs6 <- df_tariff_hs6$commodity 
  df_no_tariffs_hs6 <- filter(hs6_master, !(commodity %in% tariff_codes_hs6))
  
  # ---------------------------------------------------------
  # Validation Checks
  # ---------------------------------------------------------
  valid_6 <- nrow(hs6_master) == (nrow(df_tariff_hs6) + nrow(df_no_tariffs_hs6))
  
  if (!valid_6) {
    # If validation fails, we warn but don't stop the whole loop
    warning(paste("Math mismatch in HS6 merges for", abbr))
  }
  
  # ---------------------------------------------------------
  # Aggregation & Export
  # ---------------------------------------------------------
  # Create output directory if it doesn't exist
  dir.create("../data/cleaned/exports/monthly/", recursive = TRUE, showWarnings = FALSE)
  
  # 1. Total HS6
  df_counts_hs6 <- hs6_master %>%
    group_by(date) %>%
    summarise(total_count = n(), value = sum(usd, na.rm = TRUE))
  write.csv(df_counts_hs6, paste0("../data/cleaned/exports/monthly/total_hs6_monthly_us_", abbr, ".csv"), row.names = FALSE)
  
  # 2. Tariffs HS6
  df_counts_tariffs_hs6 <- df_tariff_hs6 %>%
    group_by(date) %>%
    summarise(total_count = n(), value = sum(usd, na.rm = TRUE))
  write.csv(df_counts_tariffs_hs6, paste0("../data/cleaned/exports/monthly/tariffs_hs6_monthly_us_", abbr, ".csv"), row.names = FALSE)
  
  # 3. No Tariffs HS6
  df_counts_no_tariffs_hs6 <- df_no_tariffs_hs6 %>%
    group_by(date) %>%
    summarise(total_count = n(), value = sum(usd, na.rm = TRUE))
  write.csv(df_counts_no_tariffs_hs6, paste0("../data/cleaned/exports/monthly/no_tariffs_hs6_monthly_us_", abbr, ".csv"), row.names = FALSE)
  
}

cat("--------------------------------------------------\n")
cat("Batch processing complete.\n")