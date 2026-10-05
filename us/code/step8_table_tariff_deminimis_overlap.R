################################################################################
### De Minimis Proxy vs. Tariff Master List: Overlap Diagnostics
### Output: Summary tables mapping HS2 and HS4 codes to the Tariff Master List
################################################################################
rm(list = ls())
library(readr)
library(dplyr)
library(stringr)
library(this.path)

setwd(dirname(this.path()))

out_dir <- "/if/research-eme/omar/Eva/bilateralTrade/us/output/de_minimis_proxy"
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

# ==============================================================================
# 1. LOAD TARIFF MASTER LIST
# ==============================================================================
cat("Loading Tariff Master List...\n")

# Load HS6 list
tariffs_hs6_raw <- read_csv("../../../tariff_codes/master_hs6.csv", show_col_types = FALSE)

tariffs_hs6 <- unique(tariffs_hs6_raw) %>%
  dplyr::rename(hs6_commodity = hs6) %>% # Explicitly calling dplyr here to avoid masking errors
  mutate(hs6_commodity = as.character(hs6_commodity)) %>%
  # Crucial: Pad with leading zeroes if any codes dropped them during import
  mutate(hs6_commodity = str_pad(hs6_commodity, 6, pad = "0"))

# ==============================================================================
# 2. DEFINE DE MINIMIS PROXY
# ==============================================================================
proxy_chapters <- c("33", "39", "42", "48", "61", "62", "63", "64", 
                    "65", "71", "85", "90", "91", "94", "95", "96")

# ==============================================================================
# 3. EXTRACT AND AGGREGATE
# ==============================================================================
cat("Calculating Overlaps...\n")

overlap_data <- tariffs_hs6 %>%
  mutate(
    hs2_prefix = substr(hs6_commodity, 1, 2),
    hs4_prefix = substr(hs6_commodity, 1, 4),
    in_de_minimis_proxy = ifelse(hs2_prefix %in% proxy_chapters, "Yes", "No")
  )

# --- Summary 1: High-level HS2 Aggregation ---
# This shows how many tariffed HS6 codes fall under each HS2 family.
summary_hs2 <- overlap_data %>%
  group_by(hs2_prefix, in_de_minimis_proxy) %>%
  summarise(
    unique_hs4_tariffed = n_distinct(hs4_prefix),
    unique_hs6_tariffed = n_distinct(hs6_commodity),
    .groups = 'drop'
  ) %>%
  arrange(desc(in_de_minimis_proxy), hs2_prefix)

# --- Summary 2: Granular HS4 Breakdown ---
# This breaks it down further to see which specific HS4 groupings are heavily tariffed
summary_hs4 <- overlap_data %>%
  filter(in_de_minimis_proxy == "Yes") %>% # Only looking at the proxy chapters
  group_by(hs2_prefix, hs4_prefix) %>%
  summarise(
    unique_hs6_tariffed = n_distinct(hs6_commodity),
    .groups = 'drop'
  ) %>%
  arrange(hs2_prefix, hs4_prefix)


# ==============================================================================
# 4. EXPORT & CONSOLE SUMMARY
# ==============================================================================
write_csv(summary_hs2, paste0(out_dir, "/tariff_overlap_summary_hs2.csv"))
write_csv(summary_hs4, paste0(out_dir, "/tariff_overlap_granular_hs4.csv"))

# Calculate Summary Statistics
total_tariff_codes <- n_distinct(tariffs_hs6$hs6_commodity)
overlapping_codes <- sum(summary_hs4$unique_hs6_tariffed)
pct_overlap <- round((overlapping_codes / total_tariff_codes) * 100, 1)

cat("--------------------------------------------------\n")
cat("Diagnostic Tables Saved:\n")
cat("1. tariff_overlap_summary_hs2.csv (Overview of all HS2 codes)\n")
cat("2. tariff_overlap_granular_hs4.csv (Deep dive into the Proxy chapters)\n\n")

cat("=== OVERLAP SUMMARY ===\n")
cat(paste("Total unique HS6 codes in Tariff Master List:", total_tariff_codes, "\n"))
cat(paste("Total overlapping HS6 codes found in proxy:", overlapping_codes, "\n"))
cat(paste("Percentage of tariffed codes captured by proxy:", pct_overlap, "%\n"))
cat("--------------------------------------------------\n")