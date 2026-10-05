################################################################################
### Panel Regressions: Tariff Evasion Margin Plots (HS6 Level)
### Stack: R (fixest for high-dimensional FE, ggplot2 for Fed margins)
################################################################################
rm(list = ls())
suppressMessages(source("/if/appl/R/Functions/IFfunctions.r"))
library(haven); library(readr); library(dplyr); library(tidyr); library(zoo)
library(fixest); library(ggplot2); library(lubridate); library(cowplot); library(this.path)

# Load IF specs
attach(IFSPECS$COL)
attach(IFSPECS$LW)
attach(IFSPECS$LT)

setwd(dirname(this.path()))

# Define Root Architecture
ROOT <- "/if/research-eme/omar/Eva/bilateralTrade"
ROOT_tariff_codes <- "/if/research-eme/omar/Eva"
out_dir <- paste0(ROOT, "/us/output/regressions")
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

# ==============================================================================
# 1. REGION CONFIGURATION
# ==============================================================================
region_map <- list(
  "Emerging_Asia" = c("ma", "vt", "id", "th", "ph"),
  "NorthAm_AFE"   = c("ca", "mx", "uk", "ge", "ea"), 
  "Emerging_LA"   = c("bz", "ar", "cl", "co"),
  "Advanced_Asia" = c("jn", "ko", "ta", "hk", "si")
)

# ==============================================================================
# 2. TARIFF PROJECTION & 2017 TRADE WEIGHTING
# ==============================================================================
cat("Processing Amiti et al. Tariff Panel...\n")

tariff_dta_path <- "1pct_sample_import_tariffs_panel.dta" 
if(!file.exists(tariff_dta_path)) tariff_dta_path <- "../data/tariffs/import_tariffs_panel.dta"

# 1. THE FIX: Robustly parse Stata %tm dates (months since Jan 1960) into standard R Dates
tariffs_raw <- read_dta(tariff_dta_path) %>%
  filter(cty_code == "5700") %>% 
  select(hts10, date, AddRate) %>%
  mutate(
    date_val = as.numeric(date),
    # If the integer is < 3000, it's a Stata month. If it's larger, it's an R day-count.
    date = if_else(date_val < 3000, 
                   as.Date(zoo::as.yearmon(1960 + date_val/12)), 
                   as.Date(date_val, origin="1970-01-01"))
  ) %>%
  select(-date_val)

# 2. Extrapolate the 2019 Amiti data forward to 2024
dates_grid <- expand_grid(
  hts10 = unique(tariffs_raw$hts10),
  date = seq.Date(as.Date("2016-01-01"), as.Date("2024-12-01"), by="month")
)

tariffs_proj <- dates_grid %>%
  left_join(tariffs_raw, by=c("hts10", "date")) %>%
  arrange(hts10, date) %>%
  group_by(hts10) %>% fill(AddRate, .direction="down") %>% ungroup() %>%
  mutate(
    AddRate = replace_na(AddRate, 0),
    hs6 = substr(hts10, 1, 6)
  )

# 3. Apply the Definitive Master HS6 List
master_tariffs <- read_csv(paste0(ROOT_tariff_codes, "/tariff_codes/master_hs6.csv"), show_col_types=FALSE) %>%
  mutate(hs6 = as.character(hs6)) %>% pull(hs6) %>% unique()

# 4. Execute 2017 Baseline Trade Weighting for HS6 Aggregation
weight_file <- paste0(ROOT, "/us/data/raw/imports/monthly/us_rep_hs10_values_ch.csv")
if(file.exists(weight_file)) {
  cat("  -> Found 2017 baseline for trade weighting.\n")
  w17 <- read_csv(weight_file, show_col_types = FALSE, col_types = cols(.default = "c")) %>%
    mutate(
      date = if("period" %in% names(.)) ym(period) else as.Date(date),
      usd = as.numeric(usd)
    ) %>%
    filter(year(date) == 2017) %>%
    mutate(hs6 = substr(commodity, 1, 6)) %>%
    group_by(hs6, hts10 = commodity) %>% summarise(val = sum(usd, na.rm=TRUE), .groups="drop") %>%
    group_by(hs6) %>% mutate(weight = val / sum(val, na.rm=TRUE)) %>% ungroup()
  
  tariffs_hs6 <- tariffs_proj %>%
    left_join(w17 %>% select(hts10, weight), by="hts10") %>%
    mutate(weight = replace_na(weight, 1)) %>% 
    group_by(hs6, date) %>%
    summarise(mean_addrate = weighted.mean(AddRate, weight, na.rm=TRUE), .groups="drop")
} else {
  cat("  -> No 2017 baseline found. Using simple mean for HS6 aggregation.\n")
  tariffs_hs6 <- tariffs_proj %>%
    group_by(hs6, date) %>% summarise(mean_addrate = mean(AddRate, na.rm=TRUE), .groups="drop")
}

# ==============================================================================
# 3. ROBUST FILE FINDER & PANEL BUILDER
# ==============================================================================
build_regional_panel <- function(partner_list, perspective) {
  panel_list <- list()
  
  for(p in partner_list) {
    
    if (perspective == "US_Partner") {
      imp_paths <- c(paste0(ROOT, "/us/data/raw/imports/monthly/us_rep_hs10_values_", p, ".csv"),
                     paste0(ROOT, "/us/data/raw/imports/monthly/comtrade_imports_from_", p, "_us.csv"))
      exp_paths <- c(paste0(ROOT, "/us/data/raw/exports/monthly/comtrade_exports_to_us_", p, ".csv"))
    } else {
      imp_paths <- c(paste0(ROOT, "/china/data/raw/imports/monthly/comtrade_imports_from_china_", p, ".csv"))
      exp_paths <- c(paste0(ROOT, "/china/data/raw/exports/monthly/comtrade_china_exports_to_", p, ".csv"))
    }
    
    imp_file <- imp_paths[file.exists(imp_paths)][1]
    exp_file <- exp_paths[file.exists(exp_paths)][1]
    
    if(is.na(imp_file) | is.na(exp_file)) {
      cat(paste0("    [Missing Data] Skipping ", toupper(p), "\n"))
      next 
    }
    
    # Process Imports
    imp <- read_csv(imp_file, show_col_types=FALSE, col_types = cols(.default = "c"))
    names(imp) <- tolower(names(imp))
    if("commodity" %in% names(imp)) imp$cmdcode <- imp$commodity
    if("usd" %in% names(imp)) imp$primaryvalue <- imp$usd
    
    imp <- imp %>%
      mutate(
        date = if("period" %in% names(.)) ym(period) else as.Date(date),
        date = as.Date(paste0(format(date, "%Y-%m"), "-01")),
        hs6 = substr(cmdcode, 1, 6),
        usd = as.numeric(primaryvalue)
      ) %>%
      group_by(date, hs6) %>% summarise(imp_usd = sum(usd, na.rm=TRUE), .groups="drop")
    
    # Process Exports
    exp <- read_csv(exp_file, show_col_types=FALSE, col_types = cols(.default = "c"))
    names(exp) <- tolower(names(exp))
    if("commodity" %in% names(exp)) exp$cmdcode <- exp$commodity
    if("usd" %in% names(exp)) exp$primaryvalue <- exp$usd
    
    exp <- exp %>%
      mutate(
        date = if("period" %in% names(.)) ym(period) else as.Date(date),
        date = as.Date(paste0(format(date, "%Y-%m"), "-01")),
        hs6 = substr(cmdcode, 1, 6),
        usd = as.numeric(primaryvalue)
      ) %>%
      group_by(date, hs6) %>% summarise(exp_usd = sum(usd, na.rm=TRUE), .groups="drop")
    
    # Compute Dependent Variable
    df_p <- full_join(imp, exp, by=c("date", "hs6")) %>%
      mutate(
        imp_usd = replace_na(imp_usd, 0),
        exp_usd = replace_na(exp_usd, 0),
        log_gap = asinh(imp_usd) - asinh(exp_usd),
        partner = factor(toupper(p))
      )
    panel_list[[p]] <- df_p
  }
  return(bind_rows(panel_list))
}

# ==============================================================================
# 4. REGRESSION & MARGIN PLOT ENGINE
# ==============================================================================
run_and_plot_margins <- function(region_name, partner_list, perspective) {
  
  cat(paste0("\n--- Processing ", region_name, " (Perspective: ", perspective, ") ---\n"))
  
  df_panel <- build_regional_panel(partner_list, perspective)
  if(nrow(df_panel) == 0) { cat("  -> No raw data found for region. Skipping.\n"); return() }
  
  # Ensure the master HS6 filter drives the Tariffed vs. Non-Tariffed Split
  df_reg <- df_panel %>%
    left_join(tariffs_hs6, by=c("date", "hs6")) %>%
    filter(!is.na(mean_addrate)) %>%
    mutate(is_tariffed = hs6 %in% master_tariffs)
  
  df_tar <- df_reg %>% filter(is_tariffed == TRUE)
  df_non <- df_reg %>% filter(is_tariffed == FALSE)
  
  if(nrow(df_tar) < 10 | nrow(df_non) < 10) { cat("  -> Sparse sample. Skipping plot.\n"); return() }
  
  cat("  Running Tariffed Goods FE Model...\n")
  m_tar <- feols(log_gap ~ 0 + partner:mean_addrate | hs6 + partner + date, 
                 data=df_tar, cluster=~hs6)
  
  cat("  Running Non-Tariffed Goods FE Model...\n")
  m_non <- feols(log_gap ~ 0 + partner:mean_addrate | hs6 + partner + date, 
                 data=df_non, cluster=~hs6)
  
  extract_margins <- function(model, label) {
    df_coef <- coeftable(model) %>% as.data.frame() %>% tibble::rownames_to_column("term")
    df_coef %>%
      filter(grepl("mean_addrate", term) & grepl("partner", term)) %>%
      mutate(
        partner = gsub("partner|:mean_addrate", "", term),
        type = label,
        ci_low = Estimate - 1.96 * `Std. Error`,
        ci_high = Estimate + 1.96 * `Std. Error`
      )
  }
  
  res_all <- bind_rows(extract_margins(m_tar, "Tariffed Goods Trade-reporting Gap"), 
                       extract_margins(m_non, "Non-Tariffed Goods Trade-reporting Gap"))
  
  if(nrow(res_all) == 0) { cat("  -> No valid coefficients extracted. Skipping plot.\n"); return() }
  
  title_text <- ifelse(perspective == "US_Partner", 
                       paste0("Marginal Effect of US-China Tariffs on US-", gsub("_", " ", region_name), " Reporting Gaps"),
                       paste0("Marginal Effect of US-China Tariffs on ", gsub("_", " ", region_name), "-China Reporting Gaps"))
  
  p <- ggplot(res_all, aes(x = partner, y = Estimate, color = type)) +
    geom_hline(yintercept = 0, linetype = IFDASHED, color = "black", linewidth=IFTHIN) +
    geom_pointrange(aes(ymin = ci_low, ymax = ci_high), size = 1, linewidth = IFTHICK, position = position_dodge(width = 0.5)) +
    labs(
      title = title_text,
      subtitle = "Point estimates on products tariffed by US on China during 2018-19. 95% Confidence Intervals",
      caption = "Coefficients from Panel Regression: log_gap ~ AddRate. Fixed Effects: HS6, Partner, Time. SEs clustered at HS6 level."
    ) +
    scale_color_manual(values = c("Tariffed Goods Trade-reporting Gap" = IFRED, "Non-Tariffed Goods Trade-reporting Gap" = IFBLUE)) +
    theme_if_policystyle() +
    theme(axis.title.x = element_blank(), axis.title.y = element_blank(),
          legend.position = "top", legend.title = element_blank())
  
  filename <- paste0("/margin_", perspective, "_", region_name, ".png")
  ggsave(paste0(out_dir, filename), p, width = 8, height = 5, dpi = 800, bg="white")
  cat("  -> Saved", filename, "\n")
}

# ==============================================================================
# 5. EXECUTION LOOP
# ==============================================================================
for (region in names(region_map)) {
  run_and_plot_margins(region, region_map[[region]], perspective = "US_Partner")
}

for (region in names(region_map)) {
  run_and_plot_margins(region, region_map[[region]], perspective = "Partner_China")
}

cat("\nAll Regional Margin Plots Complete!\n")
