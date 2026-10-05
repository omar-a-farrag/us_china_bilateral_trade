################################################################################
### Robustness Check: Spillover Effects on Non-Tariffed Goods
### Method 1: Post-July 2018 Dummy
### Method 2: Aggregate Macro-Tariff Continuous Rate
################################################################################
rm(list = ls())
suppressMessages(source("/if/appl/R/Functions/IFfunctions.r"))
library(haven); library(readr); library(dplyr); library(tidyr); library(zoo)
library(fixest); library(ggplot2); library(lubridate); library(cowplot); library(this.path)

attach(IFSPECS$COL); attach(IFSPECS$LW); attach(IFSPECS$LT)
setwd(dirname(this.path()))

ROOT <- "/if/research-eme/omar/Eva/bilateralTrade"
ROOT_tariff_codes <- "/if/research-eme/omar/Eva"

out_dir <- paste0(ROOT, "/us/output/regressions")

region_map <- list(
  "Emerging_Asia" = c("ma", "vt", "id", "th", "ph"),
  "NorthAm_AFE"   = c("ca", "mx", "uk", "ge", "ea"), 
  "Emerging_LA"   = c("bz", "ar", "cl", "co"),
  "Advanced_Asia" = c("jn", "ko", "ta", "hk", "si")
)

# ==============================================================================
# 1. LOAD TARIFFS & CREATE MACRO SHOCKS
# ==============================================================================
cat("Processing Amiti et al. for Spillover Shocks...\n")

tariff_dta_path <- "1pct_sample_import_tariffs_panel.dta" 
if(!file.exists(tariff_dta_path)) tariff_dta_path <- "../data/tariffs/import_tariffs_panel.dta"

tariffs_raw <- read_dta(tariff_dta_path) %>%
  filter(cty_code == "5700") %>% select(hts10, date, AddRate) %>%
  mutate(
    date_val = as.numeric(date),
    date = if_else(date_val < 3000, as.Date(zoo::as.yearmon(1960 + date_val/12)), as.Date(date_val, origin="1970-01-01"))
  ) %>% select(-date_val)

dates_grid <- expand_grid(
  hts10 = unique(tariffs_raw$hts10),
  date = seq.Date(as.Date("2016-01-01"), as.Date("2024-12-01"), by="month")
)

tariffs_proj <- dates_grid %>%
  left_join(tariffs_raw, by=c("hts10", "date")) %>%
  arrange(hts10, date) %>% group_by(hts10) %>% fill(AddRate, .direction="down") %>% ungroup() %>%
  mutate(AddRate = replace_na(AddRate, 0), hs6 = substr(hts10, 1, 6))

master_tariffs <- read_csv(paste0(ROOT_tariff_codes, "/tariff_codes/master_hs6.csv"), show_col_types=FALSE) %>%
  mutate(hs6 = as.character(hs6)) %>% pull(hs6) %>% unique()

# Calculate the Macro-Tariff (Average US-China Tariff across all goods for each month)
macro_shocks <- tariffs_proj %>%
  group_by(date) %>%
  summarise(macro_tariff = mean(AddRate, na.rm=TRUE), .groups="drop") %>%
  mutate(post_trade_war = ifelse(date >= as.Date("2018-07-01"), 1, 0))

# ==============================================================================
# 2. PANEL BUILDER
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
    if(is.na(imp_file) | is.na(exp_file)) next 
    
    imp <- read_csv(imp_file, show_col_types=F, col_types = cols(.default = "c"))
    names(imp) <- tolower(names(imp))
    if("commodity" %in% names(imp)) imp$cmdcode <- imp$commodity
    if("usd" %in% names(imp)) imp$primaryvalue <- imp$usd
    imp <- imp %>% mutate(date = if("period" %in% names(.)) ym(period) else as.Date(date), date = as.Date(paste0(format(date, "%Y-%m"), "-01")), hs6 = substr(cmdcode, 1, 6), usd = as.numeric(primaryvalue)) %>% group_by(date, hs6) %>% summarise(imp_usd = sum(usd, na.rm=T), .groups="drop")
    
    exp <- read_csv(exp_file, show_col_types=F, col_types = cols(.default = "c"))
    names(exp) <- tolower(names(exp))
    if("commodity" %in% names(exp)) exp$cmdcode <- exp$commodity
    if("usd" %in% names(exp)) exp$primaryvalue <- exp$usd
    exp <- exp %>% mutate(date = if("period" %in% names(.)) ym(period) else as.Date(date), date = as.Date(paste0(format(date, "%Y-%m"), "-01")), hs6 = substr(cmdcode, 1, 6), usd = as.numeric(primaryvalue)) %>% group_by(date, hs6) %>% summarise(exp_usd = sum(usd, na.rm=T), .groups="drop")
    
    df_p <- full_join(imp, exp, by=c("date", "hs6")) %>%
      mutate(imp_usd = replace_na(imp_usd, 0), exp_usd = replace_na(exp_usd, 0), log_gap = asinh(imp_usd) - asinh(exp_usd), partner = factor(toupper(p)))
    panel_list[[p]] <- df_p
  }
  return(bind_rows(panel_list))
}

# ==============================================================================
# 3. REGRESSION & PLOTTING
# ==============================================================================
run_and_plot_spillovers <- function(region_name, partner_list, perspective) {
  cat(paste0("\n--- Spillovers: ", region_name, " (", perspective, ") ---\n"))
  
  df_panel <- build_regional_panel(partner_list, perspective)
  if(nrow(df_panel) == 0) return()
  
  # KEEP ONLY NON-TARIFFED GOODS
  df_non <- df_panel %>%
    mutate(is_tariffed = hs6 %in% master_tariffs) %>%
    filter(is_tariffed == FALSE) %>%
    left_join(macro_shocks, by="date") 
  
  if(nrow(df_non) < 10) return()
  
  # Method 1: The Dummy Method
  cat("  Running Dummy Method...\n")
  m_dummy <- feols(log_gap ~ 0 + partner:post_trade_war | hs6 + partner, data=df_non, cluster=~hs6) # Removed date FE so the dummy doesn't drop due to collinearity
  
  # Method 2: The Macro-Tariff Method
  cat("  Running Macro-Tariff Method...\n")
  m_macro <- feols(log_gap ~ 0 + partner:macro_tariff | hs6 + partner + date, data=df_non, cluster=~hs6)
  
  extract_margins <- function(model, label, term_match) {
    df_coef <- coeftable(model) %>% as.data.frame() %>% tibble::rownames_to_column("term") %>%
      filter(grepl(term_match, term)) %>%
      mutate(
        partner = sub(paste0("^partner([A-Za-z]+):", term_match), "\\1", term),
        type = label,
        ci_low = Estimate - 1.96 * `Std. Error`,
        ci_high = Estimate + 1.96 * `Std. Error`
      )
  }
  
  res_dummy <- extract_margins(m_dummy, "Spillover Dummy (Post-July 2018)", "post_trade_war")
  res_macro <- extract_margins(m_macro, "Macro-Tariff Intensity", "macro_tariff")
  
  # Plotting function
  make_plot <- function(data, plot_title, color_val, file_suffix, cap_text) {
    p <- ggplot(data, aes(x = partner, y = Estimate, color = type)) +
      geom_hline(yintercept = 0, linetype = IFDASHED, color = "black", linewidth=IFTHIN) +
      geom_pointrange(aes(ymin = ci_low, ymax = ci_high), size = 1, linewidth = IFTHICK, position = position_dodge(width = 0.5)) +
      labs(title = plot_title, subtitle = "Point Estimates with 95% CIs (Non-Tariffed Goods Only)", caption = cap_text) +
      scale_color_manual(values = setNames(color_val, data$type[1])) +
      theme_if_policystyle() +
      theme(axis.title.x = element_blank(), axis.title.y = element_blank(), legend.position = "top", legend.title = element_blank())
    
    filename <- paste0("/margin_", perspective, "_", region_name, file_suffix, ".png")
    ggsave(paste0(out_dir, filename), p, width = 8, height = 5, dpi = 800, bg="white")
    cat("  -> Saved", filename, "\n")
  }
  
  title_base <- ifelse(perspective == "US_Partner", paste0("US-", gsub("_", " ", region_name)), paste0(gsub("_", " ", region_name), "-China"))
  
  make_plot(res_dummy, paste0("Trade War Shock (Dummy): ", title_base), IFPURPLE, "_spillover_dummy", "Fixed Effects: HS6, Partner. (Time FE omitted to identify dummy)")
  make_plot(res_macro, paste0("Spillover via Macro-Tariff: ", title_base), IFORANGE, "_spillover_macro", "Fixed Effects: HS6, Partner, Time.")
}

for (region in names(region_map)) {
  run_and_plot_spillovers(region, region_map[[region]], perspective = "US_Partner")
  run_and_plot_spillovers(region, region_map[[region]], perspective = "Partner_China")
}
cat("\nAll Spillover Plots Complete!\n")