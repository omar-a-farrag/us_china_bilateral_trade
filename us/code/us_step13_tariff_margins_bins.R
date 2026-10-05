################################################################################
### Robustness Check: Binned Tariff Evasion Margin Plots
### Investigating threshold effects (e.g., does evasion only spike > 20%?)
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
# 1. LOAD & BIN TARIFFS
# ==============================================================================
cat("Processing Amiti et al. Binned Tariff Panel...\n")

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

# Weight and aggregate
weight_file <- paste0(ROOT, "/us/data/raw/imports/monthly/us_rep_hs10_values_ch.csv")
if(file.exists(weight_file)) {
  w17 <- read_csv(weight_file, show_col_types = FALSE, col_types = cols(.default = "c")) %>%
    mutate(date = if("period" %in% names(.)) ym(period) else as.Date(date), usd = as.numeric(usd)) %>%
    filter(year(date) == 2017) %>% mutate(hs6 = substr(commodity, 1, 6)) %>%
    group_by(hs6, hts10 = commodity) %>% summarise(val = sum(usd, na.rm=TRUE), .groups="drop") %>%
    group_by(hs6) %>% mutate(weight = val / sum(val, na.rm=TRUE)) %>% ungroup()
  
  tariffs_hs6 <- tariffs_proj %>%
    left_join(w17 %>% select(hts10, weight), by="hts10") %>% mutate(weight = replace_na(weight, 1)) %>% 
    group_by(hs6, date) %>% summarise(mean_addrate = weighted.mean(AddRate, weight, na.rm=TRUE), .groups="drop")
} else {
  tariffs_hs6 <- tariffs_proj %>% group_by(hs6, date) %>% summarise(mean_addrate = mean(AddRate, na.rm=TRUE), .groups="drop")
}

# --- THE BINS ---
# Breakrates: [0], (0-10%], (10-20%], (>20%]
tariffs_hs6 <- tariffs_hs6 %>%
  mutate(
    tariff_bin = cut(mean_addrate, 
                     breaks = c(-Inf, 0.001, 0.101, 0.201, Inf), 
                     labels = c("0%", "1-10%", "11-20%", ">20%"),
                     right = FALSE),
    tariff_bin = relevel(tariff_bin, ref="0%") # 0% is the absorbed baseline
  )

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
run_and_plot_bins <- function(region_name, partner_list, perspective) {
  cat(paste0("\n--- Bins Processing: ", region_name, " (", perspective, ") ---\n"))
  
  df_panel <- build_regional_panel(partner_list, perspective)
  if(nrow(df_panel) == 0) return()
  
  df_tar <- df_panel %>%
    left_join(tariffs_hs6, by=c("date", "hs6")) %>%
    filter(!is.na(tariff_bin), hs6 %in% master_tariffs) # ONLY Tariffed Goods
  
  if(nrow(df_tar) < 10) return()
  
  # The Interaction drops the 0% reference bin dynamically due to relevel()
  m_bin <- feols(log_gap ~ partner:tariff_bin | hs6 + partner + date, data=df_tar, cluster=~hs6)
  
  # Extract Coefficients
  df_coef <- coeftable(m_bin) %>% as.data.frame() %>% tibble::rownames_to_column("term") %>%
    filter(grepl("tariff_bin", term)) %>%
    mutate(
      partner = sub("^partner([A-Za-z]+):.*", "\\1", term),
      bin_level = sub(".*tariff_bin(.*)", "\\1", term),
      ci_low = Estimate - 1.96 * `Std. Error`,
      ci_high = Estimate + 1.96 * `Std. Error`
    )
  
  # Re-order bins for the legend
  df_coef$bin_level <- factor(df_coef$bin_level, levels = c("1-10%", "11-20%", ">20%"))
  
  title_text <- ifelse(perspective == "US_Partner", 
                       paste0("Marginal Effect by Tariff Bin: US-", gsub("_", " ", region_name)),
                       paste0("Marginal Effect by Tariff Bin: ", gsub("_", " ", region_name), "-China"))
  
  bin_colors <- c("1-10%" = IFGREEN, "11-20%" = IFORANGE, ">20%" = IFRED)
  
  p <- ggplot(df_coef, aes(x = partner, y = Estimate, color = bin_level, group = bin_level)) +
    geom_hline(yintercept = 0, linetype = IFDASHED, color = "black", linewidth=IFTHIN) +
    geom_pointrange(aes(ymin = ci_low, ymax = ci_high), size = 1, linewidth = IFTHICK, position = position_dodge(width = 0.6)) +
    labs(
      title = title_text,
      subtitle = "Relative to 0% Tariff baseline (Tariffed Goods Only)",
      caption = "Fixed Effects: HS6, Partner, Time. SE clustered at HS6 level."
    ) +
    scale_color_manual(values = bin_colors) +
    theme_if_policystyle() +
    theme(axis.title.x = element_blank(), axis.title.y = element_blank(),
          legend.position = "top", legend.title = element_blank())
  
  filename <- paste0("/margin_", perspective, "_", region_name, "_bins.png")
  ggsave(paste0(out_dir, filename), p, width = 8, height = 5, dpi = 800, bg="white")
  cat("  -> Saved", filename, "\n")
}

for (region in names(region_map)) {
  run_and_plot_bins(region, region_map[[region]], perspective = "US_Partner")
  run_and_plot_bins(region, region_map[[region]], perspective = "Partner_China")
}
cat("\nAll Bin Robustness Plots Complete!\n")