################################################################################
### Canada Imports: China vs ASEAN (Split Scale Analysis)
### Output: Side-by-side panels for Total, Tariffed, and Non-Tariffed Goods
## Omar Farrag, IF Division - EME
################################################################################

rm(list = ls())
suppressMessages(source("/if/appl/R/Functions/IFfunctions.r"))

library(readr); library(dplyr); library(zoo); library(policyPlot)
library(ggplot2); library(pracma); library(xts); library(sjmisc)
library(grid); library(tidyr); library(cowplot); library(this.path)

# Load IF specs
attach(IFSPECS$COL)
attach(IFSPECS$LT)
attach(IFSPECS$LW)

setwd(dirname(this.path()))

# 1. SETUP & CUSTOMIZATION
# ------------------------------------------------------------------------------
asean_map <- c(
  "ch" = "China", "id" = "Indonesia", "ma" = "Malaysia",
  "ph" = "Philippines", "th" = "Thailand", "vt" = "Vietnam"
)

custom_cols <- c("China" = IFRED, "Vietnam" = IFBLUE, "Malaysia" = IFPURPLE, 
                 "Thailand" = IFGREEN, "Indonesia" = IFORANGE, "Philippines" = IFGRAY)

custom_lws <- c("China" = IFTHICK, "Vietnam" = IFMEDIUM, "Malaysia" = IFMEDIUM, 
                "Thailand" = IFMEDIUM, "Indonesia" = IFMEDIUM, "Philippines" = IFMEDIUM)

custom_lts <- c("China" = "solid", "Vietnam" = "solid", "Malaysia" = "solid", 
                "Thailand" = IFDASHED, "Indonesia" = IFDOTTED, "Philippines" = "solid")

# Output Directory
root <- "/if/research-eme/omar/Eva/bilateralTrade/ca"
out_dir <- paste0(root, "/output/imports_asean_ch")
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

# 2. MAIN PROCESSING LOOP
# ------------------------------------------------------------------------------
categories <- c("total", "tariffs", "no_tariffs")
titles <- c("Total Goods", "Tariffed Goods", "Non-Tariffed Goods")

cat("Starting Canada Imports: ASEAN vs China (Split Scale)...\n")

start_date <- as.Date("2016-01-01")

for (i in 1:3) {
  
  cat_type <- categories[i]
  cat_title <- titles[i]
  
  cat(paste0("  Processing: ", cat_title, "...\n"))
  df_list <- list()
  
  # --- LOAD DATA ---
  for (abbr in names(asean_map)) {
    file_prefix <- paste0(cat_type, "_hs6_ca_imp_from_")
    path_file <- paste0("../data/cleaned/imports/", file_prefix, abbr, ".csv")
    
    if (file.exists(path_file)) {
      temp <- read_csv(path_file, show_col_types = FALSE) %>%
        mutate(value = value/(10^9), country_code = abbr, country_label = asean_map[[abbr]]) %>%
        arrange(date) %>%
        mutate(value_roll = rollsum(value, 12, align = "right", fill = NA)) %>%
        na.omit()
      df_list[[abbr]] <- temp
    }
  }
  
  if (length(df_list) == 0) next
  df_all <- bind_rows(df_list)
  
  end_date <- ceiling_date(max(df_all$date), "year")
  end_date_data <- max(df_all$date)
  last_date_txt <- format(end_date_data, "%B %Y")
  
  # --- SPLIT DATA ---
  df_asean <- df_all %>% filter(country_code != "ch")
  df_china <- df_all %>% filter(country_code == "ch")
  
  # --- CALCULATE INDEPENDENT LIMITS ---
  lims_asean <- limint(df_asean$value_roll, extraspace = 0)
  lims_china <- limint(df_china$value_roll, extraspace = 0)
  
  # --- PLOT 1: ASEAN-5 (Left Panel) ---
  p_asean <- df_asean %>%
    ggplot(aes(x = date, y = value_roll, color = country_label, 
               linewidth = country_label, linetype = country_label)) +
    geom_line() +
    labs(title = paste0("Imports from ASEAN: ", cat_title),
         subtitle = "Billions USD (12-month rolling sum)", caption = " ") + 
    scale_color_manual(values = custom_cols) + scale_linewidth_manual(values = custom_lws) + scale_linetype_manual(values = custom_lts) +
    scale_y_continuous(limits = c(0, lims_asean[2]), position = "right", breaks = seq(0, lims_asean[2], lims_asean[3])) +
    scale_x_date(expand = c(0.05,0.05), breaks = seq(start_date, end_date, by = "1 year"), labels = date_format('%Y'), limits = c(start_date, end_date)) +
    theme_if_policystyle() +
    theme(legend.position = c(0.02, 0.95), legend.justification = c("left", "center"),
          plot.subtitle = element_text(vjust = 1.25), axis.title.x = element_blank(), axis.title.y = element_blank()) +     
    guides(color = guide_legend(nrow = 2, ncol = 3, byrow = TRUE), linetype = guide_legend(nrow = 2, ncol = 3, byrow = TRUE), linewidth = guide_legend(nrow = 2, ncol = 3, byrow = TRUE))
  
  p_asean = customXAxis(p_asean, minorticks = 11, skip = 1, center = F)
  
  # --- PLOT 2: CHINA (Right Panel) ---
  p_china <- df_china %>%
    ggplot(aes(x = date, y = value_roll, color = country_label, 
               linewidth = country_label, linetype = country_label)) +
    geom_line() +
    labs(title = paste0("Imports from China: ", cat_title),
         subtitle = "Billions USD (12-month rolling sum)",
         caption = paste0("Source: UN Comtrade (Canada Reporter).\nData through ", last_date_txt, ".")) +
    scale_color_manual(values = custom_cols) + scale_linewidth_manual(values = custom_lws) + scale_linetype_manual(values = custom_lts) +
    scale_y_continuous(limits = c(0, lims_china[2]), position = "right", breaks = seq(0, lims_china[2], lims_china[3])) +
    scale_x_date(expand = c(0.05,0.05), breaks = seq(start_date, end_date, by = "1 year"), labels = date_format('%Y'), limits = c(start_date, end_date)) +
    theme_if_policystyle() +
    theme(legend.position = "none", plot.subtitle = element_text(vjust = 1.25), axis.title.x = element_blank(), axis.title.y = element_blank())
  
  p_china = customXAxis(p_china, minorticks = 11, skip = 1, center = F)
  
  # --- CREATE 2-CHART PANEL ---
  row_panel <- plot_grid(p_asean, p_china, align = 'h', rel_widths = c(1, 1), nrow = 1, ncol = 2)
  
  # --- EXPORT PANEL ---
  filename <- paste0("ca_imports_split_scale_", cat_type, ".png")
  ggsave(paste0(out_dir, "/", filename), row_panel, width = 8.5, height = 4.5, units = 'in', dpi = 800, bg = "white")
  
  cat(paste0("    Saved: ", filename, "\n"))
}

cat("Canada Split Scale Analysis Complete.\n")