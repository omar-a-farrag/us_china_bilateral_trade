################################################################################
### Plotting Trade Differences (US Imports - Partner Exports)
### Output: Total, Tariffed, Non-Tariffed, and Panel
## Omar Farrag, IF Division - EME
## Created on: 01/22/2026; last edited: 01/22/2026
################################################################################

# ------------------------------------------------------------------------------
# VERIFY COUNTRIES OF INTEREST
# ------------------------------------------------------------------------------
country_map <- c(
  "ar" = "Argentina", "bz" = "Brazil", "ca" = "Canada", "cl" = "Chile",
  "ch" = "China", "co" = "Colombia", "ea" = "Euro Area", "ge" = "Germany", "hk" = "Hong Kong",
  "in" = "India", "id" = "Indonesia", "jn" = "Japan", "ma" = "Malaysia",
  "mx" = "Mexico", "ph" = "Philippines", "ko" = "South Korea", "si" = "Singapore",
  "ta" = "Taiwan", "th" = "Thailand", "uk" = "United Kingdom", "vt" = "Vietnam"
)

rm(list = setdiff(ls(), "country_map"))
suppressMessages(source("/if/appl/R/Functions/IFfunctions.r"))

library(readr); library(dplyr); library(zoo); library(policyPlot)
library(ggplot2); library(pracma); library(xts); library(sjmisc)
library(grid); library(tidyr); library(cowplot); library(this.path)

# Load IF specs
attach(IFSPECS$COL)
attach(IFSPECS$LT)
attach(IFSPECS$LW)

setwd(dirname(this.path()))

cat("Starting Trade Difference Batch...\n")

for (abbr in names(country_map)) {
  
  full_name <- country_map[[abbr]]
  
  # Define Paths
  path_us_imp_tot <- paste0("../data/cleaned/imports/monthly/total_hs6_monthly_us_", abbr, ".csv")
  path_us_imp_tar <- paste0("../data/cleaned/imports/monthly/tariffs_hs6_monthly_us_", abbr, ".csv")
  path_us_imp_no  <- paste0("../data/cleaned/imports/monthly/no_tariffs_hs6_monthly_us_", abbr, ".csv")
  
  path_pt_exp_tot <- paste0("../data/cleaned/exports/monthly/total_hs6_monthly_us_", abbr, ".csv")
  path_pt_exp_tar <- paste0("../data/cleaned/exports/monthly/tariffs_hs6_monthly_us_", abbr, ".csv")
  path_pt_exp_no  <- paste0("../data/cleaned/exports/monthly/no_tariffs_hs6_monthly_us_", abbr, ".csv")
  
  if (!file.exists(path_pt_exp_tot)) {
    cat(paste("SKIPPING:", full_name, "(File not found)\n"))
    next
  }
  
  cat(paste("Processing Difference:", full_name, "...\n"))
  
  # --- HELPER TO PROCESS DATA ---
  process_diff <- function(path_us, path_pt) {
    df_us <- read_csv(path_us, show_col_types = FALSE) %>% 
      mutate(value = value/(10^9)) %>% 
      rename(us_val = value)
    
    df_pt <- read_csv(path_pt, show_col_types = FALSE) %>% 
      mutate(value = value/(10^9)) %>% 
      rename(pt_val = value)
    
    # Full Join to keep all dates, compute difference
    df_merge <- full_join(df_us, df_pt, by = "date") %>%
      arrange(date) %>%
      mutate(
        us_val = rollsum(us_val, 12, align = "right", fill = NA),
        pt_val = rollsum(pt_val, 12, align = "right", fill = NA),
        # CALCULATION: US Imports - Partner Exports
        diff_val = us_val - pt_val
      ) %>%
      na.omit()
    
    return(df_merge)
  }
  
  # Process 3 Sets
  df_total <- process_diff(path_us_imp_tot, path_pt_exp_tot)
  df_tariff <- process_diff(path_us_imp_tar, path_pt_exp_tar)
  df_no_tar <- process_diff(path_us_imp_no, path_pt_exp_no)
  
  # Common Limits/Dates
  start_date <- as.Date("2016-01-01")
  end_date <- as.Date(max(df_total$date))
  last_date_txt <- format(end_date, "%b %Y")
  
  # --- GRAPHING FUNCTION ---
  plot_diff <- function(df, val_col, title_txt) {
    
    # Calculate limits for just this series
    lims <- limint(df[[val_col]], extraspace = 0)
    
    p <- df %>%
      ggplot(aes(x = date, y = .data[[val_col]])) +
      geom_line(color = IFBLUE, linewidth = IFTHICK) +
      geom_hline(yintercept = 0, color = "black", linewidth = IFTHIN) + # Zero line
      
      labs(title = paste0(title_txt, ": \nUS vs ", full_name),
           subtitle = "Difference (Billions USD)",
           caption = paste0(" Direction: US Imports minus Partner Exports.\n",
                            " Note: 12-month rolling sums. Data through ", last_date_txt, ".")) +
      
      scale_y_continuous(limits = c(lims[1], lims[2]), position = "right", 
                         breaks = seq(lims[1], lims[2], lims[3])) +
      scale_x_date(expand = c(0.05,0.05), breaks = seq(start_date, end_date, by = "2 year"), 
                   labels = date_format('%Y'), limits = c(start_date, end_date)) +
      
      theme_if_policystyle() +
      theme(plot.subtitle = element_text(vjust = 1.25),
            axis.title.x = element_blank(), axis.title.y = element_blank())
    
    return(customXAxis(p, minorticks = 11, skip = 0, center = F))
  }
  
  # Create Charts
  chart_tot <- plot_diff(df_total, "diff_val", "Total Goods Difference")
  chart_tar <- plot_diff(df_tariff, "diff_val", "Tariffed Goods Difference")
  chart_no  <- plot_diff(df_no_tar, "diff_val", "Non-Tariffed Goods Difference")
  
  # --- EXPORT ---
  root <- "/if/research-eme/omar/Eva/bilateralTrade/us"
  # Creates .../us/output/ar/trade_difference/
  out_dir <- paste0(root, "/output/", abbr, "/trade_difference")
  
  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
  
  # Save Individual Charts
  ggsave(paste0(out_dir, "/diff_total_", abbr, ".png"), chart_tot, width = 3.9, height = 3.5, units='in', dpi=800, bg="white")
  ggsave(paste0(out_dir, "/diff_tariff_", abbr, ".png"), chart_tar, width = 3.9, height = 3.5, units='in', dpi=800, bg="white")
  ggsave(paste0(out_dir, "/diff_notariff_", abbr, ".png"), chart_no, width = 3.9, height = 3.5, units='in', dpi=800, bg="white")
  
  # --- CREATE PANEL ---
  # Using custom setMargins and plot_grid logic
  panel <- setMargins(chart_tot, chart_tar, chart_no)
  
  row_panel <- plot_grid(
    panel[[1]], panel[[2]], panel[[3]], 
    align = 'h', 
    rel_widths = c(3.3, 3.3, 3.3), 
    nrow = 1, 
    ncol = 3
  )
  
  ggsave(paste0(out_dir, "/diff_panel_", abbr, ".png"), 
         row_panel, 
         width = 3.33*3, 
         height = 3.2 + 0.13*3 + 0.2, 
         units = 'in', 
         dpi = 800, 
         bg = "white"
  )
  
  cat(paste("Saved charts & panel for", full_name, "\n"))
}

cat("Trade Difference Script Complete.\n")