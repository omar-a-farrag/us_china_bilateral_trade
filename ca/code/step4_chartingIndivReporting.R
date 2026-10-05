################################################################################
### Comparing Tariffed vs. non-Tariffed Goods (Canada vs Partners)
### Automated for China and ASEAN-5
## Omar Farrag, IF Division - EME
################################################################################
### Sources ###
# Canada Imports: UN Comtrade (via API pull)
# Partner Exports: UN Comtrade (via API pull)
################################################################################

country_map <- c(
  "ch" = "China", "id" = "Indonesia", "ma" = "Malaysia",
  "ph" = "Philippines", "th" = "Thailand", "vt" = "Vietnam"
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

cat("Starting Canada Trade Graphing Batch...\n")

for (abbr in names(country_map)) {
  
  full_name <- country_map[[abbr]]
  
  # Define file paths dynamically
  path_ca_imp_tot <- paste0("../data/cleaned/imports/total_hs6_ca_imp_from_", abbr, ".csv")
  path_ca_imp_tar <- paste0("../data/cleaned/imports/tariffs_hs6_ca_imp_from_", abbr, ".csv")
  path_ca_imp_no  <- paste0("../data/cleaned/imports/no_tariffs_hs6_ca_imp_from_", abbr, ".csv")
  
  path_pt_exp_tot <- paste0("../data/cleaned/exports/total_hs6_partner_exp_to_ca_", abbr, ".csv")
  path_pt_exp_tar <- paste0("../data/cleaned/exports/tariffs_hs6_partner_exp_to_ca_", abbr, ".csv")
  path_pt_exp_no  <- paste0("../data/cleaned/exports/no_tariffs_hs6_partner_exp_to_ca_", abbr, ".csv")
  
  if (!file.exists(path_ca_imp_tot) || !file.exists(path_pt_exp_tot)) {
    cat(paste("SKIPPING:", full_name, "(Data file not found)\n")); next
  }
  
  cat(paste("Processing Charts for:", full_name, "...\n"))
  
  # --- HELPER TO PROCESS DATA ---
  process_data <- function(path_ca, path_pt) {
    df_ca <- read_csv(path_ca, show_col_types = FALSE) %>% 
      mutate(value = value/(10^9), country = "ca") %>% 
      rename(val_6 = value) %>%
      mutate(val_6 = rollsum(val_6, 12, align = "right", fill = NA))
    
    df_pt <- read_csv(path_pt, show_col_types = FALSE) %>% 
      mutate(value = value/(10^9), country = abbr) %>% 
      rename(val_6 = value) %>%
      mutate(val_6 = rollsum(val_6, 12, align = "right", fill = NA))
    
    return(bind_rows(df_ca, df_pt) %>% na.omit())
  }
  
  df_total <- process_data(path_ca_imp_tot, path_pt_exp_tot)
  df_tariff <- process_data(path_ca_imp_tar, path_pt_exp_tar)
  df_no_tar <- process_data(path_ca_imp_no, path_pt_exp_no)
  
  # --- GRAPHING SETUP ---
  start_date <- as.Date("2016-01-01")
  end_date <- as.Date(max(df_total$date))
  
  series_names <- c("ca", abbr)
  label_names  <- c("Canada-reported imports", paste0(full_name, "-reported exports"))
  
  colors = c(IFBLUE, IFRED)
  linewidths <- c(IFMEDIUM, IFTHICK)
  linetypes = c("solid", IFDOTTED)
  
  last_date_txt <- format(end_date, "%b %Y")
  
  # --- GRAPHING FUNCTION ---
  plot_trade <- function(df, title_txt) {
    lims <- limint(df$val_6, extraspace = 0)
    
    p <- df %>%
      ggplot(aes(x = date, y = val_6, colour = country, linetype = country, linewidth = country)) + 
      geom_line() + 
      labs(title = paste(title_txt, ": \nCanada - ", full_name, sep=""),
           subtitle = "Billions USD",
           caption = paste0("  Note: values are 12-month rolling sums. \nData through ", last_date_txt, ".",
                            "\n Goods matched at HS 6-digit level. \n  Source: UN Comtrade.")) +
      scale_color_manual(breaks = series_names, values = setNames(colors, series_names), labels = setNames(label_names,series_names))+  
      scale_linewidth_manual(breaks = series_names, values = setNames(linewidths, series_names), labels = setNames(label_names,series_names)) +
      scale_linetype_manual(breaks = series_names, values =setNames(linetypes, series_names), labels = setNames(label_names,series_names)) +
      scale_y_continuous(limits = c(lims[1], lims[2]), position = "right", expand = c(0, 0), breaks = seq(lims[1], lims[2],  lims[3])) +
      scale_x_date(expand = c(0.05,0.05), breaks = seq(start_date, end_date, by = "2 year"), labels = date_format('%Y'), limits = c(start_date, end_date)) +
      theme_if_policystyle() +
      theme(legend.justification = c("left", "top"), legend.position = c(0.01,0.99),
            plot.subtitle = element_text(vjust = 1.25), axis.title.x = element_blank(), axis.title.y = element_blank())
    
    return(customXAxis(p, minorticks = 11, skip = 0, center = F))
  }
  
  # Create Charts
  chart_tot <- plot_trade(df_total, "Total Goods")
  chart_tar <- plot_trade(df_tariff, "Tariffed Goods")
  chart_no  <- plot_trade(df_no_tar, "Non-tariffed Goods")
  
  # --- EXPORT ---
  root <- "/if/research-eme/omar/Eva/bilateralTrade/ca"
  out_dir <- paste0(root, "/output/", abbr)
  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
  
  # Save Individual Charts
  ggsave(paste0(out_dir, "/ca_", abbr, "_trade_total.png"), chart_tot, width = 3.9, height = (3.2 + 0.13*5), units='in', dpi=800, bg="white")
  ggsave(paste0(out_dir, "/ca_", abbr, "_trade_tariffed.png"), chart_tar, width = 3.9, height = (3.2 + 0.13*5), units='in', dpi=800, bg="white")
  ggsave(paste0(out_dir, "/ca_", abbr, "_trade_notariff.png"), chart_no, width = 3.9, height = (3.2 + 0.13*5), units='in', dpi=800, bg="white")
  
  # --- CREATE PANEL ---
  panel <- setMargins(chart_tot, chart_tar, chart_no)
  row_panel <- plot_grid(panel[[1]], panel[[2]], panel[[3]], align = 'h', rel_widths = c(3.3,3.3,3.3), nrow = 1, ncol = 3)
  
  ggsave(paste0(out_dir, "/ca_", abbr, "_trade_panel.png"), row_panel, width = 3.33*4, height = 3.2*1.2 + 0.13*2 + 0.2, units = 'in', dpi = 800, bg = "white")
}

cat("Canada Graphing Complete.\n")