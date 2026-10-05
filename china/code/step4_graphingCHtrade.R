################################################################################
### Comparing Tariffed vs. non-Tariffed Goods (China vs Partners)
### Automated for Multiple Countries
## Omar Farrag, IF Division - EME
# Created on: 12-23-2025 ; Last edited on: 12-23-2025
################################################################################
### Sources ###
# China Exports: UN Comtrade (via API pull)
# Partner Imports: UN Comtrade (via API pull)
################################################################################

# ------------------------------------------------------------------------------
# VERIFY COUNTRIES OF INTEREST HERE
# ------------------------------------------------------------------------------
# Note: "ch" (China) is removed as it is the anchor. "us" is added.
country_map <- c(
  "ar" = "Argentina",
  "bz" = "Brazil",
  "ca" = "Canada",
  "cl" = "Chile",
  "co" = "Colombia",
  "ge" = "Germany",
  "hk" = "Hong Kong",
  "in" = "India",
  "id" = "Indonesia",
  "jn" = "Japan",
  "ma" = "Malaysia",
  "mx" = "Mexico",
  "ph" = "Philippines",
  "ko" = "South Korea",
  "si" = "Singapore",
  "ta" = "Taiwan",
  "th" = "Thailand",
  "uk" = "United Kingdom",
  "us" = "United States",
  "vt" = "Vietnam"
)
# ------------------------------------------------------------------------------

rm(list = setdiff(ls(), "country_map")) # Clear env but keep the map
suppressMessages(source("/if/appl/R/Functions/IFfunctions.r"))

# load in libraries
library(readr)
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
library(cowplot)

#load in IF colors, linetypes, and linewidths
attach(IFSPECS$COL)
attach(IFSPECS$LT)
attach(IFSPECS$LW)

require(this.path)
setwd(dirname(this.path()))

################################################################################
### START LOOP
################################################################################

for (abbr in names(country_map)) {
  
  full_name <- country_map[[abbr]]
  
  # Define file paths dynamically (Matching previous cleaning scripts)
  # China Exports TO Partner (Reporter: China)
  path_ch_exp_tot <- paste0("../data/cleaned/exports/monthly/total_hs6_monthly_ch_to_", abbr, ".csv")
  path_ch_exp_tar <- paste0("../data/cleaned/exports/monthly/tariffs_hs6_monthly_ch_to_", abbr, ".csv")
  path_ch_exp_no  <- paste0("../data/cleaned/exports/monthly/no_tariffs_hs6_monthly_ch_to_", abbr, ".csv")
  
  # Partner Imports FROM China (Reporter: Partner)
  path_ptr_imp_tot <- paste0("../data/cleaned/imports/monthly/total_hs6_monthly_", abbr, "_from_ch.csv")
  path_ptr_imp_tar <- paste0("../data/cleaned/imports/monthly/tariffs_hs6_monthly_", abbr, "_from_ch.csv")
  path_ptr_imp_no  <- paste0("../data/cleaned/imports/monthly/no_tariffs_hs6_monthly_", abbr, "_from_ch.csv")
  
  # SAFETY CHECK: If files don't exist, skip iteration
  if (!file.exists(path_ch_exp_tot)) {
    cat(paste("SKIPPING:", full_name, "(China Export file not found)\n"))
    next
  }
  if (!file.exists(path_ptr_imp_tot)) {
    cat(paste("SKIPPING:", full_name, "(Partner Import file not found)\n"))
    next
  }
  
  cat(paste("Processing:", full_name, "...\n"))
  
  ##############################################################################
  ### Data Pull & Prep
  ##############################################################################
  
  ## China Exports (Reporter: China) ##
  ch_export_total <- read_csv(path_ch_exp_tot, show_col_types = FALSE) %>%
    mutate(value = value/(10^9), country = "ch")
  
  ch_export_tariffed <- read_csv(path_ch_exp_tar, show_col_types = FALSE) %>%
    mutate(value = value/(10^9), country = "ch")
  
  ch_export_notariff <- read_csv(path_ch_exp_no, show_col_types = FALSE) %>%
    mutate(value = value/(10^9), country = "ch")
  
  ## Partner Imports (Reporter: Partner) ##
  partner_import_total <- read_csv(path_ptr_imp_tot, show_col_types = FALSE) %>%
    mutate(value = value/(10^9), country = abbr)
  
  partner_import_tariffed <- read_csv(path_ptr_imp_tar, show_col_types = FALSE) %>%
    mutate(value = value/(10^9), country = abbr)
  
  partner_import_notariff <- read_csv(path_ptr_imp_no, show_col_types = FALSE) %>%
    mutate(value = value/(10^9), country = abbr)
  
  ##############################################################################
  ### Data Cleaning (Rolling Sums)
  ##############################################################################
  
  ## Process China Data ##
  ch_total <- ch_export_total %>%
    mutate(value = rollsum(value, 12, align = "right", fill = NA)) %>%
    rename(value_total_6 = value)
  
  ch_tariffed <- ch_export_tariffed %>%
    mutate(value = rollsum(value, 12, align = "right", fill = NA)) %>%
    rename(value_tariffed_6 = value)
  
  ch_notariff <- ch_export_notariff %>%
    mutate(value = rollsum(value, 12, align = "right", fill = NA)) %>%
    rename(value_notariff_6 = value)
  
  ## Process Partner Data ##
  partner_total <- partner_import_total %>%
    mutate(value = rollsum(value, 12, align = "right", fill = NA)) %>%
    rename(value_total_6 = value)
  
  partner_tariffed <- partner_import_tariffed %>%
    mutate(value = rollsum(value, 12, align = "right", fill = NA)) %>%
    rename(value_tariffed_6 = value)
  
  partner_notariff <- partner_import_notariff %>%
    mutate(value = rollsum(value, 12, align = "right", fill = NA)) %>%
    rename(value_notariff_6 = value)
  
  ## Merge ##
  df_total <- bind_rows(ch_total, partner_total) %>% na.omit()
  df_tariffed <- bind_rows(ch_tariffed, partner_tariffed) %>% na.omit()
  df_notariff <- bind_rows(ch_notariff, partner_notariff) %>% na.omit()
  
  ##############################################################################
  ### Graphing Setup
  ##############################################################################
  
  start_date <- as.Date("2016-01-01")
  
  # Handle cases where data might end early
  if (nrow(df_total) > 0) {
    end_date <- as.Date(max(df_total$date))
  } else {
    cat(paste("  Warning: No overlapping data for", full_name, "- Skipping Graph.\n"))
    next
  }
  
  # Dynamic Limits
  limit_total <- limint(df_total$value_total_6, extraspace = 0)
  limit_tariffed <- limint(df_tariffed$value_tariffed_6, extraspace = 0)
  limit_notariff <- limint(df_notariff$value_notariff_6, extraspace = 0)
  
  # Dynamic Legend Labels
  series_names <- c("ch", abbr)
  label_names  <- c("China-reported exports", paste0(full_name, "-reported imports"))
  
  # Captions
  last_date_total_ch <- format(as.Date(max(ch_total$date)), "%b %Y")
  last_date_tariffed_ch <- format(as.Date(max(ch_tariffed$date)), "%b %Y")
  last_date_notariff_ch <- format(as.Date(max(ch_notariff$date)), "%b %Y")
  
  last_date_total_partner <- format(as.Date(max(partner_total$date)), "%b %Y")
  last_date_tariffed_partner <- format(as.Date(max(partner_tariffed$date)), "%b %Y")
  last_date_notariff_partner <- format(as.Date(max(partner_notariff$date)), "%b %Y")
  
  colors = c(IFRED, IFBLUE)
  linewidths <- c(IFTHICK, IFMEDIUM)
  linetypes = c(IFDOTTED, "solid")
  
  partner_abb <- toupper(abbr)
  
  ##############################################################################
  ### Graph 1: Total
  ##############################################################################
  total_chart <- df_total %>%
    ggplot(data=., aes(x = date, y = value_total_6, colour = country, linetype = country, linewidth = country)) + 
    geom_line() + 
    labs(title = paste("Total Goods: \nChina - ", full_name, sep=""),
         subtitle = "Billions USD",
         caption = paste0(" Note: values are 12-month rolling sums. Data through \n", 
                          last_date_total_ch, " for China. Data through ", last_date_total_partner, " for ", 
                          partner_abb,". \nGoods matched at HS 6-digit level.",
                          "\n Source: UN Comtrade (Both Reporters).", sep="")) +
    scale_color_manual(breaks = series_names, values = setNames(colors, series_names), labels = setNames(label_names,series_names))+  
    scale_linewidth_manual(breaks = series_names, values = setNames(linewidths, series_names), labels = setNames(label_names,series_names)) +
    scale_linetype_manual(breaks = series_names, values =setNames(linetypes, series_names), labels = setNames(label_names,series_names)) +
    scale_y_continuous(limits = c(limit_total[1], limit_total[2]), position = "right", sec.axis = dup_axis(labels = NULL), expand = c(0, 0), breaks = seq(limit_total[1], limit_total[2],  limit_total[3])) +
    scale_x_date(expand = c(0.05,0.05), breaks = seq(start_date, end_date, by = "2 year"), labels = date_format('%Y'), limits = c(start_date, end_date)) +
    theme_if_policystyle() +
    theme(legend.justification = c("left", "top"), legend.position = c(0.01,1.02),
          plot.subtitle = element_text(vjust = 1.25), axis.title.x = element_blank(), axis.text.x = element_text(angle = 0), axis.title.y = element_blank())
  
  total_chart = customXAxis(total_chart, minorticks = 11, skip = 0, center = F)
  
  ##############################################################################
  ### Graph 2: Tariffed
  ##############################################################################
  tariffed_chart <- df_tariffed %>%
    ggplot(data=., aes(x = date, y = value_tariffed_6, colour = country, linetype = country, linewidth = country)) + 
    geom_line() + 
    labs(title = paste("Tariffed Goods: \nChina - ", full_name, sep=""),
         subtitle = "Billions USD",
         caption = paste0(" Note: values are 12-month rolling sums. Data through \n", 
                          last_date_tariffed_ch, " for China. Data through ", last_date_tariffed_partner, " for ", 
                          partner_abb,". \nGoods matched at HS 6-digit level.",
                          "\n Source: UN Comtrade (Both Reporters).", sep="")) +
    scale_color_manual(breaks = series_names, values = setNames(colors, series_names), labels = setNames(label_names,series_names))+  
    scale_linewidth_manual(breaks = series_names, values = setNames(linewidths, series_names), labels = setNames(label_names,series_names)) +
    scale_linetype_manual(breaks = series_names, values =setNames(linetypes, series_names), labels = setNames(label_names,series_names)) +
    scale_y_continuous(limits = c(limit_tariffed[1], limit_tariffed[2]), position = "right", sec.axis = dup_axis(labels = NULL), expand = c(0, 0), breaks = seq(limit_tariffed[1], limit_tariffed[2],  limit_tariffed[3])) +
    scale_x_date(expand = c(0.05,0.05), breaks = seq(start_date, end_date, by = "2 year"), labels = date_format('%Y'), limits = c(start_date, end_date)) +
    theme_if_policystyle() +
    theme(legend.justification = c("left", "top"), legend.position = c(0.01,1.02),
          plot.subtitle = element_text(vjust = 1.25), axis.title.x = element_blank(), axis.text.x = element_text(angle = 0), axis.title.y = element_blank())
  
  tariffed_chart = customXAxis(tariffed_chart, minorticks = 11, skip = 0, center = F)
  
  ##############################################################################
  ### Graph 3: Non-Tariffed
  ##############################################################################
  notariff_chart <- df_notariff %>%
    ggplot(data=., aes(x = date, y = value_notariff_6, colour = country, linetype = country, linewidth = country)) + 
    geom_line() + 
    labs(title = paste("Non-tariffed Goods: \nChina - ", full_name, sep=""),
         subtitle = "Billions USD",
         caption = paste0(" Note: values are 12-month rolling sums. Data through \n", 
                          last_date_notariff_ch, " for China. Data through ", last_date_notariff_partner, " for ", 
                          partner_abb,". \nGoods matched at HS 6-digit level.",
                          "\n Source: UN Comtrade (Both Reporters).", sep="")) +
    scale_color_manual(breaks = series_names, values = setNames(colors, series_names), labels = setNames(label_names,series_names))+  
    scale_linewidth_manual(breaks = series_names, values = setNames(linewidths, series_names), labels = setNames(label_names,series_names)) +
    scale_linetype_manual(breaks = series_names, values =setNames(linetypes, series_names), labels = setNames(label_names,series_names)) +
    scale_y_continuous(limits = c(limit_notariff[1], limit_notariff[2]), position = "right", sec.axis = dup_axis(labels = NULL), expand = c(0, 0), breaks = seq(limit_notariff[1], limit_notariff[2],  limit_notariff[3])) +
    scale_x_date(expand = c(0.05,0.05), breaks = seq(start_date, end_date, by = "2 year"), labels = date_format('%Y'), limits = c(start_date, end_date)) +
    theme_if_policystyle() +
    theme(legend.justification = c("left", "top"), legend.position = c(0.01,1.02),
          plot.subtitle = element_text(vjust = 1.25), axis.title.x = element_blank(), axis.text.x = element_text(angle = 0), axis.title.y = element_blank())
  
  notariff_chart = customXAxis(notariff_chart, minorticks = 11, skip = 0, center = F)
  
  ##############################################################################
  ### Exporting
  ##############################################################################
  # Dynamic root folder creation (Updated to 'china' folder)
  root <- "/if/research-eme/omar/Eva/bilateralTrade/china"
  output_folder_name <- paste0(root, "/output/", abbr) 
  
  if (!dir.exists(output_folder_name)) {
    dir.create(output_folder_name, recursive = TRUE)
  }
  
  # Export Individuals
  ggsave(paste0(output_folder_name, "/ch_", abbr, "_trade_total.png"), total_chart, width = 3.9, height = (3.3 + 0.13*4), units='in', dpi=800, bg="white")
  ggsave(paste0(output_folder_name, "/ch_", abbr, "_trade_tariffed.png"), tariffed_chart, width = 3.9, height = (3.3 + 0.13*4), units='in', dpi=800, bg="white")
  ggsave(paste0(output_folder_name, "/ch_", abbr, "_trade_notariff.png"), notariff_chart, width = 3.9, height = (3.3 + 0.13*4), units='in', dpi=800, bg="white")
  
  # Export Panel
  panel <- setMargins(total_chart, tariffed_chart, notariff_chart)
  row1_panel <- plot_grid(panel[[1]], panel[[2]], panel[[3]], align = 'h', rel_widths = c(3.3,3.3,3.3), nrow = 1, ncol = 3)
  
  ggsave(paste0(output_folder_name, "/ch_", abbr, "_trade_panel.png"), row1_panel, width = 3.33*3, height = 3.3 + 0.13*4 + 0.2, units = 'in', dpi = 800, bg = "white")
  
  cat(paste("Done with", full_name, "\n"))
}

cat("-----------------------------------------------------------------\n")
cat("Batch Graphing Complete.\n")