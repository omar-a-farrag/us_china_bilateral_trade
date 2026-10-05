################################################################################
### U.S.-China De Minimis Proxy Analysis (Maximalist Sectors & Toggles)
### Sectors (16): 33, 39, 42, 48, 61, 62, 63, 64, 65, 71, 85, 90, 91, 94, 95, 96
### Outputs: Standalone Proxy, and Overlays for Total, Tariffed, Non-Tariffed
################################################################################
rm(list = ls())
suppressMessages(source("/if/appl/R/Functions/IFfunctions.r"))
library(readr); library(dplyr); library(zoo); library(policyPlot)
library(ggplot2); library(pracma); library(xts); library(tidyr); library(this.path)

# Load IF specs
attach(IFSPECS$COL)
attach(IFSPECS$LW)
attach(IFSPECS$LT)

setwd(dirname(this.path()))

# ==============================================================================
# 0. CONFIGURATION: SELECT LINES FOR OVERLAY CHART
# ==============================================================================
SHOW_CH_TOTAL <- TRUE
SHOW_CH_PROXY <- TRUE
SHOW_MX       <- FALSE
SHOW_VT       <- TRUE
SHOW_ASEAN    <- TRUE
SHOW_ASEAN_MX <- FALSE 
SHOW_EA       <- TRUE 
SHOW_CA       <- FALSE 

out_dir <- "/if/research-eme/omar/Eva/bilateralTrade/us/output/de_minimis_proxy"
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

# ==============================================================================
# 1. CALCULATE DE MINIMIS PROXY (16 Maximalist Sectors)
# ==============================================================================
cat("Calculating De Minimis Proxy from raw data...\n")

target_chapters <- c("33", "39", "42", "48", "61", "62", "63", "64", 
                     "65", "71", "85", "90", "91", "94", "95", "96")

# A. Raw US Imports
path_us_raw <- "../data/raw/imports/monthly/us_rep_hs10_values_ch.csv"
us_proxy <- read_csv(path_us_raw, show_col_types = FALSE, col_types = cols(commodity = col_character())) %>%
  filter(substr(commodity, 1, 2) %in% target_chapters) %>%
  group_by(date) %>% summarise(us_val = sum(usd, na.rm=TRUE))

# B. Raw China Exports
path_pt_raw <- "../data/raw/exports/monthly/comtrade_exports_to_us_ch.csv"
pt_proxy <- read_csv(path_pt_raw, show_col_types = FALSE, col_types = cols(cmdcode = col_character())) %>%
  rename(commodity = cmdcode, date_str = period, usd = primaryvalue) %>%
  mutate(usd = as.numeric(usd), date = ym(date_str)) %>%
  filter(substr(commodity, 1, 2) %in% target_chapters) %>%
  group_by(date) %>% summarise(pt_val = sum(usd, na.rm=TRUE))

# C. Calculate Proxy Difference (INVERTED: Partner Exports - US Imports)
df_proxy <- full_join(us_proxy, pt_proxy, by="date") %>%
  mutate(diff_raw = (pt_val - us_val) / 10^9, country = "ch_proxy") %>% 
  arrange(date) %>%
  mutate(diff_roll = rollsum(diff_raw, 12, align="right", fill=NA)) %>%
  filter(date >= "2016-01-01") %>%
  na.omit()

# ==============================================================================
# 2. CHART 1: STANDALONE PROXY
# ==============================================================================
cat("Generating Standalone Proxy Chart...\n")

start_date <- as.Date("2016-01-01")
end_date <- max(df_proxy$date)
last_date_txt <- format(end_date, "%B %Y")
lims_proxy <- limint(df_proxy$diff_roll, extraspace = 0)

p1 <- df_proxy %>%
  ggplot(aes(x=date, y=diff_roll)) +
  geom_line(color=IFRED, linewidth=IFTHICK, linetype="solid") +
  geom_hline(yintercept=0, color="black", linewidth=IFTHIN) +
  labs(title="Trade Reporting Difference: de minimis, US-China",
       subtitle="Billions USD (12-month rolling sum)",
       caption=paste0(" Direction: China-reported exports minus US-reported imports.\n",
                      " Note: de minimis proxy includes HS 33, 39, 42, 48, 61, 62, 63, 64, 65, 71, 85, 90, 91, 94, 95, 96.\n",
                      " Data through ", last_date_txt, ".\n Source:US HS Trade (Census) & UN Comtrade. " )) +
  scale_y_continuous(limits = c(lims_proxy[1], lims_proxy[2]), position = "right", 
                     sec.axis = dup_axis(labels = NULL), expand = c(0, 0), breaks = seq(lims_proxy[1], lims_proxy[2], lims_proxy[3])) +
  scale_x_date(expand = c(0.05,0.05), breaks = seq(start_date, end_date, by="2 years"), labels = date_format("%Y"), limits = c(start_date, end_date)) +
  theme_if_policystyle() +
  theme(plot.subtitle = element_text(vjust = 1.25), axis.title.x = element_blank(), axis.title.y = element_blank())

p1 <- customXAxis(p1, minorticks=11, skip=0, center=F)
ggsave(paste0(out_dir, "/ch_deminimis_proxy_standalone.png"), p1, width=6.5, height=4.5, dpi=800, bg="white")

# ==============================================================================
# 3 & 4. GENERATE MULTI-COUNTRY OVERLAYS (TOTAL, TARIFFED, NON-TARIFFED)
# ==============================================================================
categories <- c("total", "tariffs", "no_tariffs")
titles <- c("Total Goods", "Tariffed Goods", "Non-Tariffed Goods")
codes <- c("ch", "mx", "vt", "id", "ma", "ph", "th", "ea", "ca")

# Filter lines based on configuration
plot_lines <- c()
if(SHOW_CH_TOTAL) plot_lines <- c(plot_lines, "ch")
if(SHOW_CH_PROXY) plot_lines <- c(plot_lines, "ch_proxy")
if(SHOW_MX)       plot_lines <- c(plot_lines, "mx")
if(SHOW_VT)       plot_lines <- c(plot_lines, "vt")
if(SHOW_ASEAN)    plot_lines <- c(plot_lines, "asean")
if(SHOW_ASEAN_MX) plot_lines <- c(plot_lines, "asean_mx")
if(SHOW_EA)       plot_lines <- c(plot_lines, "ea")
if(SHOW_CA)       plot_lines <- c(plot_lines, "ca")

# Master style definitions
colors <- c("ch"=IFRED, "ch_proxy"=IFRED, "mx"=IFGREEN, "vt"=IFBLUE, "asean"=IFORANGE, "asean_mx"=IFPURPLE, "ea"="pink", "ca"=IFLIGHTBLUE)
labels <- c("ch"="China", "ch_proxy"="de minimis proxy", "mx"="Mexico", "vt"="Vietnam", "asean"="ASEAN-5", "asean_mx"="ASEAN-5 + Mexico", "ea"="Euro Area", "ca"="Canada")
widths <- c("ch"=IFTHICK, "ch_proxy"=IFXTHICK, "mx"=IFMEDIUM, "vt"=IFMEDIUM, "asean"=IFMEDIUM, "asean_mx"=IFTHICK, "ea"=IFMEDIUM, "ca"=IFMEDIUM)
types  <- c("ch"="solid", "ch_proxy"=IFDOTTED, "mx"="solid", "vt"="solid", "asean"=IFDASHED, "asean_mx"=IFDOTDASH, "ea"="solid", "ca"="solid")

active_colors <- colors[plot_lines]
active_labels <- labels[plot_lines]
active_widths <- widths[plot_lines]
active_types  <- types[plot_lines]

get_diff <- function(abbr, cat_type) {
  us_file <- paste0("../data/cleaned/imports/monthly/", cat_type, "_hs6_monthly_us_", abbr, ".csv")
  pt_file <- paste0("../data/cleaned/exports/monthly/", cat_type, "_hs6_monthly_us_", abbr, ".csv")
  if (!file.exists(us_file) || !file.exists(pt_file)) return(NULL)
  
  us <- read_csv(us_file, show_col_types=F) %>% dplyr::rename(us_val=value) %>% select(date, us_val)
  pt <- read_csv(pt_file, show_col_types=F) %>% dplyr::rename(pt_val=value) %>% select(date, pt_val)
  
  df <- full_join(us, pt, by="date") %>% mutate(diff = (us_val - pt_val) / 10^9) %>% select(date, diff)
  names(df)[2] <- abbr 
  return(df)
}

make_category_plot <- function(cat_type, cat_title, is_bottom_chart = FALSE) {
  cat("Generating Multiline Overlay for:", cat_title, "...\n")
  
  list_dfs <- lapply(codes, get_diff, cat_type = cat_type)
  list_dfs <- list_dfs[!sapply(list_dfs, is.null)]
  
  df_wide <- Reduce(function(x, y) full_join(x, y, by="date"), list_dfs) %>% arrange(date) %>%
    mutate(asean = rowSums(select(., any_of(c("id", "ma", "ph", "th", "vt"))), na.rm = TRUE),
           asean_mx = asean + if_else(is.na(mx), 0, mx))
  
  df_long_standard <- df_wide %>%
    select(date, any_of(c("ch", "mx", "vt", "asean", "asean_mx", "ea", "ca"))) %>%
    pivot_longer(cols = -date, names_to = "country", values_to = "diff_raw") %>%
    group_by(country) %>% arrange(date) %>%
    mutate(diff_roll = rollsum(diff_raw, 12, align="right", fill=NA)) %>%
    ungroup() %>% na.omit() %>% filter(date >= as.Date("2016-01-01"))
  
  df_long_combined <- bind_rows(df_long_standard, df_proxy %>% select(date, country, diff_roll))
  df_plot <- df_long_combined %>% filter(country %in% plot_lines)
  
  # Calculate limits and dates LOCALLY to prevent dropping rows
  local_start_date <- as.Date("2016-01-01")
  local_end_date <- max(df_plot$date, na.rm = TRUE)
  local_last_date_txt <- format(local_end_date, "%B %Y")
  lims_multi <- limint(df_plot$diff_roll, extraspace = 0)
  
  current_labels <- active_labels
  if("ch" %in% names(current_labels)) current_labels["ch"] <- paste0("China (", cat_title, ")")
  
  p_base <- df_plot %>%
    ggplot(aes(x=date, y=diff_roll, color=country, linewidth=country, linetype=country)) +
    geom_line() + geom_hline(yintercept=0, color="black", linewidth=IFTHIN) +
    labs(title=paste0("Trade Reporting Difference: ", cat_title),
         subtitle="Billions USD (12-month rolling sum)",
         caption=paste0("   Note: de minimis proxy includes HS 33, 39, 42, 48, 61, 62, 63, 64, 65, 71, 85, 90, 91, 94, 95, 96.\n",
                        " Proxy line is inverted (China Exports minus US Imports).The U.S. does not include de minimis trade in reporting to UN Comtrade, \n while China does. \n",
                        "   Source: US HS Trade (Census) & UN Comtrade. Data through ", local_last_date_txt, ".")) +
    scale_color_manual(breaks=plot_lines, values=active_colors, labels=current_labels) +
    scale_linewidth_manual(breaks=plot_lines, values=active_widths, labels=current_labels) +
    scale_linetype_manual(breaks=plot_lines, values=active_types, labels=current_labels) +
    scale_y_continuous(limits = c(lims_multi[1], lims_multi[2]), position = "right", sec.axis = dup_axis(labels = NULL), expand = c(0, 0), breaks = seq(lims_multi[1], lims_multi[2], lims_multi[3])) +
    scale_x_date(expand = c(0.05,0.05), breaks = seq(local_start_date, local_end_date, by="2 years"), labels = date_format("%Y"), limits = c(local_start_date, local_end_date)) +
    theme_if_policystyle() +
    theme(legend.position = c(0.02, 0.98), legend.justification = c("left", "top"), plot.subtitle = element_text(vjust = 1.25), axis.title.x = element_blank(), axis.title.y = element_blank()) +
    guides(color = guide_legend(ncol = 2), linewidth = guide_legend(ncol = 2), linetype = guide_legend(ncol = 2))
  
  # Save the individual chart fully formatted
  p_save <- customXAxis(p_base, minorticks=11, skip=0, center=F)
  file_suffix <- ifelse(cat_type == "total", "total", ifelse(cat_type == "tariffs", "tariffed", "notariff"))
  ggsave(paste0(out_dir, "/us_trade_difference_overlay_", file_suffix, ".png"), p_save, width=8, height=5, dpi=800, bg="white")
  
  # Return the properly formatted chart for the panel stack
  if(!is_bottom_chart) {
    # Skip customXAxis and just strip the axes via standard ggplot theme
    p_panel <- p_base + theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), plot.caption = element_blank())
    return(p_panel)
  } else {
    return(p_save)
  }
}

# Generate all three charts
plot_total     <- make_category_plot("total", "Total Goods", is_bottom_chart = FALSE)
plot_tariffs   <- make_category_plot("tariffs", "Tariffed Goods", is_bottom_chart = FALSE)
plot_notariffs <- make_category_plot("no_tariffs", "Non-Tariffed Goods", is_bottom_chart = TRUE)

# ==============================================================================
# 5. CREATE 3-PANEL VERTICAL EXHIBIT
# ==============================================================================
cat("Assembling Vertical Panel...\n")

col_panel <- plot_grid(
  plot_total, 
  plot_tariffs, 
  plot_notariffs, 
  align = 'v', 
  rel_heights = c(1, 1, 1.2), 
  nrow = 3, 
  ncol = 1
)

ggsave(paste0(out_dir, "/us_trade_difference_overlay_exhibit.png"), col_panel, width = 8.5, height = 12, units = 'in', dpi = 800, bg = "white")
cat("De Minimis Proxy Analysis & Panel Generation Complete!\n")