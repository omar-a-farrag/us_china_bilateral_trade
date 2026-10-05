################################################################################
### De Minimis Levels & US-China Gap Reconstruction
### Chart 1: De Minimis Proxy vs. NBER Estimates & Official Parcels
### Chart 2-4: 2x2 Panel Gap Reconstructions (Proxy, Parcels, NBER)
################################################################################
rm(list = ls())
suppressMessages(source("/if/appl/R/Functions/IFfunctions.r"))
library(readr); library(readxl); library(dplyr); library(zoo); library(policyPlot)
library(ggplot2); library(pracma); library(xts); library(tidyr); library(cowplot); library(this.path)

# Load IF specs
attach(IFSPECS$COL)
attach(IFSPECS$LW)
attach(IFSPECS$LT)

setwd(dirname(this.path()))

# ==============================================================================
# 0. CONFIGURATION FLAGS
# ==============================================================================
MAKE_ANNUAL <- TRUE          # If TRUE: Aggregate to calendar years. If FALSE: 12-month rolling sum.
NORMALIZE_TARIFF_GAP <- FALSE # If TRUE: Subtract 40 billion from the tariff gap.

# Dynamic Output Directory based on toggles
sub_dir <- ifelse(MAKE_ANNUAL, "annual", "monthly")
if(NORMALIZE_TARIFF_GAP) sub_dir <- paste0(sub_dir, "_norm_gap")
out_dir <- paste0("/if/research-eme/omar/Eva/bilateralTrade/us/output/deminimis_levels/", sub_dir)

if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

# ==============================================================================
# 1. LOAD MONTHLY COMPONENTS
# ==============================================================================
cat("Loading components for Gap Reconstruction...\n")

# A. De Minimis Proxy (16 Chapters)
target_chapters <- c("33", "39", "42", "48", "61", "62", "63", "64", 
                     "65", "71", "85", "90", "91", "94", "95", "96")

us_raw <- read_csv("../data/raw/imports/monthly/us_rep_hs10_values_ch.csv", show_col_types = F, col_types = cols(commodity = col_character())) %>%
  filter(substr(commodity, 1, 2) %in% target_chapters) %>% group_by(date) %>% summarise(us_proxy_val = sum(usd, na.rm=T))
pt_raw <- read_csv("../data/raw/exports/monthly/comtrade_exports_to_us_ch.csv", show_col_types = F, col_types = cols(cmdcode = col_character())) %>%
  rename(commodity = cmdcode, date_str = period, usd = primaryvalue) %>% mutate(usd = as.numeric(usd), date = ym(date_str)) %>%
  filter(substr(commodity, 1, 2) %in% target_chapters) %>% group_by(date) %>% summarise(ch_proxy_val = sum(usd, na.rm=T))
df_proxy <- full_join(us_raw, pt_raw, by="date") %>%
  mutate(proxy_gap = abs(ch_proxy_val - us_proxy_val) / 10^9) %>% select(date, proxy_gap)

# B. Total Goods
us_tot <- read_csv("../data/cleaned/imports/monthly/total_hs6_monthly_us_ch.csv", show_col_types=F) %>% rename(us_tot_val=value) %>% select(date, us_tot_val)
ch_tot <- read_csv("../data/cleaned/exports/monthly/total_hs6_monthly_us_ch.csv", show_col_types=F) %>% rename(ch_tot_val=value) %>% select(date, ch_tot_val)
df_total <- full_join(us_tot, ch_tot, by="date") %>%
  mutate(us_tot_b = us_tot_val / 10^9, ch_tot_b = ch_tot_val / 10^9) %>% select(date, us_tot_b, ch_tot_b)

# C. Tariffed Goods Gap
us_tar <- read_csv("../data/cleaned/imports/monthly/tariffs_hs6_monthly_us_ch.csv", show_col_types=F) %>% rename(us_tar_val=value) %>% select(date, us_tar_val)
ch_tar <- read_csv("../data/cleaned/exports/monthly/tariffs_hs6_monthly_us_ch.csv", show_col_types=F) %>% rename(ch_tar_val=value) %>% select(date, ch_tar_val)
df_tariff <- full_join(us_tar, ch_tar, by="date") %>%
  mutate(tariff_gap = abs(ch_tar_val - us_tar_val) / 10^9) %>% select(date, tariff_gap)

# D. NBER Estimates
df_nber <- tibble::tribble(
  ~year, ~nber_val,
  2013, 0.07, 2014, 0.7, 2015, 1.6, 2016, 9.2, 2017, 13,
  2018, 29.2, 2019, 56.2, 2020, 67.0, 2021, 43.5, 2022, 46.5, 2023, 54.5, 2024, 47.8
)

# E. US Small Parcels XLSX
df_parcel <- read_excel("small_parcels.xlsx") %>%
  mutate(date = as.Date(date), parcel_b = as.numeric(US) / 1000) %>%
  select(date, parcel_b)

# ==============================================================================
# 2. AGGREGATION & NORMALIZATION
# ==============================================================================
df_raw <- df_total %>% 
  full_join(df_proxy, by="date") %>% 
  full_join(df_tariff, by="date") %>% 
  full_join(df_parcel, by="date") %>%
  filter(date >= as.Date("2016-01-01")) %>% arrange(date)

if (MAKE_ANNUAL) {
  cat("Mode: ANNUAL (Summing calendar months)...\n")
  df_all <- df_raw %>%
    mutate(year = as.numeric(format(date, "%Y"))) %>%
    group_by(year) %>%
    summarise(
      us_tot_val = sum(us_tot_b, na.rm=TRUE),
      ch_tot_val = sum(ch_tot_b, na.rm=TRUE),
      proxy_val  = sum(proxy_gap, na.rm=TRUE),
      tariff_val = sum(tariff_gap, na.rm=TRUE),
      parcel_val = sum(parcel_b, na.rm=TRUE),
      .groups = "drop"
    ) %>%
    mutate(date = as.Date(paste0(year, "-01-01"))) %>%
    left_join(df_nber, by="year") %>% rename(nber_interp = nber_val)
} else {
  cat("Mode: MONTHLY (12-month rolling sums)...\n")
  df_all <- df_raw %>%
    mutate(
      us_tot_val = rollsum(us_tot_b, 12, align="right", fill=NA),
      ch_tot_val = rollsum(ch_tot_b, 12, align="right", fill=NA),
      proxy_val  = rollsum(proxy_gap, 12, align="right", fill=NA),
      tariff_val = rollsum(tariff_gap, 12, align="right", fill=NA),
      parcel_val = rollsum(parcel_b, 12, align="right", fill=NA)
    ) %>%
    mutate(year = as.numeric(format(date, "%Y")), month = as.numeric(format(date, "%m"))) %>%
    left_join(df_nber %>% mutate(date = as.Date(paste0(year, "-12-01"))), by=c("date", "year")) %>%
    mutate(
      nber_interp = na.approx(nber_val, na.rm = FALSE),
      nber_interp = na.locf(na.locf(nber_interp, na.rm=FALSE), fromLast=TRUE, na.rm=FALSE)
    )
}

# Normalization Step
if (NORMALIZE_TARIFF_GAP) {
  cat("Mode: TARIFF NORMALIZATION (Subtracting 40B)...\n")
  df_all <- df_all %>% mutate(tariff_val = tariff_val - 40)
}

# Final Additions (Separated independently for the 2x2 grid)
df_all <- df_all %>%
  filter(!is.na(us_tot_val) & !is.na(ch_tot_val)) %>% 
  mutate(
    # The Independent Tariff Addition (Appears in Panel 3 alongside Proxy)
    adj_tariff = us_tot_val + tariff_val,
    
    # The Explanatory Waterfall Additions
    adj_proxy_only  = us_tot_val + proxy_val,
    adj_proxy_both  = adj_proxy_only + tariff_val,
    
    adj_parcel_only = us_tot_val + parcel_val,
    adj_parcel_both = adj_parcel_only + tariff_val,
    
    adj_nber_only   = us_tot_val + nber_interp,
    adj_nber_both   = adj_nber_only + tariff_val
  )

# Global Chart Parameters
local_start_date <- min(df_all$date, na.rm = TRUE)
local_end_date <- max(df_all$date, na.rm = TRUE)
local_last_date_txt <- format(local_end_date, "%B %Y")

time_subtitle <- ifelse(MAKE_ANNUAL, "Billions USD (Annual Total)", "Billions USD (12-month rolling sum)")
caption_norm <- ifelse(NORMALIZE_TARIFF_GAP, " Tariffed gap is normalized (shifted by -40B USD).", "")
caption_text <- paste0(" Source: US HS Trade, UN Comtrade, NBER, Bernardo.", caption_norm, "\n Data through ", local_last_date_txt, ".")

# Creates a visually identical blank space to align captions perfectly
blank_caption <- gsub("[^\n]", " ", caption_text) 

nber_points <- df_nber %>% filter(year >= 2016) %>%
  mutate(date = if(MAKE_ANNUAL) as.Date(paste0(year, "-01-01")) else as.Date(paste0(year, "-12-01")))

# ==============================================================================
# 3. CHART 1: DE MINIMIS LEVELS
# ==============================================================================
cat("Generating De Minimis Levels Chart...\n")

df_chart1 <- df_all %>% select(date, proxy_val, parcel_val) %>%
  pivot_longer(cols = c(proxy_val, parcel_val), names_to = "series", values_to = "value") %>% na.omit()

lims_lvl <- limint(c(df_chart1$value, nber_points$nber_val), extraspace = 0)
if(lims_lvl[1] > 0) lims_lvl[1] <- 0

colors1 <- c("proxy_val"=IFRED, "parcel_val"=IFBLUE, "nber"="gray40")
labels1 <- c("proxy_val"="De Minimis Proxy (16 Chapters)", "parcel_val"="Bernardo Small Parcels", "nber"="Fajgelbaum & Khandelwal Est.")
widths1 <- c("proxy_val"=IFTHICK, "parcel_val"=IFTHICK, "nber"=IFTHIN)
types1  <- c("proxy_val"="solid", "parcel_val"="solid", "nber"=IFDASHED)

p_levels <- ggplot() +
  geom_line(data=df_chart1, aes(x=date, y=value, color=series, linetype=series, linewidth=series)) +
  geom_line(data=nber_points, aes(x=date, y=nber_val, color="nber", linetype="nber", linewidth="nber")) +
  geom_point(data=nber_points, aes(x=date, y=nber_val, color="nber"), size=2) +
  geom_hline(yintercept=0, color="black", linewidth=IFTHIN) +
  labs(title="De Minimis Trade Estimates (Value in Levels)", subtitle=time_subtitle, caption=caption_text) +
  scale_color_manual(values=colors1, labels=labels1) +
  scale_linewidth_manual(values=widths1, labels=labels1) +
  scale_linetype_manual(values=types1, labels=labels1) +
  scale_y_continuous(limits = c(lims_lvl[1], lims_lvl[2]), position = "right", sec.axis = dup_axis(labels = NULL), expand = c(0, 0), breaks = seq(lims_lvl[1], lims_lvl[2], lims_lvl[3])) +
  scale_x_date(expand = c(0.05,0.05), breaks = seq(local_start_date, local_end_date, by="2 years"), labels = date_format("%Y"), limits = c(local_start_date, local_end_date)) +
  theme_if_policystyle() +
  theme(legend.position = c(0.02, 0.98), legend.justification = c("left", "top"),
        plot.subtitle = element_text(vjust = 1.25), axis.title.x = element_blank(), axis.title.y = element_blank()) +
  guides(color = guide_legend(nrow = 3), linewidth = guide_legend(nrow = 3), linetype = guide_legend(nrow = 3))

p_levels_save <- customXAxis(p_levels, minorticks=11, skip=0, center=F)
ggsave(paste0(out_dir, "/ch1_deminimis_levels.png"), p_levels_save, width=8, height=5, dpi=800, bg="white")

# ==============================================================================
# 4. CHARTS 2, 3 & 4: 2x2 PANEL RECONSTRUCTIONS
# ==============================================================================
cat("Generating 2x2 Panel Reconstruction Exhibits...\n")

generate_reconstruction_panel <- function(adj1_col, adj2_col, adj1_label, filename_suffix) {
  
  max_val <- max(c(df_all$us_tot_val, df_all$ch_tot_val, df_all[[adj2_col]]), na.rm = TRUE)
  min_val <- min(c(0, df_all$us_tot_val, df_all$ch_tot_val, df_all[[adj2_col]]), na.rm = TRUE)
  lims_pan <- limint(c(min_val, max_val), extraspace = 0)
  
  # Set up 5 variables to cleanly route the colors and legend formatting
  colors2 <- c("us"=IFBLUE, "ch"=IFRED, "adj_proxy"=IFPURPLE, "adj_tariff"=IFGREEN, "adj_both"=IFORANGE)
  labels2 <- c("us"="US-Reported Imports", "ch"="China-Reported Exports", "adj_proxy"=adj1_label, "adj_tariff"="US + Tariffed Goods Gap", "adj_both"="US + Both Factors")
  widths2 <- c("us"=IFTHICK, "ch"=IFTHICK, "adj_proxy"=IFTHICK, "adj_tariff"=IFTHICK, "adj_both"=IFTHICK)
  types2  <- c("us"="solid", "ch"="solid", "adj_proxy"=IFDASHED, "adj_tariff"=IFDOTTED, "adj_both"="longdash")
  
  make_panel <- function(data_subset, breaks_to_show, title_text, cap_text = NULL) {
    p <- ggplot(data_subset, aes(x=date, y=value, color=series, linetype=series, linewidth=series)) +
      geom_line() + geom_hline(yintercept=0, color="black", linewidth=IFTHIN) +
      labs(title=title_text) +
      scale_color_manual(breaks=breaks_to_show, values=colors2[breaks_to_show], labels=labels2[breaks_to_show]) +
      scale_linewidth_manual(breaks=breaks_to_show, values=widths2[breaks_to_show], labels=labels2[breaks_to_show]) +
      scale_linetype_manual(breaks=breaks_to_show, values=types2[breaks_to_show], labels=labels2[breaks_to_show]) +
      scale_y_continuous(limits = c(lims_pan[1], lims_pan[2]), position = "right", sec.axis = dup_axis(labels = NULL), expand = c(0, 0), breaks = seq(lims_pan[1], lims_pan[2], lims_pan[3])) +
      scale_x_date(expand = c(0.05,0.05), breaks = seq(local_start_date, local_end_date, by="2 years"), labels = date_format("%Y"), limits = c(local_start_date, local_end_date)) +
      theme_if_policystyle() +
      theme(legend.position = c(0.02, 0.50), legend.justification = c("left", "top"), 
            legend.background = element_rect(fill = alpha("white", 0.7), color = NA),
            legend.key = element_rect(fill = "transparent"),
            plot.title = element_text(size = 12, face = "bold"),
            axis.title.x = element_blank(), axis.title.y = element_blank())
    
    if(!is.null(cap_text)) {
      p <- p + labs(caption=cap_text)
    } else {
      p <- p + theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
    }
    return(p)
  }
  
  df_long <- df_all %>% select(date, us=us_tot_val, ch=ch_tot_val, adj_proxy=all_of(adj1_col), adj_tariff=adj_tariff, adj_both=all_of(adj2_col)) %>%
    pivot_longer(cols = -date, names_to = "series", values_to = "value") %>% filter(!is.na(value))
  
  # Panel 1: Baseline
  pA <- make_panel(df_long %>% filter(series %in% c("us", "ch")), 
                   c("us", "ch"), "1. Baseline: Reported Total Goods", cap_text=NULL)
  
  # Panel 2: Adds Proxy
  pB <- make_panel(df_long %>% filter(series %in% c("us", "ch", "adj_proxy")), 
                   c("us", "ch", "adj_proxy"), paste0("2. Adding ", gsub("US \\+ ", "", adj1_label)), cap_text=NULL)
  
  # Panel 3: Adds Tariff Gap Independently
  pC <- make_panel(df_long %>% filter(series %in% c("us", "ch", "adj_proxy", "adj_tariff")), 
                   c("us", "ch", "adj_proxy", "adj_tariff"), "3. Adding Tariffed Goods Gap", cap_text=blank_caption)
  
  # Panel 4: Combines Both Additions
  pD <- make_panel(df_long %>% filter(series %in% c("us", "ch", "adj_proxy", "adj_tariff", "adj_both")), 
                   c("us", "ch", "adj_proxy", "adj_tariff", "adj_both"), "4. Adding Both Factors", cap_text=caption_text)
  
  pC_save <- customXAxis(pC, minorticks=11, skip=0, center=F)
  pD_save <- customXAxis(pD, minorticks=11, skip=0, center=F)
  
  # Assemble 2x2 grid. We pass rel_heights to give the bottom row room for the axis text and caption.
  col_panel <- plot_grid(pA, pB, pC_save, pD_save, align = 'hv', axis = 'tblr', 
                         nrow = 2, ncol = 2, rel_heights=c(1, 1.1))
  
  file_name <- paste0("ch2_gap_reconstruction_panel_", filename_suffix, ".png")
  ggsave(paste0(out_dir, "/", file_name), col_panel, width = 14, height = 10, units = 'in', dpi = 800, bg = "white")
  cat("  -> Saved:", file_name, "\n")
}

# Generate all three panel scenarios
generate_reconstruction_panel("adj_proxy_only", "adj_proxy_both", "US + De Minimis Proxy", "proxy")
generate_reconstruction_panel("adj_parcel_only", "adj_parcel_both", "US + Bernardo Small Parcels", "parcel")
generate_reconstruction_panel("adj_nber_only", "adj_nber_both", "US + NBER Estimates", "nber")

cat("All Gap Reconstructions Complete!\n")
