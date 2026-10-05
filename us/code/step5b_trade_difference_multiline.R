################################################################################
### US Trade Reporting Difference (Core Countries Only)
### Categories: Total, Tariffed, Non-Tariffed + 3-Panel
################################################################################
rm(list = ls())
suppressMessages(source("/if/appl/R/Functions/IFfunctions.r"))
library(readr); library(dplyr); library(zoo); library(policyPlot)
library(ggplot2); library(pracma); library(xts); library(tidyr); library(cowplot); library(this.path)

# Load IF specs
attach(IFSPECS$COL); attach(IFSPECS$LW); attach(IFSPECS$LT)
setwd(dirname(this.path()))

# ==============================================================================
# 1. SETUP & DEFINITIONS
# ==============================================================================
# EA and CA removed from codes, colors, labels, etc.
codes <- c("ch", "mx", "vt", "id", "ma", "ph", "th")

colors <- c("ch"=IFRED, "mx"=IFGREEN, "vt"=IFBLUE, "asean"=IFORANGE, "asean_mx"=IFPURPLE)
labels <- c("ch"="China", "mx"="Mexico", "vt"="Vietnam", "asean"="ASEAN-5", "asean_mx"="ASEAN-5 + Mexico")
widths <- c("ch"=IFTHICK, "mx"=IFMEDIUM, "vt"=IFMEDIUM, "asean"=IFMEDIUM, "asean_mx"=IFTHICK)
types  <- c("ch"="solid", "mx"="solid", "vt"="solid", "asean"=IFDASHED, "asean_mx"=IFDOTDASH)

categories <- c("total", "tariffs", "no_tariffs")
titles <- c("Total Goods", "Tariffed Goods", "Non-Tariffed Goods")
plot_list <- list()

out_dir <- "/if/research-eme/omar/Eva/bilateralTrade/us/output/trade_diff_core"
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

# ==============================================================================
# 2. HELPER FUNCTION
# ==============================================================================
get_diff <- function(abbr, cat_type) {
  us_file <- paste0("../data/cleaned/imports/monthly/", cat_type, "_hs6_monthly_us_", abbr, ".csv")
  pt_file <- paste0("../data/cleaned/exports/monthly/", cat_type, "_hs6_monthly_us_", abbr, ".csv")
  
  if (!file.exists(us_file) || !file.exists(pt_file)) return(NULL)
  
  us <- read_csv(us_file, show_col_types=F) %>% rename(us_val=value) %>% select(date, us_val)
  pt <- read_csv(pt_file, show_col_types=F) %>% rename(pt_val=value) %>% select(date, pt_val)
  
  df <- full_join(us, pt, by="date") %>%
    mutate(diff = (us_val - pt_val) / 10^9) %>% 
    select(date, diff)
  
  names(df)[2] <- abbr 
  return(df)
}

# ==============================================================================
# 3. PROCESSING LOOP
# ==============================================================================
cat("Starting Trade Difference Analysis (Core Only)...\n")
start_date <- as.Date("2016-01-01")

for (i in 1:3) {
  cat_type <- categories[i]
  cat_title <- titles[i]
  cat(paste("  Processing:", cat_title, "...\n"))
  
  list_dfs <- lapply(codes, get_diff, cat_type=cat_type)
  list_dfs <- list_dfs[!sapply(list_dfs, is.null)]
  
  if(length(list_dfs) == 0) next
  
  df_wide <- Reduce(function(x, y) full_join(x, y, by="date"), list_dfs) %>% arrange(date)
  
  df_wide <- df_wide %>%
    mutate(
      asean = rowSums(select(., any_of(c("id", "ma", "ph", "th", "vt"))), na.rm = TRUE),
      asean_mx = asean + if_else(is.na(mx), 0, mx)
    )
  
  df_long <- df_wide %>%
    select(date, any_of(c("ch", "mx", "vt", "asean", "asean_mx"))) %>%
    pivot_longer(cols = -date, names_to = "country", values_to = "diff_raw") %>%
    group_by(country) %>% arrange(date) %>%
    mutate(diff_roll = rollsum(diff_raw, 12, align="right", fill=NA)) %>%
    ungroup() %>% na.omit() %>% filter(date >= start_date)
  
  end_date <- max(df_long$date)
  last_date_txt <- format(end_date, "%b %Y")
  lims <- limint(df_long$diff_roll, extraspace = 0)
  
  p <- df_long %>%
    ggplot(aes(x=date, y=diff_roll, color=country, linewidth=country, linetype=country)) +
    geom_line() +
    geom_hline(yintercept=0, color="black", linewidth=IFTHIN) +
    
    labs(title=paste0("Trade Difference: ", cat_title),
         subtitle="Billions USD (12-month rolling sum)",
         caption=paste0("  Note: US-reported imports minus partner-reported exports.\n",
                        "  Source: US HS Trade (Census) & UN Comtrade. Data through ", last_date_txt, ".")) +
    
    scale_color_manual(values=colors, labels=labels) +
    scale_linewidth_manual(values=widths, labels=labels) +
    scale_linetype_manual(values=types, labels=labels) +
    
    scale_y_continuous(limits = c(lims[1], lims[2]), position = "right", 
                       sec.axis = dup_axis(labels = NULL), expand = c(0, 0), 
                       breaks = seq(lims[1], lims[2], lims[3])) +
    scale_x_date(expand = c(0.05,0.05), breaks = seq(start_date, end_date, by="2 years"), 
                 labels = date_format('%Y'), limits = c(start_date, end_date)) +
    
    theme_if_policystyle() +
    theme(legend.position = c(0.02, 0.98), legend.justification = c("left", "top"),
          plot.subtitle = element_text(vjust = 1.25), axis.title.x = element_blank(), 
          axis.title.y = element_blank()) +
    guides(color = guide_legend(ncol = 1), linewidth = guide_legend(ncol = 1), linetype = guide_legend(ncol = 1))
  
  p <- customXAxis(p, minorticks=11, skip=0, center=F)
  plot_list[[i]] <- p
  
  ggsave(paste0(out_dir, "/us_diff_core_", cat_type, ".png"), p, width=8.0, height=5.0, units='in', dpi=800, bg="white")
}

# ==============================================================================
# 4. CREATE PANEL
# ==============================================================================
if (length(plot_list) == 3) {
  panel <- setMargins(plot_list[[1]], plot_list[[2]], plot_list[[3]])
  row_panel <- plot_grid(panel[[1]], panel[[2]], panel[[3]], align = 'h', rel_widths = c(3.3, 3.3, 3.3), nrow = 1, ncol = 3)
  ggsave(paste0(out_dir, "/us_diff_core_panel.png"), row_panel, width = 3.33*4, height = 3.2*1.2 + 0.13*2 + 0.2, units='in', dpi=800, bg="white")
}
cat("Job Complete (Core Only).\n")
