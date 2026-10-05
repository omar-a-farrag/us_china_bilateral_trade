################################################################################
### US Imports: China vs ASEAN (Substitution Analysis)
### Output: Total, Tariffed, Non-Tariffed, and Panel
## Omar Farrag, IF Division - EME
## Created on: 01/22/2026; last edited: 01/22/2026
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
  "ch" = "China",
  "id" = "Indonesia",
  "ma" = "Malaysia",
  "ph" = "Philippines",
  "th" = "Thailand",
  "vt" = "Vietnam", 
  "mx" = "Mexico",
  "ca" = "Canada"
)

# Colors
custom_cols <- c(
  "China" = IFRED,
  "Vietnam" = IFBLUE, 
  "Malaysia" = IFPURPLE, 
  "Thailand" = IFGREEN, 
  "Indonesia" = IFORANGE, 
  "Philippines" = IFGRAY, 
  "Mexico" = IFLIGHTGREEN, 
  "Canada" = IFLIGHTBLUE
)

# Linewidths
custom_lws <- c(
  "China" = IFTHICK,
  "Vietnam" = IFMEDIUM, 
  "Malaysia" = IFMEDIUM, 
  "Thailand" = IFMEDIUM, 
  "Indonesia" = IFMEDIUM, 
  "Philippines" = IFMEDIUM, 
  "Mexico" = IFTHIN, 
  "Canada" = IFMEDIUM
)

# Linetypes
custom_lts <- c(
  "China" = "solid",
  "Vietnam" = "solid", 
  "Malaysia" = "solid", 
  "Thailand" = IFDASHED, 
  "Indonesia" = IFDOTTED, 
  "Philippines" = "solid", 
  "Mexico" = "solid",
  "Canada" = IFDOTDASH 
)

# Output Directory
root <- "/if/research-eme/omar/Eva/bilateralTrade/us"
out_dir <- paste0(root, "/output/imports_asean_ch")
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

# 2. MAIN PROCESSING LOOP
# ------------------------------------------------------------------------------
categories <- c("total", "tariffs", "no_tariffs")
titles <- c("Total Goods", "Tariffed Goods", "Non-Tariffed Goods")
plot_list <- list() # Store charts here for the panel

cat("Starting ASEAN vs China Import Analysis...\n")

for (i in 1:3) {
  
  cat_type <- categories[i]
  cat_title <- titles[i]
  
  cat(paste0("  Processing: ", cat_title, "...\n"))
  
  df_list <- list()
  
  # --- INNER LOOP: Load Data ---
  for (abbr in names(asean_map)) {
    
    file_prefix <- paste0(cat_type, "_hs6_monthly_us_")
    path_file <- paste0("../data/cleaned/imports/monthly/", file_prefix, abbr, ".csv")
    
    if (file.exists(path_file)) {
      temp <- read_csv(path_file, show_col_types = FALSE) %>%
        mutate(
          value = value/(10^9), # Convert to Billions
          country_code = abbr,
          country_label = asean_map[[abbr]]
        ) %>%
        arrange(date) %>%
        mutate(value_roll = rollsum(value, 12, align = "right", fill = NA)) %>%
        na.omit()
      
      df_list[[abbr]] <- temp
    }
  }
  
  if (length(df_list) == 0) {
    cat("    No data found. Skipping.\n")
    next
  }
  df_all <- bind_rows(df_list)
  
  # --- GRAPHING ---
  start_date <- as.Date("2016-01-01")
  end_date <- max(df_all$date)
  last_date_txt <- format(end_date, "%B %Y")
  
  lims <- limint(df_all$value_roll, extraspace = 0)
  
  p <- df_all %>%
    ggplot(aes(x = date, y = value_roll, 
               color = country_label, 
               linewidth = country_label, 
               linetype = country_label)) +
    geom_line() +
    
    labs(title = paste0("US Imports: ", cat_title),
         subtitle = "Billions USD (12-month rolling sum)",
         caption = paste0(" Countries: China vs. ASEAN-5.\n",
                          " Source: US HS Trade (Census). Data through ", last_date_txt, ".")) +
    
    scale_color_manual(values = custom_cols) +
    scale_linewidth_manual(values = custom_lws) +
    scale_linetype_manual(values = custom_lts) +
    
    scale_y_continuous(limits = c(0, lims[2]), position = "right", 
                       breaks = seq(0, lims[2], lims[3])) +
    scale_x_date(expand = c(0.05,0.05), breaks = seq(start_date, end_date, by = "2 year"), 
                 labels = date_format('%Y'), limits = c(start_date, end_date)) +
    
    theme_if_policystyle() +
    theme(legend.position = c(0.02, 0.95),
          legend.justification = c("left", "center"),
          plot.subtitle = element_text(vjust = 1.25),
          axis.title.x = element_blank(), axis.title.y = element_blank()
          ) +     
    guides(color = guide_legend(nrow = 2, ncol = 4, byrow = TRUE),
           linetype = guide_legend(nrow = 2, ncol = 4, byrow = TRUE),
           linewidth = guide_legend(nrow = 2, ncol = 4, byrow = TRUE)
    )
  
  
  p = customXAxis(p, minorticks = 11, skip = 0, center = F)
  
  # Save to list for panel creation later
  plot_list[[i]] <- p
  
  # --- EXPORT INDIVIDUAL ---
  filename <- paste0("us_imports_china_vs_asean_", cat_type, ".png")
  ggsave(paste0(out_dir, "/", filename), 
         p, width = 6.5, height = 4.5, units='in', dpi=800, bg="white")
  
  cat(paste0("    Saved: ", filename, "\n"))
}

# 3. CREATE PANEL
# ------------------------------------------------------------------------------
if (length(plot_list) == 3) {
  cat("  Creating Panel Chart...\n")
  
  # Use setMargins to align axes (assuming policyPlot/IFfunctions logic)
  panel <- setMargins(plot_list[[1]], plot_list[[2]], plot_list[[3]])
  
  row_panel <- plot_grid(
    panel[[1]], panel[[2]], panel[[3]], 
    align = 'h', 
    rel_widths = c(3.3, 3.3, 3.3), 
    nrow = 1, 
    ncol = 3
  )
  
  panel_filename <- "us_imports_china_vs_asean_panel.png"
  ggsave(paste0(out_dir, "/", panel_filename), 
         row_panel, 
         width = 3.33*3, 
         height = 3.2 + 0.13*2 + 0.2, 
         units = 'in', 
         dpi = 800, 
         bg = "white"
  )
  cat(paste0("    Saved: ", panel_filename, "\n"))
  
} else {
  warning("Could not create panel: Expected 3 charts, but found ", length(plot_list))
}

cat("ASEAN vs China Analysis Complete.\n")