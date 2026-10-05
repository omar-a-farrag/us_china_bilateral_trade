################################################################################
### Comparing Tariffed vs. non-Tariffed Goods
## Omar Farrag, IF Division - EME
# Created on: 12-23-2025   ; Last edited on: 12-23-2025
################################################################################
### Sources ###
# US: HS Trade (via internal FRB pull)
# Others: UN Comtrade (via API pull)
################################################################################
rm(list = ls())
suppressMessages(source("/if/appl/R/Functions/IFfunctions.r"))

# load in libraries
library(readxl)
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

#load in IF colors, linetypes, and linewidths
attach(IFSPECS$COL)
attach(IFSPECS$LT)
attach(IFSPECS$LW)

require(this.path)
setwd(dirname(this.path()))
################################################################################
### Data Pull
################################################################################

## US ##
us_import_ar_total <- read_csv("../data/cleaned/imports/monthly/total_hs6_monthly_us_ar.csv") %>%
  mutate(value = value/(10^9), 
         country = "us"
  )

us_import_ar_tariffed <- read_csv("../data/cleaned/imports/monthly/tariffs_hs6_monthly_us_ar.csv") %>%
  mutate(value = value/(10^9), 
         country = "us"
  )

us_import_ar_notariff <- read_csv("../data/cleaned/imports/monthly/no_tariffs_hs6_monthly_us_ar.csv") %>%
  mutate(value = value/(10^9), 
         country="us"
  )

## AR ##
ar_export_us_total <- read_csv("../data/cleaned/exports/monthly/total_hs6_monthly_us_ar.csv") %>%
  mutate(value = value/(10^9), 
         country= "ar"
  )

ar_export_us_tariffed <- read_csv("../data/cleaned/exports/monthly/tariffs_hs6_monthly_us_ar.csv") %>%
  mutate(value = value/(10^9), 
         country = "ar"
  )

ar_export_us_notariff <- read_csv("../data/cleaned/exports/monthly/no_tariffs_hs6_monthly_us_ar.csv") %>%
  mutate(value = value/(10^9), 
         country = "ar"
  )

################################################################################
### Data Cleaning
################################################################################
## US ##
us_total <- us_import_ar_total %>%
  mutate(value = rollsum(value, 12, align = "right", fill = NA)
  ) %>%
  rename(value_total_6 = value)

us_tariffed <- us_import_ar_tariffed %>%
  mutate(value = rollsum(value, 12, align = "right", fill = NA)
  ) %>%
  rename(value_tariffed_6 = value)

us_notariff <- us_import_ar_notariff %>%
  mutate(value = rollsum(value, 12, align = "right", fill = NA)
  ) %>%
  rename(value_notariff_6 = value)

## AR ##
ar_total <- ar_export_us_total %>%
  mutate(value = rollsum(value, 12, align = "right", fill = NA)
  ) %>%
  rename(value_total_6 = value)

ar_tariffed <- ar_export_us_tariffed %>%
  mutate(value = rollsum(value, 12, align = "right", fill = NA)
  ) %>%
  rename(value_tariffed_6 = value)

ar_notariff<- ar_export_us_notariff %>%
  mutate(value = rollsum(value, 12, align = "right", fill = NA)
  ) %>%
  rename(value_notariff_6 = value)

### Merge datasets
df_total <- bind_rows(us_total, ar_total) %>%
  na.omit()

df_tariffed <- bind_rows(us_tariffed, ar_tariffed) %>%
  na.omit()

df_notariff <- bind_rows(us_notariff, ar_notariff) %>%
  na.omit()
################################################################################
### Graphing ### 
################################################################################
# Pre-define what will be used in the graphs

# start and end dates
start_date <- as.Date("2016-01-01")
end_date <- as.Date("2026-01-01")

# Y-axis min and max
limit_total <- limint(df_total$value_total_6, extraspace = 0)
limit_tariffed <- limint(df_tariffed$value_tariffed_6, extraspace = 0)
limit_notariff <- limint(df_notariff$value_notariff_6, extraspace = 0)

# Legend colors and labels
colors = c(IFBLUE, IFRED)
linewidths <- c(IFMEDIUM, IFTHICK)
linetypes = c("solid", IFDOTTED)

series_names<- c("us", "ar")
label_names= c("US-reported imports", "Argentina-reported exports")

# "Date through " caption 
last_date_total <- format(as.Date(max(df_total$date)), "%B %Y")
last_date_tariffed <- format(as.Date(max(df_tariffed$date)), "%B %Y")
last_date_notariff <- format(as.Date(max(df_notariff$date)), "%B %Y")

################################################################################

# Graph 1: Total
total_chart <- df_total %>%
  ggplot(data=., aes(x = date, y = value_total_6, colour = country, linetype = country, linewidth = country)) + 
  geom_line() + 
  
  labs(title = paste("Total Goods", sep=""),
       subtitle = "Billions USD",
       caption = paste0(" Note: values are 12-month rolling sums. Data through \n", last_date_total, ". Goods matched at HS 6-digit level.",
                        # "\n",
                        # "\n",
                        # "\n",
                        "\n Source: Imports - HS Trade (Census, US); \nExports - UN Comtrade.", sep="")) +
  
  scale_color_manual(
    breaks = series_names,
    values = setNames(colors, series_names), 
    labels = setNames(label_names,series_names)
  )+  
  
  scale_linewidth_manual( 
    breaks = series_names, 
    values = setNames(linewidths, series_names), 
    labels = setNames(label_names,series_names)
  ) +
  
  scale_linetype_manual(
    breaks = series_names,
    values =setNames(linetypes, series_names),
    labels = setNames(label_names,series_names) 
  ) +
  
  scale_y_continuous(
    limits = c(limit_total[1], limit_total[2]),
    position = "right",
    sec.axis = dup_axis(labels = NULL),
    expand = c(0, 0), 
    breaks = seq(limit_total[1], limit_total[2],  limit_total[3])) +
  
  scale_x_date(
    expand = c(0.05,0.05),
    breaks = seq(start_date, end_date, by = "2 year"), 
    labels = date_format('%Y'),
    limits = c(start_date, end_date)) +
  
  theme_if_policystyle() +
  
  theme(legend.justification = c("left", "top"),
        legend.position = c(0.01,0.99),# move legend
        #plot.margin = unit(c(0.04,0.175,0.0,0.4), "in"),
        plot.subtitle = element_text(vjust = 1.25),
        axis.title.x = element_blank(), # gets rid of x-axis label
        axis.text.x = element_text(angle = 0),
        axis.title.y = element_blank())  # gets rid of y-axis label

total_chart = customXAxis(total_chart, minorticks = 11, skip = 0, center = F) # this sets the ticks to be spaced by quarterly intervals; set to 11 to display monthly

chart_box_display(total_chart)


######################################################## Graph 2: Tariffed Goods
tariffed_chart <- df_tariffed %>%
  ggplot(data=., aes(x = date, y = value_tariffed_6, colour = country, linetype = country, linewidth = country)) + 
  geom_line() + 
  
  labs(title = paste("Tariffed Goods", sep=""),
       subtitle = "Billions USD",
       caption = paste0(" Note: values are 12-month rolling sums. Data through \n", last_date_tariffed, ". Goods matched at HS 6-digit level.",
                        # "\n",
                        # "\n",
                        # "\n",
                        "\n Source: Imports - HS Trade (Census, US); \nExports - UN Comtrade.", sep="")) +
  
  scale_color_manual(
    breaks = series_names,
    values = setNames(colors, series_names), 
    labels = setNames(label_names,series_names)
  )+  
  
  scale_linewidth_manual( 
    breaks = series_names, 
    values = setNames(linewidths, series_names), 
    labels = setNames(label_names,series_names)
  ) +
  
  scale_linetype_manual(
    breaks = series_names,
    values =setNames(linetypes, series_names),
    labels = setNames(label_names,series_names) 
  ) +
  
  scale_y_continuous(
    limits = c(limit_tariffed[1], limit_tariffed[2]),
    position = "right",
    sec.axis = dup_axis(labels = NULL),
    expand = c(0, 0), 
    breaks = seq(limit_tariffed[1], limit_tariffed[2],  limit_tariffed[3])) +
  
  scale_x_date(
    expand = c(0.05,0.05),
    breaks = seq(start_date, end_date, by = "2 year"), 
    labels = date_format('%Y'),
    limits = c(start_date, end_date)) +
  
  theme_if_policystyle() +
  
  theme(legend.justification = c("left", "top"),
        legend.position = c(0.01,0.99),# move legend
        #plot.margin = unit(c(0.04,0.175,0.0,0.4), "in"),
        plot.subtitle = element_text(vjust = 1.25),
        axis.title.x = element_blank(), # gets rid of x-axis label
        axis.text.x = element_text(angle = 0),
        axis.title.y = element_blank())  # gets rid of y-axis label

tariffed_chart = customXAxis(tariffed_chart, minorticks = 11, skip = 0, center = F) # this sets the ticks to be spaced by quarterly intervals; set to 11 to display monthly

chart_box_display(tariffed_chart)

#################################################### Graph 3: Non-tariffed Goods
notariff_chart <- df_notariff %>%
  ggplot(data=., aes(x = date, y = value_notariff_6, colour = country, linetype = country, linewidth = country)) + 
  geom_line() + 
  
  labs(title = paste("Non-tariffed Goods", sep=""),
       subtitle = "Billions USD",
       caption = paste0(" Note: values are 12-month rolling sums. Data through \n", last_date_notariff, ". Goods matched at HS 6-digit level.",
                        # "\n",
                        # "\n",
                        # "\n",
                        "\n Source: Imports - HS Trade (Census, US); \nExport - UN Comtrade.", sep="")) +
  
  scale_color_manual(
    breaks = series_names,
    values = setNames(colors, series_names), 
    labels = setNames(label_names,series_names)
  )+  
  
  scale_linewidth_manual( 
    breaks = series_names, 
    values = setNames(linewidths, series_names), 
    labels = setNames(label_names,series_names)
  ) +
  
  scale_linetype_manual(
    breaks = series_names,
    values =setNames(linetypes, series_names),
    labels = setNames(label_names,series_names) 
  ) +
  
  scale_y_continuous(
    limits = c(limit_notariff[1], limit_notariff[2]),
    position = "right",
    sec.axis = dup_axis(labels = NULL),
    expand = c(0, 0), 
    breaks = seq(limit_notariff[1], limit_notariff[2],  limit_notariff[3])) +
  
  scale_x_date(
    expand = c(0.05,0.05),
    breaks = seq(start_date, end_date, by = "2 year"), 
    labels = date_format('%Y'),
    limits = c(start_date, end_date)) +
  
  theme_if_policystyle() +
  
  theme(legend.justification = c("left", "top"),
        legend.position = c(0.01,0.99),# move legend
        #plot.margin = unit(c(0.04,0.175,0.0,0.4), "in"),
        plot.subtitle = element_text(vjust = 1.25),
        axis.title.x = element_blank(), # gets rid of x-axis label
        axis.text.x = element_text(angle = 0),
        axis.title.y = element_blank())  # gets rid of y-axis label

notariff_chart = customXAxis(notariff_chart, minorticks = 11, skip = 0, center = F) # this sets the ticks to be spaced by quarterly intervals; set to 11 to display monthly

chart_box_display(notariff_chart)

################################################################################
### Exporting Charts
################################################################################
# Create output folder
root <- "/if/research-eme/omar/Eva/bilateralTrade/us"
output_folder_name <- paste0(root, "/output/ar", sep = "") # Define the name of the output folder

# Check if the directory already exists
if (!dir.exists(output_folder_name)) {
  # If it doesn't exist, create the directory
  dir.create(output_folder_name)
  cat(sprintf("Created directory: %s\n", output_folder_name))
} else {
  cat(sprintf("Directory already exists: %s\n", output_folder_name))
}

# Optional: You can verify the new folder location
# The path will be relative to your current working directory
cat(sprintf("New folder path relative to WD: %s\n", file.path(getwd(), output_folder_name)))

# Exports each chart individually
unlink("../output/ar/us_ar_trade_total.png")
ggsave("../output/ar/us_ar_trade_total.png",
       total_chart,
       width = 3.9,
       height = (3.2 + 0.13*2),
       units='in',
       dpi=800,
       bg="white")

unlink("../output/ar/us_ar_trade_tariffed.png")
ggsave("../output/ar/us_ar_trade_tariffed.png",
       tariffed_chart,
       width = 3.9,
       height = (3.2 + 0.13*2),
       units='in',
       dpi=800,
       bg="white")

unlink("../output/ar/us_ar_trade_notariff.png")
ggsave("../output/ar/us_ar_trade_notariff.png",
       notariff_chart,
       width = 3.9,
       height = (3.2 + 0.13*2),
       units='in',
       dpi=800,
       bg="white")

# Export panel of the three charts
panel <- setMargins(total_chart, tariffed_chart, notariff_chart)

row1.1_panel <- plot_grid(panel[[1]],
                          panel[[2]],
                          panel[[3]],
                          align = 'h',
                          rel_widths = c(3.3,3.3,3.3),
                          nrow = 1,
                          ncol = 3)

unlink("../output/ar/us_ar_trade_panel.png")
ggsave("../output/ar/us_ar_trade_panel.png",
       row1.1_panel,
       width = 3.33*3,
       height = 3.2 + 0.13*2 + 0.2,
       units = 'in', 
       dpi = 800,
       bg = "white", 
       device="png"
)




