################################################################################
### Explanatory Factors for US-China Trade Gap
### Chart 1: Combined Proxy & EA Gap vs. US-China Tariffed Gap
### Chart 2: Net Unexplained Gap (Gap vs Explanation Magnitudes)
################################################################################
rm(list = ls())
suppressMessages(source("/if/appl/R/Functions/IFfunctions.r"))
library(readr); library(dplyr); library(zoo); library(policyPlot)
library(ggplot2); library(pracma); library(xts); library(tidyr); library(cowplot); library(this.path)

# Load IF specs
attach(IFSPECS$COL)
attach(IFSPECS$LW)
attach(IFSPECS$LT)

setwd(dirname(this.path()))

out_dir <- "REPLACE WITH YOUR PATH"
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

# ==============================================================================
# 1. DATA PROCESSING
# ==============================================================================
cat("Loading and calculating component data...\n")

# A. De Minimis Proxy (16 Maximalist Chapters)
target_chapters <- c("33", "39", "42", "48", "61", "62", "63", "64", 
                     "65", "71", "85", "90", "91", "94", "95", "96")

us_raw <- read_csv("../data/raw/imports/monthly/us_rep_hs10_values_ch.csv", show_col_types = F, col_types = cols(commodity = col_character())) %>%
  filter(substr(commodity, 1, 2) %in% target_chapters) %>% group_by(date) %>% summarise(us_val = sum(usd, na.rm=T))

pt_raw <- read_csv("../data/raw/exports/monthly/comtrade_exports_to_us_ch.csv", show_col_types = F, col_types = cols(cmdcode = col_character())) %>%
  rename(commodity = cmdcode, date_str = period, usd = primaryvalue) %>% mutate(usd = as.numeric(usd), date = ym(date_str)) %>%
  filter(substr(commodity, 1, 2) %in% target_chapters) %>% group_by(date) %>% summarise(pt_val = sum(usd, na.rm=T))

df_proxy <- full_join(us_raw, pt_raw, by="date") %>%
  mutate(proxy_diff = (pt_val - us_val) / 10^9) %>% select(date, proxy_diff) # China minus US

# B. US-EA Tariffed Gap (US minus EA)
ea_imp <- read_csv("../data/cleaned/imports/monthly/tariffs_hs6_monthly_us_ea.csv", show_col_types=F) %>% rename(us_val=value) %>% select(date, us_val)
ea_exp <- read_csv("../data/cleaned/exports/monthly/tariffs_hs6_monthly_us_ea.csv", show_col_types=F) %>% rename(pt_val=value) %>% select(date, pt_val)
df_ea <- full_join(ea_imp, ea_exp, by="date") %>%
  mutate(ea_diff = (us_val - pt_val) / 10^9) %>% select(date, ea_diff)

# C. US-China Tariffed Gap (US minus China)
ch_imp <- read_csv("../data/cleaned/imports/monthly/tariffs_hs6_monthly_us_ch.csv", show_col_types=F) %>% rename(us_val=value) %>% select(date, us_val)
ch_exp <- read_csv("../data/cleaned/exports/monthly/tariffs_hs6_monthly_us_ch.csv", show_col_types=F) %>% rename(pt_val=value) %>% select(date, pt_val)
df_ch <- full_join(ch_imp, ch_exp, by="date") %>%
  mutate(ch_diff = (us_val - pt_val) / 10^9) %>% select(date, ch_diff)

# ==============================================================================
# 2. COMBINE & ROLL
# ==============================================================================
# Note on Net Unexplained: 
# Calculated as |US-China Gap| - |Explanatory Factor|.
# Positive = Missing trade unexplained. Negative = Explanatory factor overshoots gap.
df_all <- df_proxy %>% full_join(df_ea, by="date") %>% full_join(df_ch, by="date") %>% arrange(date) %>%
  mutate(
    proxy_roll = rollsum(proxy_diff, 12, align="right", fill=NA),
    ea_roll = rollsum(ea_diff, 12, align="right", fill=NA),
    combined_raw = proxy_diff + ea_diff,
    combined_roll = rollsum(combined_raw, 12, align="right", fill=NA),
    ch_roll = rollsum(ch_diff, 12, align="right", fill=NA),
    net_unexplained_combined = abs(ch_roll) - abs(combined_roll),
    net_unexplained_proxy = abs(ch_roll) - abs(proxy_roll),
    net_unexplained_ea = abs(ch_roll) - abs(ea_roll)
  ) %>%
  filter(date >= "2016-01-01") %>% na.omit()

local_start_date <- as.Date("2016-01-01")
local_end_date <- max(df_all$date, na.rm = TRUE)
local_last_date_txt <- format(local_end_date, "%B %Y")

# ==============================================================================
# 3. CHART 1: COMBINED VS. US-CHINA GAP
# ==============================================================================
cat("Generating Explanatory Overlay Chart...\n")

df_chart1 <- df_all %>% select(date, combined_roll, ch_roll, proxy_roll) %>%
  pivot_longer(cols = c(combined_roll, ch_roll, proxy_roll), names_to = "series", values_to = "value")

lims1 <- limint(df_chart1$value, extraspace = 0)

p1 <- ggplot(df_chart1, aes(x=date, y=value, color=series, linetype=series, linewidth=series)) +
  geom_line() + geom_hline(yintercept=0, color="black", linewidth=IFTHIN) +
  labs(title="Tariffed Goods",
       subtitle="Billions USD (12-month rolling sum)",
       caption=paste0(" Note: US-China tariffed gap is US-reported imports minus China-reported exports.\n",
                      " EA is US-reported imports from Euro Area. de minimis is constructed as a proxy from 16 different HS 2 codes.\n",
                      " US-EA and US-China lines are constructed using tariffed goods.\n",
                      " Source: US HS Trade & UN Comtrade. Data through ", local_last_date_txt, ".")) +
  scale_color_manual(values=c("combined_roll"=IFBLUE, "ch_roll"=IFRED, "proxy_roll"=IFORANGE), 
                     labels=c("combined_roll"="de minimis + EA imports gap", "ch_roll"="US-China Gap", "proxy_roll"="de minimis gap only")) +
  scale_linewidth_manual(values=c("combined_roll"=IFTHICK, "ch_roll"=IFTHICK, "proxy_roll"=IFMEDIUM), 
                         labels=c("combined_roll"="de minimis + EA imports gap", "ch_roll"="US-China Gap", "proxy_roll"="de minimis gap only")) +
  scale_linetype_manual(values=c("combined_roll"=IFDASHED, "ch_roll"="solid", "proxy_roll"=IFDOTTED), 
                        labels=c("combined_roll"="de minimis + EA imports gap", "ch_roll"="US-China Gap", "proxy_roll"="de minimis gap only")) +
  scale_y_continuous(limits = c(lims1[1], lims1[2]), position = "right", sec.axis = dup_axis(labels = NULL), expand = c(0, 0), breaks = seq(lims1[1], lims1[2], lims1[3])) +
  scale_x_date(expand = c(0.05,0.05), breaks = seq(local_start_date, local_end_date, by="2 years"), labels = date_format("%Y"), limits = c(local_start_date, local_end_date)) +
  theme_if_policystyle() +
  theme(legend.position = c(0.02, 0.98), legend.justification = c("left", "top"), plot.subtitle = element_text(vjust = 1.25), axis.title.x = element_blank(), axis.title.y = element_blank())

p1_save <- customXAxis(p1, minorticks=11, skip=0, center=F)
ggsave(paste0(out_dir, "/ch1_explanatory_vs_china_gap.png"), p1_save, width=8, height=5, dpi=800, bg="white")

# ==============================================================================
# 4. CHART 2: ABSOLUTE DIFFERENCE (REMAINING GAP)
# ==============================================================================
cat("Generating Net Unexplained Gap Chart...\n")

df_chart2 <- df_all %>% select(date, net_unexplained_combined, net_unexplained_proxy, net_unexplained_ea) %>%
  pivot_longer(cols = c(net_unexplained_combined, net_unexplained_proxy, net_unexplained_ea), names_to = "series", values_to = "value")

lims2 <- limint(df_chart2$value, extraspace = 0)

p2 <- ggplot(df_chart2, aes(x=date, y=value, color=series, linetype=series, linewidth=series)) +
  geom_line() +
  geom_hline(yintercept=0, color="black", linewidth=IFTHIN) +
  labs(title="Net Unexplained Gap (Magnitude of Difference with US-China Gap)",
       subtitle="Billions USD (12-month rolling sum)",
       caption=paste0(" Note: US-CH gap minus (in absolute terms) the gap between the US and the other lines. \n",
                      " Data through ", local_last_date_txt, ".")) +
  scale_color_manual(values=c("net_unexplained_combined"=IFPURPLE, "net_unexplained_proxy"=IFGREEN, "net_unexplained_ea"=IFORANGE), 
                     labels=c("net_unexplained_combined"="de minimis + EA", "net_unexplained_proxy"="de minimis only", "net_unexplained_ea"="EA only")) +
  scale_linewidth_manual(values=c("net_unexplained_combined"=IFTHICK, "net_unexplained_proxy"=IFMEDIUM, "net_unexplained_ea"=IFMEDIUM), 
                         labels=c("net_unexplained_combined"="de minimis + EA", "net_unexplained_proxy"="de minimis only", "net_unexplained_ea"="EA only")) +
  scale_linetype_manual(values=c("net_unexplained_combined"="solid", "net_unexplained_proxy"=IFDASHED, "net_unexplained_ea"=IFDOTTED), 
                        labels=c("net_unexplained_combined"="de minimis + EA", "net_unexplained_proxy"="de minimis only", "net_unexplained_ea"="EA only")) +
  scale_y_continuous(limits = c(lims2[1], lims2[2]), position = "right", sec.axis = dup_axis(labels = NULL), expand = c(0, 0), breaks = seq(lims2[1], lims2[2], lims2[3])) +
  scale_x_date(expand = c(0.05,0.05), breaks = seq(local_start_date, local_end_date, by="2 years"), labels = date_format("%Y"), limits = c(local_start_date, local_end_date)) +
  theme_if_policystyle() +
  theme(legend.position = c(0.02, 0.98), legend.justification = c("left", "top"), plot.subtitle = element_text(vjust = 1.25), axis.title.x = element_blank(), axis.title.y = element_blank())

p2_save <- customXAxis(p2, minorticks=11, skip=0, center=F)
ggsave(paste0(out_dir, "/ch2_absolute_difference_magnitude.png"), p2_save, width=8, height=5, dpi=800, bg="white")

# ==============================================================================
# 5. CREATE 2-CHART VERTICAL PANEL
# ==============================================================================
cat("Assembling Explanatory Vertical Panel...\n")

# Strip X-axis from top chart for clean stack 
# (Note: We DO NOT pass this through customXAxis, because it has no x-axis!)
p1_panel <- p1 + theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), plot.caption = element_blank())

col_panel <- plot_grid(p1_panel, p2_save, align = 'v', rel_heights = c(1, 1.2), nrow = 2, ncol = 1)
ggsave(paste0(out_dir, "/ch3_explanatory_panel.png"), col_panel, width = 8.5, height = 9, units = 'in', dpi = 800, bg = "white")

cat("Explanatory Analysis Complete!\n")
