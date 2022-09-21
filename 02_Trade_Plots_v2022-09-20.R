# ------------------------------------------------------------------------------------------------------------------ #
# Monthly agricultural commodity trade charts, to identify potential seasonality in trade data                       #
# For: Data and Artificial Intelligence for African Trade (DAIAT) Initiative                                         #
# Last Edited: 2022-09-21 (RR)                                                                                       #
# ------------------------------------------------------------------------------------------------------------------ #

# *** START ***

## Global settings ##
rm(list = ls()) # Remove ALL data objects
gc() # Cleanup unused memory in R

# Specify JAVA_HOME directory
Sys.setenv(JAVA_HOME = "C:/Program Files/Java/jre1.8.0_341/")
library("rJava")

# Set "working directory" to current R-file location
setwd(dirname(rstudioapi::getSourceEditorContext()$path))
getwd() # Verify if you set the directory correctly by the following command

# Increase Java memory allocation to 2 GB
options(java.parameters = "-Xmx5120m")

# Time R script for optimization
sessiontime.start <- Sys.time(); # Save start time

#-------------------------------------------------------------------------------------------------------------------#
#  1) Install required packages and load necessary libraries                                                        #
#-------------------------------------------------------------------------------------------------------------------#

write("Load libs..", stdout())
if (!require("pacman")) install.packages("pacman")
pacman::p_load("manipulate","gtools","plyr","utils","stats","xlsx","sqldf","tcltk","tcltk2","DescTools","tm","rjson",
               "dplyr","data.table","antitrust","ggplot2","rJava","XLConnect","gdata","openxlsx","astsa","forecast",
               "taRifx","reshape","reshape2","pbapply","circlize","migest","devtools","stringr","googleVis","XML",
               "bit64","zoo","Rcpp","tidyr","scales","knitr","rvest","RCurl","httr","readxl","xts","tools","timeDate",
               "lubridate","filesstrings","tradestatistics","tidyverse","gganimate","gapminder","vroom","janitor")

#-------------------------------------------------------------------------------------------------------------------#
#  2) Read in all raw data from 'RawData' folder and clean (save to 'CleanData')                                    #
#-------------------------------------------------------------------------------------------------------------------#

# Read in all data
df <- as.data.table(fread("CleanData/comtrade_monthly_data_201701-202207.csv")) # Read from 01_Clean_Data_vYYYY-MM-DD.r
rds_compressed_df <- readRDS("CleanData/comtrade_monthly_data_201701-202207.rds") # Read object

df %>% 
  ggplot(aes(Period, Value_USD, group = Reporter, color = continent)) +
  facet_wrap(~ continent, scales = "free_y") +
  geom_line(alpha = 0.2) + 
  geom_smooth(aes(group = continent), color = "grey40", se = FALSE) +
  coord_trans(y = "S_sqrt") +
  scale_y_continuous(breaks = c(0, -1750, -7500, -17000, -30000, 0, 1750, 7500),
                     minor_breaks = NULL) +
  labs(title = "United States Trade Balance in Goods with all countries",
       subtitle = "With signed square root transformation faceted depending on continent") +
  guides(color = "none")

## Plot different variables against each other
# Apply polished col headers.
df <- setDT(comtrade.data)

# Create country specific "total weight per year" dataframe for plotting.
plotdf <- df %>% 
  group_by_(.dots = c("Partner", "Period")) %>% 
  summarise(kg = as.numeric(sum(NetWeight_kg, na.rm = TRUE))) %>% 
  as_data_frame()

# Get vector of the top 8 destination countries/areas by total weight shipped 
# across all years, then subset plotdf to only include observations related 
# to those countries/areas.
top10 <- plotdf %>% 
  group_by(Partner) %>% 
  summarise(kg = as.numeric(sum(kg, na.rm = TRUE))) %>% 
  top_n(10, kg) %>%
  arrange(desc(kg)) %>% 
  .[["Partner"]]
plotdf <- plotdf %>% filter(Partner %in% top8)

# Create plots (y-axis is NOT fixed across panels, this will allow us to ID 
# trends over time within each country/area individually).
qplot(Period, kg, data = plotdf) + 
  geom_line(data = plotdf[plotdf$Partner %in% names(which(table(plotdf$Partner) > 1)), ]) + 
  xlim(min(plotdf$Period), max(plotdf$Period)) + 
  labs(title = "Weight (KG) of Agricultural Exports, by Destination Area, 2017-2022") + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1), 
        axis.text = element_text(size = 7)) + 
  facet_wrap(~factor(Partner, levels = top10), scales = "free", nrow = 2, ncol = 4)

# Plot example
df %>% 
  filter(Reporter == "C372") %>%
  ggplot(aes(Period, Value_USD)) +
  geom_abline(slope = 0, intercept = 0, color = "orange") +
  geom_line() +
  geom_smooth(se = FALSE) +
  labs(title = "Irish Trade in Agricultural Goods")

#-------------------------------------------------------------------------------------------------------------------#
#  3) Export images/plots to "working directory" folder                                                             #
#-------------------------------------------------------------------------------------------------------------------#

# Save as PDFs or PNGs


# Save to 'figures' folder


# Save end/finish time
sessiontime.end <- Sys.time()
difftime(sessiontime.end, sessiontime.start)

## DELETE ALL FILES ##
rm(list=ls())

# *** END ***