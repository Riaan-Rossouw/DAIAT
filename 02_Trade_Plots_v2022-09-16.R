# ------------------------------------------------------------------------------------------------------------------ #
# Monthly agricultural commodity trade charts, to identify potential seasonality in trade data                       #
# For: Data and Artificial Intelligence for African Trade (DAIAT) Initiative                                         #
# Last Edited: 2022-09-16 (RR)                                                                                       #
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
#  2) Specify user-defined functions and scripts                                                                    #
#-------------------------------------------------------------------------------------------------------------------#

# Use save() function to work with object(s) that exceed memory capacity
saveobj <- function(obj, file) {
  save(list = obj, file = file)
}

# Function to replace all missing values with zeros
rzero = function(dt) {
  for (j in seq_len(ncol(dt)))
    set(dt, which(is.na(dt[[j]])), j, 0)
}

# Omit rows containing specific column of NA
completeFun <- function(data, desiredCols) {
  completeVec <- complete.cases(data[, desiredCols])
  return(data[completeVec, ])
}

# Percentage change function
myReturn <- function(x) c(NA, diff(x)/x[-length(x)]*100)

# Clear column from NULL-values and unlist it
clearCol <- function(lst) {
  lst[sapply(lst, is.null)] <- NA
  unlist(lst)
}

# Function to calculate number of months between two dates
elapsed_months <- function(end_date, start_date) {
  ed <- as.POSIXlt(end_date)
  sd <- as.POSIXlt(start_date)
  12 * (ed$year - sd$year) + (ed$mon - sd$mon)
}

#-------------------------------------------------------------------------------------------------------------------#
#  3) Read in all raw data from 'RawData' folder and clean (save to 'CleanData')                                    #
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

# Generic function for plotting different variables


# Plot different variables against each other
library(ggplot2)
library(dplyr)

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
top8 <- plotdf %>% 
  group_by(Partner) %>% 
  summarise(kg = as.numeric(sum(kg, na.rm = TRUE))) %>% 
  top_n(8, kg) %>%
  arrange(desc(kg)) %>% 
  .[["Partner"]]
plotdf <- plotdf %>% filter(Partner %in% top8)

# Create plots (y-axis is NOT fixed across panels, this will allow us to ID 
# trends over time within each country/area individually).
qplot(Period, kg, data = plotdf) + 
  geom_line(data = plotdf[plotdf$Partner %in% names(which(table(plotdf$Partner) > 1)), ]) + 
  xlim(min(plotdf$Period), max(plotdf$Period)) + 
  labs(title = "Weight (KG) of Thai Shrimp Exports, by Destination Area, 2007 - 2011") + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1), 
        axis.text = element_text(size = 7)) + 
  facet_wrap(~factor(Partner, levels = top8), scales = "free", nrow = 2, ncol = 4)

# Simple interactivity
g <- ggplot(subset(df, year > "2017" & year <= "2022"),
            aes(x = GDP, y = infant_mortality, color = country == "United States", tooltip = country)) + 
  geom_point() + 
  facet_wrap(~ date) + 
  theme(legend.position="none")

ggplotly(g)

# Save as PDFs or PNGs


# save to 'figures' folder


#-------------------------------------------------------------------------------------------------------------------#
#  4) Export results to "working directory" folder                                                                  #
#-------------------------------------------------------------------------------------------------------------------#

# Write to CSV file and save in "RawData" directory
write.csv(append_trade, file = 'RawData/UNComtrade_HS6_Monthly.csv', row.names = FALSE)

# Save end/finish time
sessiontime.end <- Sys.time()
difftime(sessiontime.end, sessiontime.start)

## DELETE ALL FILES ##
rm(list=ls())

# *** END ***

#############
## APENDIX ##
#############