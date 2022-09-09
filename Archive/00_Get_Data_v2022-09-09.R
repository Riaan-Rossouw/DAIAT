# ------------------------------------------------------------------------------------------------------------------ #
# Search for select variables in key data API's (e.g., World Bank [WB], World Development Indicators [WDI], etc.)    #
# For: Data and Artificial Intelligence for African Trade (DAIAT) Initiative                                         #
# Last Edited: 2022-09-09 (RR)                                                                                       #
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
pacman::p_load("wbstats","WDI","reshape2","ggplot2","plotly")

#-------------------------------------------------------------------------------------------------------------------#
#  2) Search for variables in selected data API's                                                                   #
#-------------------------------------------------------------------------------------------------------------------#

# Get data
new_wb_cache <- wbcache()
new_wdi_cache <- WDIcache()

wbsearch('latitude', cache = new_wb_cache)
wbsearch('temperature', cache = new_wb_cache)
wbsearch('precipitation', cache = new_wb_cache)
wbsearch("gdp.*capita.*US\\$", cache = new_wb_cache)
wbsearch("life expectancy at birth.*total", cache = new_wb_cache)
wbsearch("^mortality.*rate.*infant", cache = new_wb_cache)

WDIsearch('latitude', cache = new_wdi_cache)
WDIsearch('temperature', cache = new_wdi_cache)
WDIsearch('precipitation', cache = new_wdi_cache)
WDIsearch("gdp.*capita.*US\\$", cache = new_wdi_cache)
WDIsearch("life expectancy at birth.*total", cache = new_wdi_cache)
WDIsearch("^mortality.*rate.*infant", cache = new_wdi_cache)

wb_dat <- wb(indicator = c("NY.GDP.PCAP.KD", "SP.DYN.LE00.IN", "SP.DYN.IMRT.IN")) 
names(wb_dat)

wdi_dat <- WDI(indicator = c("NY.GDP.PCAP.KD", "SP.DYN.LE00.IN", "SP.DYN.IMRT.IN"),
               start = 1960, end = 2021, extra = TRUE) 
names(wdi_dat)

# Clean up
wb_countries <- wbcountries() 
names(wb_countries)

wb_dat <- merge(wb_dat, y = wb_countries[c("iso2c", "region")], by = "iso2c", all.x = TRUE)
wb_dat <- subset(wb_dat, region != "Aggregates") # this also removes NAs

wdi_dat <- subset(wdi_dat, region != "Aggregates") # this also removes NAs

wb_dat$indicatorID[wb_dat$indicatorID == "NY.GDP.PCAP.KD"] <- "GDP"
wb_dat$indicatorID[wb_dat$indicatorID == "SP.DYN.LE00.IN"] <- "life_expectancy"
wb_dat$indicatorID[wb_dat$indicatorID == "SP.DYN.IMRT.IN"] <- "infant_mortality"
wb_dat <- dcast(wb_dat, iso2c + country + date + region ~ indicatorID,  value.var = 'value')

names(wdi_dat)[which(names(wdi_dat) == "NY.GDP.PCAP.KD")] <- "GDP"
names(wdi_dat)[which(names(wdi_dat) == "SP.DYN.LE00.IN")] <- "life_expectancy"
names(wdi_dat)[which(names(wdi_dat) == "SP.DYN.IMRT.IN")] <- "infant_mortality"

# Plot a graph
ggplot(subset(wb_dat, date == "2008"), aes(x = GDP, y = infant_mortality)) + geom_point()
ggplot(subset(wdi_dat, year == 2008), aes(x = GDP, y = infant_mortality)) + geom_point()

# Reproduce graphs from the paper
# Life Expectancy
lifexp_countries <- subset(wb_dat, country %in% c("United States", "Rwanda", "Mongolia", "Pakistan", "Lao PDR", "Bhutan", "Malaysia", "Brazil", "Ireland", "Japan", "Sweden", "Netherlands"))

ggplot(subset(lifexp_countries, date == "2008"), aes(x = GDP, y = life_expectancy, color = country == "United States")) + 
  geom_point() +  
  geom_text(aes(label = country), size=3, nudge_y = 0.4) +
  scale_x_continuous(limits = c(0, 70000))

# Infant Mortality
infmort_countries <- subset(wb_dat, country %in% c("United States", "Tonga", "Colombia", "Grenada", "Sri Lanka", "Malta", "Germany", "Japan", "Sweden", "Netherlands"))

ggplot(subset(infmort_countries, date == "2008"),
       aes(x = GDP, y = infant_mortality, color = country == "United States")) + 
  geom_point() +  
  geom_text(aes(label = country), size=3, nudge_y = 0.2) +
  scale_x_continuous(limits = c(0, 70000))

# Simple interactivity
g <- ggplot(subset(infmort_countries, date > "1999" & date <= "2021"),
            aes(x = GDP, y = infant_mortality, color = country == "United States", tooltip = country)) + 
  geom_point() + 
  facet_wrap(~ date) + 
  theme(legend.position="none")

ggplotly(g)

#-------------------------------------------------------------------------------------------------------------------#
#  4) Export results to "working directory" folder                                                                  #
#-------------------------------------------------------------------------------------------------------------------#

# Write to CSV file and save in "RawData" directory
#write.csv(append_trade, file = 'RawData/XXX.csv', row.names = FALSE)

# Save end/finish time
sessiontime.end <- Sys.time()
difftime(sessiontime.end, sessiontime.start)

## DELETE ALL FILES ##
rm(list=ls())

# *** END ***