# ------------------------------------------------------------------------------------------------------------------ #
# Collect (from 'RawData'), clean (saved to 'CleanData') and compile final database (saved to 'FinalDatabase')       #
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
               "bit64","zoo","Rcpp","tidyr","scales","knitr","rvest","RCurl","httr","readxl","xts","tools","vroom",
               "lubridate","filesstrings","tradestatistics","tidyverse","gganimate","gapminder","timeDate","janitor",
               "sf","sp","fuzzyjoin","geosphere","arrangements")

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

# Create Agricultural Growing Season and Trade (AGST) Dataset outline
string <- "http://comtrade.un.org/data/cache/partnerAreas.json" # Get list of reporters/partners
countries <- fromJSON(file=string)
countries <- as.data.table(t(sapply(countries$results, rbind)))
countries <- countries[-1, ]
countries <- countries[-1, ]
countries <- as.data.table(countries[ ,-2])
#countries <- unlist(countries, use.names = FALSE)
#countries.combo = t(combn(countries, 2))
#colnames(countries.combo) <- c("reporter", "partner")
#countries.combo = unique(as.data.table(countries.combo))
commodities <- as.data.table(read.xlsx2("RawData/2022-06-24_Agric_Growth_Cycle_Products.xlsx",
                                        sheetIndex = 1, colIndex = 1, header = TRUE))
start_date <- as.Date("2017/01/01") # Create start date
period <- seq(start_date, by = "month", length.out = elapsed_months(Sys.time(), start_date))
period <- format(as.Date(period), format = "%Y%m")
period <- as.list(period)
AGST.dataset <- as.data.table(tidyr::crossing(period, commodities, countries)) # Create dataset outline
AGST.dataset[, `:=`(year = substr(period, 1L, 4L), month = substr(period, 5L, 6L))] # Split Date into Year and Month
setcolorder(AGST.dataset, c(1, 4:5, 2:3)) # Reorder data.table columns (without copying)
colnames(AGST.dataset) <- c('Period', 'Year','Month', 'HS6', 'Country')
AGST.dataset <- AGST.dataset[, Country := as.numeric(Country)]
AGST.dataset <- AGST.dataset[order(Country, Year, Month, HS6)]
AGST.dataset$Country <- str_pad(AGST.dataset$Country, 3, pad = "0")
AGST.dataset$Country <- paste("C", AGST.dataset$Country, sep ='')
rm(countries, period) # Remove all unused objects
gc() # Cleanup unused memory in R
fwrite(setDT(AGST.dataset), file = "CleanData/AGST_Database_Outline.csv")

# Read in FAO to HS Concordance Tables
FPCtoHS <- janitor::clean_names(as.data.table(fread("RawData/FPC&D_HS_mappings_2020-01-07.csv")))
FCLtoHS <- janitor::clean_names(as.data.table(fread("RawData/FCL_HS_mappings_2020-01-07.csv")))
FPCtoHS = within(FPCtoHS, {hs_code = str_pad(hs_code, 6, pad = "0")})
FCLtoHS = within(FCLtoHS, {hs_code = str_pad(hs_code, 6, pad = "0")})
FPCtoHS$hs_code <- paste("HS", FPCtoHS$hs_code, sep = '')
FCLtoHS$hs_code <- paste("HS", FCLtoHS$hs_code, sep = '')
setnames(FPCtoHS, "fpc_d_code", "fcl_code")
setnames(FPCtoHS, "fpc_d_label", "fcl_label")
FAOtoHS <- rbind(FCLtoHS, FPCtoHS, fill = T)
FAOtoHS[,if_partial := NULL]
colnames(FAOtoHS) <- c("FAO_Code","FAO_Label","HS6")
rm(FPCtoHS, FCLtoHS) # Remove all unused objects
gc() # Cleanup unused memory in R

# Read in CEPII country codes list
CountryCodes <- janitor::clean_names(as.data.table(fread("RawData/Country_Codes_2022-09-14.csv")))

# Read in monthly trade data from UN Comtrade
temp_path <- 'RawData/UNComtrade'
temp_files <- list.files(path = temp_path, pattern = "*.csv")
temp_files <- substr(temp_files, 1, 6) # Create list of data.frame names without the ".csv" part
commodities$HS6 <- gsub("^.{0,2}", "", commodities$HS6) # Replace first 2 characters with empty string ""
commodities[] <- lapply(commodities, as.character)
commodities <- as.character(commodities$HS6)
for (i in temp_files) { # Load all files and convert to data.tables
  filepath <- paste(temp_path, i, sep = '/')
  assign(i, subset(janitor::clean_names(as.data.table(fread(paste(filepath, ".csv", sep = "")))),
                   commodity_code %in% commodities))
}
system.time(un.comtrade <- rbindlist(mget(ls(pattern = "20")), fill = TRUE)) # Join vertically using rbindlist
rm(list = ls(pattern = "20")) # Remove a whole set of named-alike objects
comtrade.data <- un.comtrade[trade_flow != c("Re-exports", "Re-imports"),] # Remove re-exports and re-imports
# Convert to mirror data
mirror.imports <- subset(comtrade.data, trade_flow == "Imports",
                         c(period, commodity_code, reporter_code, partner_code, netweight_kg, trade_value_us))
colnames(mirror.imports) <- c("period","commodity_code","partner_code","reporter_code","netweight_kg","trade_value_us")
mirror.exports <- subset(comtrade.data, trade_flow == "Exports",
                         c(period, commodity_code, reporter_code, partner_code, netweight_kg, trade_value_us))
comtrade.data <- rbind(mirror.exports, mirror.imports)
# Drop duplicate values in data extracted by selecting maximum value for each unique flow
comtrade.data <- aggregate(trade_value_us ~ period+commodity_code+reporter_code+partner_code+netweight_kg,
                           un.comtrade, max)
setDT(comtrade.data)
comtrade.data[, `:=`(year = substr(period, 1L, 4L), month = substr(period, 5L, 6L))] # Split Date into Year and Month
setcolorder(comtrade.data, c(1, 7:8, 2:6)) # Reorder data.table columns (without copying)
colnames(comtrade.data) <- c('Period','Year','Month','HS6','Reporter','Partner','NetWeight_kg','Value_USD')
comtrade.data$HS6 <- str_pad(comtrade.data$HS6, 6, pad = "0") # Add leading zeros using "str_pad"
comtrade.data$HS6 <- paste("HS", comtrade.data$HS6, sep = '') # Change country code and product code to TEXT/CHARACTER
comtrade.data$Reporter <- str_pad(comtrade.data$Reporter, 3, pad = "0")
comtrade.data$Partner <- str_pad(comtrade.data$Partner, 3, pad = "0")
comtrade.data$Reporter <- paste("C", comtrade.data$Reporter, sep ='')
comtrade.data$Partner <- paste("C", comtrade.data$Partner, sep ='')
comtrade.data <- comtrade.data[order(Reporter, Partner, Year, Month, HS6)]
fwrite(comtrade.data, file = 'CleanData/comtrade_monthly_data_201701-202207.csv')
saveRDS(comtrade.data, file = "CleanData/comtrade_monthly_data_201701-202207.rds", compress = TRUE)
rm(un.comtrade, mirror.exports, mirror.imports) # Remove all unused objects
gc() # Cleanup unused memory in R

# Read in Distance between countries
dist <-readxl::read_xls('RawData/dist_cepii.xls')
dist <- dist[,c('iso_o','iso_d','contig','dist','distcap')]
dist <- merge(x = dist, y = CountryCodes[ , c("alpha_3_code", "numeric")], by.y = "alpha_3_code", by.x = c("iso_o"), all.x=TRUE)
setnames(dist, "numeric", "iso_o_num")
dist <- merge(x = dist, y = CountryCodes[ , c("alpha_3_code", "numeric")], by.y = "alpha_3_code", by.x = c("iso_d"), all.x=TRUE)
setnames(dist, "numeric", "iso_d_num")
setcolorder(dist, c(2,6,1,7,3:5)) # Reorder data.table columns (without copying)
#print(head(dist))
fwrite(dist, file = 'CleanData/dist_cepii.csv') #save cleaned version

# Read in Geometric Variables (Latitude, Longitude, area, dummy variables etc)
geo <-readxl::read_xls('RawData/geo_cepii.xls')
geo <- geo[, c('cnum','iso3','area','dis_int','lat','lon','landlocked')]
geo$cnum <- str_pad(geo$cnum, 3, pad = "0")
geo$cnum <- paste("C", geo$cnum, sep ='')
colnames(geo) <- c("Country_num","ISO3","Country_Area","Dist_Int","Lat","Lon","Landlocked")
setDT(geo)
fwrite(geo, file = 'CleanData/geo_cepii.csv') # Save cleaned version

# Read in Capital Stocks at Current PPP (2017)
ck <- readxl::read_xlsx('RawData/pwt100.xlsx', sheet = 3)
ck <- ck[, c('countrycode','country','year','ck')]
ck <- merge(x = ck, y = CountryCodes[ , c("alpha_3_code", "numeric")], by.y = "alpha_3_code", by.x = c("countrycode"), all.x=TRUE)
setnames(ck, "numeric", "iso_num")
setcolorder(ck, c(5,1:4)) # Reorder data.table columns (without copying)
colnames(ck) <- c("Country_num","ISO3","Country_Name","Year","CK")
fwrite(ck, file = 'CleanData/pwt100.csv') # Save cleaned version

# Read in Population data (in thousands)
pop <- as.data.table(fread('RawData/WPP2022_TotalPopulationBySex.csv'))
pop <- subset(pop, select = c(ISO3_code, LocID, Time, PopTotal, PopDensity))
colnames(pop) <- c('Country_iso','CountryCode','Year','PopTotal','PopDensity')
iso_code <- read.csv('RawData/wikipedia-iso-country-codes.csv') # Wikipedia ISO codes list
pop <- merge(iso_code[,3:4], pop, by.x = "Numeric.code", by.y = "CountryCode")
pop <- as.data.table(subset(pop, select = c(1:2,4:6)))
setcolorder(pop, c(2, 1, 3:5)) # Reorder data.table columns (without copying)
colnames(pop) <- c('Country_iso','Country_num','Year','PopTotal','PopDensity')
pop$Country_num <- str_pad(pop$Country_num, 3, pad = "0")
pop$Country_num <- paste("C", pop$Country_num, sep ='')
pop$PopTotal <- pop$PopTotal * 1000
pop <- pop[order(Country_num, Year)]
pop$Year <- as.character(pop$Year)
fwrite(pop, file = 'CleanData/population_data.csv') # Save cleaned version

# Read in Export and Import data to Calculate Real Trade Openness (Import and Export data are % of GDP)
exp <- read.csv(file = 'RawData/Exports_world_bank.csv')
names(exp) <- gsub('X', '', names(exp))
imp <- read.csv(file = 'RawData/Imports_world_bank.csv')
names(imp) <- gsub('X', '', names(imp))
exp <- melt(exp, id.vars = c('CountryName','CountryCode'), variable.name = 'Year', value.name = 'Exports')
colnames(exp) <- c('CountryName', 'CountryCode','Year', 'Exports')
imp <- melt(imp, id.vars = c('CountryName','CountryCode'), variable.name = 'Year', value.name = 'Imports')
colnames(imp) <- c('CountryName', 'CountryCode','Year', 'Imports')
Real_TO <- merge(exp, imp, by = c("CountryName","CountryCode", 'Year'))
TO <- (Real_TO[, 4] + Real_TO[, 5]) / 100
Real_TO <- cbind(Real_TO, TO)
col_drop <- c('Exports', 'Imports')
Real_TO <- Real_TO[,!names(Real_TO) %in% col_drop]
Real_TO <- merge(x = Real_TO, y = CountryCodes[ , c("alpha_3_code", "numeric")], by.y = "alpha_3_code", by.x = c("CountryCode"), all.x=TRUE)
setnames(Real_TO, "numeric", "iso_num")
setcolorder(Real_TO, c(5,1:4)) # Reorder data.table columns (without copying)
rm(exp, imp) # Remove all unused objects
gc() # Cleanup unused memory in R

# Read in monthly temperature data
temp_path <- 'RawData/Temperature'
temp_files <- list.files(path=temp_path,pattern= "*.csv")
temp_files <- lapply(temp_files, function(x) paste(temp_path,x,sep='/')) # List of file names
temp <- temp_files %>%
  setNames(nm = .) %>% 
  map_df(~read_csv(.x, skip = 2, col_types = cols(), col_names = TRUE), .id = "Country_iso")
temp$Country_iso <- substr(temp$Country_iso, 58, 60) # Create list of data.frame names without the ".csv" part
setnames(temp, "X1", "Year")
temp <- merge(x = temp, y = CountryCodes[ , c("alpha_3_code", "numeric")], by.y = "alpha_3_code", by.x = c("Country_iso"), all.x=TRUE)
setnames(temp, "numeric", "Country_num")
setcolorder(temp, c(15,1:14)) # Reorder data.table columns (without copying)
temp.m <- melt(setDT(temp), id = c("Country_num", "Country_iso", "Year")) # Reshape data with "melt" function
setnames(temp.m, "variable", "Month")
setnames(temp.m, "value", "Temperature")
temp.m$Year <- as.character(temp.m$Year)
temp.m$Month <- as.character(temp.m$Month)
temp.m$Month <- match(temp.m$Month, month.abb)
temp.m$Month <- str_pad(temp.m$Month, 2, pad = "0")
#names(temp) <- sub('X.','',names(temp))
temp.a = aggregate(x = temp.m$Temperature, by = list(temp.m$Country_num, temp.m$Country_iso, temp.m$Year), FUN = 'mean')
colnames(temp.a) <- c('Country_num','Country_iso','Year','Temperature')
setDT(temp.a)
setDT(temp.m)
fwrite(temp.a, file = 'RawData/temperature_annual_data.csv')
fwrite(temp.m, file = 'RawData/temperature_monthly_data.csv')
rm(temp, temp_files) # Remove all unused objects
gc() # Cleanup unused memory in R

# Read in monthly rainfall data
rf_path <- 'RawData/Rainfall'
rf_files <- list.files(path=rf_path,pattern= "*.csv")
rf_files <- lapply(rf_files, function(x) paste(rf_path,x,sep='/')) # List of file names
#rf <- do.call(rbind, lapply(rf_files, function(x) read.csv(x, stringsAsFactors = FALSE)))
rf <- rf_files %>%
  setNames(nm = .) %>% 
  map_df(~read_csv(.x, skip = 2, col_types = cols(), col_names = TRUE), .id = "Country_iso")
rf$Country_iso <- substr(rf$Country_iso, 54, 56) # Create list of data.frame names without the ".csv" part
setnames(rf, "X1", "Year")
rf <- merge(x = rf, y = CountryCodes[ , c("alpha_3_code", "numeric")], by.y = "alpha_3_code", by.x = c("Country_iso"), all.x=TRUE)
setnames(rf, "numeric", "Country_num")
setcolorder(rf, c(15,1:14)) # Reorder data.table columns (without copying)
rf.m <- melt(setDT(rf), id = c("Country_num", "Country_iso", "Year")) # Reshape data with "melt" function
setnames(rf.m, "variable", "Month")
setnames(rf.m, "value", "Rainfall")
rf.m$Year <- as.character(rf.m$Year)
rf.m$Month <- as.character(rf.m$Month)
rf.m$Month <- match(rf.m$Month, month.abb)
rf.m$Month <- str_pad(rf.m$Month, 2, pad = "0")
#names(rf) <- sub('X.','',names(rf))
rf.a = aggregate(x = rf.m$Rainfall, by = list(rf.m$Country_num, rf.m$Country_iso, rf.m$Year), FUN = 'mean')
colnames(rf.a) <- c('Country_num','Country_iso','Year','Rainfall')
setDT(rf.a)
setDT(rf.m)
fwrite(rf.a, file = 'RawData/rainfall_annual_data.csv')
fwrite(rf.m, file = 'RawData/rainfall_monthly_data.csv')
rm(rf, rf_files) # Remove all unused objects
gc() # Cleanup unused memory in R

# Read in WTO and RTA data
gravity <- as.data.table(fread("RawData/Gravity_V202102.csv"))
gravity$iso3num_o <- str_pad(gravity$iso3num_o, 3, pad = "0")
gravity$iso3num_d <- str_pad(gravity$iso3num_d, 3, pad = "0")
gravity$iso3num_o <- paste("C", gravity$iso3num_o, sep ='')
gravity$iso3num_d <- paste("C", gravity$iso3num_d, sep ='')
rta <- as.data.table(subset(gravity, select = c(1:5,63:65)))
yrs <- 1948:2020
wto <- as.data.table(subset(gravity, year %in% yrs, select = c(1:2,4,59)))
wto <- unique(wto)
setcolorder(wto, c(3,2,1,4)) # Reorder data.table columns (without copying)
colnames(wto) <- c('Members','ISO','Year','WTO')
wto_com <- as.data.table(wto)
rm(gravity) # Remove all unused objects
gc() # Cleanup unused memory in R

# Read in list of countries by northern and southern hemisphere (GeoDataSource)
hemisphere <- as.data.table(fread('RawData/GEODATASOURCE-NORTHERN-SOUTHERN-HEMISPHERE-COUNTRIES.CSV'))
hemisphere <- merge(iso_code[,2:4], hemisphere, by.x = "Alpha.2.code", by.y = "country_code")
hemisphere$Numeric.code <- str_pad(hemisphere$Numeric.code, 3, pad = "0")
hemisphere$Numeric.code <- paste("C", hemisphere$Numeric.code, sep ='')
hemisphere <- as.data.table(subset(hemisphere, select = c(2:4)))
colnames(hemisphere) <- c('Country_iso','Country_num','Hemisphere')
hemisphere[, Hemisphere_Code := ifelse(Hemisphere == "northern", 1, 0)]
fwrite(hemisphere, file = 'CleanData/hemisphere.csv') # Save cleaned version

# Read in (TFP) International Agricultural Productivity (USDA)
TFP <- readxl::read_xlsx('RawData/AgTFPInternational2019_long.xlsx', sheet = 2)
TFP <- as.data.table(subset(TFP, select = c(2,3,9:30)))
TFP <- merge(x = TFP, y = CountryCodes[ , c("alpha_3_code", "numeric")], by.y = "alpha_3_code", by.x = c("ISO3"), all.x=TRUE)
setnames(TFP, "numeric", "Country_num")
setcolorder(TFP, c(25,1:24)) # Reorder data.table columns (without copying)

# Read in FAO data and create agfile_sorted data file (Annual)
FAO <- as.data.table(fread("RawData/Production_Crops_Livestock_E_All_Data_(Normalized).csv"))
ISO <- as.data.table(fread("RawData/iso3.csv"))
yrs <- 2017:2020
FAO <- merge(FAO, ISO, by.x = c("Area"), by.y = c("name"))
FAO <- as.data.table(subset(FAO, Year %in% yrs, select = c(12,1,4:6,8:10)))
FAO <- merge(x = FAO, y = CountryCodes[ , c("alpha_3_code", "numeric")], by.y = "alpha_3_code", by.x = c("iso3"), all.x=TRUE)
setnames(FAO, "numeric", "Country_num")
setcolorder(FAO, c(9,1:8)) # Reorder data.table columns (without copying)
fwrite(FAO, file = 'CleanData/Agfile_sorted_iso.csv') # Save cleaned version
rm(ISO) # Remove all unused objects
gc() # Cleanup unused memory in R

# Read in Irrigated Crop Calendars (AQUASTAT)
CC_path <- 'RawData/CropCalendars'
CC_files <- list.files(path = CC_path, pattern = "*.csv")
CC_files <- substr(CC_files, 1, 6) # Create list of data.frame names without the ".csv" part
for (i in CC_files) { # Load all files and convert to data.tables
  filepath <- paste(CC_path, i, sep = '/')
  assign(i, as.data.table(fread(paste(filepath, ".csv", sep = ""))))
}
rm(CC_files, CC_path)
ICC <- rbindlist(mget(ls(pattern = "CC_")), fill = TRUE) # Join vertically using rbindlist
rm(list = ls(pattern = "CC_")) # Remove a whole set of named-alike objects
#IrgCropCalendars <- readxl::read_xlsx('RawData/AQUASTAT_Irrigated_Crop_Calendars_Database_Clean.xlsx', sheet = 1)
ICC <- melt(setDT(ICC), id.vars = c("ISO","Country","Year","Irrigated_Crops","Area"))
colnames(ICC) <- c("ISO","Country","Year","Irrigated_Crops","Area","Month","Crop_Area")
system.time(rzero(ICC)) # Replace all missing values with zeros
ICC <- merge(x = ICC, y = CountryCodes[ , c("alpha_3_code", "numeric")], by.y = "alpha_3_code", by.x = c("ISO"), all.x=TRUE)
setnames(ICC, "numeric", "Country_num")
setcolorder(ICC, c(8,1:2,4,3,6,5,7)) # Reorder data.table columns (without copying)
ICC = within(ICC, { # Add leading zeros using "str_pad"
  Month = str_pad(Month, 2, pad = "0")
})
ICC <- setDT(ICC)[order(ISO, Year, Month)]
#ICC <- merge(x = ICC, y = FAOtoHS[ , c("FAO_Code","HS6")], by.x = "FAO_Label", by.y = c("Irrigated_Crops"), all.x=TRUE)
#setnames(FAO, "numeric", "Country_num")
setcolorder(FAO, c(9,1:8)) # Reorder data.table columns (without copying)
fwrite(ICC, file = 'CleanData/IrgCropCalendars.csv') # Save cleaned version

# Read in Global data set of Monthly Irrigated and Rainfed Crop Areas around the year 2000 (MIRCA2000, v.1.1)
growing.period05 <- as.data.table(fread('RawData/CELL_SPECIFIC_CROPPING_CALENDARS.TXT'))
growing.period30 <- as.data.table(fread('RawData/CELL_SPECIFIC_CROPPING_CALENDARS_30MN.TXT'))
setnames(growing.period05, "long", "lon")
CC.Alt <- as.data.table(subset(geo, select = c(1:2,5:6)))
loc_sf <- CC.Alt %>% st_as_sf(coords = c('lon', 'lat'), remove = T) %>% st_set_crs(4326) # Create sf objects from lat/lon points
stop_sf <- growing.period05 %>% st_as_sf(coords = c('lon', 'lat'), remove = T) %>% st_set_crs(4326) # Create sf objects from lat/lon points
joined_sf <- stop_sf %>% cbind(loc_sf[st_nearest_feature(stop_sf, loc_sf),]) # Use st_nearest_feature to cbind loc to stop by nearest points
CC.Alt <- as.data.table(subset(joined_sf, select = c(4:10)))
ICC.Alt <- merge(FAOtoHS, CC.Alt, by.y = c("crop"), by.x = c("FAO_Code"))
ICC.Alt = within(ICC.Alt, { # Add leading zeros using "str_pad"
  start = str_pad(start, 2, pad = "0")
  end = start = str_pad(end, 2, pad = "0")
})
setcolorder(setDT(ICC.Alt), c(9,8,1:7)) # Reorder data.table columns (without copying)
fwrite(ICC.Alt, file = 'CleanData/IrgCropCalendars_Alt.csv') # Save cleaned version
rm(loc_sf, stop_sf, joined_sf, growing.period05, growing.period30, CC.Alt) # Remove all unused objects
gc() # Cleanup unused memory in R

#-------------------------------------------------------------------------------------------------------------------#
#  4) Compile final database (saved to 'Final Database')                                                            #
#-------------------------------------------------------------------------------------------------------------------#

## Merge data sources on country, commodity, year and/or month
AGST.data <- merge(AGST.dataset, temp.m, by.y = c("Country_num","Year","Month"), by.x = c("Country","Year","Month"), all.x=TRUE)
AGST.data[,Country_iso := NULL]
AGST.data <- merge(AGST.data, rf.m, by.y = c("Country_num","Year","Month"), by.x = c("Country","Year","Month"), all.x=TRUE)
AGST.data[,Country_iso := NULL]
AGST.data <- left_join(AGST.data, geo, by = c("Country" = "Country_num"))
AGST.data[,c("ISO3","Lat","Lon") := NULL]

setkey(AGST.data, Country, Year)
setkey(pop, Country_num, Year)
AGST.data <- AGST.data[pop]
AGST.data[,c("ISO3","Lat","Lon") := NULL]
AGST.data <- merge(x = AGST.data, y = pop, by.y = c("Country_num","Year"), by.x = c("Country","Year"), all.x=TRUE, allow.cartesian=TRUE)
data[,Country_num := NULL]

df <- merge(rta, dist, by.y = c("Country_num","Year","Month"), by.x = c("Country","Year", "Month"))
df <- df[which(df$year==1995:2020),]
colnames(wto_com)[3] <- "Year"
df <- merge(df, wto_com, by.x=c("iso3num_o","year"), by.y=c("ISO",'Year'))
df <- df[,-19]
df_sub <- df %>% select(iso_d, year) %>%
  rename(ISO=iso_d)
wto_sub_com <- right_join(wto1,df_sub, by = "ISO") %>%
  mutate(wto_d = ifelse(year >= Year, 1, 0))
wto_sub_com<-wto_sub_com[,-c(1,4)]
df$wto_d <- wto_sub_com$wto_d
df <- merge(df, rta, by.x=c("countrycode","iso_d","year"), by.y=c("iso_o","iso_d",'Year'))
df <- merge(df, pop[,c("iso","Year","Population")], by.x=c("iso_d","year"), by.y=c("iso",'Year'))
df <- df[,-6]
colnames(df)[6] <- "pop_o"
colnames(df)[21] <- "pop_d"
colnames(df)[3] <- "iso_o"
df <- df[,-4]
colnames(df)[16] <- "rainfall"
colnames(df)[15] <- "temperature"
df <- merge(df, geo[,c(2:3)], by.x=c("iso_d"), by.y=c("iso3"))
colnames(df)[6] <- "area_o"
colnames(df)[21] <- "area_d"

# df <- merge(ck,pop,by.x=c("countrycode","country","year"),by.y=c("iso","Region, subregion, country or area *","Year"))
# df <- merge(df,geo,by.x=c("countrycode","country"),by.y=c("iso3","country"))
# df <- merge(df,dist,by.x=c("countrycode"),by.y=c("iso_o"))
# df <- merge(df,Real_TO,by.x=c("countrycode","country","year"),by.y=c("CountryCode",'CountryName','Year'))
# df <- merge(df,temp,by.x=c("countrycode","year"),by.y=c("Country_iso",'Year'))
# df <- merge(df,rf,by.x=c("countrycode","year"),by.y=c("Country_iso",'Year'))

# Save merged data.table as new .csv file
fwrite('FinalDatabase/AGST_Database_Final.csv')
saveRDS(AGST.dataset, file = "CleanData/AGST_Database_Final.rds", compress = TRUE)

# Save end/finish time
sessiontime.end <- Sys.time()
difftime(sessiontime.end, sessiontime.start)

## DELETE ALL FILES ##
rm(list=ls())

# *** END ***

#############
## APENDIX ##
#############

# Read in WTO (Old)
#wto <- read.csv(file = "RawData/wto_new.csv")
#yrs <- 1995:2020
#tmp <- as.data.frame(matrix(0,nrow=(nrow(wto)*length(yrs)),ncol=ncol(wto)-2))
#index <- rep(seq_len(nrow(wto)), each = length(yrs))
#con_repeated <- as.character(wto[,'Members'])
#iso_repeated <- as.character(wto[,'ISO'])
#colnames(tmp) <- c('Members','Year','ISO')
#tmp['Members'] <- con_repeated
#tmp['ISO'] <- iso_repeated
#tmp <- tmp[order(tmp[,1]),]
#rownames(tmp) <- NULL
#tmp['Year'] <- rep(yrs,nrow(wto))
#wto1 <- wto[-c(2,3)]
#wto_com <- left_join(wto1,tmp,by = c("Members","ISO")) %>% mutate(wto_o = 0)
#wto_com <- wto_com %>%   mutate(wto_o = ifelse(Year.y >= Year.x, 1, 0))
#wto_com <- wto_com[,-2]