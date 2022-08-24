# ------------------------------------------------------------------------------------------------------------------ #
# Collect (from 'RawData'), clean (saved to 'CleanData') and compile final database (saved to 'FinalDatabase')       #
# For: Data and Artificial Intelligence for African Trade (DAIAT) Initiative                                         #
# Last Edited: 2022-08-25 (RR)                                                                                       #
# ------------------------------------------------------------------------------------------------------------------ #

# *** START ***

## Global settings ##
rm(list = ls()) # Remove ALL data objects

# Specify JAVA_HOME directory
Sys.setenv(JAVA_HOME = "C:/Program Files/Java/jre1.8.0_341/")
library("rJava")

# Set "working directory" to current file location
setwd(dirname(rstudioapi::getSourceEditorContext()$path))
getwd() # Verify if you set the directory correctly by the following command

# Increase Java memory allocation to 2 GB
options(java.parameters = "-Xmx5120m")

# Time R script for optimization
sessiontime.start <- Sys.time(); # Save start time

#-------------------------------------------------------------------------------------------------------------------#
#  1) Setting the R-CRAN repository for South Africa (TENET, Johannesburg)                                          #
#-------------------------------------------------------------------------------------------------------------------#

# Install required packages and load necessary libraries
write("Load libs..", stdout())
if (!require("pacman")) install.packages("pacman")
pacman::p_load("manipulate","gtools","plyr","utils","stats","xlsx","sqldf","tcltk","tcltk2","DescTools","tm","rjson",
               "dplyr","data.table","antitrust","ggplot2","rJava","XLConnect","gdata","openxlsx","astsa","forecast",
               "taRifx","reshape","reshape2","pbapply","circlize","migest","devtools","stringr","googleVis","XML",
               "bit64","zoo","Rcpp","tidyr","scales","knitr","rvest","RCurl","httr","readxl","xts","tools","timeDate",
               "lubridate","filesstrings","tradestatistics","tidyverse","gganimate","gapminder")

#-------------------------------------------------------------------------------------------------------------------#
#  2) Read in all raw data from 'RawData' folder                                                                    #
#-------------------------------------------------------------------------------------------------------------------#

# Read in Distance between countries
dist <-readxl::read_xls('RawData/dist_cepii.xls')
dist <- dist[,c('iso_o','iso_d','contig','dist','distcap')]
print(head(dist))
write.csv(dist, file='CleanData/dist_cepii.csv', row.names = FALSE) #save cleaned version

# Read in Geometric Variables(Latitude, Longitude, area, dummy variables etc)
geo <-readxl::read_xls('RawData/geo_cepii.xls')
geo <- geo[,c('country','iso3','area','dis_int','lat','lon','landlocked')]
print(head(geo))
write.csv(geo, file='CleanData/geo_cepii.csv', row.names = FALSE) #save cleaned version

# Read in Capital Stocks at Current PPPs (2017)
ck <- readxl::read_xlsx('RawData/pwt90.xlsx',sheet=3)
ck <- ck[,c('countrycode','country','year','ck')]
print(head(ck))
write.csv(ck,file='CleanData/pwt90.csv',row.names = FALSE) #save cleaned version

# Read in Population data (in thousands)...
pop <- readxl::read_xlsx('RawData/WPP2017_POP_F01_1_TOTAL_POPULATION_BOTH_SEXES_1.xlsx', sheet = 1)
col_drop <- c('Index', 'Variant', 'Notes')
pop <- pop[,!names(pop) %in% col_drop]
pop <- melt(pop, id.vars= c('Region, subregion, country or area *','Country code'), variable.name='Year', value.name='Population')
write.csv(pop,file='CleanData/population_data.csv',row.names = FALSE) #save cleaned version
iso_code <- read.csv('RawData/wikipedia-iso-country-codes.csv')#Wikipedia iso codes list
pop <- merge(iso_code[,3:4],pop,by.x="Numeric.code",by.y="Country code")
names(pop)[names(pop)=="Alpha.3.code"]<-'iso'
pop$Population <- pop$Population*1000

# Read in Export and Import to Calculate Real Trade Openness (Import and Export data are % of GDP)
exp <- read.csv(file = 'RawData/Exports_world_bank.csv')
names(exp) <- gsub('X','',names(exp))
imp <- read.csv(file = 'RawData/Imports_world_bank.csv')
names(imp) <- gsub('X','',names(imp))
exp <- melt(exp, id.vars= c('CountryName','CountryCode'), variable.name='Year', value.name='Exports')
imp <- melt(imp, id.vars= c('CountryName','CountryCode'), variable.name='Year', value.name='Imports')
Real_TO <-merge(exp,imp,by=c("CountryName","CountryCode", 'Year'))
TO <- (Real_TO[,4]+Real_TO[,5])/100
Real_TO <- cbind(Real_TO,TO)
col_drop <- c('Exports', 'Imports')
Real_TO <- Real_TO[,!names(Real_TO) %in% col_drop]

# Read in temperature data
temp_path <- 'RawData/Temperature'
temp_files <- list.files(path=temp_path,pattern= "*.csv")
temp_files <- lapply(temp_files, function(x) paste(temp_path,x,sep='/')) #list of file names
temp <- temp_files %>%
  setNames(nm = .) %>% 
  map_df(~read_csv(.x, skip = 2, col_types = cols(), col_names = TRUE), .id = "Country_iso")
temp$Country_iso <- substr(temp$Country_iso, 58, 60) # Create list of data.frame names without the ".csv" part
setnames(temp, "X1", "Year")
temp.m <- melt(temp, id = c("Country_iso", "Year")) # Reshape data with "melt" function
setnames(temp.m, "variable", "Month")
setnames(temp.m, "value", "Temperature")
#names(temp) <- sub('X.','',names(temp))
temp.a = aggregate(x = temp.m$Temperature, by = list(temp.m$Country_iso, temp.m$Year), FUN = 'mean')
colnames(temp.a) <- c('Country_iso','Year','Temperature')
write.csv(temp.a, file = 'RawData/temperature_annual_data.csv', row.names = FALSE)
write.csv(temp.m, file = 'RawData/temperature_monthly_data.csv', row.names = FALSE)
rm(temp, temp_files) # Remove all unused objects

# Read in rainfall data
rf_path <- 'RawData/Rainfall'
rf_files <- list.files(path=rf_path,pattern= "*.csv")
rf_files <- lapply(rf_files, function(x) paste(rf_path,x,sep='/')) #list of file names
#rf <- do.call(rbind, lapply(rf_files, function(x) read.csv(x, stringsAsFactors = FALSE)))
rf <- rf_files %>%
  setNames(nm = .) %>% 
  map_df(~read_csv(.x, skip = 2, col_types = cols(), col_names = TRUE), .id = "Country_iso")
rf$Country_iso <- substr(rf$Country_iso, 54, 56) # Create list of data.frame names without the ".csv" part
setnames(rf, "X1", "Year")
rf.m <- melt(rf, id = c("Country_iso", "Year")) # Reshape data with "melt" function
setnames(rf.m, "variable", "Month")
setnames(rf.m, "value", "Rainfall")
#names(rf) <- sub('X.','',names(rf))
rf.a = aggregate(x = rf.m$Rainfall, by = list(rf.m$Country_iso, rf.m$Year), FUN = 'mean')
colnames(rf.a) <- c('Country_iso','Year','Rainfall')
write.csv(rf.a, file = 'RawData/rainfall_annual_data.csv', row.names = FALSE)
write.csv(rf.m, file = 'RawData/rainfall_monthly_data.csv', row.names = FALSE)
rm(rf, rf_files) # Remove all unused objects

# Read in wto data
wto <- read.csv(file = "RawData/wto_new.csv")
yrs <- 1995:2016
tmp <- as.data.frame(matrix(0,nrow=(nrow(wto)*length(yrs)),ncol=ncol(wto)-2))
index <- rep(seq_len(nrow(wto)), each = length(yrs))
con_repeated <- as.character(wto[,'Members'])
iso_repeated <- as.character(wto[,'ISO'])
colnames(tmp) <- c('Members','Year','ISO')
tmp['Members'] <- con_repeated
tmp['ISO'] <- iso_repeated
tmp<-tmp[order(tmp[,1]),]
rownames(tmp) <- NULL
tmp['Year'] <- rep(yrs,nrow(wto))
wto1 <- wto[-c(2,3)]
wto_com <- left_join(wto1,tmp,by = c("Members","ISO")) %>% mutate(wto_o = 0)
wto_com <- wto_com %>%   mutate(wto_o = ifelse(Year.y >= Year.x, 1, 0))
wto_com <-wto_com[,-2]

# Read in rta data
rta <- read.csv(file = "RawData/rta.csv")

#-------------------------------------------------------------------------------------------------------------------#
#  3) Merge datasets on country and year                                                                            #
#-------------------------------------------------------------------------------------------------------------------#

### Merge datasets on country and year
df <- merge(rta,dist,by.y=c("countrycode","iso_d","year"),by.x=c("iso_o","iso_d",'Year'))
# df <- merge(ck,pop,by.x=c("countrycode","country","year"),by.y=c("iso","Region, subregion, country or area *","Year"))
# df <- merge(df,geo,by.x=c("countrycode","country"),by.y=c("iso3","country"))
# df <- merge(df,dist,by.x=c("countrycode"),by.y=c("iso_o"))
# df <- merge(df,Real_TO,by.x=c("countrycode","country","year"),by.y=c("CountryCode",'CountryName','Year'))
# df <- merge(df,temp,by.x=c("countrycode","year"),by.y=c("Country_iso",'Year'))
# df <- merge(df,rf,by.x=c("countrycode","year"),by.y=c("Country_iso",'Year'))
df<-df[which(df$year==1995:2016),]
colnames(wto_com)[3]<-"Year"
df<-merge(df,wto_com,by.x=c("countrycode","year"),by.y=c("ISO",'Year'))
df<-df[,-19]
df_sub<- df %>% select(iso_d, year) %>%
  rename(ISO=iso_d)
wto_sub_com <- right_join(wto1,df_sub, by = "ISO") %>%
  mutate(wto_d = ifelse(year>= Year, 1, 0))
wto_sub_com<-wto_sub_com[,-c(1,4)]
df$wto_d<-wto_sub_com$wto_d
df<-merge(df,rta,by.x=c("countrycode","iso_d","year"),by.y=c("iso_o","iso_d",'Year'))
df<-merge(df,pop[,c("iso","Year","Population")],by.x=c("iso_d","year"),by.y=c("iso",'Year'))
df<-df[,-6]
colnames(df)[6]<-"pop_o"
colnames(df)[21]<-"pop_d"
colnames(df)[3]<-"iso_o"
df<-df[,-4]
colnames(df)[16]<-"rainfall"
colnames(df)[15]<-"temperature"
df <- merge(df,geo[,c(2:3)],by.x=c("iso_d"),by.y=c("iso3"))
colnames(df)[6]<-"area_o"
colnames(df)[21]<-"area_d"

# Save end/finish time
sessiontime.end <- Sys.time()
difftime(sessiontime.end, sessiontime.start)

## DELETE ALL FILES ##
rm(list=ls())

# *** END ***