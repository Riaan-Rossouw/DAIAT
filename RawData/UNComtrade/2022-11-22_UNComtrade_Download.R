# ------------------------------------------------------------------------------------------------------------------ #
# Code to download specific data needed from UN Comtrade and then save it as a csv file                              #
# For: Data and Artificial Intelligence for African Trade (DAIAT) Initiative                                         #
# Last Edited: 2022-11-21 (RR)                                                                                       #
# ------------------------------------------------------------------------------------------------------------------ #

# *** START ***

## Global settings ##
rm(list = ls()) # Remove ALL data objects
gc() # Cleanup unused memory in R

# Specify JAVA_HOME directory
Sys.setenv(JAVA_HOME = "C:/Program Files/Java/jre1.8.0_351/")
library("rJava")

# Set "working directory" to current R-file location
setwd(dirname(rstudioapi::getSourceEditorContext()$path))
getwd() # Verify if you set the directory correctly by the following command

# Increase Java memory allocation to 2 GB
options(java.parameters = "-Xmx5120m")

# Time R script for optimization
sessiontime.start <- Sys.time(); # Save start time

#-------------------------------------------------------------------------------------------------------------------#
#  1) Install required packages and load libraries                                                                  #
#-------------------------------------------------------------------------------------------------------------------#

write("Load libs..", stdout())
if (!require("pacman")) install.packages("pacman")
pacman::p_load("manipulate","gtools","plyr","utils","stats","xlsx","sqldf","tcltk","tcltk2","DescTools","tm","rjson",
               "dplyr","data.table","antitrust","ggplot2","rJava","XLConnect","gdata","openxlsx","astsa","forecast",
               "taRifx","reshape","reshape2","pbapply","circlize","migest","devtools","stringr","googleVis","maps",
               "bit64","zoo","Rcpp","tidyr","scales","knitr","rvest","RCurl","httr","XML","readxl","xts","tools",
               "lubridate","timeDate","filesstrings","tradestatistics","comtradr","purrr","remotes","rlist","curl",
               "rsconnect","tidyverse","naptime")

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

# Script/function (get.Comtrade) to download data from UN Comtrade
# Reference: http://comtrade.un.org/data/Doc/api/ex/r
# A user-defined function to extract data from the UN Comtrade API
# The function defined in this example, get.Comtrade(), extracts data from UN Comtrade using either the csv or the json format.
get.Comtrade <- function(url="http://comtrade.un.org/api/get?"
                         ,maxrec=50000
                         ,type="C"
                         ,freq="A"
                         ,px="HS"
                         ,ps="now"
                         ,r
                         ,p
                         ,rg="all"
                         ,cc="TOTAL"
                         ,fmt="json"
)
{
  string<- paste(url
                 ,"max=",maxrec,"&" #maximum no. of records returned
                 ,"type=",type,"&" #type of trade (c=commodities)
                 ,"freq=",freq,"&" #frequency
                 ,"px=",px,"&" #classification
                 ,"ps=",ps,"&" #time period
                 ,"r=",r,"&" #reporting area
                 ,"p=",p,"&" #partner country
                 ,"rg=",rg,"&" #trade flow
                 ,"cc=",cc,"&" #classification code
                 ,"fmt=",fmt #Format
                 ,sep = ""
  )
  
  if(fmt == "csv") {
    raw.data<- read.csv(string,header=TRUE)
    return(list(validation=NULL, data=raw.data))
  } else {
    if(fmt == "json" ) {
      raw.data<- fromJSON(file=string)
      data<- raw.data$dataset
      validation<- unlist(raw.data$validation, recursive=TRUE)
      ndata<- NULL
      if(length(data)> 0) {
        var.names<- names(data[[1]])
        data<- as.data.frame(t( sapply(data,rbind)))
        ndata<- NULL
        for(i in 1:ncol(data)){
          data[sapply(data[,i],is.null),i]<- NA
          ndata<- cbind(ndata, unlist(data[,i]))
        }
        ndata<- as.data.frame(ndata)
        colnames(ndata)<- var.names
      }
      return(list(validation=validation,data =ndata))
    }
  }
}

# chunk_vector: This will chunk any vector in chunks of max.len
chunk_vector <- function(vtr, max.len = 20) {
  split(vtr, ceiling(seq_along(vtr) / max.len))
}

# chunk_dates: Breaks dates by years in chunks of size 5. Use lubridate and pass actual dates to it.
chunk_dates <- function(start_date, end_date, size = 5) {
  force(start_date)
  force(end_date)
  mainint <- interval(start_date, end_date)
  mainper <- as.period(mainint, unit = "years")
  yearspan <- mainper@year
  chunk_vector(seq(from = start_date, to = end_date, by = 'year'), max.len = size) %>%
    rlist::list.apply(function(dates) {
      mi <- min(dates)
      ma <- max(dates)
      list(start = mi, end = ma + period(11, 'months') + period(31, 'days'))
    })
}

# ct_wait_for_available: Waits about until counter reset time if you've exhausted your queries for the hour.
# Keep in mind, the counter is kept locally. Upstream UN Comtrade API does not tell you how many queries you have left.
ct_wait_for_available <- function(at_least = 0) {
  #ct_register_token(COMTRADE_TOKEN)
  repeat {
    remqueries <- ct_get_remaining_hourly_queries()
    message(paste0("\n ", at_least, " <= ", remqueries, " queries remaining in UN COMTRADE API query for the hour",
                   "\n Time to next cycle is: ", ct_get_reset_time()))
    if (remqueries <= at_least) {
      remtime <- ct_get_reset_time() - lubridate::now() 
      cat(paste0("\nStill ", remtime, " ", units(remtime), " to reset counter \n"))
      wtime <- as.period(remtime) + period(1, units = "mins")
      if (as.numeric(wtime) > 0) {
        cat(paste0("\nSleeping ", as.numeric(wtime), " seconds"))
        #Sys.sleep(as.numeric(wtime)) # Kept timing-out; replaced line with 'naptime' to force 1-hour pause
        naptime(lubridate::hours(1))
      } else {
        break
      }
    } else {
      break
    }
  }
}

#-------------------------------------------------------------------------------------------------------------------#
#  3) Download user-specified data from UN Comtrade using API and append data                                       #
#-------------------------------------------------------------------------------------------------------------------#

# API rate limits: The Comtrade API imposes rate limits on both guest users and premium users. 'comtradr' features
# automated throttling of API calls to ensure the user stays within the limits defined by UN Comtrade.

# Without user token: 1 request per second, 100 requests per hour.
# With valid user token: 1 request per second, 10,000 requests per hour.

# In addition to these rate limits, the API imposes some limits on parameter combinations. Between args reporters,
# partners, and the query date range, only one of these three may use the catch-all input "All". For the same group
# of three (reporters, partners, date range), if the input is not "All", then the maximum number of input values for
# each is five. For date range, if not using "All", then the start_date and end_date must not span more than five 
# months or five years. There is one exception to this rule, if arg freq is "monthly", then a single year can be
# passed to start_date and end_date and the API will return all of the monthly data for that year. For arg commod_codes,
# if not using input "All", then the maximum number of input values is 20 (although "All" is always a valid input).
# Additionally, the # maximum number of returned records from a single query without a token is 50,000. With a token,
# that number is 250,000.

# 'comtradr' features a few functions for working with the API rate limits and tokens.

# ct_register_token() allows the user to set an API token within the package environment.
# ct_get_remaining_hourly_queries() will return the number of remaining queries for the current hour.
# ct_get_reset_time() will return the date/time in which the current hourly time limit will reset, as a POSIXct object.

# Import list of free, public, forward proxy servers
#proxy.list <- fread('proxy-list-raw.txt')
# Add to Lappy/loop below: url = GET("https://comtrade.un.org/api/get?", use_proxy(proxy.list$proxy_list[a], proxy.list$port[a]), verbose())

# Get list of reporters/partners, commodities and create period (years/months) list
string <- "http://comtrade.un.org/data/cache/partnerAreas.json"
reporter <- fromJSON(file=string)
reporter <- as.data.table(t(sapply(reporter$results,rbind)))
reporter <- reporter[-1, ]
reporter <- reporter[-1, ]
reporter <- as.list(reporter[[2]])
reporter <- chunk_vector(reporter, max.len = 5)

commodities <- read.xlsx2("2022-06-24_Agric_Growth_Cycle_Products.xlsx", sheetIndex=1, colIndex=1, header=TRUE)
commodities$HS6 <- gsub("^.{0,2}", "", commodities$HS6) # Replace first 2 characters with empty string ""
commodities[] <- lapply(commodities, as.character)
commodities <- as.character(commodities$HS6)
commodities <- chunk_vector(commodities, max.len = 20)

period = 2017:2022 # Change period as required
period <- as.list(period)

# Create fields/headers for UN Comtrade data download using an example
x <- ct_search(reporters = "all",
               partners = "World",
               trade_direction = "all",
               commod_codes = "280461",
               start_date = 2017,
               end_date = 2017, 
               freq = "monthly")
x <- x[-c(1:1050),]

# Run loops to download monthly data, limits of 100 requests per hour, ps, r, p and cc are limited to 5 codes each. 
# Only one of the above codes may use the special ALL value in a given API call. Classification codes are limited to 20 items.
# ALL is always a valid classification code (http://comtrade.un.org/data/doc/api/)
lapply(reporter, function(i) {
  lapply(commodities, function(j) {
    lapply(period, function(k) {
      ct_wait_for_available(at_least = 0)
      x_temp <- ct_search(reporters = "all",
                          partners = i,
                          trade_direction = "all",
                          commod_codes = j,
                          start_date = k,
                          end_date = k, 
                          freq = "monthly")
      dx_temp <- as.data.frame(do.call(rbind, x_temp))
    })
  })
})

# Remove re-exports and re-imports
comtrade.data <- setDT(x)[trade_flow != c("Re-exports", "Re-imports"),]

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

# Split Date into Year and Month
setDT(comtrade.data)
comtrade.data[, `:=`(year = substr(period, 1L, 4L), month = substr(period, 5L, 6L))]
setcolorder(comtrade.data, c(1, 7:8, 2:6)) # Reorder data.table columns (without copying)
colnames(comtrade.data) <- c('Period','Year','Month','HS6','Reporter','Partner','NetWeight_kg','Value_USD')

# Add leading zeros using "str_pad"
comtrade.data$HS6 <- str_pad(comtrade.data$HS6, 6, pad = "0")
comtrade.data$HS6 <- paste("HS", comtrade.data$HS6, sep = '')
comtrade.data$Reporter <- str_pad(comtrade.data$Reporter, 3, pad = "0")
comtrade.data$Partner <- str_pad(comtrade.data$Partner, 3, pad = "0")
comtrade.data$Reporter <- paste("C", comtrade.data$Reporter, sep ='')
comtrade.data$Partner <- paste("C", comtrade.data$Partner, sep ='')
comtrade.data <- comtrade.data[order(Reporter, Partner, Year, Month, HS6)]

# Remove all unused objects
rm(x_temp, dx_temp, commodities, reporter, period)

#-------------------------------------------------------------------------------------------------------------------#
#  4) Export results to "working directory" folder                                                                  #
#-------------------------------------------------------------------------------------------------------------------#

# Write to CSV file and save in "CleanData" directory
fwrite(comtrade.data, file = 'CleanData/comtrade_monthly_data_201701-202207.csv')
saveRDS(comtrade.data, file = "CleanData/comtrade_monthly_data_201701-202207.rds", compress = TRUE)

# Save end/finish time
sessiontime.end <- Sys.time()
difftime(sessiontime.end, sessiontime.start)

## DELETE ALL FILES ##
rm(list=ls())

# *** END ***

#############
## APENDIX ##
#############

## Old (comtradr) For Loop replaced with lapply
for(i in reporter){
  for(j in commodities){
    for(k in period){
      ct_wait_for_available(at_least = 2)
      x_temp <- ct_search(reporters = "all",
                          partners = i,
                          trade_direction = "all",
                          commod_codes = j,
                          start_date = k,
                          end_date = k, 
                          freq = "monthly")
      x = do.call(rbind, list(x_temp, x))
    } 
  }
}

## Old (get.Comtrade) For Loop replaced with lapply
for(i in reporter){
  for(j in commodities){
    for(k in period){
      ct_wait_for_available(at_least = 2)
      x_temp <- get.Comtrade(r = "all", p = i, ps = k, freq = "M", fmt = "csv", rg = "all", cc = j)
      dx_temp <- as.data.frame(do.call(rbind, x_temp))
      x = do.call(rbind, list(dx_temp, x))
    } 
  }
}

## Alternative approach to above 'comtradr' setup, which also works.
# Get list of reporters/partners, commodities and years
string <- "http://comtrade.un.org/data/cache/partnerAreas.json"
reporter <- fromJSON(file=string)
reporter <- as.data.table(t(sapply(reporter$results,rbind)))
reporter <- reporter[-1, ]
reporter <- str_c(reporter[[1]], collapse = ",")
reporter <- as.list(str_extract_all(reporter, "\\d+(,\\d+){1,4}")[[1]])

commodities <- read.xlsx2("2022-06-24_Agric_Growth_Cycle_Products.xlsx", sheetIndex=1, colIndex=1, header=TRUE)
commodities$HS6 <- gsub("^.{0,2}", "", commodities$HS6) # Replace first 2 characters with empty string ""
commodities[] <- lapply(commodities, as.character)
commodities <- str_c(commodities[[1]], collapse = ",")
commodities <- as.list(str_extract_all(commodities, "\\d+(,\\d+){1,4}")[[1]])

start_date <- as.Date("2017/01/01") # Create start date
period <- seq(start_date, by = "month", length.out = elapsed_months(Sys.time(), start_date))
period <- format(as.Date(period), format = "%Y%m")
period <- str_c(period, collapse = ",")
period <- as.list(str_extract_all(period, "\\d+(,\\d+){1,4}")[[1]])

# After running the UN Comtrade function, test for loop
x <- get.Comtrade(r = "all", p = "0", ps = "201701", freq = "M", fmt = "csv", rg = "all", cc = "280461")
x <- as.data.frame(do.call(rbind, x))
x <- x[-c(1:88),]

# Run loops to download monthly data, limits of 100 requests per hour, ps, r, p and cc are limited to 5 codes each. 
# Only one of the above codes may use the special ALL value in a given API call. Classification codes are limited to 20 items.
# ALL is always a valid classification code (http://comtrade.un.org/data/doc/api/) 
remqueries <- 1
lapply(reporter, function(i) {
  lapply(commodities, function(j) {
    lapply(period, function(k) {
      while (remqueries <= 100) {
        remqueries <- remqueries + 1
        x_temp <- get.Comtrade(r = "all", p = i, ps = k, freq = "M", fmt = "csv", rg = "all", cc = j)
        dx_temp <- as.data.frame(do.call(rbind, x_temp))
        x = do.call(rbind, list(dx_temp, x))
      }
      cat(paste0("\nTime to next cycle is: ", format(.POSIXct(3700,tz="GMT"), "%H:%M:%S")))
      remqueries <- 1
      Sys.sleep(3700) # Sleep for 3700 seconds = 1 hour 1 minute and 40 seconds.
    })
  })
})

## End of alternative approach

## Tobi's RCA Dissertation Data Collection 

# Clear all 
rm(list = ls()) 

# Upload libraries 
library(rjson) 
library(plyr) 
library(tidyr) 
library(dplyr) 

# Read list of country codes into R 
string <- "http://comtrade.un.org/data/cache/partnerAreas.json" 
reporters <- fromJSON(file=string) 
reporters <- as.data.frame(t(sapply(reporters$results,rbind))) 

# Collect Data from UN Comtrade - code from UN Comtrade 
get.Comtrade <- function(url="http://comtrade.un.org/api/get?" 
                         ,maxrec=50000 
                         ,type="C" 
                         ,freq="A" 
                         ,px="HS" 
                         ,ps="now" 
                         ,r 
                         ,p 
                         ,rg="all" 
                         ,cc="TOTAL" 
                         ,fmt="json" 
) 
{ 
  string<- paste(url 
                 ,"max=",maxrec,"&" #maximum no. of records returned 
                 ,"type=",type,"&" #type of trade (c=commodities) 
                 ,"freq=",freq,"&" #frequency 
                 ,"px=",px,"&" #classification 
                 ,"ps=",ps,"&" #time period 
                 ,"r=",r,"&" #reporting area 
                 ,"p=",p,"&" #partner country 
                 ,"rg=",rg,"&" #trade flow 
                 ,"cc=",cc,"&" #classification code 
                 ,"fmt=",fmt #Format 
                 ,sep = "" 
  ) 
  
  if(fmt == "csv") { 
    raw.data<- read.csv(string,header=TRUE) 
    return(list(validation=NULL, data=raw.data)) 
  } else { 
    if(fmt == "json" ) { 
      raw.data<- fromJSON(file=string) 
      data<- raw.data$dataset 
      validation<- unlist(raw.data$validation, recursive=TRUE) 
      ndata<- NULL 
      if(length(data)> 0) { 
        var.names<- names(data[[1]]) 
        data<- as.data.frame(t( sapply(data,rbind))) 
        ndata<- NULL 
        for(i in 1:ncol(data)){ 
          data[sapply(data[,i],is.null),i]<- NA 
          ndata<- cbind(ndata, unlist(data[,i])) 
        } 
        ndata<- as.data.frame(ndata) 
        colnames(ndata)<- var.names 
      } 
      return(list(validation=validation,data =ndata)) 
    } 
  } 
}

# Break reporters down for batch downloading 
reporters1 = reporters[3:40,]
reporters2 = reporters[41:77,]
reporters3 = reporters[78:120,]
reporters4 = reporters[121:178,]
reporters5 = reporters[179:275,]
reporters6 = reporters[276:281,]
reporters7 = reporters[282:293,]

# Run loops to download data, limits of 100 requests per hour,ps, r and p are limited to 5 codes each. 
# Only one of the above codes may use the special ALL value in a given API call. Classification codes (cc)
# are limited to 20 items. ALL is always a valid classification code (http://comtrade.un.org/data/doc/api/) 

# Loop 1
x1 = reporters1$V1 
WorldTrade1 <- data.frame("Year"=as.Date(character()),"Flow"=character(),"Product_Code"=integer(),
                          "Description"=character(), "Reporter"=character(), "Value"=integer(), stringsAsFactors=FALSE) 
for (i in x1){ 
  print(i) 
  pull <- get.Comtrade(r=i, p="all", ps="2006,2007,2008,2009,2010", 
                       cc="280461,845430,848610,854144,854140,8501,700719", fmt="csv") 
  df.i = pull$data 
  print("df.i") 
  str(df.i) 
  result <- data.frame("Year"=df.i$Period,"Flow"=df.i$Trade.Flow,"Product_Code"=df.i$Commodity.Code,
                       "Description"=df.i$Commodity,"Reporter"=i,"Value"=df.i$Trade.Value..US..,stringsAsFactors=FALSE) 
  print("result") 
  str(result) 
  WorldTrade1 <- rbind(WorldTrade1,result) 
}

# Loop 2 
reporters2 = reporters[41:80,] 
x2 = reporters2$V1 
WorldTrade2 <- data.frame("Year"=as.Date(character()),"Flow"=character(),"Product_Code"=integer(),
                          "Description"=character(), "Reporter"=character(), "Value"=integer(), stringsAsFactors=FALSE) 
for (i in x2){ 
  print(i) 
  pull <- get.Comtrade(r=i, p="all", ps="2006,2007,2008,2009,2010", 
                       cc="280461,845430,848610,854144,854140,8501,700719", fmt="csv") 
  df.i = pull$data 
  print(df.i) 
  str(df.i) 
  result <- data.frame("Year"=df.i$Period,"Flow"=df.i$Trade.Flow,"Product_Code"=df.i$Commodity.Code,
                       "Description"=df.i$Commodity, "Reporter"=i,"Value"=df.i$Trade.Value..US..,stringsAsFactors=FALSE) 
  print(result) 
  str(result) 
  WorldTrade2<- rbind(WorldTrade2,result) 
}

# Loop 3 - in an hour 
reporters3= reporters[78:120,] 
x3 = reporters3$V1 
WorldTrade3 <- data.frame("Year"=as.Date(character()),"Flow"=character(),"Product_Code"=integer(),
                          "Description"=character(), "Reporter"=character(), "Value"=integer(), stringsAsFactors=FALSE) 
for (i in x3){ 
  print(i) 
  pull <- get.Comtrade(r=i, p="all", ps="2006,2007,2008,2009,2010", 
                       cc="280461,845430,848610,854144,854140,8501,700719", fmt="csv") 
  df.i = pull$data 
  print("df.i") 
  str(df.i) 
  result <- data.frame("Year"=df.i$Period,"Flow"=df.i$Trade.Flow,"Product_Code"=df.i$Commodity.Code,
                       "Description"=df.i$Commodity,"Reporter"=i,"Value"=df.i$Trade.Value..US..,stringsAsFactors=FALSE) 
  print(result) 
  str(result) 
  WorldTrade3 <- rbind(WorldTrade3,result) 
}

# Loop 4
x4 = reporters4$V1
WorldTrade4 <- data.frame("Year"=as.Date(character()),"Flow"=character(),"Product_Code"=integer(),
                          "Description"=character(), "Reporter"=character(), "Value"=integer(), stringsAsFactors=FALSE)
for (i in x4){
  print(i)
  pull <- get.Comtrade(r=i, p="all", ps="2006,2007,2008,2009,2010",
                       cc="280461,845430,848610,854144,854140,8501,700719", fmt="csv")
  df.i = pull$data
  print("df.i")
  str(df.i)
  result <- data.frame("Year"=df.i$Period,"Flow"=df.i$Trade.Flow,"Product_Code"=df.i$Commodity.Code,
                       "Description"=df.i$Commodity,"Reporter"=i,"Value"=df.i$Trade.Value..US..,stringsAsFactors=FALSE)
  print(result)
  str(result)
  WorldTrade4<- rbind(WorldTrade4,result)
} %in% 
  
  # Loop 5 
  x5 = reporters5$V1 
WorldTrade5 <- data.frame("Year"=as.Date(character()),"Flow"=character(),"Product_Code"=integer(),
                          "Description"=character(), "Reporter"=character(), "Value"=integer(), stringsAsFactors=FALSE) 
for (i in x5){ 
  print(i) 
  pull <- get.Comtrade(r=i, p="all", ps="2006,2007,2008,2009,2010", 
                       cc="280461,845430,848610,854144,854140,8501,700719", fmt="csv") 
  df.i = pull$data 
  print("df.i") 
  str(df.i) 
  result <- data.frame("Year"=df.i$Period,"Flow"=df.i$Trade.Flow,"Product_Code"=df.i$Commodity.Code,
                       "Description"=df.i$Commodity,"Reporter"=i,"Value"=df.i$Trade.Value..US..,stringsAsFactors=FALSE) 
  print(result) 
  str(result) 
  WorldTrade5<- rbind(WorldTrade5,result) 
}

# Loop 6 
x6 = reporters6$V1 
WorldTrade6 <- data.frame("Year"=as.Date(character()),"Flow"=character(),"Product_Code"=integer(),
                          "Description"=character(), "Reporter"=character(), "Value"=integer(), stringsAsFactors=FALSE) 
for (i in x6){ 
  print(i) 
  pull <- get.Comtrade(r=i, p="all", ps="2006,2007,2008,2009,2010", 
                       cc="280461,845430,848610,854144,854140,8501,700719", fmt="csv") 
  df.i = pull$data 
  print("df.i") 
  str(df.i) 
  result <- data.frame("Year"=df.i$Period,"Flow"=df.i$Trade.Flow,"Product_Code"=df.i$Commodity.Code,
                       "Description"=df.i$Commodity,"Reporter"=i,"Value"=df.i$Trade.Value..US..,stringsAsFactors=FALSE) 
  print(result) 
  str(result) 
  WorldTrade6<- rbind(WorldTrade6,result) 
} 

# Loop 7 
x7 = reporters7$V1 
WorldTrade7 <- data.frame("Year"=as.Date(character()),"Flow"=character(),"Product_Code"=integer(),
                          "Description"=character(), "Reporter"=character(), "Value"=integer(), stringsAsFactors=FALSE) 
for (i in x7){ 
  print(i) 
  pull <- get.Comtrade(r=i, p="all", ps="2006,2007,2008,2009,2010", 
                       cc="280461,845430,848610,854144,854140,8501,700719", fmt="csv") 
  df.i = pull$data 
  print("df.i") 
  str(df.i) 
  result <- data.frame("Year"=df.i$Period,"Flow"=df.i$Trade.Flow,"Product_Code"=df.i$Commodity.Code,
                       "Description"=df.i$Commodity,"Reporter"=i,"Value"=df.i$Trade.Value..US..,stringsAsFactors=FALSE) 
  print("result") 
  str(result) 
  WorldTrade7<- rbind(WorldTrade7,result) 
} 

# Loop 8 
x8 = reporters8$V1 
WorldTrade8 <- data.frame("Year"=as.Date(character()),"Flow"=character(),"Product_Code"=integer(),
                          "Description"=character(), "Reporter"=character(), "Value"=integer(), stringsAsFactors=FALSE) 
for (i in x8){ 
  print(i) 
  pull <- get.Comtrade(r=i, p="all", ps="2006,2007,2008,2009,2010", 
                       cc="280461,845430,848610,854144,854140,8501,700719", fmt="csv") 
  df.i = pull$data 
  print("df.i") 
  str(df.i) 
  result <- data.frame("Year"=df.i$Period,"Flow"=df.i$Trade.Flow,"Product_Code"=df.i$Commodity.Code,
                       "Description"=df.i$Commodity,"Reporter"=i,"Value"=df.i$Trade.Value..US..,stringsAsFactors=FALSE) 
  print("result") 
  str(result) 
  WorldTrade8<- rbind(WorldTrade8,result) 
}

# Saving the downloads 
fwrite(WorldTrade1,file = "WT3a.csv") 
fwrite(WorldTrade2,file = "WT3b.csv") 
fwrite(WorldTrade3,file = "WT3c.csv") 
fwrite(WorldTrade4,file = "WT3d.csv") 
fwrite(WorldTrade5,file = "WT3e.csv") 
fwrite(WorldTrade6,file = "WT3f.csv") 
fwrite(WorldTrade7,file = "WT3g.csv") 
fwrite(WorldTrade8,file = "WTh.csv") 

# Collate and view final database as downloaded 
Trade <- rbind(WorldTrade1,WorldTrade2,WorldTrade3,WorldTrade4,WorldTrade5,WorldTrade6,WorldTrade7) 
View(Trade) 

# Drop duplicate values in data extracted by selecting maximum value for each unique flow 
Trade1 <- ddply(Trade,.(Year,Flow,Product_Code,Description,Reporter),summarise,Value=max(Value, na.rm=TRUE)) 
View(Trade1)

# Expand Reporters and Compute Sums Across Columns 
ByReporter <- spread(Trade1,Reporter,Value, fill=0) 
View(ByReporter)

# Sum reporters to obtain total values 
ByReporter$Value <- apply(ByReporter[,5:276], 1, function(x) sum(x))

# Compute India Totals 
IndiaT <- cbind(ByReporter[,1:4],ByReporter["699"]) 
names(IndiaT) <- c("Year","Flow","Product_Code","Description","India") 
India <- spread(IndiaT, Flow, India,fill=0) 
India1 <- transform(India,"Export" = India [,4] + India[,6], "Import"= India[,5] + India[,7]) 
India <- India1[,1:5] 

# Compute World Totals 
WorldT <- cbind(ByReporter[,1:4],ByReporter["Value"]) 
World <- spread(WorldT, Flow, Value,fill=0) 
WT <- transform(World,"Export" = World [,4] + World[,6], "Import"= World[,5] + World[,7]) 
World <- WT[,1:5] 

# Combine India and World Totals 
TFinal <- cbind(India, World[,4:5]) 
names(TFinal) <- c("Year","Product_Code","Description","IExport","IImport","WExport","WImport") 

# Download Data for All Commodities. Read list of country codes into R 
string <- "http://comtrade.un.org/data/cache/partnerAreas.json" 
reporters <- fromJSON(file=string) 
reporters <- as.data.frame(t(sapply(reporters$results,rbind))) 

reportersa= reporters[3:35,] 
reportersb= reporters[36:136,] 
reportersc= reporters[137:293,] 

# First 100 requests 
x = reportersa$V1 
WorldTradea <- data.frame("Year"=as.Date(character()), 
                          "Flow"=character(), 
                          "Product_Code"=integer(), 
                          "Description"=character(), 
                          "Reporter"=character(), 
                          "Value"=integer(), 
                          stringsAsFactors=FALSE) 
for (i in x){ 
  print(i) 
  pullall <- get.Comtrade(r=i, p="all", ps="2011,2012,2013,2014,2015", fmt="csv") 
  df.i = pullall$data 
  print("df.i") 
  str(df.i) 
  result <- data.frame("Year"=df.i$Period,"Flow"=df.i$Trade.Flow,"Product_Code"=df.i$Commodity.Code,
                       "Description"=df.i$Commodity,"Reporter"=i,"Value"=df.i$Trade.Value..US..,stringsAsFactors=FALSE) 
  print("result") 
  str(result) 
  WorldTradea<- rbind(WorldTradea,result) 
}

# Second 100 requests 
xb = reportersb$V1 
WorldTradeb <- data.frame("Year"=as.Date(character()), 
                          "Flow"=character(), 
                          "Product_Code"=integer(), 
                          "Description"=character(), 
                          "Reporter"=character(), 
                          "Value"=integer(), 
                          stringsAsFactors=FALSE) 
for (i in xb){ 
  print(i) 
  pullall <- get.Comtrade(r=i, p="all", ps="2011,2012,2013,2014,2015", fmt="csv") 
  df.i = pullall$data 
  print("df.i") 
  str(df.i) 
  resultb <- data.frame("Year"=df.i$Period,"Flow"=df.i$Trade.Flow,"Product_Code"=df.i$Commodity.Code,
                        "Description"=df.i$Commodity,"Reporter"=i,"Value"=df.i$Trade.Value..US..,stringsAsFactors=FALSE) 
  print("resultb") 
  str(resultb) 
  WorldTradeb<- rbind(WorldTradeb,resultb) 
}

# Third 100 requests 
xc = reportersc$V1 
WorldTradec <- data.frame("Year"=as.Date(character()), 
                          "Flow"=character(), 
                          "Product_Code"=integer(), 
                          "Description"=character(), 
                          "Reporter"=character(), 
                          "Value"=integer(), 
                          stringsAsFactors=FALSE) 
for (i in xc){
  print(i)
  pullall <- get.Comtrade(r=i, p="all", ps="2011,2012,2013,2014,2015", fmt="csv")
  df.i = pullall$data
  print("df.i")
  str(df.i)
  resultc <- data.frame("Year"=df.i$Period,"Flow"=df.i$Trade.Flow,"Product_Code"=df.i$Commodity.Code,
                        "Description"=df.i$Commodity,"Reporter"=i,"Value"=df.i$Trade.Value..US..,stringsAsFactors=FALSE) 
  print("resultc") 
  str(resultc) 
  WorldTradec <- rbind(WorldTradec,resultc) 
}

# Bind 3 data sets and remove possible duplication
AllC <- rbind(WorldTradea,WorldTradeb,WorldTradec)
AllC <- distinct(AllC)

# Create Table to Compute Total World and India Trade by Year
y <- c(2011:2015)
Totals <- data.frame("Year"=as.Date(character()),
                     "Flow"=character(),
                     "IndiaTotalEx"=as.numeric(),
                     "IndiaTotalIm"=as.numeric(),
                     "WorldTotalEx"=as.numeric(), stringsAsFactors = FALSE)
for (i in y) {
  print(i)
  All.i <- filter(AllC, Year==i)
  AllE.i <- rbind((filter(All.i, Flow=="Export")), (filter(All.i, Flow=="Re-Export")))
  WorldTotalEx <- as.numeric(colSums(AllE.i["Value"]))
  AllI.i <- rbind((filter(All.i, Flow=="Import")), (filter(All.i, Flow=="Re-Import")))
  WorldTotalIm <- as.numeric(colSums(AllE.i["Value"]))
  IndiaE.i <- filter(AllE.i, Reporter==699)
  IndiaTotalEx <- as.numeric(colSums(IndiaE.i["Value"]))
  IndiaI.i <- filter(AllI.i, Reporter==699)
  IndiaTotalIm <- as.numeric(colSums(IndiaI.i["Value"]))
  result <- data.frame("Year"=i, "IndiaTotalEx"=IndiaTotalEx,
                       "WorldTotalEx"=WorldTotalEx, "IndiaTotalIm"=IndiaTotalIm,
                       "WorldTotalIm"=WorldTotalIm)
  Totals <- rbind(Totals,result)
  print(Totals)
}

# Compute RCA, Significance, Specialization and Trade Balance
for (i in 1:dim(TFinal)[1]) {
  # Sum is removing NA errors in code
  TFinal$RCA[i] <- sum((TFinal$IExport[i]/Totals$IndiaTotalEx[Totals$Year==TFinal$Year[i]])/(TFinal$WExport[i]/Totals$WorldTotalEx[Totals$Year==TFinal$Year[i]]),na.rm=FALSE)
  TFinal$Significance[i] <- sum(((TFinal$IExport[i]/Totals$IndiaTotalEx[Totals$Year==TFinal$Year[i]]))*100, na.rm=FALSE)
  TFinal$Specialization[i] <- sum(((TFinal$IExport[i]/TFinal$IImport[i])/(Totals$IndiaTotalEx[Totals$Year==TFinal$Year[i]]/Totals$IndiaTotalIm[Totals$Year==TFinal$Year[i]])),na.rm=FALSE)
  TFinal$TradeBalance[i] <- sum((TFinal$IExport[i]-TFinal$IImport[i])/(TFinal$IExport[i]+TFinal$IImport[i]),na.rm=FALSE)
}

fwrite(TFinal,file="TF3.csv")
fwrite(Totals,file="Totals.csv")

# ***END***