# ------------------------------------------------------------------------------------------------------------------ #
# Code to download specific data needed from UN Comtrade and then save it as a csv file                              #
# For: Data and Artificial Intelligence for African Trade (DAIAT) Initiative                                         #
# Last Edited: 2022-10-27 (RR)                                                                                       #
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
               "rsconnect")

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
ct_wait_for_available <- function(at_least = 2) {
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
        Sys.sleep(as.numeric(wtime))
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
      x = do.call(rbind, list(x_temp, x))
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