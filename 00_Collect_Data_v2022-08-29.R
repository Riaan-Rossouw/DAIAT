# ------------------------------------------------------------------------------------------------------------------ #
# Code to download specific data needed from UN Comtrade and then save it as a csv file                              #
# For: Data and Artificial Intelligence for African Trade (DAIAT) Initiative                                         #
# Last Edited: 2022-08-29 (RR)                                                                                       #
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

# Install required packages and load libraries
write("Load libs..", stdout())
if (!require("pacman")) install.packages("pacman")
pacman::p_load("manipulate","gtools","plyr","utils","stats","xlsx","sqldf","tcltk","tcltk2","DescTools","tm","rjson",
               "dplyr","data.table","antitrust","ggplot2","rJava","XLConnect","gdata","openxlsx","astsa","forecast",
               "taRifx","reshape","reshape2","pbapply","circlize","migest","devtools","stringr","googleVis","maps",
               "bit64","zoo","Rcpp","tidyr","scales","knitr","rvest","RCurl","httr","XML","readxl","xts","tools",
               "lubridate","timeDate","filesstrings","tradestatistics","comtradr","purrr","remotes")

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

#' Get UN Comtrade dictionary for a parameter
#'
#' @param parameter which parameter do you want the dictionary for? Specify "HS", "H1" etc for the product classifications (see link below)
#' @details This function downloads the data dictionary from the UN Comtrade API. See link below.
#' @return the dictionary as a data.frame
#' @export

getComtradeDict <- function(parameter = "reporter")
{
  string <- switch(parameter,
                   reporter = "http://comtrade.un.org/data/cache/reporterAreas.json",
                   partner = "http://comtrade.un.org/data/cache/partnerAreas.json",
                   HS = "http://comtrade.un.org/data/cache/classificationHS.json",
                   H1 = "http://comtrade.un.org/data/cache/classificationH1.json",
                   H2 = "http://comtrade.un.org/data/cache/classificationH2.json",
                   H3 = "http://comtrade.un.org/data/cache/classificationH3.json",
                   H4 = "http://comtrade.un.org/data/cache/classificationH4.json",
                   ST = "http://comtrade.un.org/data/cache/classificationST.json",
                   S1 = "http://comtrade.un.org/data/cache/classificationS1.json",
                   S2 = "http://comtrade.un.org/data/cache/classificationS2.json",
                   S3 = "http://comtrade.un.org/data/cache/classificationS3.json",
                   S4 = "http://comtrade.un.org/data/cache/classificationHS.json",
                   BEC = "http://comtrade.un.org/data/cache/classificationBEC.json",
                   tradeflow = "http://comtrade.un.org/data/cache/tradeRegimes.json")
  
  dict <- fromJSON(file = string)
  dict <- as.data.frame(t(sapply(dict[["results"]], unlist)))
  
  return(dict)
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

#-------------------------------------------------------------------------------------------------------------------#
#  3) Download user-specified data from UN Comtrade using API and append data                                       #
#-------------------------------------------------------------------------------------------------------------------#

# Get list of reporters/partners, commodities and years
string <- "http://comtrade.un.org/data/cache/partnerAreas.json"
countries <- fromJSON(file=string)
countries <- as.data.table(t(sapply(countries$results,rbind)))
countries <- countries[-1, ]
countries <- str_c(countries[[1]], collapse = ",")
countries <- as.list(str_extract_all(countries, "\\d+(,\\d+){1,4}")[[1]])

commodities <- read.xlsx2("2022-06-24_Agric_Growth_Cycle_Products.xlsx", sheetIndex=1, colIndex=1, header=TRUE)
commodities$HS6 <- gsub("^.{0,2}", "", commodities$HS6) # Replace first 2 characters with empty string ""
commodities[] <- lapply(commodities, as.character)
commodities <- str_c(commodities[[1]], collapse = ",")
commodities <- as.list(str_extract_all(commodities, "\\d+(,\\d+){1,4}")[[1]])

# period = c(t(outer(2017:2022, sprintf("%02d", 1:12), paste, sep = ""))))
start_date <- as.Date("2017/01/01") # Create start date
period <- seq(start_date, by = "month", length.out = elapsed_months(Sys.time(), start_date))
period <- format(as.Date(period), format = "%Y%m")
period <- str_c(period, collapse = ",")
period <- as.list(str_extract_all(period, "\\d+(,\\d+){1,4}")[[1]])

# After running the UN Comtrade function, test for loop
x <- get.Comtrade(r = "all", p = "4", ps = "201701", freq = "M", fmt = "csv", rg = "all", cc = "410390")
x <- as.data.frame(do.call(rbind, x))
x <- x[-c(1:36),]

# Run loops to download monthly data, limits of 100 requests per hour, ps, r and p are limited to 5 codes each. 
# Only one of the above codes may use the special ALL value in a given API call. Classification codes are limited to 20 items.
# ALL is always a valid classification code (http://comtrade.un.org/data/doc/api/) 
for(i in countries){
  for(j in commodities){
    for(k in period){
      x_temp <- get.Comtrade(r = "all", p = i, ps = k, freq = "M", fmt = "csv", rg = "all", cc = j)
      dx_temp <- as.data.table(do.call(rbind, x_temp))
      x = do.call(rbind, list(dx_temp, x))
      Sys.sleep(2)
    } 
  }
}

# Drop duplicate values in data extracted by selecting maximum value for each unique flow 
comtrade.data<- ddply(x,.(Year, Flow, Product_Code, Description, Reporter),
                      summarise, Value = max(Value, na.rm=TRUE)) 

# Expand Reporters and Compute Sums Across Columns 
ByReporter <- spread(comtrade.data, Reporter, Value, fill=0) 
View(ByReporter)

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