# ------------------------------------------------------------------------------------------------------------------ #
# Monthly agricultural commodity trade charts, to identify potential seasonality in trade data                       #
# For: Data and Artificial Intelligence for African Trade (DAIAT) Initiative                                         #
# Last Edited: 2022-12-12 (RR)                                                                                       #
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
#  1) Install required packages and load necessary libraries                                                        #
#-------------------------------------------------------------------------------------------------------------------#

write("Load libs..", stdout())
if (!require("pacman")) install.packages("pacman")
pacman::p_load("manipulate","gtools","plyr","utils","stats","xlsx","sqldf","tcltk","tcltk2","DescTools","tm","rjson",
               "dplyr","data.table","antitrust","ggplot2","rJava","XLConnect","gdata","openxlsx","tidyverse","XML",
               "reshape","reshape2","pbapply","circlize","migest","devtools","stringr","googleVis","XML","janitor",
               "bit64","zoo","Rcpp","tidyr","scales","knitr","rvest","RCurl","httr","readxl","xts","tools","timetk",
               "lubridate","filesstrings","tidyverse","gganimate","skimr","tidytext","gt","tidyquant","timeDate",
               "tidymodels","modeltime","comtradr","purrr")

#-------------------------------------------------------------------------------------------------------------------#
#  2) Import trade data and prepare data for use with ggplot to plot time series plots                              #
#-------------------------------------------------------------------------------------------------------------------#

# Load UN Comtrade trade flow data (Read from 01_Clean_Data_vYYYY-MM-DD.r)
Comtrade.Agric <- as.data.table(fread("CleanData/comtrade_monthly_data_201701-202207.csv"))
Comtrade.Agric <- as.data.table(subset(Comtrade.Agric, Reporter != "C000" & Partner != "C000"))

# Read in CEPII Reporter codes list
string <- "http://comtrade.un.org/data/cache/partnerAreas.json"
ReporterCodes <- fromJSON(file=string)
ReporterCodes <- as.data.table(t(sapply(ReporterCodes$results,rbind)))
ReporterCodes <- ReporterCodes[-1, ]
colnames(ReporterCodes) <- c("Country","Country_Name")
ReporterCodes$Country <- str_pad(ReporterCodes$Country, 3, pad = "0")
ReporterCodes$Country <- paste("C", ReporterCodes$Country, sep ='')
ReporterCodes$Country_Name <- as.character(ReporterCodes$Country_Name)

# Read in CEPII Partner codes list
string <- "http://comtrade.un.org/data/cache/partnerAreas.json"
PartnerCodes <- fromJSON(file=string)
PartnerCodes <- as.data.table(t(sapply(PartnerCodes$results,rbind)))
PartnerCodes <- PartnerCodes[-1, ]
colnames(PartnerCodes) <- c("Country","Country_Name")
PartnerCodes$Country <- str_pad(PartnerCodes$Country, 3, pad = "0")
PartnerCodes$Country <- paste("C", PartnerCodes$Country, sep ='')
PartnerCodes$Country_Name <- as.character(PartnerCodes$Country_Name)

# Read in CEPII product codes list
string <- "http://comtrade.un.org/data/cache/classificationH4.json"
H4 <- fromJSON(file=string)
H4 <- as.data.table(t(sapply(H4$results,rbind)))
colnames(H4) <- c("HS6","HS6_Description","Parent")
H4 <- as.data.table(subset(H4, nchar(H4$HS6)==6, select = c(1:2)))
H4$HS6 <- paste("HS", H4$HS6, sep ='')
H4$HS6_Description <- paste("HS", H4$HS6_Description, sep ='')

# Add country and product names to data.table
Comtrade.Agric <- merge(x = Comtrade.Agric, y = ReporterCodes[ , c("Country", "Country_Name")],
                        by.y = "Country", by.x = c("Reporter"), all.x=TRUE)
setnames(Comtrade.Agric, "Country_Name", "Reporter_Name")
Comtrade.Agric <- merge(x = Comtrade.Agric, y = PartnerCodes[ , c("Country", "Country_Name")],
                        by.y = "Country", by.x = c("Partner"), all.x=TRUE)
setnames(Comtrade.Agric, "Country_Name", "Partner_Name")
Comtrade.Agric <- merge(x = Comtrade.Agric, y = H4[ , c("HS6", "HS6_Description")],
                        by.y = "HS6", by.x = "HS6", all.x=TRUE)
setcolorder(Comtrade.Agric, c(3,9,2,10,1,11,4:8)) # Reorder data.table columns (without copying)
Comtrade.Agric$Period <- lubridate::ym(Comtrade.Agric$Period)
Comtrade.Agric$Period <- as.POSIXct(Comtrade.Agric$Period, format = "%Y %m %d")
Comtrade.Agric <- Comtrade.Agric[order(Reporter, Partner, HS6, Period)]
rm(ReporterCodes, PartnerCodes, H4) # Remove all unused objects
gc() # Cleanup unused memory in R

#-------------------------------------------------------------------------------------------------------------------#
#  3) Plot trade for each unique combination of Reporter, Partner and HS6 and export images to "SeasonalityPlots"   #
#-------------------------------------------------------------------------------------------------------------------#

# Function to create line / bar plots for each combination of Reporter, Partner and HS6
monthly_plot <- function(Comtrade.Agric, x, y, z) {
  # create list of reports and criteria in data to loop over
  
  rpt_list <- unique(Comtrade.Agric$Reporter_Name)
  prt_list <- unique(Comtrade.Agric$Partner_Name)
  prd_list <- unique(Comtrade.Agric$HS6_Description)
  hs6_list <- unique(Comtrade.Agric$HS6)
  
  for(i in seq_along(rpt_list)) {
    for(j in seq_along(prt_list)) {
      for(k in seq_along(prd_list)) {
        data = subset(
          Comtrade.Agric, 
          Reporter_Name == rpt_list[[i]] & Partner_Name == prt_list[[j]] & HS6_Description == prd_list[[k]]
        )
        if (nrow(data) == 0) {
          next
        }
        x_var <- enquo(x)
        y_var <- enquo(y)
        
        cat("\n\n### Exports of", hs6_list[[k]], "from", rpt_list[[i]], "to", prt_list[[j]], "###", "\n")
        monthly_trade <- ggplot(data, aes(x = Period,
                                          y = Value_USD/10^6,
                                          fill = Partner_Name)) +
          geom_col(alpha = 0.6, position = "dodge") +
          theme(legend.title = element_text(size = 7),
                legend.text = element_text(size = 7),
                legend.key.size = unit(0.7, "lines"),
                legend.position = c(0.948, 0.925)) +
          scale_y_continuous(labels = scales::number) +
          scale_x_datetime(date_breaks = "1 month", date_labels = "'%y/%m") +
          labs(
            title = paste(rpt_list[[i]], prd_list[[k]], sep = " exports of "),
            subtitle = "2017 - 2022",
            x = "Period ('YY/MM)",
            y = "Export value (US$ Mn)",
            caption = "Data: UN Comtrade"
          ) +
          theme(axis.text.x = element_text(size = 6, angle = 45, vjust = 1, hjust = 1))
        print(monthly_trade)
        
        # Save to 'SeasonalityPlots' folder
        ggsave(paste0("SeasonalityPlots/",
                      paste(rpt_list[[i]], paste(prt_list[[j]], paste0(hs6_list[[k]], ".png"), sep = "_"), sep = "_")),
               width = 8.91, height = 4.79, dpi = 600)
      }
    }
  }
}

# Visualizing the monthly export values in millions of US$ (2017-2022) using a bar plot
monthly_plot(Comtrade.Agric, Reporter_Name, Partner_Name, HS6_Description)

# Save end/finish time
sessiontime.end <- Sys.time()
difftime(sessiontime.end, sessiontime.start)

## DELETE ALL FILES ##
rm(list=ls())

# *** END ***

#############
## APENDIX ##
#############

# Test example for visualisation
Comtrade.Agric %>% 
  as_tibble() %>% 
  filter(Reporter_Name == "Ireland", Partner_Name == "Thailand", HS6_Description == "HS080410 - Fruit, edible; dates, fresh or dried") %>% 
  select(Value_USD, Reporter_Name, Partner_Name, HS6_Description, everything()) %>% 
  ggplot(aes(x = Period,
             y = Value_USD/10^6,
             fill = Partner_Name)) +
  geom_col(alpha = 0.6, position = "dodge") +
  theme(legend.title = element_text(size = 7),
        legend.text = element_text(size = 7),
        legend.key.size = unit(0.7, "lines"),
        legend.position = c(0.948, 0.925)) +
  scale_y_continuous(labels = scales::number) +
  scale_x_datetime(date_breaks = "1 month", date_labels = "'%y/%m") +
  labs(
    title = "Ireland exports of HS080410 - Fruit, edible; dates, fresh or dried",
    subtitle = "2017 - 2022",
    x = "Period ('YY/MM)",
    y = "Export value (US$ Mn)",
    caption = "Data: UN Comtrade"
  ) +
  theme(axis.text.x = element_text(size = 6, angle = 45, vjust = 1, hjust = 1))

# Visualizing the monthly export values in millions of US$ (2017-2022) using a line plot
monthly_plot = function(data, x, y, z) {
  Comtrade.Agric %>% 
    split(list(.$Reporter_Name, .$Partner_Name, .$HS6_Description)) %>% 
    discard(~ nrow(.x) == 0) %>% 
    map(function(sub_data) {
      ggplot(sub_data, aes(x = Period,
                           y = Value_USD/10^6,
                           fill = y)) +
        geom_line(aes(color = Partner_Name), size = 0.7) +
        theme(legend.title = element_text(size = 7),
              legend.text = element_text(size = 7),
              legend.key.size = unit(0.7, "lines"),
              legend.position = "bottom") +
        scale_y_continuous(labels = scales::number) +
        scale_x_datetime(date_breaks = "1 month", date_labels = "'%y/%m") +
        labs(
          title = paste(x, z, sep = "exports of"),
          subtitle = "2017 - 2022",
          x = "Period ('YY/MM)",
          y = "Export value (US$ Mn)",
          caption = "Data: UN Comtrade"
        ) +
        theme(axis.text.x = element_text(size = 6, angle = 45, vjust = 1, hjust = 1))
      ggsave(paste0("SeasonalityPlots/", paste(x, paste(y, paste0(z, ".png"), sep = "_"), sep = "_")),
             width = 8.91, height = 4.79, dpi = 600)
    })
}

# Generate time-series plots
monthly_plot(Comtrade.Agric, Reporter_Name, Partner_Name, HS6_Description)

# *** END ***