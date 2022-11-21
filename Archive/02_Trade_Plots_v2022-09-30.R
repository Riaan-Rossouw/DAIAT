# ------------------------------------------------------------------------------------------------------------------ #
# Monthly agricultural commodity trade charts, to identify potential seasonality in trade data                       #
# For: Data and Artificial Intelligence for African Trade (DAIAT) Initiative                                         #
# Last Edited: 2022-09-30 (RR)                                                                                       #
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
               "tidymodels","modeltime")

#-------------------------------------------------------------------------------------------------------------------#
#  2) Read in all raw data from 'RawData' folder and clean (save to 'CleanData')                                    #
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
ReporterCodes$Country<- paste("C", ReporterCodes$Country, sep ='')
ReporterCodes$Country_Name <- as.character(ReporterCodes$Country_Name)

# Read in CEPII Partner codes list
string <- "http://comtrade.un.org/data/cache/partnerAreas.json"
PartnerCodes <- fromJSON(file=string)
PartnerCodes <- as.data.table(t(sapply(PartnerCodes$results,rbind)))
PartnerCodes <- PartnerCodes[-1, ]
colnames(PartnerCodes) <- c("Country","Country_Name")
PartnerCodes$Country <- str_pad(PartnerCodes$Country, 3, pad = "0")
PartnerCodes$Country<- paste("C", PartnerCodes$Country, sep ='')
PartnerCodes$Country_Name <- as.character(PartnerCodes$Country_Name)

# Read in CEPII product codes list
string <- "http://comtrade.un.org/data/cache/classificationH4.json"
H4 <- fromJSON(file=string)
H4 <- as.data.table(t(sapply(H4$results,rbind)))
colnames(H4) <- c("HS6","HS6_Description","Parent")
H4 <- as.data.table(subset(H4, nchar(H4$HS6)==6, select = c(1:2)))
H4$HS6<- paste("HS", H4$HS6, sep ='')
H4$HS6_Description<- paste("HS", H4$HS6_Description, sep ='')

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
Comtrade.Agric$Period <- as.yearmon(as.character(Comtrade.Agric$Period), "%Y%m")
#Comtrade.Agric$Month <- month.abb[Comtrade.Agric$Month] # Convert Numeric Vector to Month Abbreviations
#Comtrade.Agric$Month <- month.name[Comtrade.Agric$Month]
Comtrade.Agric$Month <- as.factor(Comtrade.Agric$Month)
Comtrade.Agric <- Comtrade.Agric %>% mutate_if(is.character, as_factor) # Converting categorical features to factor
Comtrade.Agric <- Comtrade.Agric[order(Reporter, Partner, HS6, Period)]
rm(ReporterCodes, PartnerCodes, H4) # Remove all unused objects
gc() # Cleanup unused memory in R

## Data wrangling before plot
# Adding two columns: Year and Month
Comtrade.Agric <- Comtrade.Agric %>% 
  mutate(Year = year(Year),
         Month = month(Month, abbr = FALSE, label = TRUE, locale = Sys.setlocale("LC_COLLATE", "C")))

# Total of tons by year (considering just exports)
expt_year_total_tbl <- Comtrade.Agric %>%
  filter(type == "Export") %>% 
  group_by(year) %>% 
  summarise(total_tons = sum(tons)) %>% 
  ungroup()

# Total of tons for each month of the year (considering just exports)
expt_year_month_total_tbl <- Comtrade.Agric %>% 
  filter(Reporter_Name == "South Africa" & HS6 == "HS060110") %>%
  group_by_(.dots = c(Year, Month)) %>% 
  summarise(Total_kg = as.numeric(sum(NetWeight_kg, na.rm = TRUE))) %>% 
  as.data.table()

expt_year_month_total_tbl <- Comtrade.Agric %>%
  filter(Reporter == "C710" & Partner == "C372" & HS6 == "HS060110") %>%
  group_by_(.dots = c(Year, Month)) %>% 
  summarise(kg = as.numeric(sum(NetWeight_kg, na.rm = TRUE))) %>% 
  ungroup()

# Visualizing the monthly total of tons (1997 - 2020)
lapply(Comtrade.Agric$Reporter_Name, function(i) {
  lapply(Comtrade.Agric$HS6, function(j) {
    Comtrade.Agric %>%
      filter(Reporter_Name == "South Africa" & HS6 == "HS060110") %>%
      mutate(Month = Month %>% str_to_title() %>% as_factor()) %>% 
      ggplot(aes(x = Year,
                 y = NetWeight_kg)) +
      geom_point(size = .8) +
      geom_line() +
      facet_wrap(~Partner_Name) +
      theme_tq() +
      scale_y_continuous(labels = scales::number_format(scale = 1e-6, suffix = "M")) +
      scale_x_continuous(breaks = c(2017, 2018, 2019, 2020, 2021, 2022)) +
      labs(
        title = "Total of Tons for the Monthly Exports between 2017 and 2022",
        subtitle = paste("South Africa", paste(" exports by partner country for ", "HS060110")),
        x = "",
        y = "Millions of Tons",
        caption = "Data: UN Comtrade"
      ) +
      theme(axis.text.x = element_text(size = 7))
  })
})

# Data wrangling before plot filtering for recent years and type == "Export"
# Total of tons for these groups
top_3_product_exp_tbl <- exp_imp_year_month_tbl %>% 
  filter(year %in% c(2019:2015)) %>% 
  filter(type == "Export") %>% 
  group_by(product) %>% 
  summarise(total_tons_exp = sum(tons)) %>% 
  ungroup() %>% 
  slice_max(total_tons_exp, n = 3)

top_3_product_exp_tbl %>% 
  mutate(product_str = case_when(
    product == "soybeans" ~ "Soybean",
    product == "corn" ~ "Corn",
    TRUE ~ "Sugar"
  )) %>% 
  ggplot(aes(x = total_tons_exp,
             y = fct_reorder(product_str, total_tons_exp),
             fill = product_str)) +
  geom_col() +
  scale_fill_manual(values = c("#7EBEF7", "#2595F5", "#BBD7F0")) +
  scale_x_continuous(labels = scales::number_format(scale = 1e-6, suffix = "M")) +
  guides(fill = FALSE) +
  theme_tq() +
  labs(
    title = "Top 3 - Brazilian Commodities Exports",
    subtitle = "Considering the Last 5 Years",
    caption = "linkedin.com/in/lucianobatistads/",
    x = "Millions of Tons",
    y = "Commodities"
  )

# Goods tables to plot
top_3_product_exp_tbl %>% 
  rename(Commodities = product, `Total of Tons` = total_tons_exp) %>%
  mutate(`Total of Tons` = `Total of Tons` %>% scales::number(scale = 1e-6, suffix = "M")) %>% 
  gt()

# Plot time-series data
# Data wrangling
ts_HS_tbl <- Comtrade.Agric %>% 
  select(Period, HS6, NetWeight_kg) %>% 
  filter(HS6 == "HS070310") %>% 
  group_by(Period, HS6) %>% 
  summarise(total_kg = sum(NetWeight_kg)) %>% 
  ungroup()

# Visualizing
ts_HS_tbl %>% 
  plot_time_series(.date_var = Period,
                   .value = total_kg, 
                   .interactive = F,
                   .smooth = F) +
  scale_y_continuous(labels = scales::number_format(scale = 1e-6, suffix = "M")) +
  labs(
    y = "Millions of Tons",
    title = "Agricultural Products Time Series",
    caption = "Data: UN Comtrade"
  )

# Plot Multiple charts
for (r in Comtrade.Agric$Partner_Name){
  cat("\n\n###", r, "\n")
  p <- ggplot(data=filter(Comtrade.Agric, Partner_Name == r), 
              aes(x=Period, y=factor(NetWeight_kg), 
                  xend=Period+1, yend=Reporter_Name, 
                  linetype=factor(NetWeight_kg))) +
    geom_segment(size=3) + 
    facet_grid(Partner_Name~HS6) +
    ggtitle(paste("Weight (KG) of ", r, "Agricultural Trades, 2017-2022 in"))
  plot(p)
}

## Plot different variables against each other
# Create country specific "total weight per month" and "total value per month" dataframe for plotting
plotVol <- Comtrade.Agric %>% 
  group_by_(.dots = c("Reporter_Name","Partner_Name","Period")) %>% 
  summarise(kg = as.numeric(sum(NetWeight_kg, na.rm = TRUE))) %>% 
  as.data.table()

plotVal <- Comtrade.Agric %>% 
  group_by_(.dots = c("Reporter_Name","Partner_Name","Period")) %>% 
  summarise(USD = as.numeric(sum(Value_USD, na.rm = TRUE))) %>% 
  as.data.table()

# Get vector of the top 8 destination countries/areas by total weight shipped across all years,
# then subset plotVol to only include observations related to those countries/areas.
Top10 <- plotVol %>% 
  group_by(Partner_Name) %>% 
  summarise(kg = as.numeric(sum(kg, na.rm = TRUE))) %>% 
  top_n(20, kg) %>%
  arrange(desc(kg)) %>% 
  .[["Partner_Name"]]
plotVol <- plotVol %>% filter(Partner_Name %in% Top10)

# Create plots (y-axis is NOT fixed across panels, this will allow us to ID trends over time
# within each country/area individually).
qplot(Period, kg, data = plotVol) + 
  geom_line(data = plotVol[plotVol$Partner_Name %in% names(which(table(plotVol$Partner_Name) > 1)), ]) + 
  xlim(min(plotVol$Period), max(plotVol$Period)) + 
  labs(title = "Weight (KG) of Irish Agricultural Exports, by Destination Area, 2017-2022") + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1), 
        axis.text = element_text(size = 7)) + 
  facet_wrap(~factor(Partner_Name, levels = Top10), scales = "free", nrow = 5, ncol = 5)

# Plot Multiple charts
for (r in Comtrade.Agric$Partner_Name){
  cat("\n\n###", r, "\n")
  p <- ggplot(data=filter(Comtrade.Agric, Partner_Name == r), 
              aes(x=Period, y=factor(NetWeight_kg), 
                  xend=Period+1, yend=Reporter_Name, 
                  linetype=factor(NetWeight_kg))) +
    geom_segment(size=3) + 
    facet_grid(Partner_Name~HS6) +
    ggtitle(paste("Weight (KG) of ", r, "Agricultural Trades, 2017-2022 in"))
  plot(p)
}

#-------------------------------------------------------------------------------------------------------------------#
#  3) Export images/plots to "working directory" folder                                                             #
#-------------------------------------------------------------------------------------------------------------------#

# Save as PDFs or PNGs
ggsave("SeasonalityPlots/Example_Plot.png")

# Save to 'SeasonalityPlots' folder
for(i in 1:p) {
  png(paste("Rplot_", i, ".png"))
  set.seed(i)
  plot(rnorm(20), pch = 16, col = i)
  dev.off()
}

# Save end/finish time
sessiontime.end <- Sys.time()
difftime(sessiontime.end, sessiontime.start)

## DELETE ALL FILES ##
rm(list=ls())

# *** END ***

#############
## APENDIX ##
#############

# Prepare table to show partner countries
# remove world
tfdata3 <- tfdata2 %>% 
  filterworldeu28 %>%
  select(year, classification, flow, reporter, partner) %>%
  arrange(year, classification, flow)
for (r in unique(completeness$reporter)){
  cat("\n\n###", r, "\n")
  
  # plot completeness
  p <- ggplot(data=filter(completeness, reporter == r), 
              aes(x = year, y = variable, 
                  xend = year + 1, yend = variable, 
                  color = value, linetype = value)) +
    scale_colour_manual(values = c("TRUE" = "blue", "FALSE" = "red")) +
    scale_linetype_manual(values = c("TRUE" = 2, "FALSE" = 1)) +
    geom_segment(size=3) + 
    facet_grid(classification~.) +
    ggtitle(paste("Data completeness of", productcodeinreport,
                  "in", r))
  plot(p)
  
  
  ###################################################### #
  # Number of trade partners per classification and year #
  ###################################################### #
  # Reported by the country
  npartnersbyr <- tfdata3 %>% 
    filter(reporter == r) %>%
    dcast(year + classification ~ flow, value.var="partner", length) 
  try(print(kable(npartnersbyr,
                  caption = paste("Number of trade partners reported by", r))))
  
  # Reported by partner countries
  try(npartnersbyothers <- tfdata3 %>% 
        filter(partner == r) %>%
        dcast(year + classification ~ flow, value.var="reporter", length))
  try(print(kable(npartnersbyothers,
                  caption = paste("Number of countries who report trading with",
                                  r))))
}
r = "France"
# plot completeness
p <- ggplot(data=filter(completeness, reporter == r), 
            aes(x = year, y = variable, 
                xend = year + 1, yend = variable, 
                linetype = value)) +
  scale_colour_manual(values = c("TRUE" = "blue", "FALSE" = "red")) +
  scale_linetype_manual(values = c("TRUE" = 2, "FALSE" = 1)) +
  geom_segment(aes(color = value), size=3) + 
  geom_text(aes(y = "classification", label=classification)) +
  #     coord_cartesian(ylim = c(-0.3, 3.5)) +
  ggtitle(paste("Data completeness of", productcodeinreport,
                "in", r))
plot(p)

createreportfromdb
# plot flag ##########
pflagfr <- ggplot(data = filter(flagcountry, reporter == r),
                  aes(x= year, y = tradevalue, fill = flag)) + 
  geom_bar(stat = "identity") + facet_grid(flow ~ ., scales = "free_y") 
plot(pflagfr)
pflagfr + aes(y = weight)
pflagfr + aes(y = quantity)

message("Change flag labels to a short description for the plot")

pflag <- ggplot(data = flagregion,
                aes(x = year, y = tradevalue, fill = flag)) + 
  geom_bar(stat = "identity") +
  facet_grid(flow ~ regionreporter, scales = "free_y")
plot(pflag)
pflag + aes(y = weight)
pflag + aes(y = quantity)


# what are those extreme quantities?
bigquantity <-filter(tfdata2, quantity>1e6) %>%
  mutate(flag = paste("Flag", flag))
ggplot(data = bigquantity,
       aes(x = reporter,y = quantity, label = partner, 
           color = as.factor(year))) +
  geom_text() + scale_y_log10() + 
  facet_grid(flag ~ flow) + theme_bw()

ggplot(data = bigquantity,
       aes(x = year, y = quantity, fill = as.factor(flag))) + 
  geom_bar(stat = "identity") +
  facet_grid(flow ~ regionreporter, scales = "free_y") +
  ggtitle("It seems all large quantities are flag 0 or 4")

ggplot(data = filter(tfdata2, quantity<1e7),
       aes(x = year, y = quantity, fill = as.factor(flag))) + 
  geom_bar(stat = "identity") +
  facet_grid(flow ~ regionreporter, scales = "free_y") +
  ggtitle("quantity < 1e7")