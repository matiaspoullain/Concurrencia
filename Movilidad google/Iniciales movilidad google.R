#Movilidad google

csv <- read.csv("Region_Mobility_Report_CSVs/2020_AR_Region_Mobility_Report.csv", encoding =  "UTF-8")

tdf <- csv[csv$sub_region_1 == "Tierra del Fuego Province",]

tdf.solo <- tdf[tdf$sub_region_2 =="", c("sub_region_1"
                                         , "date"
                                         , "retail_and_recreation_percent_change_from_baseline"
                                         ,"grocery_and_pharmacy_percent_change_from_baseline" 
                                         , "parks_percent_change_from_baseline"                
                                         , "transit_stations_percent_change_from_baseline"     
                                         , "workplaces_percent_change_from_baseline"           
                                         , "residential_percent_change_from_baseline")]


library(data.table)
tdf.solo <- as.data.frame(melt(setDT(tdf.solo), id.vars = c("sub_region_1", "date"), variable.name = "tipo_local"))

tdf.solo$date <- as.Date(tdf.solo$date)

library(ggplot2)
windows()
ggplot(tdf.solo, aes(x = date, y = value, col = tipo_local)) +
  geom_line() +
  facet_grid(tipo_local~.) +
  geom_hline(yintercept = 0) +
  theme_classic()

