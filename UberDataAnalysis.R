#Uber Data Analysis

library(ggplot2)
library(dplyr)   
library(lubridate) 
library(scales) 
library(tidyr) 
library(DT)
library(ggthemes)

Uber.Jan.Feb.FOIL <- read.csv("Uber-Jan-Feb-FOIL.csv") 
View(Uber.Jan.Feb.FOIL) 
attach(Uber.Jan.Feb.FOIL)

mycolors <-  c("#CC0000", "#666666", "#009E73", "#CCCCCC", "#F0E442", "#0072B2", "#CC79A7")


## Exploring Uber trip data for 2014 (April - September)

uber.raw.data.apr14 <- read.csv("uber-raw-data-apr14.csv")
View(uber.raw.data.apr14)
attach(uber.raw.data.apr14)

uber.raw.data.may14 <- read.csv("uber-raw-data-may14.csv")
View(uber.raw.data.may14)
attach(uber.raw.data.may14)

uber.raw.data.jun14 <- read.csv("uber-raw-data-jun14.csv")
View(uber.raw.data.jun14)
attach(uber.raw.data.jun14)

uber.raw.data.jul14 <- read.csv("uber-raw-data-jul14.csv")
View(uber.raw.data.jul14)
attach(uber.raw.data.jul14)

uber.raw.data.aug14 <- read.csv("uber-raw-data-aug14.csv")
View(uber.raw.data.aug14)
attach(uber.raw.data.aug14)

uber.raw.data.sep14 <- read.csv("uber-raw-data-sep14.csv")
View(uber.raw.data.sep14)
attach(uber.raw.data.sep14)

uber_2014 <- rbind(uber.raw.data.apr14,uber.raw.data.may14, uber.raw.data.jun14, uber.raw.data.jul14, 
                   uber.raw.data.aug14, uber.raw.data.sep14)
uber_2014$Date.Time <- as.POSIXct(uber_2014$Date.Time, format = "%m/%d/%Y %H:%M:%S")
uber_2014$Time <- format(as.POSIXct(uber_2014$Date.Time, format = "%m/%d/%Y %H:%M:%S"), format="%H:%M:%S")
uber_2014$Date.Time <- ymd_hms(uber_2014$Date.Time)

uber_2014$day <- factor(day(uber_2014$Date.Time))
uber_2014$month <- factor(month(uber_2014$Date.Time, label = TRUE))
uber_2014$year <- factor(year(uber_2014$Date.Time))
uber_2014$dayofweek <- factor(wday(uber_2014$Date.Time, label = TRUE))

uber_2014$hour <- factor(hour(hms(uber_2014$Time)))
uber_2014$minute <- factor(minute(hms(uber_2014$Time)))
uber_2014$second <- factor(second(hms(uber_2014$Time)))


#Number of Trips by Hour

by_hour <- uber_2014 %>% group_by(hour) %>% dplyr::summarize(Total = n()) #summarize() creates a new data frame
datatable(by_hour)

ggplot(by_hour, aes(hour, Total)) + geom_bar( stat = "identity", fill = "darkgreen") + ggtitle("Trips Every Hour") +
  theme(legend.position = "none") + scale_y_continuous(labels = comma)

by_month_hour <- uber_2014 %>% group_by(month, hour) %>% dplyr::summarize(Total = n()) 

ggplot(by_month_hour, aes(hour, Total, fill = month)) + geom_bar( stat = "identity") + 
  ggtitle("Trips by Hour and Month") + scale_y_continuous(labels = comma) + scale_fill_manual(values = mycolors)


### Number of Trips by Day

by_day <- uber_2014 %>% group_by(day) %>% dplyr::summarize(Total = n()) 
datatable(by_day) #data.table is a package that provides an enhanced version of data

ggplot(by_day, aes(day, Total)) + geom_bar( stat = "identity", fill = "darkred") + ggtitle("Trips Every Day") +
  theme(legend.position = "none") + scale_y_continuous(labels = comma)

by_month_day <- uber_2014 %>% group_by(month, day) %>% dplyr::summarize(Total = n()) 

ggplot(by_month_day, aes(day, Total, fill = month)) + geom_bar( stat = "identity") +
  ggtitle("Trips by Day and Month") + scale_y_continuous(labels = comma) + scale_fill_manual(values = mycolors)


### Number of Trips by Month

by_month <- uber_2014 %>% group_by(month) %>% dplyr::summarize(Total = n()) 
datatable(by_month)

ggplot(by_month, aes(month, Total, fill = month)) + geom_bar( stat = "identity") + ggtitle("Trips by Month") +
  theme(legend.position = "none") + scale_y_continuous(labels = comma) + scale_fill_manual(values = mycolors)

by_month_weekday <- uber_2014 %>% group_by(month, dayofweek) %>% dplyr::summarize(Total = n()) 

ggplot(by_month_weekday, aes(month, Total, fill = dayofweek)) + geom_bar( stat = "identity", position = "dodge") +
  ggtitle("Trips by Day and Month") +  scale_y_continuous(labels = comma) + scale_fill_manual(values = mycolors)


### Number of Trips by bases

ggplot(uber_2014, aes(Base)) + geom_bar(fill = "darkblue") + scale_y_continuous(labels = comma) +
  ggtitle("Trips by Bases")

ggplot(uber_2014, aes(Base, fill = month)) + geom_bar(position = "dodge") + scale_y_continuous(labels = comma) +
  ggtitle("Trips by Bases and Month") + scale_fill_manual(values = mycolors)

ggplot(uber_2014, aes(Base, fill = dayofweek)) + geom_bar(position = "dodge") + scale_y_continuous(labels = comma) +
  ggtitle("Trips by Bases and DayofWeek") + scale_fill_manual(values = mycolors)


### Heat Map of Hour, Day and Month

by_hour_day <- uber_2014 %>% group_by(day, hour) %>% dplyr::summarize(Total = n())
datatable(by_hour_day)

ggplot(by_hour_day, aes(day, hour, fill = Total)) + geom_tile(color = "white") + ggtitle("Heat Map by Hour and Day")

ggplot(by_month_day, aes(day, month, fill = Total)) + geom_tile(color = "white") + 
  ggtitle("Heat Map by Month and Day")

ggplot(by_month_weekday, aes(dayofweek, month, fill = Total)) + geom_tile(color = "white") +
  ggtitle("Heat Map by Month and Day of Week")


### Heat Map by Bases, Day and Month 

by_bases_month <-  uber_2014 %>% group_by(Base, month) %>% dplyr::summarize(Total = n())

by_bases_dayofweek <-  uber_2014 %>% group_by(Base, dayofweek) %>% dplyr::summarize(Total = n())

ggplot(by_bases_month, aes(Base, month, fill = Total)) + geom_tile(color = "white") + 
  ggtitle("Heat Map by Month and Bases")

ggplot(by_bases_dayofweek, aes(Base, dayofweek, fill = Total)) + geom_tile(color = "white") +
  ggtitle("Heat Map by Bases and Day of Week")


### Exploring tuber trip statistics in January and February 2015

Uber.Jan.Feb.FOIL$date <- as.Date(Uber.Jan.Feb.FOIL$date, "%m/%d/%Y")
Uber.Jan.Feb.FOIL$day <- factor(day(Uber.Jan.Feb.FOIL$date))
Uber.Jan.Feb.FOIL$month <- factor(month(Uber.Jan.Feb.FOIL$date, label = TRUE))
Uber.Jan.Feb.FOIL$year <- factor(year(Uber.Jan.Feb.FOIL$date))
Uber.Jan.Feb.FOIL$dayofweek <- factor(wday(Uber.Jan.Feb.FOIL$date, label = TRUE))
Uber.Jan.Feb.FOIL$TripsperVehicle  <- (Uber.Jan.Feb.FOIL$trips/Uber.Jan.Feb.FOIL$active_vehicles)


### Number of Trips by Day of the month

datatable(Uber.Jan.Feb.FOIL %>% group_by(day) %>% dplyr::summarize(Total = sum(trips)))

ggplot(Uber.Jan.Feb.FOIL, aes(day, trips)) + geom_bar(fill = "darkred", stat = "identity") + 
  ggtitle("Uber trips by Day of the Month") + scale_fill_manual(values = mycolors)

ggplot(Uber.Jan.Feb.FOIL, aes(day, trips, fill = month)) + geom_bar(stat = "identity") +
  ggtitle("Uber trips by Day and Month") + scale_fill_manual(values = mycolors)


### Number of Active Vehicles by Day of the month

datatable(Uber.Jan.Feb.FOIL %>% group_by(day) %>% dplyr::summarize(Total = sum(active_vehicles)))

ggplot(Uber.Jan.Feb.FOIL, aes(day, active_vehicles)) + geom_bar(fill = "#0072B2", stat = "identity") +
  ggtitle("Active Vehicles by Day of the Month") + scale_fill_manual(values = mycolors)

ggplot(Uber.Jan.Feb.FOIL, aes(day, active_vehicles, fill = month)) + geom_bar(stat = "identity") +
  ggtitle("Active Vehicles by Day and Month") + scale_fill_manual(values = mycolors)


### Number of Trips by Weekday 

datatable(Uber.Jan.Feb.FOIL %>% group_by(dayofweek) %>% dplyr::summarize(Total = sum(trips)))

ggplot(Uber.Jan.Feb.FOIL, aes(dayofweek, trips, fill = dayofweek)) + geom_bar( stat = "identity") +
  ggtitle("Uber trips by Weekday of the Month") + theme(legend.position = "none") + 
  scale_y_continuous(labels = comma) + scale_fill_manual(values = mycolors)

ggplot(Uber.Jan.Feb.FOIL, aes(dayofweek, trips, fill = month)) + geom_bar(stat = "identity", position = "dodge") +
  ggtitle("Uber trips by Weekday and Month") + scale_y_continuous(labels = comma) + 
  scale_fill_manual(values = mycolors)


### Number of Active Vehicles By Weekday

datatable(Uber.Jan.Feb.FOIL %>% group_by(dayofweek) %>% dplyr::summarize(Total = sum(active_vehicles)))

ggplot(Uber.Jan.Feb.FOIL, aes(dayofweek, active_vehicles, fill = dayofweek)) + geom_bar( stat = "identity") +
  ggtitle("Active Vehicles By Weekday") + theme(legend.position = "none") + scale_y_continuous(labels = comma) +
  scale_fill_manual(values = mycolors)

ggplot(Uber.Jan.Feb.FOIL, aes(dayofweek, active_vehicles, fill = month)) + 
  geom_bar(stat = "identity", position = "dodge") + ggtitle("Active Vehicles By Weekday and Month") +
  scale_y_continuous(labels = comma) + scale_fill_manual(values = mycolors)


ggplot(Uber.Jan.Feb.FOIL, aes(dayofweek, active_vehicles, fill = dayofweek)) + geom_boxplot() + scale_y_log10() +
  ggtitle("Distribution of Active Vehicles By Weekday") + theme(legend.position = "top") + coord_flip()


### Number of Trips Per Vehicle

ggplot(Uber.Jan.Feb.FOIL, aes(TripsperVehicle)) + geom_histogram(bins = 30, fill  = "#0072B2") + 
  ggtitle("Distribution of Trips Per Vehicle") + scale_fill_manual(values = mycolors)

ggplot(Uber.Jan.Feb.FOIL, aes(TripsperVehicle, fill = dayofweek)) + geom_histogram(bins = 30) + 
  ggtitle("Distribution of Trips Per Vehicle by Weekday") + scale_fill_manual(values = mycolors)


ggplot(Uber.Jan.Feb.FOIL, aes(dayofweek, TripsperVehicle, fill = dayofweek)) + geom_boxplot() + scale_y_log10() +
  ggtitle("Distribution of Trips Per Vehicle by Weekday") + theme(legend.position = "top") + coord_flip()


### Distribution of Number of Trips

ggplot(Uber.Jan.Feb.FOIL, aes(trips)) + geom_histogram(bins = 50, fill  = "#009E73") + 
  ggtitle("Distribution of Number of Trips") + scale_fill_manual(values = mycolors)

ggplot(Uber.Jan.Feb.FOIL, aes(trips, fill = dayofweek)) + geom_histogram(bins = 30) + 
  ggtitle("Distribution of Number of Trips by Weekday") + scale_fill_manual(values = mycolors)

ggplot(Uber.Jan.Feb.FOIL, aes(dayofweek, trips, fill = dayofweek)) + geom_boxplot() + scale_y_log10() +
  ggtitle("Distribution of Trips by Weekday") + theme(legend.position = "top") + coord_flip()


### Mapping number of rides

min_lat <- 40.5774
max_lat <- 40.9176
min_long <- -74.15
max_long <- -73.7004

ggplot(uber_2014, aes(x=Lon, y=Lat)) + geom_point(size=1, color = "blue") + 
  scale_x_continuous(limits=c(min_long, max_long)) + scale_y_continuous(limits=c(min_lat, max_lat)) + theme_map() +
  ggtitle("NYC MAP BASED ON UBER RIDES DURING 2014 (APR-SEP)")

ggplot(uber_2014, aes(x=Lon, y=Lat, color = Base)) + geom_point(size=1) + 
  scale_x_continuous(limits=c(min_long, max_long)) + scale_y_continuous(limits=c(min_lat, max_lat)) + theme_map() +
  ggtitle("NYC MAP BASED ON UBER RIDES DURING 2014 (APR-SEP) by BASE")







