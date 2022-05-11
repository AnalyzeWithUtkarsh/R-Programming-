In this project, I analysed the uber dataset from 2014 subjected to one state in the USA. I studied the cause of the sudden rise or fall of rides on a particular day or month. The dataset included 4 columns which later become 9-10 columns using dplyr package of R.


#CODE 

#import files and libraries 
library(ggplot2) #visualization
library(ggthemes) #add on with ggplot
library(dplyr) #data manipulation
library(lubridate) #date and time
library(scales) #graphical scaling
library(tidyr) #tidy data
library(DT) #table formatted result

#reading the chunk of data

apr_data <- read.csv("D:\\III YEAR ASSIGNMENTS\\Kaggle Datasets\\Uber-dataset\\uber-raw-data-apr14.csv\\uber-raw-data-apr14.csv")
may_data <- read.csv("D:\\III YEAR ASSIGNMENTS\\Kaggle Datasets\\Uber-dataset\\uber-raw-data-may14.csv\\uber-raw-data-may14.csv")
jun_data <- read.csv("D:\\III YEAR ASSIGNMENTS\\Kaggle Datasets\\Uber-dataset\\uber-raw-data-jun14.csv\\uber-raw-data-jun14.csv")
jul_data <- read.csv("D:\\III YEAR ASSIGNMENTS\\Kaggle Datasets\\Uber-dataset\\uber-raw-data-jul14.csv\\uber-raw-data-jul14.csv")
aug_data <- read.csv("D:\\III YEAR ASSIGNMENTS\\Kaggle Datasets\\Uber-dataset\\uber-raw-data-aug14.csv\\uber-raw-data-aug14.csv")
sep_data <- read.csv("D:\\III YEAR ASSIGNMENTS\\Kaggle Datasets\\Uber-dataset\\uber-raw-data-sep14.csv\\uber-raw-data-sep14.csv")

#combine all data of 6 months

uber_data_2014 <- rbind(apr_data,may_data,jun_data,jul_data,aug_data,sep_data)

#structure & summary
str(uber_data_2014)
summary(uber_data_2014)
head(uber_data_2014)

#separate date time column
uber_data_2014$Date.Time <- as.POSIXct(uber_data_2014$Date.Time,format = "%m/%d/%Y %H:%M:%S")
uber_data_2014$date_component1 <- format(uber_data_2014$Date.Time,'%Y-%m-%d')

#extract time from date time column
uber_data_2014$Time <- format(as.POSIXct(uber_data_2014$Date.Time,format = "%m/%d/%Y %H:%M:%S"), format = "%H:%M:%S")
uber_data_2014$day <- format(day(uber_data_2014$Date.Time))
uber_data_2014$time_component <- format(uber_data_2014$Date.Time, format = "%H:%M:%S")
uber_data_2014$month <- format(month(uber_data_2014$Date.Time, label = TRUE))
uber_data_2014$dayofweek <- format(wday(uber_data_2014$Date.Time, label = TRUE))
uber_data_2014$hour <- factor(hour(hms(uber_data_2014$time_component)))
uber_data_2014$minute <- factor(minute(hms(uber_data_2014$time_component)))
uber_data_2014$seconds <- factor(second(hms(uber_data_2014$time_component)))

#visualization

hour_data <- uber_data_2014 %>% group_by(hour) %>% summarise(Total = n())
datatable(hour_data)
ggplot(hour_data, aes(hour, Total)) + geom_bar(stat = "identity") + scale_y_continuous(labels = comma)

month_hour_data <- uber_data_2014 %>% group_by(month,hour) %>% summarise(Total = n())
datatable(month_hour_data)
ggplot(month_hour_data, aes(hour,Total, fill = month)) + geom_bar(stat = "identity") + scale_y_continuous(labels = comma)
ggplot(month_hour_data, aes(month, Total)) + geom_bar(stat = "identity") + scale_y_continuous(labels = comma)


#most rides in a month of September

sept_hr <- uber_data_2014 %>% group_by(month,hour) %>% filter(month == "Sep") %>% summarise(Total = n())
ggplot(sept_hr, aes(hour,Total, fill = hour)) + geom_bar(stat = "identity") + scale_y_continuous(labels = comma)


#most rides in the month of April

apr_hr <- uber_data_2014 %>% group_by(month,hour) %>% filter(month == "Apr") %>% summarise(Total = n())
ggplot(apr_hr, aes(hour,Total, fill = hour)) + geom_bar(stat = "identity") + scale_y_continuous(labels = comma)

#most rides in the month of May

may_hr <- uber_data_2014 %>% group_by(month,hour) %>% filter(month == "May") %>% summarise(Total = n())
ggplot(may_hr, aes(hour,Total, fill = hour)) + geom_bar(stat = "identity") + scale_y_continuous(labels = comma)

#most rides in the month of June

jun_hr <- uber_data_2014 %>% group_by(month,hour) %>% filter(month == "Jun") %>% summarise(Total = n())
ggplot(jun_hr, aes(hour,Total, fill = hour)) + geom_bar(stat = "identity") + scale_y_continuous(labels = comma)


#most rides in the month of July

jul_hr <- uber_data_2014 %>% group_by(month,hour) %>% filter(month == "Jul") %>% summarise(Total = n())
ggplot(jul_hr, aes(hour,Total, fill = hour)) + geom_bar(stat = "identity") + scale_y_continuous(labels = comma)


#most rides in the month of August

aug_hr <- uber_data_2014 %>% group_by(month,hour) %>% filter(month == "Aug") %>% summarise(Total = n())
ggplot(aug_hr, aes(hour,Total, fill = hour)) + geom_bar(stat = "identity") + scale_y_continuous(labels = comma)


#day by analysis of month

day_data <- uber_data_2014 %>% group_by(day) %>% summarise(Total = n())
datatable(hour_data)

sept_day <- uber_data_2014 %>% group_by(day,month) %>% filter(month == "Sep") %>% summarise(Total = n())
ggplot(sept_day, aes(day,Total, fill = day)) + geom_bar(stat = "identity") + scale_y_continuous(label = comma)

weekday <- uber_data_2014 %>% group_by(month, dayofweek) %>% summarise(Total = n())
ggplot(weekday, aes(dayofweek,Total)) + geom_bar(stat = "identity") + scale_y_continuous(labels = comma)


#analysis of bases

ggplot(uber_data_2014, aes(Base)) + geom_bar() + scale_y_continuous(labels = comma)
ggplot(uber_data_2014, aes(Base, fill = month)) + geom_bar(position = "dodge") + scale_y_continuous(labels = comma)

