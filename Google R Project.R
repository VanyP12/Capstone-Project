##Capstone Project- Google Data Analytics

library(dplyr)
library(readr)

#create all csv files into data frames- to merge all into one data frame
D1.df<- read_csv("202004-divvy-tripdata.csv")
D2.df<- read_csv("202005-divvy-tripdata.csv")
D3.df<- read_csv("202006-divvy-tripdata.csv")
D4.df<- read_csv("202007-divvy-tripdata.csv")
D5.df<- read_csv("202008-divvy-tripdata.csv")
D6.df<- read_csv("202009-divvy-tripdata.csv")
D7.df<- read_csv("202010-divvy-tripdata.csv")
D8.df<- read_csv("202011-divvy-tripdata.csv")
D9.df<- read_csv("202012-divvy-tripdata.csv")
D10.df<- read_csv("202101-divvy-tripdata.csv")
D11.df<- read_csv("202102-divvy-tripdata.csv")
D12.df<- read_csv("202103-divvy-tripdata.csv")

#structure of data frames
str(D1.df)
str(D2.df)
str(D3.df)
str(D4.df)
str(D5.df)
str(D6.df)
str(D7.df)
str(D8.df)
str(D9.df)
str(D10.df)
str(D11.df)
str(D12.df)

#convert double into char for start_station_id column
#initial problem with merging: for dataframe D1-8 the column was a double
#and for dataframe D8-D12 the column was set as a char
D1.df$start_station_id<- as.character(D1.df$start_station_id)
class(D1.df$start_station_id)

D2.df$start_station_id<- as.character(D2.df$start_station_id)
class(D2.df$start_station_id)

D3.df$start_station_id<- as.character(D3.df$start_station_id)
class(D3.df$start_station_id)

D4.df$start_station_id<- as.character(D4.df$start_station_id)
class(D4.df$start_station_id)

D5.df$start_station_id<- as.character(D5.df$start_station_id)
class(D5.df$start_station_id)

D6.df$start_station_id<- as.character(D6.df$start_station_id)
class(D6.df$start_station_id)

D7.df$start_station_id<- as.character(D7.df$start_station_id)
class(D7.df$start_station_id)

D8.df$start_station_id<- as.character(D8.df$start_station_id)
class(D8.df$start_station_id)

#convert char to datetime utc- started_at & ended_at variables
D9.df$started_at<- as.POSIXct(D9.df$started_at,format="%m/%d/%Y %H:%M:%S",tz=Sys.timezone())
class(D9.df$started_at)

D9.df$ended_at<- as.POSIXct(D9.df$ended_at,format="%m/%d/%Y %H:%M:%S",tz=Sys.timezone())
class(D9.df$ended_at)

D12.df$started_at<- as.POSIXct(D12.df$started_at,format="%m/%d/%Y %H:%M:%S",tz=Sys.timezone())
class(D9.df$started_at)

D12.df$ended_at<- as.POSIXct(D12.df$ended_at,format="%m/%d/%Y %H:%M:%S",tz=Sys.timezone())
class(D9.df$ended_at)

#convert double into char
D1.df$end_station_id<- as.character(D1.df$end_station_id)
class(D1.df$end_station_id)

D2.df$end_station_id<- as.character(D2.df$end_station_id)
class(D2.df$end_station_id)

D3.df$end_station_id<- as.character(D3.df$end_station_id)
class(D3.df$end_station_id)

D4.df$end_station_id<- as.character(D4.df$end_station_id)
class(D4.df$end_station_id)

D5.df$end_station_id<- as.character(D5.df$end_station_id)
class(D5.df$end_station_id)

D6.df$end_station_id<- as.character(D6.df$end_station_id)
class(D6.df$end_station_id)

D7.df$end_station_id<- as.character(D7.df$end_station_id)
class(D7.df$end_station_id)

D8.df$end_station_id<- as.character(D8.df$end_station_id)
class(D8.df$end_station_id)


#Combine all data frames into one
All_12_Months.df<-bind_rows(D1.df, D2.df, D3.df,D4.df, D5.df, D6.df, D7.df, D8.df, D9.df, D10.df,D11.df, D12.df)

#add the new variable (ride_length) to the data frame 
All_12_Months2.df<-mutate(All_12_Months.df, ride_length = difftime(ended_at,started_at, units="hours"))
class(All_12_Months2.df$ride_length)

#add the new variable (day_of_week) to the data frame 
All_12_Months2.df$day_of_week<-weekdays(All_12_Months2.df$started_at)

# Missing Values
sum(is.na(All_12_Months2.df$ride_id))
sum(is.na(All_12_Months2.df$rideable_type))
sum(is.na(All_12_Months2.df$started_at))
sum(is.na(All_12_Months2.df$ended_at))
sum(is.na(All_12_Months2.df$start_station_name))
sum(is.na(All_12_Months2.df$start_station_id))
sum(is.na(All_12_Months2.df$end_station_name))
sum(is.na(All_12_Months2.df$end_station_id))
sum(is.na(All_12_Months2.df$start_lat))
sum(is.na(All_12_Months2.df$start_lng))
sum(is.na(All_12_Months2.df$end_lat))
sum(is.na(All_12_Months2.df$end_lng))
sum(is.na(All_12_Months2.df$member_casual))
sum(is.na(All_12_Months2.df$ride_length))
sum(is.na(All_12_Months2.df$day_of_week))

# Omit any missing data 
All_months.df<-na.omit(All_12_Months2.df)

#recheck
sum(is.na(All_months.df$ride_id))
sum(is.na(All_onths.df$rideable_type))
sum(is.na(All_months.df$started_at))
sum(is.na(All_months.df$ended_at))
sum(is.na(All_months.df$start_station_name))
sum(is.na(All_months.df$start_station_id))
sum(is.na(All_months.df$end_station_name))
sum(is.na(All_months.df$end_station_id))
sum(is.na(All_months.df$start_lat))
sum(is.na(All_months.df$start_lng))
sum(is.na(All_months.df$end_lat))
sum(is.na(All_months.df$end_lng))
sum(is.na(All_months.df$member_casual))
sum(is.na(All_months.df$ride_length))
sum(is.na(All_months.df$day_of_week))

#Summary of data frame
summary(All_months.df)

#Frequency of members vs. casual
library(janitor)
x<-All_months.df$member_casual
tabyl(x)

#information from the columns
glimpse(All_months.df)

#max and mean of ride length
library(tidyverse)
All_months.df %>% group_by(member_casual) %>% 
  summarize(max_ride_length= max(ride_length),mean_ride_length= mean(ride_length))

#mode (highest frequency) for day of week
y<-All_months.df$day_of_week
tabyl(y)

#Frequency of the type of bikes
FrequencyRideType<-All_months.df$rideable_type
tabyl(FrequencyRideType)

#average ride_length per member/casual and day of week
Table<-All_months.df %>% group_by(day_of_week, member_casual) %>% 
  summarize(Mean_ride_length= mean(ride_length))

#counted the rides taken by casual or member group by day of the week
All_months.df %>% group_by(day_of_week) %>% 
  count(member_casual)

#Which type of bike is preferred by members/casual riders
All_months.df %>% group_by(rideable_type, member_casual) %>% 
  count(member_casual)

#Represents the quantity of rides taken by casual/member riders per day
library("ggplot2")
ggplot(data=All_months.df)+
  geom_bar(mapping=aes(x=day_of_week, fill=member_casual))+
  labs(title="The Quantity of Rides: Members vs. Casual Riders",
       caption ="Data collected by Lyft Bikes and Scooters, LLC (“Bikeshare”)" )

#chart of the type of bikes that are preferred
#if you want to specify specific color, ex:scale_fill_manual(values=c("purple","yellow"))
ggplot(data=All_months.df)+
  geom_bar(mapping=aes(x=member_casual, fill=member_casual))+
  facet_wrap(~rideable_type)+
  labs(title="The Type of Bikes Utilized by Members and Casual Riders",
       caption ="Data collected by Lyft Bikes and Scooters, LLC (“Bikeshare”)" )

#Average ride length chart
ggplot(data=Table, aes(x=day_of_week,y= Mean_ride_length, fill=member_casual))+
  geom_bar(stat='identity')+
  labs(title="The Average Length of Rides Taken Throughout the Week: Members vs. Casual Riders",
       caption ="Data collected by Lyft Bikes and Scooters, LLC (“Bikeshare”)" )
