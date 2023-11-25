# Load tidyverse for data analysis
library(tidyverse)
# Load skimr for summary statistics
library(skimr)
# Load janitor for data cleaning
library(janitor)
# Load lubridate to work with dates
library(lubridate)
# Import data
trip_202201 <- read_csv("202201-divvy-tripdata.csv")
trip_202202 <- read_csv("202202-divvy-tripdata.csv")
trip_202203 <- read_csv("202203-divvy-tripdata.csv")
trip_202204 <- read_csv("202204-divvy-tripdata.csv")
trip_202205 <- read_csv("202205-divvy-tripdata.csv")
trip_202206 <- read_csv("202206-divvy-tripdata.csv")
trip_202207 <- read_csv("202207-divvy-tripdata.csv")
trip_202208 <- read_csv("202208-divvy-tripdata.csv")
trip_202209 <- read_csv("202209-divvy-publictripdata.csv")
trip_202210 <- read_csv("202210-divvy-tripdata.csv")
trip_202211 <- read_csv("202211-divvy-tripdata.csv")
trip_202212 <- read_csv("202212-divvy-tripdata.csv")
# Compare column names of all the files
compare_df_cols(trip_202201,trip_202202,trip_202203,trip_202204,trip_202205,trip_202206,trip_202207,trip_202208,trip_202209,trip_202210,trip_202211,trip_202212,return="mismatch")
# Compare column order
compare_df_cols(trip_202201,trip_202202,trip_202203,trip_202204,trip_202205,trip_202206,trip_202207,trip_202208,trip_202209,trip_202210,trip_202211,trip_202212)
# Combine all files into one
trip_2022 <- bind_rows(trip_202201,trip_202202,trip_202203,trip_202204,trip_202205,trip_202206,trip_202207,trip_202208,trip_202209,trip_202210,trip_202211,trip_202212)
# Check the internal structure
str(trip_2022)
# Remove unnecessary columns
trip_2022 <- subset(trip_2022, select=-c(start_station_id,end_station_id,start_lat,start_lng,end_lat,end_lng))
# Calculate the ride length in a new column
library(dplyr)
trip_2022 <- trip_2022 %>%
  mutate(ride_length=ended_at-started_at)
# Sort by ride length
trip_2022 <- trip_2022 %>%
  arrange(ride_length)
# Convert ride length column from seconds to hh:mm:ss
library(lubridate)
library(hms)
trip_2022$ride_length_hms <- hms::hms(seconds_to_period(trip_2022$ride_length))
# Extract ride start hour
trip_2022$start_hour <- hour(trip_2022$started_at)
# Calculate the day of the week that each ride started
trip_2022$day_of_week <- weekdays(as.Date(trip_2022$started_at))
# Calculate the season that each ride started
install.packages("hydroTSM")
library("hydroTSM")
trip_2022$season <- time2season(trip_2022$started_at,out.fmt = "seasons")
# Add a numeric column based on ride length so we can remove the negative and 0 values
trip_2022$ride_length_num <- as.numeric(as.character(trip_2022$ride_length))
trip_2022 <- trip_2022[trip_2022$ride_length_num>0, ]
# Check for NA values
colSums(is.na(trip_2022))
# Remove NA values from start station name
trip_2022 <- trip_2022[!is.na(trip_2022$start_station_name),]
# Check for NA values
colSums(is.na(trip_2022))
# Remove NA values from end station name
trip_2022 <- trip_2022[!is.na(trip_2022$end_station_name),]
# Check for NA values
colSums(is.na(trip_2022))
### Outliers
library(ggstatsplot)
Quantile <- quantile(trip_2022$ride_length_num,probs=c(.25,.75),na.rm=FALSE)
iqr <- IQR(trip_2022$ride_length_num)
up <- Quantile[2]+1.5*iqr
low <- Quantile[1]-1.5*iqr
trip_2022 <- subset(trip_2022,trip_2022$ride_length_num>(Quantile[1]-1.5*iqr)&trip_2022$ride_length_num<(Quantile[2]+1.5*iqr))
# Remove trips that last less than 1 minute
trip_2022 <- trip_2022[trip_2022$ride_length_num>60, ]
# Install and load pivottabler and ggplot2 packages
install.packages("pivottabler")
library(pivottabler)
install.packages("ggplot2")
library(ggplot2)
# Compare number of members vs casual riders
pt <- PivotTable$new()
pt$addData(trip_2022)
pt$addRowDataGroups("member_casual")
pt$defineCalculation(calculationName="No_of_riders",summariseExpression="n()")
pt$renderPivot()
# Graph pt
ggplot(data=trip_2022)+geom_bar(mapping=aes(x=member_casual,fill=member_casual))+labs(title="No of Members vs Casual Riders",x="Rider type",y="Count",fill="Rider type")
# Calculate the average ride length for members and casual riders
pt1 <- PivotTable$new()
pt1$addData(trip_2022)
pt1$addRowDataGroups("member_casual")
pt1$defineCalculation(calculationName="averageRideLength",caption="Average Ride Length",summariseExpression="round(mean(ride_length_num,na.rm=TRUE),1)")
pt1$renderPivot()
# Graph pt1
ggplot(trip_2022,aes(x=member_casual,y=ride_length_num,fill=member_casual))+geom_bar(stat="summary",fun="mean")+labs(title="Average Ride Length",x="Rider type",y="Average",fill="Rider type")
# Calculate the average ride length for users by day of week
pt2 <- PivotTable$new()
pt2$addData(trip_2022)
pt2$addColumnDataGroups("day_of_week")
pt2$addRowDataGroups("member_casual")
pt2$defineCalculation(calculationName="averageRideLength",caption="Average Ride Length",summariseExpression="round(mean(ride_length_num,na.rm=TRUE),1)")
pt2$renderPivot()
# Graph pt2
ggplot(trip_2022,aes(x=factor(day_of_week,levels=c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday")),y=ride_length_num,fill=day_of_week))+geom_bar(stat="summary",fun="mean",position = position_stack())+facet_wrap(~member_casual)+labs(title="Average Ride Length",x="Day of week",y="Average",fill="Day of week")
# Calculate the number of rides for users by day of week
pt3 <- PivotTable$new()
pt3$addData(trip_2022)
pt3$addColumnDataGroups("day_of_week")
pt3$addRowDataGroups("member_casual")
pt3$defineCalculation(calculationName="NoOfRides",summariseExpression="n()")
pt3$renderPivot()
# Graph pt3
ggplot(data=trip_2022)+geom_bar(mapping=aes(x=factor(day_of_week,levels=c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday")),fill=day_of_week))+facet_wrap(~member_casual)+labs(title="Number of rides by day of week",x="Day of week",y="Count",fill="Day of week")
# Calculate the number of bikes for members and casual riders
pt4 <- PivotTable$new()
pt4$addData(trip_2022)
pt4$addColumnDataGroups("rideable_type")
pt4$addRowDataGroups("member_casual")
pt4$defineCalculation(calculationName="NoOfBikes",summariseExpression="n()")
pt4$renderPivot()
# Graph pt4
ggplot(data=trip_2022)+geom_bar(mapping=aes(x=rideable_type,fill=rideable_type))+facet_wrap(~member_casual)+labs(title="No of bikes per rider type",x="Bike Type",y="Count",fill="Bike type")
# Calculate the number of rides for members and casual riders per season
pt5 <- PivotTable$new()
pt5$addData(trip_2022)
pt5$addColumnDataGroups("season")
pt5$addRowDataGroups("member_casual")
pt5$defineCalculation(calculationName="NoOfRides",summariseExpression="n()")
pt5$renderPivot()
# Graph pt5
ggplot(data=trip_2022)+geom_bar(mapping=aes(x=season,fill=season))+facet_wrap(~member_casual)+labs(title="No of rides per rider type by season",x="Season",y="Count",fill="Season")
# Calculate the number of rides for members and casual riders per ride start hour
pt6 <- PivotTable$new()
pt6$addData(trip_2022)
pt6$addColumnDataGroups("member_casual")
pt6$addRowDataGroups("start_hour")
pt6$defineCalculation(calculationName="NoOfRides",summariseExpression="n()")
pt6$renderPivot()
# Graph pt6
ggplot(data=trip_2022)+geom_bar(mapping=aes(x=factor(start_hour)))+facet_wrap(~member_casual)+labs(title="No of rides per rider type by ride start hour",x="Ride Start Hour",y="Count")
# Calculate the mean of ride length
as_hms(mean(trip_2022$ride_length))
# Calculate the max ride length
as_hms(max(trip_2022$ride_length))
# Calculate the min ride length
as_hms(min(trip_2022$ride_length))
# Calculate the mode of day of week
find_mode <- function(x) {
  u <- unique(x)
  tab <- tabulate(match(x,u))
  u[tab==max(tab)]
}
find_mode(trip_2022$day_of_week)
# Key findings:
# 1. Most of the customers are already members (63% vs 37%)
# 2. The average ride length for casual riders is higher than the average ride length of members (14 min vs 11 min)
# 3. Most casual customers ride on weekends, while most members ride on weekdays
# 4. Both casual customers and members ride for a longer period of time on weekends
# 5. Casual customers are almost evenly split between classic and electric bikes, while members are leaning more towards classic bikes
# 6. The percentage of rides between seasons is almost the same for both casuals and members
# 7. Most casual customers and members start their ride between 16:00-19:00, but we also see a spike between 07:00-09:00 for members
# Recommendations:
# 1. Unlike members, casual customers do not seem to use their bike to commute to work, so creating a campaign to showcase the benefits of riding to work by bike might be beneficial.
# 2. Most of the classic bike riders are already members, but electric bike riders are divided more evenly, so maybe targeting casual electric bike riders to explain them the benefits of membership would be more beneficial.
# 3. Casual customers' rides tend to last longer than those of members, maybe because they try to make the most out of their paid ride, so explaining them that based on their ride times might actually be cheaper being a member, might convince them to make the switch.