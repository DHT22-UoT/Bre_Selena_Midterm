#Imported datasets by clicking on station.csv, trip.csv and weather.csv in the working directory and clicking on "import dataset"
#Data are cleaned by Bre
# trip <- read_csv("~/Desktop/trip.csv")
# weather <- read_csv("~/Desktop/weather.csv")
# station <- read_csv("~/Desktop/station.csv")
library(dplyr)
library(tidyr)

#check the dimensions of the data set
dim(trip) #326339 observations (rows) and 11 variables (columns)

#column names of trip
names(trip) 

#view the structure of the data
str(trip)

#summary of data
summary(trip) #No NAs found

# Find the name of all stations
station_names <- data.frame(station$name)
#Create 2 columns, all zeros, for the starting counts (0's) to assist later counting the starting and ending stations
name_count_start <- rep(0, times = nrow(station_names))
name_count_end <- rep(0, times = nrow(station_names))
#Obtain the station names column in the station file for looping later
name_col <- station_names$station.name
#Create 2 columns in the station.csv data for counting the starting and ending stations based on the trip.csv data
station_names$start_station_count <- name_count_start
station_names$end_station_count <- name_count_end

#Task 1: Perform an EDA for the weather and trips data set 
#Attach proper packages needed for an exploratory data analysis
library(funModeling)
library(tidyverse)
library(Hmisc)

Trip.eda <- function(trip)
{
  glimpse(trip)
  print(status(trip))
  freq(trip) 
  print(profiling_num(trip))
  plot_num(trip)
  describe(trip)
}
Trip.eda(trip)

#EDA fordata set:
weather.eda <- function(weather)
{
  glimpse(weather)
  print(status(weather))
  freq(weather) 
  print(profiling_num(weather))
  plot_num(weather)
  describe(weather)
}
weather.eda(trip)

#Task 2: Remove any trips that are considered cancelled 
trip.clean <- trip %>%
  filter(!duration < 120) %>% #120s = 2mins which are cancelled trips 
  filter(duration < 17270400) #Task 3 - Identifying any outliers in the trip data set:
#One extremely large value in the duration column that needs to be removed 

#Task 3:
#Identifying any outliers in the weather data set:
#Method: Remove all values that are over the 99.9% range 
#Check which values within each variable are over the 99.9% interval
quantile(weather$max_temperature_f,probs=c(.0,.999))
quantile(weather$mean_temperature_f,probs=c(.0,.999))
quantile(weather$min_temperature_f,probs=c(.0,.999))
quantile(weather$max_visibility_miles,probs=c(.0,.999), na.rm = T)
quantile(weather$mean_visibility_miles,probs=c(.0,.999), na.rm = T)
quantile(weather$min_visibility_miles,probs=c(.0,.999), na.rm = T)
quantile(weather$max_wind_Speed_mph,probs=c(.0,.999), na.rm = T)
quantile(weather$mean_wind_speed_mph,probs=c(.0,.999))
quantile(weather$max_gust_speed_mph,probs=c(.0,.999), na.rm = T)
quantile(weather$cloud_cover,probs=c(.0,.999))
#Convert T values to 0 in PrecipitationIn column
as.numeric(weather$precipitation_inches)
weather$precipitation_inches <- stringr::str_replace(weather$precipitation_inches, "T", "0")
weather$precipitation_inches <- as.numeric(weather$precipitation_inches)
quantile(weather$precipitation_inches,probs=c(.0,.999), na.rm = T) 

#Remove the values that are over the 99% interval 
weather <- weather %>%
  filter(max_temperature_f < 95) %>%
  filter(mean_temperature_f < 79) %>% 
  filter(min_temperature_f < 68) %>%
  filter(max_visibility_miles < 20) %>%
  filter(mean_visibility_miles < 20) %>%
  filter(min_visibility_miles < 20) %>%
  filter(max_wind_Speed_mph < 115) %>%
  filter(mean_wind_speed_mph < 18) %>% 
  filter(max_gust_speed_mph < 114) %>%
  filter(precipitation_inches < 1.38) %>%
  filter(cloud_cover < 8)

#Task 4:
#Find the highest volume of hours on weekdays; find the hours of weekdays where the trip volume is highest:
library(lubridate)
#Change the type for start and end date from character to date and time
trip.clean$start_date <- mdy_hm(trip.clean$start_date)
trip.clean$end_date <- mdy_hm(trip.clean$end_date)
class(trip.clean$start_date) #Check to make sure the type switched properly 
class(trip.clean$end_date)

#Make 2 new columns separating the date and times 
trip.clean$start.date <- as.Date(trip.clean$start_date)
trip.clean$start.time <- format(trip.clean$start_date,"%H")
trip.clean$end.date <- as.Date(trip.clean$end_date)
trip.clean$end.time <- format(trip.clean$end_date,"%H")

#Filter the date column so that only weekdays are shown
library(chron)
tripstart <- trip.clean %>%
  mutate(weekday = wday(start.date, label = TRUE)) %>%
  filter(weekday == "Mon" | weekday == "Tue" | weekday == "Wed" | weekday == "Thu" | weekday == "Fri") %>%
  group_by(weekday, start.time) %>%
  summarise(cnt=n())

view(tripstart) #Each number of counts per hour are shown in this new data frame, so the rush hours are as follows:
#Monday: 8AM - 9AM has 8162 trips 
#Tuesday: 8AM - 9AM has 8849 trips
#Wednesday: 8AM - 9AM has 8440 trips
#Thursday: 8AM -9AM has 7968 trips
#Friday: 8AM - 9AM has 6939 trips

#Peak hours for end stations 
tripend <- trip.clean %>%
  mutate(weekday = wday(end.date, label = TRUE)) %>%
  filter(weekday == "Mon" | weekday == "Tue" | weekday == "Wed" | weekday == "Thu" | weekday == "Fri") %>%
  group_by(weekday, end.time) %>%
  summarise(cnt=n())
view(tripend)

#Monday: 5PM - 6PM has 7920 trips 
#Tuesday: 8AM - 9AM has 8271 trips
#Wednesday: 8AM - 9AM has 8076 trips
#Thursday: 8AM -9AM has 7474 trips
#Friday: 8AM - 9AM has 6587 trips


#Task 5. Determine the 10 most frequent starting stations and ending stations during the rush hours you established. 
#Each number of counts per hour are shown in this new data frame, so the rush hours are as follows:
#Monday: 8AM - 9AM has 8219 trips 
#Tuesday: 8AM - 9AM has 8902 trips
#Wednesday: 8AM - 9AM has 8490 trips
#Thursday: 8AM -9AM has 8016 trips
#Friday: 8AM - 9AM has 6984 trips

#Monday: 5PM - 6PM has 7920 trips 
#Tuesday: 8AM - 9AM has 8271 trips
#Wednesday: 8AM - 9AM has 8076 trips
#Thursday: 8AM -9AM has 7474 trips
#Friday: 8AM - 9AM has 6587 trips

#Convert start_date and end_date columns into appropriate formats
my_trip_copy <- trip.clean %>%
  mutate(start_date = as.POSIXct(start_date, format = "%m/%d/%Y %H:%M")) %>%
  mutate(end_date = as.POSIXct(end_date, format = "%m/%d/%Y %H:%M")) %>%
  mutate(start_hr = hour(start_date)) %>% #Obtain the starting date hours
  mutate(end_hr = hour(end_date)) %>% #Obtain the ending date hours
  mutate(start_day = lubridate::wday(start_date, label = T)) %>% #Get the day in a week of starting day
  mutate(end_day = lubridate::wday(end_date, label = T)) %>% #Get the day in a week of ending day
  filter(start_day == "Mon" | start_day == "Tue" | start_day == "Wed" | start_day == "Thu" | start_day == "Fri" |
           end_day == "Mon" | end_day == "Tue" | end_day == "Wed" | end_day == "Thu" | end_day == "Fri") %>% #Keeping data where the starting and ending days are weekdays
  mutate(start_stations_rush = ifelse(start_hr == 8, start_station_name, NA)) %>% #Obtain starting stations during rush hours on weekdays
  mutate(end_stations_rush = ifelse(((end_hr == 17 & end_day == "Mon") | (end_hr == 8 & end_day != "Mon")), end_station_name, NA))#Obtain ending stations during rush hours on weekdays

top_10_start_stations <- sort(table(my_trip_copy$start_stations_rush), decreasing = T)[1:10] #Obtain top 10 (most frequent) starting stations during rush hours on weekdays
top_10_end_stations <- sort(table(my_trip_copy$end_stations_rush), decreasing = T)[1:10] #Obtain top 10 (most frequent) ending stations during rush hours on weekdays

#Task 6. Determine the 10 most frequent starting stations and ending stations during the weekends. (more sophisticated method than Q5 but same goal)
#Convert start_date and end_date columns into appropriate formats
trip_copy <- trip.clean %>%
  mutate(start_date = as.POSIXct(start_date, format = "%m/%d/%Y %H:%M")) %>%
  mutate(end_date = as.POSIXct(end_date, format = "%m/%d/%Y %H:%M"))

#Find weekends in start_date, then use filter to extract those rows
trip_copy <- trip_copy %>%
  filter(lubridate::wday(trip_copy$start_date) %in% c(1, 7)) #Find weekends in start_date; 1 and 7 represent Sunday and Saturday, repectively.

#Obtain the starting station names in the trip.csv file
start_station_names <- trip_copy$start_station_name

#Count the number of times each starting station was used based on the trip.csv data 
# and store the counts in the start_station_count column in station_names
for (name in name_col) {
  for (i in seq_along(start_station_names)) {
    if (name == start_station_names[i]) {
      station_names$start_station_count[station_names$station.name == name] <- 
        station_names$start_station_count[station_names$station.name == name] + 1
    }
  }
}

#Find weekends in end_date, then use filter to extract those rows
trip_copy <- trip_copy %>%
  filter(lubridate::wday(trip_copy$end_date) %in% c(1, 7)) #Find weekends in end_date; 1 and 7 represent Sunday and Saturday, repectively.

#Obtain the ending station names in the trip.csv file
end_station_names <- trip_copy$end_station_name

#Count the number of times each ending station was used based on the trip.csv data 
# and store the counts in the end_station_count column in station_names
for (name2 in name_col) {
  for (j in seq_along(end_station_names)) {
    if (name2 == end_station_names[j]) {
      station_names$end_station_count[station_names$station.name == name2] <- 
        station_names$end_station_count[station_names$station.name == name2] + 1
    }
  }
}

#arrange station_names in descending order according to start_station_count
station_names_ordered <- arrange(station_names, desc(station_names$start_station_count))
#Obtain the 10 most frequent starting stations
most_freq_start <- station_names_ordered$station.name[1:10]

station_names_ordered2 <- arrange(station_names, desc(station_names$end_station_count))
# Obtain the 10 most frequent ending stations
most_freq_end <- station_names_ordered2$station.name[1:10]

#Task 7. Calculate the average utilization of bikes for each month (total time(duration) used/total time in month).
#(total duration used in month/total number of times in month)

#Use original trip data, remove outliers and change dates into appropriate formats 
#Convert start_date and end_date columns into appropriate formats
trip_copy2 <- trip.clean %>%
  mutate(start_date = as.POSIXct(start_date, format = "%m/%d/%Y %H:%M")) %>%
  mutate(end_date = as.POSIXct(end_date, format = "%m/%d/%Y %H:%M"))

#trip_copy3 contains total utilization (duration) for starting-date month == ending-date month for each month, and the number of times the bikes were used in each month
trip_copy3 <- trip_copy2 %>%
  mutate(start_month = lubridate::month(start_date)) %>% #Find each month using lubridate, put month numbers into a new column (one for start, one for end)
  mutate(end_month = lubridate::month(end_date)) %>%
  mutate(number_of_times = 1) %>% #Count the number of times bikes were used in a month
  group_by(start_month) %>%
  filter(start_month == end_month) %>% 
  summarise(utilization_per_month = sum(duration, na.rm = T), #Counting utilization in each month for when starting-date month == ending-date month
            total_number_of_times = sum(number_of_times, na.rm = T)) #Total number of times bikes were used in each month

months_not_matching <- trip_copy2 %>% #Using data where the starting and ending months do not match
  mutate(start_month = lubridate::month(start_date)) %>% #Find each month using lubridate, put month numbers into a new column (one for start, one for end)
  mutate(end_month = lubridate::month(end_date)) %>%
  mutate(number_of_times = 1) %>% #Count the number of times bikes were used in a month
  group_by(start_month) %>%
  filter(start_month != end_month)

months_not_matching$utilization_prev <- rep(0, nrow(months_not_matching)) #For dividing duration into utilization in the starting-date month
months_not_matching$utilization_next <- months_not_matching$utilization_prev #For dividing duration into utilization in the ending-date month
months_not_matching$times_next <- months_not_matching$utilization_prev #need to add the number of times bikes were used twice, because the bike was used once in each month, e.g. if a trip starts 
#in January and ends in Feb, the number of times it is used in Jan is 1, and the number of times it is used in Feb is 1
#Maximum difference between start and end months is 1, so we only need to divide duration into two months

for (start_index in seq(1:nrow(months_not_matching))) {
  end_of_month <- round_date(months_not_matching$start_date[start_index], "month") #Finding date for the end of the starting month
  months_not_matching$utilization_prev[start_index] <- 
    difftime(end_of_month, months_not_matching$start_date[start_index], units = "secs")  #Duration or utilization in the starting month
  months_not_matching$utilization_next[start_index] <- months_not_matching$duration[start_index] - months_not_matching$utilization_prev[start_index] #Duration or utilization in the ending month
  if (months_not_matching$utilization_next[start_index] > 0) {
    months_not_matching$times_next[start_index] <- months_not_matching$times_next[start_index] + 1 #if the bikes were used in the ending month, the number of times the bikes were used increases by 1 because it is also used in the ending month
  }
}

start_months_add <-  months_not_matching %>% #The divided utilization and number of times to be added to the starting months
  arrange(start_month) %>% #arrange table in ascending order according to start_month
  group_by(start_month) %>%
  summarise(utilization_per_month = sum(utilization_prev, na.rm = T), 
            total_number_of_times = sum(number_of_times, na.rm = T))

add_to_start <- data.frame(12, 0, 0) #A row to be added to start_months_add because the starting months do not include December. Therefore, when I merge the tables (adding utilizations and number of times) together, the numbers could match. 
names(add_to_start) <- c("start_month","utilization_per_month", "total_number_of_times") #The matching column names allow us to merge the tables later using rbind
start_months_add <- rbind(start_months_add, add_to_start) #Adds the row
names(start_months_add) <- c("month", "utilization_per_month", "total_number_of_times") #so we can use rbind later to combine the values (since the columns such as month would match with month, etc)

end_months_add <-  months_not_matching #The divided utilization and number of times to be added to the ending months
add_to_end <- data.frame(1, 0, 0) #A row to be added to end_months_add because the ending months do not include January. Therefore, when I merge the tables (adding utilizations and number of times) together, the numbers could match. 
names(add_to_end)<-c("end_month","utilization_next", "total_number_of_times") #The matching column names allow us to merge the tables later using rbind
end_months_add <- rbind(end_months_add, add_to_end) #Adds the row

end_months_add <- end_months_add %>%
  arrange(end_month) %>% #arrange table in ascending order according to end_month
  group_by(end_month) %>%
  summarise(utilization_per_month = sum(utilization_next, na.rm = T), #summing utilization per month
            total_number_of_times = sum(c(number_of_times, times_next), na.rm = T)) #summing number of times per month

names(end_months_add) <- c("month", "utilization_per_month", "total_number_of_times") #so we can use rbind later to combine the values
names(trip_copy3) <- c("month", "utilization_per_month", "total_number_of_times") #The matching column names allow us to merge the tables later using rbind
merged_table <- rbind(start_months_add, end_months_add, trip_copy3) #Merging the three tables that include utilization and number of times per month 
merged_table <- merged_table %>%
  group_by(month) %>%
  summarise(utilization_per_month = sum(utilization_per_month, na.rm = T), #Calculates the total time used in a month (utilization)
            total_number_of_times = sum(total_number_of_times, na.rm = T), #Calculates the total number of time bikes were used in a month
            average_utilization_per_month = floor(utilization_per_month / total_number_of_times)) #Calculates the average utilization in each month

#Task 8
library(corrplot)
trip_for_weather <- trip.clean %>% 
  mutate(start_city = station$city[match(trip.clean$start_station_id, station$id)]) %>% #Finding cities for the starting stations 
  mutate(start_date = as.Date(start_date, format = "%m/%d/%Y")) #Turn starting dates into appropriate formats


weather$date <- as.Date(weather$date, format="%m/%d/%Y") #Turn weather dates into appropriate formats

joined_table <- inner_join(trip_for_weather, weather, by = c("start_date" = "date", "start_city" = "city")) #creating a new dataset combining trip data with the weather data using date and city columns

#Data containing relevant weather measurements 
joined_table2 <- select(joined_table, c(duration, max_temperature_f:cloud_cover)) 

my_corr <- cor(joined_table2, use = "complete.obs")  #Matrix for finding correlation coefficients between duration and each of the weather measurements
corrplot(my_corr, method = "number", tl.cex = 0.8, cl.cex = 0.8, number.cex = 0.8) #no correlation for duration
                                               
