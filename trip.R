q1_2020 <- read.csv("Divvy_Trips_2020_Q1.csv")
q2_2019 <- read.csv("Divvy_Trips_2019_Q2.csv")
q3_2019 <- read.csv("Divvy_Trips_2019_Q3.csv")
q4_2019 <- read.csv("Divvy_Trips_2019_Q4.csv")
q2_2019 <- rename(q2_2019
                  ,ride_id = X01...Rental.Details.Rental.ID
                  ,started_at = X01...Rental.Details.Local.Start.Time
                  ,ended_at = X01...Rental.Details.Local.End.Time
                  ,rideable_type = X01...Rental.Details.Bike.ID
                  ,start_station_id = X03...Rental.Start.Station.ID
                  ,start_station_name = X03...Rental.Start.Station.Name
                  ,end_station_id = X02...Rental.End.Station.ID
                  ,end_station_name = X02...Rental.End.Station.Name
                  ,member_casual = User.Type)
q3_2019 <- rename(q3_2019
                  ,ride_id = trip_id
                  ,started_at = start_time
                  ,ended_at = end_time
                  ,rideable_type = bikeid
                  ,start_station_id = from_station_id
                  ,start_station_name = from_station_name
                  ,end_station_id = to_station_id
                  ,end_station_name = to_station_name
                  ,member_casual = usertype )
q4_2019 <- rename(q4_2019
                  ,ride_id = trip_id
                  ,started_at = start_time
                  ,ended_at = end_time
                  ,rideable_type = bikeid
                  ,start_station_id = from_station_id
                  ,start_station_name = from_station_name
                  ,end_station_id = to_station_id
                  ,end_station_name = to_station_name
                  ,member_casual = usertype )
q2_2019 <- mutate( q2_2019,ride_id = as.character("ride_id"), rideable_type = as.character("rideable_type")) 
q3_2019 <- mutate( q3_2019,ride_id = as.character("ride_id"), rideable_type = as.character("rideable_type")) 
q4_2019 <- mutate( q4_2019,ride_id = as.character("ride_id"), rideable_type = as.character("rideable_type")) 
all_trip <- bind_rows(q1_2020,q2_2019,q3_2019,q4_2019)
all_trip <-  all_trip %>% 
  mutate(member_casual = recode(member_casual
                                ,"Subscriber" = "member"
                                ,"Customer" = "casual"))
all_trip <- all_trip %>% select(-c(X01...Rental.Details.Duration.In.Seconds.Uncapped, tripduration, birthyear, gender, start_lat, start_lng, end_lat, end_lng, Member.Gender, X05...Member.Details.Member.Birthday.Year))
all_trip$ride_length <- difftime(all_trip$ended_at,all_trip$started_at)
all_trip$ride_length <- as.numeric(as.character(all_trip$ride_length))
all_trip_v2 <- all_trip[!(all_trip$start_station_name == "HQ QR" | all_trip$ride_length<0),]
all_trip$date <- as.Date(all_trip$started_at) 
all_trip$month <- format(as.Date(all_trip$date),"%m")  
all_trip$year <- format(as.Date(all_trip$date),"%y") 
all_trip$day <- format(as.Date(all_trip$date),"%d")  
all_trip$day_of_week <-format(as.Date(all_trip$date),"%A")
all_trip$day_of_week <- ordered(all_trip$day_of_week,levels=c("Sunday" , "Monday","Tuesday", "Wednesday","Thursday","Friday","Saturday")) 
aggregate(all_trip_v2$ride_length ~ all_trip_v2$member_casual + all_trip_v2$day_of_week, FUN = mean)
all_trip_v2 %>% 
            mutate(weekday = wday(started_at, label = TRUE)) %>% 
           group_by(member_casual,weekday) %>% 
           summarise(number_of_rides=n(),average_duration = mean(ride_length)) %>% 
           arrange(member_casual,weekday) %>% 
ggplot( aes(x=weekday , y=average_duration , fill = member_casual)) + geom_col(position = "dodge")