# This was my Google Analytics Capstone Case Study. This was the code I used in R to produce my analysis. I know it still looks amaturish, this was my first time writing R.

install.packages("tidyverse") #includes dplyr, readr & ggplot2                     
install.packages("plyr")
install.packages("janitor")
install.packages("lubridate")
install.packages("skimr")
install.packages("rmarkdown")
install.packages("ggrepel")
install.packages("sf")
install.packages("scales")

library("plyr")
library("dplyr")
library("readr")
getwd()
setwd("H:/R")

# Convert all the CSV files into one data frame
year <- list.files(path = "H:/R/CSV",  # Identify all CSV files
                       pattern = "*.csv", full.names = TRUE) %>% 
  lapply(read_csv) %>%                              # Store all files in list
  bind_rows                                         # Combine data sets into one data set

#cleaning time
library("janitor")
clean_names(`year`) #To clean column titles
get_dupes(`year`)
remove_empty(`year`)
library("tidyr")
"year" <- `year` %>% drop_na()

colnames(year)  #List of column names
nrow(year)  #How many rows are in data frame?
dim(year)  #Dimensions of the data frame?
head(year)  #See the first 6 rows of data frame.  Also tail(all_trips)
str(year)  #See list of columns and data types (numeric, character, etc)
summary(year)  #Statistical summary of data. Mainly for numerics



#Prepare
library("lubridate")

"yearT" <- `year` %>% #Calculate time, add the weekday and the week number
  select(ride_id,rideable_type,started_at,ended_at,member_casual,start_lat,start_lng,end_lat,end_lng,start_station_name,end_station_name)%>%
  mutate(time = difftime(ended_at,started_at,units = "mins"))%>%
  filter(time > 5)%>% #to filter out people who just picked up a bike, found it was faulty and went for another or equivalent
  mutate(week_day = weekdays(started_at))%>% #create a column showing the weekday
  mutate(week_no = week(started_at))%>% #week bicycle was used
  mutate(days = difftime(ended_at, started_at, units="days"))


glimpse(`yearT`)
count(`yearT`,member_casual) #check the contents of member_casual. Could also use table(year$member_casual)
summary(yearT)
library("skimr")
skim_without_charts(yearT) #quickly using this to see if we can pull any useful data

str(yearT) #check to see what data type time has been assigned
# Convert "time" to numeric so we can run calculations on the data
yearT$time <- as.numeric(as.character(yearT$time)) #change to numeric
is.numeric(yearT$time) #check to see if it is numeric


#Analyse
#Noticed some of the time the bikes were hired for was a very large amount of time. Lets investigate that.
"day" <- count(yearT,days,member_casual) %>%
  filter(days>1)%>% #If we run the script just to here we can see that annual members only keep the bike for one day the rest are casual members
  group_by(member_casual)%>% #groups the data into the seperate variables in member_casual, cannot see groups only effects other functions. This is for summarise.
  summarise("days_greater_1" = sum(n)) %>% #creates a function that can sum all in column n. Could also use the aggregate() function
  inner_join(count(`yearT`,member_casual,name = "total")) %>%
  mutate(percentage_dif = days_greater_1/total*100) #This is such a tiny difference, it wasn't worth doing all of this but it was fun
  
View(day) #My original theory was that members were likely to rent the bikes longer. But it appears that casual members rent the bikes longer. However the amount of times this happens is so small, it's not even worth registering.
#I will filter out this data in the future analysis since it is so rare and could be the bikes are away for maintenance

"yearTC" <- `yearT` %>%
  filter(days<1)%>%
  select(-days)
  

"YearTCsum" <- `yearTC` %>% # We can easily see that casual members use the bikes longer than annual members, could also use aggregate function
  group_by(member_casual)%>% #Min shows the time as 5.02 for both. I originally put a filter for less than 5 minutes perhaps due to faulty bikes or the negative values could show that they were being repaired
  summarise("mean" = mean(time), "median" = median(time), "max" = max(time), "min" = min(time))

yearTC$week_day <- ordered(yearTC$week_day, levels=c("mandag", "tirsdag", "onsdag", "torsdag", "fredag", "lørdag", "søndag"))#We have to order the data for days of the week before we process it

#This shows the favourite week day for taking the bikes between casual and annual members
"yearTCcount" <- `yearTC` %>% 
  group_by(member_casual, week_day)%>%
  summarise(number_of_rides = n(),average_duration = mean(time)) %>% # calculates the average duration and number of rides
  mutate(number_of_rides / 1000)

`yearT` %>%
  group_by(member_casual) %>%
  count(member_casual, rideable_type)
  

library(ggplot2)
# Let's visualize the number of rides by rider type
`yearTCcount` %>% #It's easy to see that casual members prefer weekends over annual members and annual members only slightly prefer wednesdays to use the bikes
  ggplot(aes(x = week_day, y = `number_of_rides/1000`, group = member_casual, color = member_casual)) +
  geom_line() +
  geom_point() +
  theme_minimal() +  
  labs(title = "Number of rides vs Weekday", 
       subtitle = "Comparing Casual Riders with Annual Members over a 12 month period",
       x = "Weekday",
       y = "Average Number of Rides in thousands (e.g. 400 is 400,000)",
       fill = "Casual Riders 
       vs Annual Members",
       caption = "Casual riders prefer to ride on the weekend, whereas annual members prefer mid-week")

# Let's create a visualization for average duration
`yearTCcount` %>% #Its easy to see that casual members prefer to use the bikes longer than annual
  ggplot(aes(x = week_day, y = average_duration, group = member_casual, color = member_casual)) +
  geom_line() +
  geom_point() +
  expand_limits(y = 0) +
  theme_minimal() +
  labs(title = "Ride Duration vs Weekday", 
       subtitle = "Comparing Casual Riders with Annual Members",
       x = "Weekday",
       y = "Average Ride Duration",
       fill = "Casual Riders 
       vs Annual Members",
       caption = "Casual riders tend to ride the bikes longer than annual members. People like shorter rides mid-week.")



#This shows the favourite week for taking the bikes between casual and annual members
"yearTCWcount" <- `yearTC` %>%
  group_by(member_casual, week_no)%>%
  summarise(number_of_rides = n(),average_duration = mean(time))%>% # calculates the average duration and number of rides
  arrange(member_casual,week_no)

# Let's visualize the number of rides by rider type
`yearTCWcount` %>% #Its easy to see that everyone prefers to ride in the summer rather then winter, however annual members are more likely to use the bikes in the winter
  ggplot(aes(x = week_no, y = number_of_rides, fill = member_casual)) +
  geom_col(position = "dodge") +
  labs(title = "Number of Rides vs Week", 
       subtitle = "Comparing Casual Riders with Annual Members",
       x = "Week number",
       y = "Average Number of Rides",
       fill = "Casual Riders 
       vs Annual Members",
       caption = "Everyone prefers to ride the bikes during the summer months. Casual riders are effected more.")

# Let's create a visualization for average duration
`yearTCWcount` %>% #This doesn't tell us anything other then casual members like to use the bikes longer, everyone seems to use the bikes the same amount of time regardless of whether it is cold or not.
  ggplot(aes(x = week_no, y = average_duration, fill = member_casual)) +
  geom_col(position = "dodge") + 
  labs(title = "Ride Duration vs Week", 
       subtitle = "Comparing Casual Riders with Annual Members",
       x = "Week number",
       y = "Average Ride Length",
       fill = "Casual Riders 
       vs Annual Members",
       caption = "Annual members appear to only be slightly effected by season, whereas casual riders ride longer in the summer.")


#Lets have a look at how people use the stations

"yearN" <- `year` %>%
  add_count(start_station_name, name = "start_station_count")%>%
  add_count(end_station_name, name = "end_station_count")

#counting how many times the stations are used so we can look at return trips vs non-return.
#Can look to see how many times people have regular journeys, casual vs member bikes
#This data could be completely skeptical since it's created with the assumption that stations have equal amounts of usage due to return journeys taking place. This could also be a fluke and we can't know for sure without customer id numbers. But it could get the board interested in investigating further.
"yearNret" <- `yearN` %>% #We can clearly see that everyone prefers the electric bike over the classic when taking return journeys.
  filter(rideable_type != "docked_bike") %>%
  filter(start_station_count == end_station_count) %>% #To look at people taking the bikes from one station, dropping them off at another and then coming back again on a return journey
  count(member_casual, rideable_type, name = "count") %>%
  mutate("perc" = `count` / sum(`count`)) %>%
  mutate("ID" = "Return")


"yearNnonret" <- `yearN` %>% #This is surprising, members are more likely to take non-return trips compared with casual members and everyone prefers to use the classic bike on non-return trips
  filter(rideable_type != "docked_bike") %>%
  filter(start_station_count != end_station_count) %>% #To look at people taking the bikes from one station, dropping them off at another and then coming back again on a return journey
  count(member_casual, rideable_type, name = "count") %>%
  mutate("perc" = `count` / sum(`count`))%>%
  mutate("ID" = "Non-Return")


#Combining tables of return bikes to the same station and non-return bikes.
yearNret %>%
  bind_rows(yearNnonret)%>% 
  #unite("ride_member", member_casual:rideable_type) %>%
  ggplot(aes(x=rideable_type, y= perc)) + 
  facet_wrap(~ ID, scales = "free_x") + 
  geom_col(position = "dodge", aes(fill = member_casual)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(title = "Returning vs Non-Returning", 
       subtitle = "Looking at stations that are equally used for pick-up and drop off",
       x = "Bike Type",
       y = "Percentage of Overall Rides",
       fill = "Casual Riders 
       vs Annual Members",
       caption = "Casual riders are more likely to make return trips. Everyone prefers the classic bike.")
  


#Are casual members more likely to return a bike to the same station? This done with the assumption that casual members may be tourists
"SSpercentage" <- `yearN` %>% #This shows the how often each member type return to the same station. Here we can see casual members are more likely to return the bikes to the same station which was expected.
  filter(start_station_id == end_station_id) %>% #More than 10% of casual members return to the same station whereas less then 5% of annual members do the same
  count(member_casual,name="equal_station_count") %>%
  full_join(count(`yearN`,member_casual, name="total_count"))%>%
  mutate("return_percentage" = equal_station_count / total_count)

#`SSpercentage` %>%
#  ggplot(aes(x = member_casual, y = return_percentage, fill = member_casual)) +
#  geom_col(position = "dodge")

#Lets have a look at how long each member is using bikes that they return to the same station
yearT$time <- as.numeric(as.character(yearT$time))

"SSduration" <- `yearN` %>%
  full_join(yearT) %>% #This shows that casual members are more likely to use a bike to and from the same station for at least over an hour, perhaps tourists. Whereas members are probably more likely to use it to pick something up from the shop or use it for exercise.
  filter(start_station_id == end_station_id) %>%
  group_by(member_casual) %>%
  summarise(duration = mean(time, na.rm = TRUE))#The NA's is the filter I applied earlier for anyone renting a bike for less than 5 minutes
  
#`SSduration` %>%
#  ggplot(aes(x = member_casual, y = duration, fill = member_casual)) +
#  geom_col(position = "dodge")

#This chart originally consisted of two charts which I put into one. This shows us the average duration and the percentage of rides that are returned to the same station for both groups.
`SSpercentage` %>% #As you can see, not only do the casual riders bring the bikes back to the same station more than the annual members, they also tend to ride the bikes for longer.
  select(member_casual, return_percentage) %>% #Perhaps this is due to them being tourists. The annual members are more likely to use the bike for quick excercise or trips to the shops.
  full_join(`SSduration`) %>% 
  group_by(member_casual) %>%
  ggplot(aes(x = duration, y = return_percentage, label = member_casual)) +
  geom_rect(aes(xmin = 0, ymin = 0, xmax = duration, ymax = return_percentage, fill = member_casual)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
#  geom_text(hjust=4, vjust=7, nudge_x = 8, nudge_y = 0.5, size=5) +
  geom_label( size=5, nudge_x = -14, nudge_y = -0.017 ) +
#  geom_text(position = position_stack(vjust = 0.5),position_stack(hjust = 0.5)) +
  theme_minimal() +
  theme(legend.position="none") + #Get rid of legend (key)
  labs(title = "Same Station Return", 
     subtitle = "Showing how many rides get returned to the same station vs the length of those rides",
     x = "Average Ride duration for trips to and from the same station",
     y = "Percentage of overall rides",
     fill = "Casual Riders 
       vs Annual Members",
     caption = "Casual riders are likely to return a bike to the same station. Annual members use it for a shorter time.")
  

colnames(yearT)
#library(ggrepel)
library(sf)
chi_map <- read_sf("https://raw.githubusercontent.com/thisisdaryn/data/master/geo/chicago/Comm_Areas.geojson") 

#"yearTFC" <- `yearT` %>%
#  filter(start_lng != end_lng) %>%
#  filter(start_lat != end_lat) %>%
#  filter(member_casual == "casual") %>%
##  count(start_lat,start_lng,end_lat,end_lng)
#  count(start_station_name,start_lat,start_lng,end_station_name,end_lat,end_lng)
##  group_by(start_station_name, end_station_name) %>%
#  tally()

#"yearTFM" <- `yearT` %>%
#  filter(start_lng != end_lng) %>%
#  filter(start_lat != end_lat) %>%
#  filter(member_casual == "member") %>%
#  count(start_station_name,start_lat,start_lng,end_station_name,end_lat,end_lng)

#"yearTF" <- `yearT` %>%
#  filter(start_lng != end_lng) %>%
#  filter(start_lat != end_lat) %>%
#  count(member_casual, start_station_name,start_lat,start_lng,end_station_name,end_lat,end_lng)

#glimpse(yearTFC)

#count(yearTF,start_lng != end_lng)

#ggplot() + 
#  geom_sf(data = chi_map) +
#  geom_curve(data=yearTFC,
#             aes(x=start_lng, y=start_lat, xend=end_lng, yend=end_lat),
#             col="#00008b",
#             size=.5,
#             curvature=0.3) +
#  geom_point(data=yearTFC,
#             aes(x=start_lng, y=start_lat), 
#             colour="blue",
#             size=1.5) +
#  geom_point(data=yearTFC,
#             aes(x=end_lng, y=end_lat), 
#             colour="blue") +
#  theme(axis.line=element_blank(),
#        axis.text.x=element_blank(),
#        axis.text.y=element_blank(),
#        axis.title.x=element_blank(),
#        axis.title.y=element_blank(),
#        axis.ticks=element_blank(),
#        plot.title=element_text(hjust=0.5, size=12)) +
#  ggtitle("Casual Member bike usage")



#ggplot() + 
#  geom_sf(data = chi_map) +
# geom_curve(data=yearTFC,
#             aes(x=start_lng, y=start_lat, xend=end_lng, yend=end_lat),
#             col="#00008b",
#             size=.5,
#             curvature=0.2) +
#  geom_point(data=yearTFC,
#             aes(x=start_lng, y=start_lat), 
#             colour="blue",
#             size=1.5) +
#  geom_point(data=yearTFC,
#             aes(x=end_lng, y=end_lat), 
#             colour="blue") +
#  theme(axis.line=element_blank(),
#        axis.text.x=element_blank(),
#        axis.text.y=element_blank(),
#        axis.title.x=element_blank(),
#        axis.title.y=element_blank(),
#        axis.ticks=element_blank(),
#        plot.title=element_text(hjust=0.5, size=12)) +
#  ggtitle("Migration Map: Three Generations of Adult Decendants of Walter F. Herget, Pekin, IL")


"yearTFStartM" <- `yearT` %>%
  filter(start_lng != end_lng) %>%
  filter(start_lat != end_lat) %>%
  filter(member_casual == "member") %>%
  count("station_name" = start_station_name, name = "m_start_count")

"yearTFEndM" <- `yearT` %>%
  filter(start_lng != end_lng) %>%
  filter(start_lat != end_lat) %>%
  filter(member_casual == "member") %>%
  count("station_name" = end_station_name, name = "m_end_count")

"yearTFStartC" <- `yearT` %>%
  filter(start_lng != end_lng) %>%
  filter(start_lat != end_lat) %>%
  filter(member_casual == "casual") %>%
  count("station_name" = start_station_name, name = "c_start_count")

"yearTFEndC" <- `yearT` %>%
  filter(start_lng != end_lng) %>%
  filter(start_lat != end_lat) %>%
  filter(member_casual == "casual") %>%
  count("station_name" = end_station_name, name = "c_end_count")

"yearTcoordS" <- `yearT` %>%
  select(start_station_name,start_lat,start_lng) %>%
  group_by(start_station_name) %>%
  summarize(lat = median(start_lat), lng = median(start_lng)) 

"yearTcoordE" <- `yearT` %>% #It was decided to use this since typos were found in yearTcoords and the difference between them both is 3 rows
  select(end_station_name,end_lat,end_lng) %>%
  group_by(end_station_name) %>%
  summarize(lat = median(end_lat), lng = median(end_lng)) %>%
  rename(station_name = end_station_name)

"yearTF" <- `yearTFStartC` %>%
  full_join(`yearTFEndC`, by = "station_name") %>%
  full_join(`yearTFStartM`, by = "station_name") %>%
  full_join(`yearTFEndM`, by = "station_name") %>%
  mutate(c_count = c_start_count + c_end_count) %>%
  mutate(m_count = m_start_count + m_end_count) %>%
  select(station_name,c_count,m_count) %>%
  left_join(yearTcoordE,by = "station_name")

# library(scales)

ggplot() + 
  geom_sf(data = chi_map) +
  geom_point(data=yearTF,
             aes(x=lng, y=lat, 
                 size=c_count), 
             colour="blue") +
  geom_point(data=yearTF,
             aes(x=lng, y=lat,
                 size = m_count),
             colour="yellow",
             alpha = 0.5) +
  theme_minimal() +  
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        axis.ticks=element_blank(),
        plot.title=element_text(hjust=0.5, size=12)) +
  ggtitle("Map of Annual Members station usage in yellow, imposed over Casual Riders in blue") +
  labs(
       size = "Usage over 
       12 months")

  
  ggplot() + 
    geom_sf(data = chi_map) +
    geom_point(data=yearTF,
                aes(x=lng, y=lat,
                    size = m_count,
                    alpha = 0.5),
                colour="yellow") +
    geom_point(data=yearTF,
                aes(x=lng, y=lat, 
                    size=c_count), 
                colour="blue") +
    theme_minimal() +
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        axis.ticks=element_blank(),
        plot.title=element_text(hjust=0.5, size=12)) +
    ggtitle("Migration Map: Three Generations of Adult Decendants of Walter F. Herget, Pekin, IL") +
  labs(subtitle = "Showing how many rides get returned to the same station vs the length of those rides",
       x = "Average Ride duration for trips to and from the same station",
       y = "Percentage of overall rides",
       fill = "Casual Riders 
       vs Annual Members",
       caption = "Casual riders are likely to return a bike to the same station. Annual members use it for a shorter time.")
  

library(rmarkdown)
  
 # Wrap up
"CRiders" <- c("Casual Riders","- Bike ride duration is longer", "- Prefer riding on the weekend","- Prefer to ride in the summer","- More likely to go on return trips", "- At 10%, they are twice as likely to drop a bike off from where they got it from", "- They take double the amount of time to use a bike when they drop it off from the same place they got it","- They are more likely to rent a bike along the lake"  ) 
"AMembers" <- c("vs Annual Members", "- Prefer riding mid-week", "- Ride for slightly long on weekend", "- Use bikes more in summer but use them more in winter than casual riders","- Less likely to take return trips" , "- At 5%, annual members are less likely to drop their bike off where they got it from", "- They take half the time to use a bike when they drop a bike off in the same place they got it", "- More likely to rent a bike outside of the city center")

"CvsA" <- data.frame(CRiders, AMembers)
  

