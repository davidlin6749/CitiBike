#Rider performance by Gender and Age based on avg trip distance (station to station), median speed (trip duration / distance traveled)




#1
library(dplyr)
library(ggplot2)
library(geosphere)
library(magrittr)
#step 1
#creates distance in miles  column HOLY took 30 - 45 minutes to RUN!!!! try not to run again..
df3<- data %>% rowwise()%>% mutate(DIS=distHaversine(c(Start_Station_Longitude,`Start_Station_Latitude`),c(`End_Station_Longitude`,`End_Station_Latitude`))/1609)
#step 2
#creats age column
df3$Age<- +(2018-df3$Birth_Year)
#step 3
#groups the ages. RUN TIME IS HUGE 30-45 minutes
#df4<- df3 %>% mutate(age=cut(`Birth_Year`, breaks=seq(1917,2018, by=5), right = TRUE, labels = seq(100,5,by=-5)))

#step 1-3 use part4v2 data. Instead of waiting more than 45 minutes creating data.

#3
df6<-ungroup(df3)
#4
df_cust <- df6 %>% 
  group_by(User_Type, Age, Gender) %>% 
  summarise(median_speed=median(DIS*60*60/Trip_Duration),
            median_sum_duration=median(sum(Trip_Duration/360)),
            avg_trip_distance=sum(DIS)/n(), 
            count=n())

df_cust$Gender <-  gsub("1", 'Male',df_cust$Gender) 
df_cust$Gender <- gsub("2", 'Female',df_cust$Gender) 


#subset all unknown genders, remove them
df_cust<-df_cust[df_cust$Gender !=0,]
#subset all ages. No age over 100 and under 16
df_cust<- df_cust[!(df_cust$Age>=100),]
df_cust<- df_cust[!(df_cust$Age<=16),]

library(tidyr)
df_cust<-df_cust %>% drop_na()
#round all number to 2 decimal places
is.num<- sapply(df_cust, is.numeric)
df_cust[is.num]<- lapply(df_cust[is.num],round,2)
#5
library(plotly)
po <- plot_ly(df_cust, x =df_cust$avg_trip_distance , y =df_cust$median_speed, type = 'scatter',
              mode = 'markers', size = df_cust$count, color = df_cust$Gender, 
              colors = 'Paired',
              sizes = c(10, 50),
              marker = list(opacity = 0.5, sizemode = 'diameter'),
              hoverinfo = 'text',
              text = ~paste('Age:', df_cust$Age, '<br>avg_trip distance:', df_cust$avg_trip_distance,
                            '<br>Median Speed:', df_cust$median_speed, '<br>Gender:', df_cust$Gender, 
                            '<br>Count:', df_cust$count)) %>%
  layout(title = 'Riders Performance',
         xaxis = list(title = 'Average Miles Per Trip',range = c(0.1, 1.5), showgrid = FALSE),
         yaxis = list(title = 'Speed(MPH)',range = c(0.1, 6.5), showgrid = FALSE),
         showlegend = TRUE)
(po)









