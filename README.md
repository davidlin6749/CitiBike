# CitiBike

# DevUp Program Data Science -Capstone project – Citi Bike

One Paragraph of project description goes here

## Data Sources:

Citi Bike data - https://www.citibikenyc.com/system-data <br />
  -Provided all primary Bike data<br />
  Weather Undergound - https://www.wunderground.com/weather/api/<br />
  -Provided current weather data<br />
  NOAA - https://www7.ncdc.noaa.gov/CDO/dataproduct<br />
	-Provided weather history data<br />


### Aims:

Use Citi Bike data to visualize:
1)	Top 5 stations with the most starts (showing # of starts)
2)	Trip duration by user type
3)	Most popular trips based on start station and stop station)
4)	Rider performance by Gender and Age based on average trip distance (station to station), median speed (trip duration / distance traveled)
5)	What is the busiest bike in NYC in 2017? How many times was it used? How many minutes was it in use?

Create a model that can predict how long a trip will take given a starting point and destination given weather status.


### Data Importing:

Citi Bike split 2017 data into 12 csv files. One for each month. Downloaded all 12 files and into a folder.<br />
  Combines all 12 csv into one data.frame <br />
  Used Fread and took about 4-5 minutes to complete this process <br />
  Total rows of data was about 16 million <br />
  Total columns was 14   <br />


```
# Load library
library(data.table)

# Get a List of all files named with a key word, say all `.csv` files
filenames <- list.files("C:/Users/DavidLin/Desktop/201702-citibike-tripdata.csv", full.names=TRUE)

# Load and bind all data sets
data <- rbindlist(lapply(filenames,fread))
```

## Data Cleaning

Change all column names without spaces. <br />
Change Birth year from Character to Integer. <br />
Quickly remove all rows that contains a NA value. <br />
Remove all unknown in User_type  <br />
Split Starting Time to date and time. <br />
Split Ending time to date and time. <br />
Change both start dates to end dates to date formats. <br />


Notes: There are other data cleaning later on during data visualization. Ex. Removing unknowns genders, having age limits, speed limits… 


```
#change col names remove spaces
colnames(data)[1] <- "Trip_Duration"
colnames(data)[2] <- "Starting_Time"
colnames(data)[3] <- "Stop_Time"
colnames(data)[4] <- "Start_Station_ID"
colnames(data)[5] <- "Start_Station_Name"
colnames(data)[6] <- "Start_Station_Latitude"
colnames(data)[7] <- "Start_Station_Longitude"
colnames(data)[8] <- "End_Station_ID"
colnames(data)[9] <- "End_Station_Name"
colnames(data)[10] <- "End_Station_Latitude"
colnames(data)[11] <- "End_Station_Longitude"
colnames(data)[12] <- "Bike_ID"
colnames(data)[13] <- "User_Type"
colnames(data)[14] <- "Birth_Year"

#change Birth Year to Interger
data$Birth_Year<-as.integer(data$Birth_Year)

#remove all NA 
library(tidyr)
data<-data %>% drop_na()
data<-data[!(is.na(data$User_Type) | data$User_Type==""), ]

library(stringr)
#cutting Start time to two new colomns for Start Date and Start Time
data$Start_Date <- sub("^(.*?) .*", "\\1", data$Starting_Time)
data$Start_Time <- str_sub(data$Starting_Time, start=-8)
#Same with Stop Time
data$End_Date <- sub("^(.*?) .*", "\\1", data$Stop_Time)
data$End_Time <- str_sub(data$Stop_Time, start=-8)

library(date)
data$Start_Date=as.Date(data$Start_Date)
data$End_Date=as.Date(data$End_Date)
```

# Visualizations

Create Visualizations using the Citi Bike data

## 1)	Top 5 stations with the most starts (showing # of starts)

Create two bar charts. Listing the top 5 Start and End stations names with the most number of usage.
Pershing Square North is the most popular station of all.
```
#create subset to show only the top 5 start stations
david<- sort(table(data$Start_Station_Name),decreasing =TRUE)[1:5]

# Find a range of y's that'll leave sufficient space above the tallest bar
ylim <- c(0, 1.1*max(david))


# Plot, and store x-coordinates of bars in xx
part1 <- barplot(sort(table(data$Start_Station_Name),decreasing =TRUE)[1:5], xlab = 'Station Name',  ylim = ylim,
              main = "Top 5 Start Stations with Most Starts", 
              ylab = "Frequency")
# Add text at top of bars
text(x = part1, y = david, label = david, pos = 3, cex = 0.8, col = "red")


#subset to show only the top 5 end stations
david2<- sort(table(data$End_Station_Name),decreasing =TRUE)[1:5]

# Find a range of y's that'll leave sufficient space above the tallest bar
ylim <- c(0, 1.1*max(david2))


#Plot, and store x-coordinates of bars in xx
part1 <- barplot(sort(table(data$End_Station_Name),decreasing =TRUE)[1:5], xlab = 'Station Name',  ylim = ylim,
                 main = "Top 5 End Stations with Most Destination", 
                 ylab = "Frequency")
# Add text at top of bars
text(x = part1, y = david2, label = david, pos = 3, cex = 0.8, col = "red")
```
## 2)	Trip duration by user type

Turn Trip_Duration into hours. Round to only 2 decimal places. Plot bar chart using ggplot

```
library(plyr)
library(ggplot2)
library(reshape2)
colnames(data)
#turn trip duration seconds to hours
aa<- (data$Trip_Duration/3600)
userType<- data$User_Type

#read the number with out sci  notation
options(scipen=999)
#aggregates the data
df2<- aggregate(aa~userType,data,sum)
#round all number to 2 decimal places
is.num<- sapply(df2, is.numeric)
df2[is.num]<- lapply(df2[is.num],round,2)
#plots the bar graph
part2<-ggplot(df2,aes(x=userType,y=aa))+geom_bar(stat="identity")+
  xlab("Customer Type")+ylab("Number of Hours")+ggtitle("Trip Duration by Customers")
#labels the value
part2+geom_text(aes(label=aa),position=position_dodge(.9),vjust=-0.5)
```
## 3) Most popular trips based on start station and stop station)

Find all combinations in Start and End Stations. <br /> 
Create new column to show number of times that specific trip was used. <br />
Used ggplot to show which route was most popular. <br />

```
library(data.table)
library(ggplot2)
library(dplyr)

#create new data.frame called nroute to get all possible combinations 

  nroute <- data%>%  
    group_by(Start_Station_Name,End_Station_Name) %>% 
    summarise(nroute=n()) %>% 
    arrange(desc(nroute)) 
  
#grab the top 5
  Top5routes<-nroute[1:5,]
  Top5routes$Start_End_Station

#create new coln for x axis
  Top5routes$Start_End_Station<-paste(Top5routes$Start_Station_Name,Top5routes$`End_Station_Name`,sep="     ---     ")
#for y axis
  numroute<-Top5routes$nroute
#plot graph
  part3f<-ggplot(Top5routes,aes(x=Start_End_Station,y=numroute))+geom_bar(stat="identity")+
    xlab("Start & End Station")+ylab("Number of times route used")+ggtitle("Most Popular Citi Bike Routes")
#labels the value
  part3f+geom_text(aes(label=numroute),position=position_dodge(.9),vjust=-0.5)
  
```
## 4)	Rider performance by Gender and Age based on average trip distance (station to station), median speed (trip duration / distance traveled)

Used distHaversine formula from geosphere package to calculate distance using log and lat for start and end stations. <br />
Distance is in meters. Changed to Miles <br />
Create age column using the year born. <br />
Creating new dataframe using summarise to show only columns that I need. <br />
Change 1 to males and 0 to females <br />
Citi Bike age limit is at least 16 years old to ride Bike. Removed all ages under 16. No one over 100 years of age. <br />
Create a interactive scatter plot using plotly. <br />
Hover mouse to a certain bubble to show age, trip distance, speed, gender. <br />


```
#1
library(dplyr)
library(ggplot2)
library(geosphere)
library(magrittr)
#
#creates distance in miles  column HOLY took 30 - 45 minutes to RUN!!!! try not to run again.. Use file  called part 4 data v2.RData
df3<- data %>% rowwise()%>% mutate(DIS=distHaversine(c(Start_Station_Longitude,`Start_Station_Latitude`),c(`End_Station_Longitude`,`End_Station_Latitude`))/1609)
#step 2
#creates age column to show age
df3$Age<- +(2018-df3$Birth_Year)
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
```
## 5)	What is the busiest bike in NYC in 2017? How many times was it used? How many minutes was it in use?

Create Bike datafram to show only Bike Id, Trip Duration in minutes, and number of uses. <br />
Round all number to 2 decimal places <br />
Used plotly for interactive scatterplot. <br />
hover to see Bike id, Total Trip Duration in minutes, and total number of uses. <br />

```
#create bike dataframe to show trip duration in minutes and number of times.
bike <- data%>% 
  group_by(Bike_ID) %>% 
  summarise(
    sum_duration=(sum(Trip_Duration/60)),  
    count=n())
	
#round all number to 2 decimal places
is.num<- sapply(bike, is.numeric)
bike[is.num]<- lapply(bike[is.num],round,2)

library(plotly)
Bow <- plot_ly(bike, x =bike$sum_duration , y =bike$count, type = 'scatter',
               mode = 'markers', size = bike$count, color = bike$count, 
               colors = 'Paired',
               sizes = c(10, 50),
               marker = list(opacity = 0.5, sizemode = 'diameter'),
               hoverinfo = 'text',
               text = ~paste('Bike ID:', bike$Bike_ID, '<br>Sum Trip Duration:', bike$sum_duration,
                             
                             '<br>Count:', bike$count)) %>%
  layout(title = 'Most Popular Bike and Most Used Run Time Bike',
         xaxis = list(title = 'Usage in (Minutes)',range = c(), showgrid = FALSE),
         yaxis = list(title = 'Number of times',range = c(), showgrid = FALSE),
         showlegend = TRUE)
(Bow)
```

### Model that can predict how long a trip will take given a starting point and destination given weather status.

Using temperature and wind speed as my variable to test see how long a trip would take.

## Gather past weather data
I gather data from NOAA for past weather data from 2017/01/01 to 2018/01/01 <br />
change YEARMODA from character to Date. <br />
Only used Mean Temperature and Wind Speed as the variables <br />
Removed all non used columns. Most of data was missing and incomplete. <br />
Precipitation data was incorrect. I checked with WeatherUnderground. 2017/01/04 -On WU there was 0 inch of rain, but NOAA had reported .49 inch.<br />
Joined both dataframs using Start_Date as the main element.<br />

```
cleanweather<- read.table("https://www1.ncdc.noaa.gov/pub/orders/CDO3355787625108.txt",header = TRUE,sep=",")

#create new column to change int YEARMODA to Date in x 
Start_Date<- cleanweather$YEARMODA
cleanweather<- transform(cleanweather, Start_Date=as.Date(as.character(Start_Date),"%Y%m%d"))

#remove non used columns
cleanweather=subset(cleanweather,select = -c(STN...,WBAN,YEARMODA,X,DEWP,X.1,SLP,X.2,STP,X.3,VISIB,X.4,X.5,MXSPD,GUST,SNDP,FRSHTT,X.6,MIN,MAX,PRCP))

#joined both dataframe. Using date as the main factor. 
library(data.table)
setDT(df3)
setDT(cleanweather)

nroute<-nroute[cleanweather, on=c('Start_Date')]
```
## Training and Testing Model and selecting Model

Split dataframe in to 50/50.
Created 4 test modles
Best model was Model 4.
tested with a 98% correlation.
Compared predicted to acutals - very accurate

```
library(car)
#random sequence for obtaining the training set and test set
#split 50/50 for testing and training
set.seed(18494)
sample = sample.int(n=nrow(nroute),size=floor(.5*nrow(nroute)),replace = F)

train = nroute[sample,]
test = nroute[-sample,]

#first model using two variables
model1 = lm(speed ~ temp, data = train)

#second model using more variables
model2 = lm(Time ~ TEMP + median_speed + DIS, data=train)
#more training
model3 = lm(Time ~ TEMP + speed, data=train)
model4 = lm(Time ~ TEMP + median_speed + DIS + WDSP, data=train)

#testing the trained model
pred<- predict(model2,test)
pred2<- predict(model3,test)
pred3<- predict(model4,test)

#model 2 works the best. Highly accurate compare to predict to actuals
actuals_preds<-data.frame(cbind(actuals=test$Time,predicteds = pred))
actuals_preds2<-data.frame(cbind(actuals=test$Time,predicteds = pred2))
actuals_preds3<-data.frame(cbind(actuals=test$Time,predicteds = pred3))


correlation_accuracy<- cor(actuals_preds)
correlation_accuracy2<- cor(actuals_preds2)
correlation_accuracy3<- cor(actuals_preds3)
#98% really good. for model 3
correlation_accuracy
correlation_accuracy2
correlation_accuracy3

head(actuals_preds)
head(actuals_preds2)
head(actuals_preds3)

#min to max accuracy - 89% - higher the better
min_max_accuracy<- mean(apply(actuals_preds3,1,min)/apply(actuals_preds3,1,max))
#12% mean absolute percentage deviation - lower the better
mape<- mean(abs((actuals_preds3$predicteds - actuals_preds3$actuals))/actuals_preds3$actuals)

```
## Using the Model

Used the first couple code from Part 4. 
Created new colomn for time. used speed and distance to find time taken.
Round all number to 2 decimal places and drop all NA data
Remove all speed that is higher than 25 mph and lower than 5mph
Called Weather Underground API to get todays weather for NYC.
Created Function to Predict new time. Input Start station ID and End Station ID.
It will use Model4 and calculate new time based on todays weather.

```
library(geosphere)
library(magrittr)
library(dplyr)

#creates distance in miles  column HOLY took 30 - 45 minutes to RUN!!!! try not to run again..
df3<- data %>% rowwise()%>% mutate(DIS=distHaversine(c(Start_Station_Longitude,`Start_Station_Latitude`),c(`End_Station_Longitude`,`End_Station_Latitude`))/1609)

#create DataFrame for predicting how long each station to station. Distance,Median Speed..
df4<-ungroup(df3)
nroute <- df4%>%  
  group_by(Start_Station_ID,Start_Station_Name,End_Station_ID,End_Station_Name,DIS,Start_Date) %>% 
  summarise(nroute=n(),median_speed=median(DIS*60*60/Trip_Duration))

# calculate time taken for route
nroute$Time= +60*(nroute$DIS/nroute$median_speed)

#round all number to 2 decimal places
is.num<- sapply(nroute, is.numeric)
nroute[is.num]<- lapply(nroute[is.num],round,2)

#drop all NA data
library(tidyr)
nroute %>% drop_na()

#remove all median speed that is over 25 mph or equal to 0. Removing all speed less than 5 mph
nroute<- nroute[!(nroute$median_speed>=25),]
nroute<- nroute[!(nroute$median_speed==0),]
nroute<- nroute[!(nroute$median_speed<=5),]

#library to get current data from Weather Underground
library(RCurl)
library(jsonlite)
#get todays weather
myWunderGroundAPI= "80760560a566278c"
urlWeatherWG = paste0("http://api.wunderground.com/api/", myWunderGroundAPI, "/forecast/q/NY/New_York_City.json")
webWG = getURL(urlWeatherWG)
rawWG = fromJSON(webWG)
#grab data from weather underfround
tempH = rawWG$forecast$simpleforecast$forecastday$high$fahrenheit[[1]]
tempH = as.numeric(tempH)
tempL = rawWG$forecast$simpleforecast$forecastday$low$fahrenheit[[1]]
tempL = as.numeric(tempL)
avgTemp = (sum(tempH+tempL)/2)
wind= rawWG$forecast$simpleforecast$forecastday$avewind$mph
avgwind= mean(wind)

#Other data to grab, but coundnt use past history data for others
# todayWeather = rawWG$forecast$txt_forecast$forecastday$fcttext
# rainchanceday = rawWG$forecast$simpleforecast$forecastday$pop[[1]]
# rainchanceday = as.numeric(rainchanceday)
# totalrainamt = rawWG$forecast$simpleforecast$forecastday$qpf_allday$mm
# totalrainamt = as.numeric(totalrainamt)
# sumofrain= sum(totalrainamt)

# avgwind = 1
# rainchanceday = 70

#Model to predict speed
predictTime = function(ss, es) {
  
  
  
  indices<- which(nroute$Start_Station_ID==ss & nroute$End_Station_ID==es)
  
  data_subset<- nroute[indices,]
  ti= data_subset$Time
  tem = avgTemp
  spe = data_subset$median_speed
  di = data_subset$DIS
  wi = avgwind
  tedf= data.frame(Time=c(ti),TEMP = c(tem),median_speed=c(spe),DIS=c(di), WDSP=c(wi))
  newtime = predict(model4,newdata=tedf)
  
  #round all number to 2 decimal places
  is.num<- sapply(newtime, is.numeric)
  newtime[is.num]<- lapply(newtime[is.num],round,2)
 
  #print(list(newtime,data_subset$Start_Station_Name,data_subset$End_Station_Name, data_subset$DIS))
  
 
  print(paste(newtime[1],"Minutes"))
  print(paste("Start Station =",data_subset$Start_Station_Name[1]))
  print(paste("End Station =",data_subset$End_Station_Name[1]))
  print(paste(data_subset$DIS[1],"Miles"))
}

#testing speed. 
predictTime(3002,3541)

predictTime(72,498)

```

## Conclusion

Thanks!

## Acknowledgments

* Huge Thanks to Stack Overflow!!!



