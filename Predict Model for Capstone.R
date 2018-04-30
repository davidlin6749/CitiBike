library(geosphere)
library(magrittr)
library(dplyr)
#creates distance in miles  column HOLY took 30 - 45 minutes to RUN!!!! try not to run again..use 
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
totalrainamt = rawWG$forecast$simpleforecast$forecastday$qpf_allday$mm
totalrainamt = as.numeric(totalrainamt)
sumofrain= sum(totalrainamt)
todayweather= rawWG$forecast$txt_forecast$forecastday$fcttext

#Model to predict speed
predictTime = function(ss, es) {
  
  
  indices<- which(nroute$Start_Station_ID==ss & nroute$End_Station_ID==es)
  
  data_subset<- nroute[indices,]
  ti= data_subset$Time
  tem = avgTemp
  spe = data_subset$median_speed
  di = data_subset$DIS
  wi = avgwind
  tr = sumofrain
  tedf= data.frame(Time=c(ti),TEMP = c(tem),median_speed=c(spe),DIS=c(di), WDSP=c(wi), PRCP=c(tr))
  newtime = predict(model5,newdata=tedf)
  
  #round all number to 2 decimal places
  is.num<- sapply(newtime, is.numeric)
  newtime[is.num]<- lapply(newtime[is.num],round,2)
  
  print(paste("Morning Weather:",todayweather[1]))
  print(paste("Evening Weather:",todayweather[2]))
  print(paste(newtime[1],"Minutes"))
  print(paste("Start Station =",data_subset$Start_Station_Name[1]))
  print(paste("End Station =",data_subset$End_Station_Name[1]))
  print(paste(data_subset$DIS[1],"Miles"))
}

#testing speed. 
predictTime(3002,3541)

predictTime(3002,498)

predictTime(72,498)

predictTime(72,3002)







