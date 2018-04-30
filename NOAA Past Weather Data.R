cleanweather2<- read.table("C:/Users/DavidLin/Desktop/13296198.csv",header = TRUE,sep=",")
summary(cleanweather2)
colnames(cleanweather2)
colnames(nroute)

cleanweather2$Start_Date<- as.Date(cleanweather2$Start_Date, format ="%Y-%m-%d")

#joined both dataframe. Using date as the main factor. 
library(data.table)
setDT(nroute)
setDT(cleanweather2)

nroute<-nroute[cleanweather2, on=c('Start_Date')]
summary(nroute)
summary(cleanweather)
class(cleanweather$MAX)
class(nroute$PRCP)






