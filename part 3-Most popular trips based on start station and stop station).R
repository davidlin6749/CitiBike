library(data.table)
library(ggplot2)
library(dplyr)

#Works to create table

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
  

