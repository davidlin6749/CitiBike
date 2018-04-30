



library(plyr)
library(ggplot2)
library(reshape2)
colnames(data)
#turn trip duration seconds to hours
aa<- (data$Trip_Duration/3600)
head(aa)
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
