library(car)
#random sequence for obtaining the training set and test set
#split 50/50 for testing and training
set.seed(18494)
sample = sample.int(n=nrow(nroute),size=floor(.5*nrow(nroute)),replace = F)

train = nroute[sample,]
test = nroute[-sample,]
colnames(train)
#first model using two variables
model1 = lm(Time ~ TEMP, data = train)
#second model using more variables
model2 = lm(Time ~ TEMP + median_speed + DIS, data=train)
#more training
model3 = lm(Time ~ TEMP + median_speed, data=train)
model4 = lm(Time ~ TEMP + median_speed + DIS + WDSP, data=train)
model5 = lm(Time ~ TEMP + median_speed + DIS + WDSP + PRCP, data=train)

#testing the trained model
pred1<- predict(model1,test)
pred2<- predict(model2,test)
pred3<- predict(model3,test)
pred4<- predict(model4,test)
pred5<- predict(model5,test)

#model 2 works the best. Highly accurate compare to predict to actuals
actuals_preds1<-data.frame(cbind(actuals=test$Time,predicteds = pred1))
actuals_preds2<-data.frame(cbind(actuals=test$Time,predicteds = pred2))
actuals_preds3<-data.frame(cbind(actuals=test$Time,predicteds = pred3))
actuals_preds4<-data.frame(cbind(actuals=test$Time,predicteds = pred4))
actuals_preds5<-data.frame(cbind(actuals=test$Time,predicteds = pred5))


correlation_accuracy1<- cor(actuals_preds1)
correlation_accuracy2<- cor(actuals_preds2)
correlation_accuracy3<- cor(actuals_preds3)
correlation_accuracy4<- cor(actuals_preds4)
correlation_accuracy5<- cor(actuals_preds5)

#98% really good. for model 4 and 5
correlation_accuracy1
correlation_accuracy2
correlation_accuracy3
correlation_accuracy4
correlation_accuracy5

head(actuals_preds1)
head(actuals_preds2)
head(actuals_preds3)
head(actuals_preds4)
head(actuals_preds5)

#min to max accuracy - 89%
min_max_accuracy1<- mean(apply(actuals_preds1,1,min)/apply(actuals_preds1,1,max))
min_max_accuracy2<- mean(apply(actuals_preds2,1,min)/apply(actuals_preds2,1,max))
min_max_accuracy3<- mean(apply(actuals_preds3,1,min)/apply(actuals_preds3,1,max))
min_max_accuracy4<- mean(apply(actuals_preds4,1,min)/apply(actuals_preds4,1,max))
min_max_accuracy5<- mean(apply(actuals_preds5,1,min)/apply(actuals_preds5,1,max))
#12% mean absolute percentage deviation
mape1<- mean(abs((actuals_preds1$predicteds - actuals_preds1$actuals))/actuals_preds1$actuals)
mape2<- mean(abs((actuals_preds2$predicteds - actuals_preds2$actuals))/actuals_preds2$actuals)
mape3<- mean(abs((actuals_preds3$predicteds - actuals_preds3$actuals))/actuals_preds3$actuals)
mape4<- mean(abs((actuals_preds4$predicteds - actuals_preds4$actuals))/actuals_preds4$actuals)
mape5<- mean(abs((actuals_preds5$predicteds - actuals_preds5$actuals))/actuals_preds5$actuals)




