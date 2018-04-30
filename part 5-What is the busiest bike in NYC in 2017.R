#5)	What is the busiest bike in NYC in 2017? How many times was it used? How many minutes was it in use?


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


