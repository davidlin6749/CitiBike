

david<- sort(table(data$Start_Station_Name),decreasing =TRUE)[1:5]

## Find a range of y's that'll leave sufficient space above the tallest bar
ylim <- c(0, 1.1*max(david))


## Plot, and store x-coordinates of bars in xx
part1 <- barplot(sort(table(data$Start_Station_Name),decreasing =TRUE)[1:5], xlab = 'Station Name',  ylim = ylim,
              main = "Top 5 Start Stations with Most Starts", 
              ylab = "Frequency")
## Add text at top of bars
text(x = part1, y = david, label = david, pos = 3, cex = 0.8, col = "red")


#finding End station
david2<- sort(table(data$End_Station_Name),decreasing =TRUE)[1:5]

## Find a range of y's that'll leave sufficient space above the tallest bar
ylim <- c(0, 1.1*max(david2))


## Plot, and store x-coordinates of bars in xx
part1 <- barplot(sort(table(data$End_Station_Name),decreasing =TRUE)[1:5], xlab = 'Station Name',  ylim = ylim,
                 main = "Top 5 End Stations with Most Destination", 
                 ylab = "Frequency")
## Add text at top of bars
text(x = part1, y = david2, label = david, pos = 3, cex = 0.8, col = "red")