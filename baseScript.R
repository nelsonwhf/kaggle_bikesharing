#set working directory
setwd("/Users/janbrusch/Documents/R/kaggle_Bikesharing")

#import datasets from working directory
train <- read.csv("train.csv") #use only 1000 rows for speed during feature engineering
test <- read.csv("test.csv") #use only 100 rows for speed during feature engineering

#import necessary packages
library(ggplot2)

#####Feature Engineering function: accepts data frame, returns data frame
featureEngineer <- function(df) {
  
  #convert season, holiday, workingday and weather into factors
  names <- c("season", "holiday", "workingday", "weather")
  df[,names] <- lapply(df[,names], factor)
  
  #Convert datetime into timestamps (split day and hour?)
  df$datetime <- as.character(df$datetime)
  df$datetime <- strptime(df$datetime, format="%Y-%m-%d %T", tz="EST") #tz removes timestamps flagged as "NA"
  #convert hours to factors in separate feature
  df$hour <- as.integer(substr(df$datetime, 12,13))
  df$hour <- as.factor(df$hour)
  
  #Day of the week
  df$weekday <- as.factor(weekdays(df$datetime))
  df$weekday <- factor(df$weekday, levels = c("Montag", "Dienstag", "Mittwoch", "Donnerstag", "Freitag", "Samstag", "Sonntag")) #order factors
  #Is Weekend?
  df$isWeekend <- ifelse((df$weekday == "Samstag" | df$weekday == "Sonntag"),1, 0)
  df$isWeekend <- as.factor(df$isWeekend)
  #convert weekdays into numeric factors
  df$weekdayNumeric <- as.integer(df$weekday)
  
  #Create a value for each hour of the week
  df$weekHour <- (df$weekdayNumeric-1) * 24 + as.integer(df$hour)
  
  #rearrange table for better readability
  #df[,c(1,3,2,4)]
  
  #return full featured data frame
  return(df)
}


######MAIN
#Build features for train and Test set
train <- featureEngineer(train)
test <- featureEngineer(test)


#TEST: Registered vs. casual by weekhour
hourAggMean <- aggregate((train$casual), by=list(hour = train$weekHour), FUN = median)
hourAggMedian <- aggregate((train$registered), by=list(hour = train$weekHour), FUN = median)
hourAggJoin <- merge(hourAggMean, hourAggMedian, by="hour")
testPlot <- ggplot(hourAggJoin, aes(x = hour)) + geom_line(aes(y = x.x), color = "red") + geom_line(aes(y = x.y), color = "blue")
testPlot

#make separate predictions for casual and registered?




#create output file from dataset test with predictions
submit <- data.frame (datetime = test$datetime, count = test$count)
write.csv(submit, file = "BaseScript_Prediction", row.names=FALSE, col.names=TRUE)