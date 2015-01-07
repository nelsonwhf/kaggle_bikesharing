#set working directory
setwd("/Users/janbrusch/Documents/R/kaggle_Bikesharing")

#import datasets from working directory
train <- read.csv("train.csv") 
test <- read.csv("test.csv") 

#import necessary packages


#####Feature Engineering function
featureEngineer <- function(df) {
  
  #Convert datetime into timestamps (split day and hour?)
  df$datetime <- as.character(df$datetime)
  df$datetime <- strptime(df$datetime, format="%Y-%m-%d %T", tz="EST") #tz removes timestamps flagged as "NA"
  #convert hours to factors in separate feature
  df$hour <- as.integer(substr(df$datetime, 12,13))
  df$hour <- as.factor(df$hour)
  
  #Day of the week
  df$weekday <- as.factor(weekdays(df$datetime))
  df$weekday <- factor(df$weekday, levels = c("Montag", "Dienstag", "Mittwoch", "Donnerstag", "Freitag", "Samstag", "Sonntag")) #order factors
  #convert weekdays into numeric factors
  df$weekdayNumeric <- as.integer(df$weekday)
  
  #Create a value for each hour of the week
  df$weekHour <- (df$weekdayNumeric-1) * 24 + as.integer(df$hour)
  
  #rearrange table for better readability
  #df[,c(1,3,2,4)]
  
  #return full featured data frame
  return(df)
}

train <- featureEngineer(train)
test <- featureEngineer(test)

#Calculate median for each hour of the week
hourAggMedian <- aggregate((train$count), by=list(weekHour = train$weekHour), FUN = median)

#Fill these values in the test data accordingly
#round median values so they make a full number count
round(hourAggMedian$x, digits = 0)
test <- merge(test,hourAggMedian, by="weekHour")
test$count = as.integer(test$x)

#create output file from dataset test with predictions
submit <- data.frame (datetime = test$datetime, count = test$count)
write.csv(submit, file = "hourMedian_Prediction.csv", row.names=FALSE)