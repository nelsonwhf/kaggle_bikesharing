#set working directory
setwd("/Users/janbrusch/Documents/R/kaggle_Bikesharing")

#import datasets from working directory
train <- read.csv("train.csv") #use only 1000 rows for speed during feature engineering
test <- read.csv("test.csv") #use only 100 rows for speed during feature engineering

#import necessary packages
library(ggplot2)
library(rpart)

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
  
#####FEATURE ENGINEERING IDEAS:
#Day before / day after holiday
#Icy roads (could be inferred from temperature? Could be a day after a night with freezing temepratures?)
  
}


######MAIN
#Build features for train and Test set
train <- featureEngineer(train)
test <- featureEngineer(test)

#set rpart parameters
myMinsplit <- 2
myCp <- 0.00001

#make separate fits for casual and registered
casualFit <- rpart(casual ~ holiday + workingday + season + weather + atemp + hour + weekday, data=train, method="anova", control=rpart.control(minsplit=myMinsplit, cp=myCp))
registeredFit <- rpart(registered ~ holiday + workingday + season + weather + atemp + hour + weekday, data=train, method="anova", control=rpart.control(minsplit=myMinsplit, cp=myCp))
#apply to test data
casualPrediction <- predict(casualFit, test, type="matrix")
plot(casualPrediction)
test$casual <- round(casualPrediction, 0)
registeredPrediction <- predict(registeredFit,test,type="matrix")
test$registered <- round(registeredPrediction,0)
plot(registeredPrediction)
#count = casual + registered
test$count <- test$casual + test$registered


#make one fit for count
#countFit <- rpart(count ~ holiday + workingday + season + weather + atemp + hour + weekday, data=train, method="anova", control=rpart.control(minsplit=myMinsplit, cp=myCp))
#countPrediction <- predict(countFit, test, type="matrix")
#test$count <- round(countPrediction, 0)


#testplot
plot(test$count)
plot(train$count)

#create output file from dataset test with predictions
submit <- data.frame (datetime = test$datetime, count = test$count)
write.csv(submit, file = "rpart_Prediction.csv", row.names=FALSE)