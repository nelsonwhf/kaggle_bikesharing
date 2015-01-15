#set working directory
setwd("/Users/janbrusch/Documents/R/kaggle_Bikesharing")

#import datasets from working directory
train <- read.csv("train.csv") #use only 1000 rows for speed during feature engineering
test <- read.csv("test.csv") #use only 100 rows for speed during feature engineering

#import necessary packages
library(party)

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
    
  #something that incorporates yearly growth:
  #extract year from date and convert to factor
  df$year <- as.integer(substr(df$datetime, 1,4))
  df$year <- as.factor(df$year)
  
  #rearrange table for better readability
  #df[,c(1,3,2,4)]
  
  #return full featured data frame
  return(df)

}


######MAIN
#Build features for train and Test set
train <- featureEngineer(train)
test <- featureEngineer(test)

#####RANDOM FOREST STARTS HERE#########
###separate fits and separate variables for casual and registered
set.seed(162)
myNtree <- 1000
myMtry <- 3
formula <- count ~ season + holiday + workingday + weather + temp + humidity + windspeed + hour + weekday + year
#casual
#casualFit <- randomForest(casual ~ hour + year + humidity + temp + atemp + workingday + weekday, data=train, ntree=myNtree, mtry=myMtry, importance=myImportance)
#test$casual <- predict(casualFit, test)
#registered
#registeredFit <- randomForest(registered ~ hour + year + season + weather + workingday + humidity + weekday + atemp, data=train, ntree=myNtree, mtry=myMtry, importance=myImportance)
#test$registered <- predict(registeredFit, test)
#aggregate columns into count
#test$count <- round(test$casual + test$registered, 0)

#####one fit for the whole count
countFit <- cforest(formula, data=train, controls=cforest_unbiased(ntree=myNtree, mtry=myMtry))
prediction <- predict(countFit, test, OOB=TRUE)
test$count <- round(prediction, 0)

#testplot
plot(train$count)
plot(test$count)



#create output file from dataset test with predictions
submit <- data.frame (datetime = test$datetime, count = test$count)
write.csv(submit, file = "randomForest_Prediction.csv", row.names=FALSE)