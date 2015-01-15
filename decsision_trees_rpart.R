#set working directory
setwd("/Users/janbrusch/Documents/R/kaggle_Bikesharing")

#import datasets from working directory
train <- read.csv("train.csv") #use only 1000 rows for speed during feature engineering
test <- read.csv("test.csv") #use only 100 rows for speed during feature engineering

#import necessary packages
library(rpart)
library(rpart.plot)

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
#Also would be good to play around with parameter weight


####### THIS WHOLE BLOCK SHOULD BE PUT IN A SEPARATE FUNCTION(S) WHICH ELIMINATE REDUNDANT CODE ######### 
#make separate fits for casual and registered
casualFit <- rpart(casual ~ holiday + workingday + hour + weekday + year + season + weather + atemp, data=train, method="anova", control=rpart.control(minsplit=myMinsplit, cp=myCp))
registeredFit <- rpart(registered ~ holiday + workingday + hour + weekday + year + season + weather + atemp, data=train, method="anova", control=rpart.control(minsplit=myMinsplit, cp=myCp))
#apply to casuals
casualPrediction <- predict(casualFit, test, type="matrix")
test$casual <- round(casualPrediction, 0)
#plot vs train data
plot(train$casual)
plot(test$casual)
#apply to registered
registeredPrediction <- predict(registeredFit,test,type="matrix")
test$registered <- round(registeredPrediction,0)
#plot vs. train data
plot(train$registered)
plot(test$registered)
#aggregate registered and casual to count
count = casual + registered
test$count <- test$casual + test$registered

#make one fit for count
#countFit <- rpart(count ~ holiday + workingday + season + weather + atemp + hour + weekday, data=train, method="anova", control=rpart.control(minsplit=myMinsplit, cp=myCp))
#countPrediction <- predict(countFit, test, type="matrix")
#test$count <- round(countPrediction, 0)
#######################################################################################################

#testplot
plot(train$count)
plot(test$count)
prp(casualFit)

#create output file from dataset test with predictions
submit <- data.frame (datetime = test$datetime, count = test$count)
write.csv(submit, file = "rpart_Prediction.csv", row.names=FALSE)