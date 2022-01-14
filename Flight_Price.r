#Source : https://www.kaggle.com/nikhilmittal/flight-fare-prediction-mh

#Methodology : Knowledge Discovery in Databases (KDD)


library(readxl)

#DATA PREPROCESSING

#Data LOADING
dataset3 <- read_excel("Data_Train.xlsx")

#Data Wrangling
head(dataset3)

tail(dataset3)

str(dataset3)

nrow(dataset3)

dim(dataset3)

#Check for missing values
sapply(dataset3,function(x) sum(is.na(x)))

#Handling the missing values
updated_data<-subset(dataset3,Route!="NA" & Total_Stops!="NA",select=Airline:Price)

#Recheck for missing values
sapply(updated_data,function(x) sum(is.na(x)))

head(updated_data)

tail(updated_data)

#Exploratory Data Analysis

#Data Transformation

updated_data["travelling_day"] <- sapply(strsplit(updated_data$Date_of_Journey, '/'), function(x) x[1])

updated_data$travelling_day <- as.numeric(updated_data$travelling_day)

library(lubridate)

updated_data["travelling_month"] <- month(updated_data$Date_of_Journey)

str(updated_data)

updated_data$Date_of_Journey <- NULL


updated_data["departure_hour"] <-  sapply(strsplit(updated_data$Dep_Time, ':'), function(x) x[1])

updated_data$departure_hour <- as.numeric(updated_data$departure_hour)

updated_data["departure_minutes"] <- sapply(strsplit(updated_data$Dep_Time, ':'), function(x) x[2])

updated_data$departure_minutes<- as.numeric(updated_data$departure_minutes)

updated_data$Dep_Time<- NULL


updated_data["arrival_hour"] <-  sapply(strsplit(updated_data$Arrival_Time, ':'), function(x) x[1])

updated_data$arrival_hour <- as.numeric(updated_data$arrival_hour)

updated_data["arrival_minutes"] <- substr(x = updated_data$Arrival_Time, start = 4, stop = 5)

updated_data$arrival_minutes <- as.numeric(updated_data$arrival_minutes)

updated_data$Arrival_Time <- NULL


updated_data["travel_hours"] <- substr(x = updated_data$Duration, start = 1, stop = 2)

library(tidyr)

updated_data$travel_hours <- extract_numeric(updated_data$travel_hours)

updated_data["travel_minutes"] <- substr(x = updated_data$Duration, start = 4, stop = 5)

updated_data$travel_minutes <- sub("^$","0",updated_data$travel_minutes)

updated_data$travel_minutes <- as.numeric(updated_data$travel_minutes)

library(dplyr)
updated_data <- mutate_if(updated_data,is.numeric, ~replace(., is.na(.), 0))

updated_data$Duration <- NULL

unique(updated_data$Airline)
unique(updated_data$Source)
unique(updated_data$Destination)
unique(updated_data$Total_Stops)


#Encoding Categorical variables



updated_data$Airline= factor(updated_data$Airline,
                            levels = c("IndiGo","Air India","Jet Airways","SpiceJet","Multiple carriers","GoAir",
                                       "Vistara","Air Asia","Vistara Premium economy","Jet Airways Business",
                                       "Multiple carriers Premium economy","Trujet"),
                            labels = c(1,2,3,4,5,6,7,8,9,10,11,12))
updated_data$Airline <- as.numeric(as.character( updated_data$Airline))



updated_data$Source= factor(updated_data$Source,
                                  levels = c("Chennai","Banglore","Mumbai","Kolkata" ,"Delhi"),
                                  labels = c(1,3,5,7,9))
updated_data$Source = as.numeric(as.character(updated_data$Source))


updated_data$Destination= factor(updated_data$Destination,
                            levels = c("Cochin","Banglore","Hyderabad","Kolkata","Delhi","New Delhi"),
                            labels = c(2,4,6,8,10,12))
updated_data$Destination = as.numeric(as.character(updated_data$Destination))




updated_data$Total_Stops = factor(updated_data$Total_Stops,
                         levels = c("non-stop","1 stop","2 stops","3 stops" ,"4 stops"),
                         labels = c(0,1,2,3,4))
updated_data$Total_Stops = as.numeric(as.character(updated_data$Total_Stops))


str(updated_data)


updated_data$Route<-NULL
updated_data$Additional_Info <- NULL


dim(updated_data)

library(psych)
describe(updated_data)

cor(updated_data)



#Splitting the data sets into train and test sets

library(caTools)

set.seed(1234)

split = sample.split(updated_data$Price, SplitRatio = 0.7)
training_set3= subset(updated_data, split == TRUE)
test_set3 = subset(updated_data, split == FALSE)

#Feature scaling for 'Price' Varaiable
training_set3[,5] <- scale(training_set3[,5])
test_set3[,5] <- scale(test_set3[,5])

dim(training_set3)
dim(test_set3)

head(training_set3)
head(test_set3)

tail(training_set3)
tail(test_set3)

library(psych)

describe(training_set3)
describe(test_set3)


library(rpart)
library(rpart.plot)


#Applying Decisosn tree model

fourth_model <- rpart(Price~.,data = training_set3,method = "anova")

library(broom)

glance(fourth_model)


print(fourth_model)

rpart.plot(fourth_model)


#Precting the Price

my_prediction4 <- predict(fourth_model,newdata=test_set3)


MSE1 <- mean((my_prediction4- test_set3$Price)^2)
MSE1
RMSE1 <- sqrt(MSE1)
RMSE1

Accuracy1 <- (cor(test_set3$Price,my_prediction4))^2
Accuracy1

actual_and_predicted4 <- data.frame(cbind(actuals=test_set3$Price, predicteds=my_prediction4))
correlation_accuracy4 <- cor(actual_and_predicted4)  

head(actual_and_predicted4)
head(correlation_accuracy4)


library(caret)

library(randomForest)

set.seed(1234)

#Applying random forest model

fifth_model <- randomForest(Price~.,data = training_set3)

plot(fifth_model)

#Predicting the Price
my_prediction5 <- predict(fifth_model,newdata = test_set3)

MSE2 <- mean((my_prediction5- test_set3$Price)^2)
RMSE2 <- sqrt(MSE2)
MSE2
RMSE2
Accuracy2 <- (cor(test_set3$Price,my_prediction5))^2
Accuracy2

actual_and_predicted5 <- data.frame(cbind(actuals=test_set3$Price, predicteds=my_prediction5))
correlation_accuracy5 <- cor(actual_and_predicted5)  

head(correlation_accuracy5)

head(actual_and_predicted5)








