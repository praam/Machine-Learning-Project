#Source : https://www.kaggle.com/c/rossmann-store-sales

#Methodology : Knowledge Discovery in Databases (KDD)

#EXPLORATORY DATA ANALYSIS

#Data Loading
dataset1 <- read.csv("train.csv")

str(dataset1)

#Check for Missing Values
sapply(dataset1,function(x) sum(is.na(x)))

#Data Wrangling
head(dataset1)

tail(dataset1)

nrow(dataset1)

#Data Transformation

dataset1["Year"] <- sapply(strsplit(dataset1$Date, '-'), function(x) x[1])

dataset1$Year <- as.numeric(dataset1$Year)

dataset1["Month"] <- sapply(strsplit(dataset1$Date, '-'), function(x) x[2])

dataset1$Month <- as.numeric(dataset1$Month)

dataset1["Day"] <- sapply(strsplit(dataset1$Date, '-'), function(x) x[3])

dataset1$Day <- as.numeric(dataset1$Day)

dataset1$Date<-NULL

str(dataset1)


unique(dataset1$StateHoliday)
dataset1$StateHoliday = factor(dataset1$StateHoliday,
                        levels = c("0","a","b","c"),
                        labels = c(0,1,2,3))

dataset1$StateHoliday <- as.numeric(as.character(dataset1$StateHoliday))

for(i in 1:6) {
  
  dataset1[,i] <- as.numeric(dataset1[,i])
}

dataset1$SchoolHoliday = as.numeric(dataset1$SchoolHoliday)

filtered_data <- dataset1[-c(117211:1017209), ]

str(filtered_data)

unique(filtered_data$Open)
unique(filtered_data$Promo)
unique(filtered_data$StateHoliday)
unique(filtered_data$SchoolHoliday)
unique(filtered_data$Year)
unique(filtered_data$Month)

filtered_data$Year<-NULL

sapply(filtered_data,function(x) sum(is.na(x)))

str(filtered_data)

head(filtered_data)

tail(filtered_data)

set.seed(1234)

library(caTools)

split = sample.split(filtered_data$Sales, SplitRatio = 0.8)
training_set = subset(filtered_data, split == TRUE)
test_set = subset(filtered_data, split == FALSE)

str(training_set)

head(training_set)

tail(training_set)


library(psych)

describe(training_set)

cor(training_set)

#Applying Multiple Regression using backward elimination

first_model <- lm(Sales~.,data = training_set)

summary(first_model)

first_model2 <- lm(Sales~Store+DayOfWeek+Customers+Open+Promo+StateHoliday+Month+Day,data = training_set)

summary(first_model2)

first_model3 <- lm(sqrt(Sales)~Store+DayOfWeek+Customers+Open+Promo+StateHoliday+Day,data = training_set)

test_set$Sales <- sqrt(test_set$Sales)

summary(first_model3)

par(mfrow =c(2,2))
plot(first_model3)


#Multicollinearity
vif(first_model3)

#Auto Correlation
durbinWatsonTest(first_model3)

#No Influential data points
cooks.distance(first_model3)

influencePlot(model = first_model3,scale = 3,main = "Influence Plot")

#Predicting Sales
my_prediction1<-predict(first_model3,data = test_set)

actual_and_predicted <- data.frame(cbind(actuals=test_set$Sales, predicteds=my_prediction1))
correlation_accuracy <- cor(actual_and_predicted)  


head(actual_and_predicted)

head(correlation_accuracy)





