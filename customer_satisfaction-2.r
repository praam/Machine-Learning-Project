#Dataset source : https://www.kaggle.com/sjleshrac/airlines-customer-satisfaction

#Methodology : Knowledge Discovery in Databases (KDD)

#EXPLORATORY DATA ANALYSIS

#Data Loading

dataset2 <- read.csv("Invistico_Airline.csv", header=T, na.strings=c(""), stringsAsFactors = T)
str(dataset2)

#Checking for missing values
sapply(dataset2,function(x) sum(is.na(x)))

cor(dataset2$Departure.Delay.in.Minutes,dataset2$Arrival.Delay.in.Minutes, method = "pearson", use = "complete.obs")

pairs(dataset2[,22:23],pch = 19,panel = panel.smooth)

plot(Arrival.Delay.in.Minutes~Departure.Delay.in.Minutes, data=dataset2)
abline(lm(Arrival.Delay.in.Minutes~Departure.Delay.in.Minutes, data=dataset2), col="purple")

dataset2$Arrival.Delay.in.Minutes[is.na(dataset2$Arrival.Delay.in.Minutes)] <- mean(dataset2$Arrival.Delay.in.Minutes,na.rm = TRUE)

#Rechecking for missing values

sapply(dataset2,function(x) sum(is.na(x)))

#Data Wrangling
head(dataset2)

tail(dataset2)

str(dataset2)

library(Hmisc)
describe(dataset2)

unique(dataset2$satisfaction)
unique(dataset2$Gender)
unique(dataset2$Customer.Type)
unique(dataset2$Type.of.Travel)
unique(dataset2$Class)

#Data Transformation

dataset2$satisfaction= factor(dataset2$satisfaction,
                              levels = c("dissatisfied","satisfied"),
                              labels = c(0,1))


dataset2$Gender = factor(dataset2$Gender,
                         levels = c("Female","Male"),
                         labels = c(0,1))


dataset2$Customer.Type = factor(dataset2$Customer.Type,
                                levels = c("disloyal Customer","Loyal Customer"),
                                labels = c(0,1))


dataset2$Type.of.Travel = factor(dataset2$Type.of.Travel,
                                 levels = c("Personal Travel","Business travel"),
                                 labels = c(0,1))



dataset2$Class = factor(dataset2$Class,
                        levels = c("Eco","Business","Eco Plus"),
                        labels = c(0,1,2))

dataset2$Class = as.numeric(as.character(dataset2$Class))

str(dataset2)

head(dataset2)

tail(dataset2)

library(psych)
describe(dataset2)


#Splitting the data sets into train and test sets

library(caTools)

set.seed(1234)

split = sample.split(dataset2$satisfaction, SplitRatio = 0.7)
training_set2 = subset(dataset2, split == TRUE)
test_set2 = subset(dataset2, split == FALSE)

training_set2[,4]<-scale(training_set2[,4])
training_set2[,7]<-scale(training_set2[,7])
training_set2[,22:23] <- scale(training_set2[,22:23])

test_set2[,4]<-scale(test_set2[,4])
test_set2[,7]<-scale(test_set2[,7])
test_set2[,22:23] <- scale(test_set2[,22:23])

str(training_set2)
str(test_set2)

head(training_set2)
head(test_set2)

tail(training_set2)
tail(test_set2)

describe(training_set)
describe(test_set)

#Applying Logistic Regression Model
second_Model <- glm(satisfaction ~.,family = 'binomial',data = training_set2)

summary(second_Model)

library(rcompanion)

#checking Pseudo R Square
nagelkerke(second_Model)

#Preducting Customer Satisfaction
my_prediction2 <- predict(second_Model,newdata = test_set2,type = "response")
my_prediction2 <- ifelse(my_prediction2 > 0.5,1,0)


library(caret)

confusionMatrix(as.factor(my_prediction2),test_set2$satisfaction)


library(ROCR)
rc_pred_lr <- predict(second_Model, newdata=test_set2, type="response")
rc_pr <- prediction(rc_pred_lr, test_set2$satisfaction)
rc_prf_lr <- performance(rc_pr, measure = "tpr", x.measure = "fpr",auc=TRUE)
plot(rc_prf_lr,main = "ROC Curve",col = 1,lwd = 2)
abline(a = 0,b = 1,lwd = 2,lty = 3,col = "black")

auc1 <- performance(rc_pr, measure = "auc")
auc1 <- auc1@y.values[[1]]
auc1



library(class)

#Applying K-Nearest Neighbour(KNN)

number_of_observations = 90916

k_value = round(sqrt(number_of_observations))

third_model <- knn(train = training_set2, test = test_set2,cl = training_set2$satisfaction, k=k_value)

library(gmodels)

CrossTable(x = test_set2$satisfaction , y = third_model,prop.chisq = FALSE)

library(caret)

confusionMatrix(table(third_model,test_set2$satisfaction))

Accuracy_Percentage=100 * sum(test_set2$satisfaction == third_model)/NROW(test_set2$satisfaction)


for (i in 289:302){
  
  third_model2 <- knn(train=training_set2, test=test_set2, cl=training_set2$satisfaction, k=i)
  100 * sum(test_set2$satisfaction == third_model2)/NROW(test_set2$satisfaction)
}

library(ROCR)

rc_pr2 <- prediction(as.numeric(third_model),test_set2$satisfaction)
rc_prf_knn <- performance(rc_pr2, measure = "tpr", x.measure = "fpr")
plot(rc_prf_knn,main = "ROC Curve",col = 1,lwd = 2)
abline(a = 0,b = 1,lwd = 2,lty = 3,col = "black")

auc2 <- performance(rc_pr2, measure = "auc")
auc2 <- auc2@y.values[[1]]
auc2

