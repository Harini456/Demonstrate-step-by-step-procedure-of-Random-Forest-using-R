#Name: Harini G
#Roll No: 2048034

#install.packages("randomForest")
library("randomForest")
#install.packages("reuire")
library(caTools)
#importing the dataset
data=read.csv("D:/Harini(christ unniversity)/2nd sem subjects/R/heart disease.csv")
#dimension of the dataset
dim(data)#dataset contains 14 columns and 303 observations(rows)
names(data)#printing the names of the columns
head(data)
#data$target[data$target>1]
summary(data)
sapply(data, class)#displaying the datatype of each column
#chaning the datatype for few columns
data=transform(data, sex=as.factor(sex),cp=as.factor(cp),fbs=as.factor(fbs),restecg=as.factor(restecg),exang=as.factor(exang),slope=as.factor(slope),ca=as.factor(ca),thal=as.factor(thal),target=as.factor(target))
sapply(data, class)#displaying the datatype of each column
summary(data)

colSums(is.na(data))#checking if their are any null values
#splitting the dataset into training and testing 
sample=sample.split(data$target,SplitRatio=0.75)
train=subset(data,sample==TRUE)
test=subset(data,sample==FALSE)
dim(train)#dimesion of train data
dim(test)#dimension of test data
#building the random forest model
rf=randomForest(target~.,data=train)
#rf
plot(rf)
#Red line represents MCR of class not having heart diseases, 
#green line represents MCR of class having heart diseases and 
#black line represents overall MCR or OOB error. 
#Overall error rate is what we are interested in which seems considerably good.
#rf$confusion[, 'class.error']
varImpPlot(rf,sort = T,main = "Variable Importance",n.var = 5)
var.imp <- data.frame(importance(rf,type = 2))
#important variables for prediction are cp,thal,ca,thalach,oldpeak
var.imp$Variables <- row.names(var.imp)
var.imp[order(var.imp$MeanDecreaseGini, decreasing = T),]
#higher descrease in Gini means that a particular predictor variable 
#plays a greater role in partitioning the data into the defined classes.
train$predicted.response <- predict(rf, train)#training the data
library(e1071)
library(caret)
#printing the confusion matrix for taining data
confusionMatrix(data = train$predicted.response,reference = train$target)
#the accuracy we have got for taining is 100%
test$predicted.response <- predict(rf, test)#testing
#printing the confusion matrix for test data
confusionMatrix(data = test$predicted.response,reference = test$target)
#the accuracy we hae got for testing data is is more than 80%
