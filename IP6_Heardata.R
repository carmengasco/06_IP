rm(list = ls())
# Importing the dataset
dataset=read.csv('heart.data.csv')

#Displaying the count of null values per column
colSums(is.na(dataset))


# Missing data
#na. rm = TRUE to exclude missing values
dataset$biking[is.na(dataset$biking)]<-mean(dataset$biking, na.rm=TRUE) 
dataset$smoking[is.na(dataset$smoking)]<-mean(dataset$smoking, na.rm=TRUE) 
dataset$heart.disease[is.na(dataset$heart.disease)]<-mean(dataset$heart.disease, na.rm=TRUE) 
colSums(is.na(dataset))
#Create multiple copies of the dataset with no missing data
datasetMLR=dataset
datasetSVR=dataset
datasetDT=dataset
datasetRF=dataset
##################################################################
## Multiple Linear Regression

# Splitting the dataset into the Training set and Test set
# install.packages('caTools')
library(caTools)
set.seed(123)
split=sample.split(datasetMLR$heart.disease, SplitRatio=.8)
training_set=subset(datasetMLR, split==TRUE)
testing_set=subset(datasetMLR, split==FALSE)

# Fitting Multiple Linear Regression to the Training set
regressor<-lm(formula=heart.disease~., data=training_set) 
summary(regressor)
y_pred=predict(regressor, newdata=datasetMLR)

# Predicting the Validation set results
#1
new <- data.frame(biking=45,09720,smoking=21,38562 )
predict(regressor, newdata = new)
#2
new <- data.frame(biking=8,2797433, smoking=6,42371952)
predict(regressor, newdata = new)
#3
new <- data.frame(biking=42,34586, smoking=20,741327)
predict(regressor, newdata = new)
#4
new <- data.frame(biking=30,77425, smoking=23,610174)
predict(regressor, newdata = new)


#RMSE
RMSE(testing_set$heart.disease, y_pred)
R2(testing_set$heart.disease, y_pred)

########################################################
#Support Vector Regressor
# Splitting the dataset into the Training set and Test set
split=sample.split(datasetSVR$heart.disease, SplitRatio=.8)
training_set=subset(datasetSVR, split==TRUE)
testing_set=subset(datasetSVR, split==FALSE)
# Fitting SVR to the dataset
library(e1071)
regressor=svm(formula=heart.disease~., data=training_set, type='eps-regression', kernel='radial')
summary(regressor)
y_pred=predict(regressor, newdata=testing_set)
# Predicting the Validation set results

#1
new <- data.frame(biking=45,09720,smoking=21,38562 )
predict(regressor, newdata = new)
#2
new <- data.frame(biking=8,2797433, smoking=6,42371952)
predict(regressor, newdata = new)
#3
new <- data.frame(biking=42,34586, smoking=20,741327)
predict(regressor, newdata = new)
#4
new <- data.frame(biking=30,77425, smoking=23,610174)
predict(regressor, newdata = new)

library(caret)
#mse(actual, predicted)
RMSE(testing_set$heart.disease, y_pred)
R2(testing_set$heart.disease, y_pred)

########################################################
#Decision Tree Regressor
# Splitting the dataset into the Training set and Test set
split=sample.split(datasetDT$heart.disease, SplitRatio=.8)
training_set=subset(datasetDT, split==TRUE)
testing_set=subset(datasetDT, split==FALSE)
# Fitting to the dataset
library(rpart)
regressor=rpart(formula=heart.disease~., data=training_set)
y_pred=predict(regressor, newdata=testing_set)
# Predicting the Validation set results

#1
new <- data.frame(biking=45,09720,smoking=21,38562 )
predict(regressor, newdata = new)
#2
new <- data.frame(biking=8,2797433, smoking=6,42371952)
predict(regressor, newdata = new)
#3
new <- data.frame(biking=42,34586, smoking=20,741327)
predict(regressor, newdata = new)
#4
new <- data.frame(biking=30,77425, smoking=23,610174)
predict(regressor, newdata = new)

#RMSE
RMSE(testing_set$heart.disease, y_pred)
R2(testing_set$heart.disease, y_pred)

########################################################
#Random Forest Regressor
# Splitting the dataset into the Training set and Test set
split=sample.split(datasetRF$heart.disease, SplitRatio=.8)
training_set=subset(datasetRF, split==TRUE)
testing_set=subset(datasetRF, split==FALSE)
# Fitting to the dataset
library(randomForest)
regressor=randomForest(x=training_set[,1:2], y=training_set$heart.disease, ntree=20)
y_pred=predict(regressor, testing_set)

# Predicting the Validation set results
#1
new <- data.frame(biking=45,09720,smoking=21,38562 )
predict(regressor, newdata = new)
#2
new <- data.frame(biking=8,2797433, smoking=6,42371952)
predict(regressor, newdata = new)
#3
new <- data.frame(biking=42,34586, smoking=20,741327)
predict(regressor, newdata = new)
#4
new <- data.frame(biking=30,77425, smoking=23,610174)
predict(regressor, newdata = new)


#RMSE
RMSE(testing_set$heart.disease, y_pred)
R2(testing_set$heart.disease, y_pred)

