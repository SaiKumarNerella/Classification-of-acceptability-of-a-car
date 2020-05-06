#install.packages("rpart.plot")

library('rpart')
library('rpart.plot')
cars <- read.csv("C:/Users/nsaik/Downloads/DataMining/Assignment 2/car.data.csv", header = TRUE)
View(cars)

# cars$doors[cars$doors == '5more'] <- '4+'
#Check for NaN values
anyNA(cars)
# cars$seats[cars$seats == 'more'] <- '4+'
head(cars)
str(cars)
summary(cars)
#data transformation
#Assuming all the values as 5more in doors as 4+ and more in seats as 4+

library(caret)
# calculate correlation matrix
#correlationMatrix <- cor(cars[,1:6])

#write.csv(cars,"C:/Users/nsaik/OneDrive/Desktop/MCDA_Sem2/Data Mining/Assignment2/car_transform.csv", row.names = FALSE)

set.seed(100)
train <- sample(nrow(cars), 0.8*nrow(cars), replace = FALSE)
TrainSet <- cars[train,]
TestSet <- cars[-train,]
summary(TrainSet)
summary(TestSet)

#Check for NaN values
anyNA(TrainSet)
anyNA(TestSet)

View(TrainSet)

# Fitting SVM to the Training set 
#install.packages('e1071') 
#install.packages('caret') 
#install.packages('kernlab') 
library(e1071)
library(caret)

#10 fold cross validation
trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
set.seed(3233)

#SVM with linear kernel
#training
svm_Linear <- train(shouldBuy ~., data = TrainSet, method = "svmLinear",
                    trControl=trctrl,
                    preProcess = c("center", "scale"),
                    tuneLength = 10)
svm_Linear

#testing
test_pred <- predict(svm_Linear, newdata = TestSet)
test_pred
confusionMatrix(test_pred, TestSet$shouldBuy )

#SVM with non-linear kernel
#training
svm_Radial <- train(shouldBuy ~., data = TrainSet, method = "svmRadial",
                      trControl=trctrl,
                      preProcess = c("center", "scale"),
                      tuneLength = 10)
svm_Radial
plot(svm_Radial)

#testing
test_pred_Radial <- predict(svm_Radial, newdata = TestSet)
test_pred_Radial
confusionMatrix(test_pred_Radial, TestSet$shouldBuy )


