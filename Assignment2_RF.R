library(randomForest)
library('rpart')
library('pROC')
library('caret')
cars <- read.csv("C:/Users/nsaik/Downloads/DataMining/Assignment 2/car.data.csv", header = TRUE)

head(cars)
str(cars)
summary(cars)
set.seed(100)
train <- sample(nrow(cars), 0.8*nrow(cars), replace = FALSE)
TrainSet <- cars[train,]
ValidSet <- cars[-train,]

#Random Forest
rf_cars <- randomForest(shouldBuy ~ ., data = TrainSet, ntree = 100, mtry = 6, importance = TRUE)
rf_cars
predValid <- predict(rf_cars, ValidSet, type = "class")
# Checking classification accuracy
table_rf <- table(predValid,ValidSet$shouldBuy)
confusionMatrix(table_rf)
accuracy <- sum(diag(table_rf))/sum(table_rf)
accuracy
#ROC curve
predValid_numeric = as.numeric(predValid)
roc.multi<-multiclass.roc(ValidSet$shouldBuy,predValid_numeric)
roc.multi
rs <- roc.multi[['rocs']]
plot.roc(rs[[1]])
plot.roc(rs[[2]])
plot.roc(rs[[3]])
plot.roc(rs[[4]])

#importance of Parameters
importance(rf_cars)        
varImpPlot(rf_cars)