
library('caret')
library('e1071')
library('pROC')

cars <- read.csv("C:/Users/nsaik/Downloads/DataMining/Assignment 2/car.data.csv", header = TRUE)

head(cars)
str(cars)
summary(cars)
set.seed(100)
train <- sample(nrow(cars), 0.8*nrow(cars), replace = FALSE)
TrainSet <- cars[train,]
ValidSet <- cars[-train,]
library(caret)
control <- trainControl(method="repeatedcv", number=10, repeats=3)
seed <- 7
metric <- "Accuracy"
set.seed(seed)
rf_cv <- train(shouldBuy ~ .,TrainSet, method="rf",ntree = 100, metric=metric, trControl=control,tuneGrid = data.frame(mtry = 6))
predValid_cv <- predict(rf_cv, ValidSet)
table_rf <- table(predValid_cv,ValidSet$shouldBuy)
confusionMatrix(table_rf)
accuracy <- sum(diag(table_rf))/sum(table_rf)
accuracy
#ROC curve
predValid_numeric = as.numeric(predValid_cv)
roc.multi<-multiclass.roc(ValidSet$shouldBuy,predValid_numeric)
roc.multi
rs <- roc.multi[['rocs']]
plot.roc(rs[[1]])
plot.roc(rs[[2]])
plot.roc(rs[[3]])
plot.roc(rs[[4]])

#importance of Parameters
varImp(rf_cv)
ggplot(varImp(rf_cv))

