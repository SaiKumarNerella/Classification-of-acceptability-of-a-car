library('rpart')
library('rpart.plot')
library('pROC')
library('caret')
cars <- read.csv("C:/Users/nsaik/Downloads/DataMining/Assignment 2/car.data.csv", header = TRUE)

head(cars)
str(cars)
summary(cars)
#data transformation
#Assuming all the values as 5more in doors as 4+ and more in seats as 4+

set.seed(100)
train <- sample(nrow(cars), 0.8*nrow(cars), replace = FALSE)
TrainSet <- cars[train,]
ValidSet <- cars[-train,]

treecar = rpart(shouldBuy~.,data=cars,method="class",control=rpart.control(minsplit=10))
plotcp(treecar)
printcp(treecar)
rpart.plot(treecar, box.palette="RdBu", shadow.col="gray", nn=FALSE)
plot(treecar, uniform=TRUE,
     main="Classification Tree for Sonar Data")
text(treecar, use.n=FALSE, cex=.8)

Pred = predict(treecar,newdata = cars[,1:5] , type ='class' )

treeCM=table(cars[,7],Pred)
confusionMatrix(treeCM)
predValid_numeric = as.numeric(Pred)
roc.multi<-multiclass.roc(ValidSet$shouldBuy,predValid_numeric)
roc.multi
rs <- roc.multi[['rocs']]
plot.roc(rs[[1]])
plot.roc(rs[[2]])
plot.roc(rs[[3]])
plot.roc(rs[[4]])
#importance
treecar$variable.importance
