set.seed(42)
library(xgboost)
library(caret)
str(y)
dataset<-y

set.seed(42)
index <- createDataPartition(dataset$class, p = 0.7, list = FALSE)
train_data <- dataset[index, ]
test_data  <- dataset[-index, ]

library(plyr)
model_xgb <- caret::train(class ~ .,
                          data = train_data,
                          method = "xgbTree",
                          preProcess = c("scale", "center"),
                          trControl = trainControl(method = "repeatedcv", 
                                                   number = 3, 
                                                   repeats = 3, 
                                                   savePredictions = TRUE, 
                                                   verboseIter = FALSE))
#Feature Importance
importance <- varImp(model_xgb, scale = TRUE)
plot(importance)


#predicting test data
confusionMatrix(predict(model_xgb, test_data), test_data$class)

results <- data.frame(actual = test_data$class,predict(model_xgb, test_data, type = "prob"))

table(results[,1])
library(caret)
confusionMatrix(results[,1],test_data$class)

library(ROCR)
ytest<-test_data$Disengketakan
pred1<-predict(model_xgb, test_data, type = "prob")
predroc <- prediction(predictions = pred1[,2], labels=ytest)
roc<-performance(predroc,"tpr", "fpr")
plot(roc, colorize=TRUE, main="ROC Curve", xlab="Sensitivity", ylab="1-Specifity")
abline(a=0, b=1, col="red")
#AUC
auc<-performance(predroc,"auc")
auc<-unlist(slot(auc, "y.values"))
auc<-round(auc,4)
legend(.7, .5, auc, title="AUC", cex=0.75)

