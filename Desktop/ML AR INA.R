dataset <- read.csv("/home/heru/Desktop/Mydata3.csv", 
                      header = TRUE, 
                      sep = ",")

str(dataset)
#Berapa Jumlah yang disengeketakan?
summary(dataset$Class)

library(caret)
#Training, validation and test data

set.seed(42)
index <- createDataPartition(dataset$Class, p = 0.7, list = FALSE)
train_data <- dataset[index, ]
test_data  <- dataset[-index, ]


#Classification
#Decision trees
library(rpart)
library(rpart.plot)

set.seed(42)
fit <- rpart(Class ~ .,
             data = train_data,
             method = "class",
             control = rpart.control(xval = 10, 
                                     minbucket = 2, 
                                     cp = 0), 
             parms = list(split = "information"))


rpart.plot(fit)

pre1<-predict(fit, test_data, type = "class")
confusionMatrix(pre1,test_data$Class)

library(MLmetrics)
library(ROCR)
F1_score<- F1_Score(test_data$Class, pre1, positive = NULL)
F1_score

prec<-predict(fit, test_data, type = "matrix")
pra<-prec[,6]
length(pra)
length(test_data$Class)

test_data$Class<-as.numeric(test_data$Class)
test_data$Class<-ifelse(test_data$Class==2,1,0)
test_data$Class
predroc <- prediction(predictions = prec[,6], labels=test_data$Class)
predroc
roc<-performance(predroc,"tpr", "fpr")
plot(roc)

# And then a lift chart
perf <- performance(predroc,"lift","rpp")
plot(perf, main="lift curve", colorize=T)

library(ROCR)
gain.chart <- function(n) {
  score <- runif(n)
  y <- (runif(n) < score)
  plot(performance(prediction(score, y), “tpr”, “rpp”),lwd = 7, main = paste(“N =”, n))
  lines(ecdf((rank(-score)[y == T]) / n),verticals = T, do.points = F, col = “red”, lwd = 3)
}

set.seed(1)
par(mfrow = c(1, 2))
gain.chart(10)
gain.chart(10000)




Lift<-LiftAUC(test_data$Class, predroc)
LogLos1<-LogLoss(test_data$Class, pra)
LogLos1
KSS<-KS_Stat(test_data$Class, pra)
KSS
Gini(test_data$Class, pra)


warnings()
library(ROCR)
ytest<-test_data$Disengketakan
prec<-predict(fit, test_data, type = "prob")
predroc <- prediction(predictions = prec[,2], labels=ytest)
roc<-performance(predroc,"tpr", "fpr")
plot(roc, colorize=TRUE, main="ROC Curve", xlab="Sensitivity", ylab="1-Specifity")
abline(a=0, b=1, col="red")
#AUC
auc<-performance(predroc,"auc")
auc<-unlist(slot(auc, "y.values"))
auc<-round(auc,4)
legend(.7, .5, auc, title="AUC", cex=0.75)





#Random Forest
set.seed(42)
library(randomForest)
train_data<-na.omit(train_data)
dataset<-na.omit(dataset)
rf<-randomForest(Disengketakan ~., dataset, ntree = 500,
                 mtry = 2,
                 importance = TRUE,
                 proximity = TRUE)

print(rf)
attributes(rf)


# Prediction & Confusion Matrix - train data
library(caret)
p1 <- predict(rf, train_data)
confusionMatrix(p1, train_data$Disengketakan)

# # Prediction & Confusion Matrix - test data
p2 <- predict(rf, test_data)
confusionMatrix(p2, test_data$Disengketakan)

# Error rate of Random Forest
plot(rf)

# Tune mtry
t <- tuneRF(train_data[,-5], train_data[,5],
            stepFactor = 0.5,
            plot = TRUE,
            ntreeTry = 300,
            trace = TRUE,
            improve = 0.05)

rf<-randomForest(Disengketakan ~., dataset, ntree = 500,
                 mtry = 1,
                 importance = TRUE,
                 proximity = TRUE)

# # Prediction & Confusion Matrix - test data
p2 <- predict(rf, test_data)
confusionMatrix(p2, test_data$Disengketakan)


# No. of nodes for the trees
hist(treesize(rf),
     main = "No. of Nodes for the Trees",
     col = "green")

# Variable Importance
varImpPlot(rf,
           sort = T,
           main = "Variable Importance")
importance(rf)
varUsed(rf)


# Extract Single Tree
getTree(rf, 1, labelVar = TRUE)

# Multi-dimensional Scaling Plot of Proximity Matrix
#MDSplot(rf, train_data$Churn)


#Extreme gradient boosting trees
#Install XGB
#install.packages("drat", repos="https://cran.rstudio.com")
#drat:::addRepo("dmlc")
#install.packages("xgboost", repos="http://dmlc.ml/drat/", type = "source")
set.seed(42)
library(xgboost)
model_xgb <- caret::train(Disengketakan ~ .,
                          data = train_data,
                          method = "xgbTree",
                          preProcess = c("scale", "center"),
                          trControl = trainControl(method = "repeatedcv", 
                                                   number = 10, 
                                                   repeats = 10, 
                                                   savePredictions = TRUE, 
                                                   verboseIter = FALSE))
#Feature Importance
importance <- varImp(model_xgb, scale = TRUE)
plot(importance)


#predicting test data
confusionMatrix(predict(model_xgb, test_data), test_data$Disengketakan)

results <- data.frame(actual = test_data$Disengketakan,predict(model_xgb, test_data, type = "prob"))

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


library(neuralnet)
train_data$Disengketakan<-as.numeric(train_data$Disengketakan)
train_data$BentukInvoice<-as.numeric(train_data$BentukInvoice)
set.seed(43)
Nnet <- neuralnet(train_data$Disengketakan ~ train_data$HariKeterlambatan + train_data$TenggatWaktu + train_data$NilaiInvoice + train_data$BentukInvoice, data=train_data, hidden = 5, learningrate=0.01)
plot(Nnet, rep = "best")

#testing the model on the test dataset
test_data$Disengketakan<-as.numeric(test_data$Disengketakan)
test_data$BentukInvoice<-as.numeric(test_data$BentukInvoice)
temp_test <- subset(test_data, select = c("HariKeterlambatan", "TenggatWaktu", "NilaiInvoice", "BentukInvoice"))
arnet.results <- compute(Nnet, temp_test)

#showing our predictor inputs within the test dataset
head(temp_test)

#showing the difference between our predicted outcome and the actual outcome within our test data set.
results <- data.frame(actual = test_data$Disengketakan, prediction = arnet.results$net.result)
results[1:15, ]

arnet.results$net.result

#This is a rounded result in order to make it easier to read.
results$prediction1 <- round(results$prediction)
results[1:15, ]
table(results$actual,results$prediction1)
library(caret)
confusionMatrix(results$actual,results$prediction1)

prediction<-as.matrix(results$prediction)
actual<-results$actual

#ROCR
library(ROCR)
pred <-ROCR:: prediction(prediction, actual)
perf <- performance(pred, measure="tpr", x.measure="fpr")
roc<-performance(pred,"tpr", "fpr")
plot(roc, colorize=TRUE, main="ROC Curve", xlab="Sensitivity", ylab="1-Specifity")
abline(a=0, b=1, col="black")
#AUC
#AUC
auc<-performance(pred,"auc")
auc<-unlist(slot(auc, "y.values"))
auc<-round(auc,4)
legend(.7, .5, auc, title="AUC", cex=0.75)

