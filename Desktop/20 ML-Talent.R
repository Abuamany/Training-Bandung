Sys.info()[1:5]
sessionInfo()

library('stringr')
library('knitr')
library(RCurl)
library("e1071")
x <- getURL("https://raw.githubusercontent.com/Abuamany/People-Analytics/master/mydata10.csv")
y <- read.csv(text = x)
head(y)

set.seed(123)

tal_eval<-y

tal_eval<-tal_eval[,2:8]
nv <- length(tal_eval) # number of attributes
nv

cm<-list()

x<-tal_eval[,1:(nv-1)] 
y<-tal_eval[,nv]

head(y)


fmla<-paste(colnames(tal_eval)[1:(nv-1)],collapse="+")
fmla<-paste0(colnames(tal_eval)[nv],"~",fmla)
fmla<-as.formula(fmla)

fmla

nlev<-nlevels(y) # number of factors describing class
nlev


#Multinominal
library(nnet)
library(caret)
model<-multinom(fmla, data = tal_eval, maxit = 500, trace=FALSE)
prob<-predict(model,x,type="probs") 
pred<-apply(prob,1,which.max)
pred[which(pred=="1")]<-levels(y)[1] 
pred[which(pred=="2")]<-levels(y)[2] 
pred[which(pred=="3")]<-levels(y)[3] 
pred[which(pred=="4")]<-levels(y)[4] 
pred<-as.factor(pred)
l<-union(pred,y)
mtab<-table(factor(pred,l),factor(y,l))
mtab
cm[[1]]<-c("Multinomial","MULTINOM",confusionMatrix(mtab))
cm[[1]]$table
cm[[1]]$overall[1]

#Logistic Regression
library(VGAM)
model<-vglm(fmla, family = "multinomial", data = tal_eval, maxit = 100) 
prob<-predict(model,x,type="response") 
pred<-apply(prob,1,which.max) 
pred[which(pred=="1")]<-levels(y)[1] 
pred[which(pred=="2")]<-levels(y)[2] 
pred[which(pred=="3")]<-levels(y)[3] 
pred[which(pred=="4")]<-levels(y)[4] 
pred<-as.factor(pred)
l<-union(pred,y)
mtab<-table(factor(pred,l),factor(y,l))
cm[[2]]<-c("Logistic Regression","GLM",confusionMatrix(mtab)) 
cm[[2]]$table
cm[[2]]$overall[1]

#Linear Discriminant Analysis
library(MASS) 
model<-lda(fmla,data=tal_eval) 
pred<-predict(model,x)$class 
l<-union(pred,y)
mtab<-table(factor(pred,l),factor(y,l))
cm[[3]]<-c("Linear Discriminant Analysis","LDA",confusionMatrix(mtab))
cm[[3]]$table

cm[[3]]$overall[1]

#Non-Linear Classification
library(mda) 
model<-mda(fmla,data=tal_eval) 
pred<-predict(model,x) 
l<-union(pred,y)
mtab<-table(factor(pred,l),factor(y,l))
cm[[4]]<-c("Mixture Discriminant Analysis","MDA",confusionMatrix(mtab))
cm[[4]]$table

cm[[4]]$overall[1]

#Regularized Discriminant Analysis
library(klaR) 
model<-rda(fmla,data=tal_eval,gamma = 0.05,lambda = 0.01) 
pred<-predict(model,x)$class 
l<-union(pred,y)
mtab<-table(factor(pred,l),factor(y,l))
cm[[5]]<-c("Regularized Discriminant Analysis","RDA",confusionMatrix(mtab))
cm[[5]]$table
cm[[5]]$overall[1]

#Neural Netwrok
library(nnet)
library(devtools)
library(reshape)
model<-nnet(fmla,data=tal_eval,size = 4, decay = 0.0001, maxit = 700, trace = FALSE)
#import the function from Github
source_url('https://gist.githubusercontent.com/Peque/41a9e20d6687f2f3108d/raw/85e14f3a292e126f1454864427e3a189c2fe33f3/nnet_plot_update.r')
plot.nnet(model, alpha.val = 0.5, cex= 0.7, circle.col = list('lightblue', 'white'), bord.col = 'black')
pred<-predict(model,x,type="class") 
pred<-as.factor(pred)
l<-union(pred,y)
mtab<-table(factor(pred,l),factor(y,l))
cm[[6]]<-c("Neural Network","NNET",confusionMatrix(mtab))
cm[[6]]$table

cm[[6]]$overall[1]

#Flexible Discriminant Analysis
library(mda) 
model<-fda(fmla,data=tal_eval) 
pred<-predict(model,x,type="class") 
l<-union(pred,y)
mtab<-table(factor(pred,l),factor(y,l))
cm[[7]]<-c("Flexible Discriminant Analysis","FDA",confusionMatrix(mtab))
cm[[7]]$table

cm[[7]]$overall[1]

#Support Vector Machine
library(kernlab) 
model<-ksvm(fmla,data=tal_eval) 
pred<-predict(model,x,type="response") 
l<-union(pred,y)
mtab<-table(factor(pred,l),factor(y,l))
cm[[8]]<-c("Support Vector Machine","SVM",confusionMatrix(mtab))
cm[[8]]$table
cm[[8]]$overall[1]

#k-Nearest Neighbors
library(caret) 
model<-knn3(fmla,data=tal_eval,k=nlev+1) 
pred<-predict(model,x,type="class") 
l<-union(pred,y)
mtab<-table(factor(pred,l),factor(y,l))
cm[[9]]<-c("k-Nearest Neighbors","KNN",confusionMatrix(mtab))
cm[[9]]$table
cm[[9]]$overall[1]

#B8 Naive Bayes
library(e1071) 
model<-naiveBayes(fmla,data=tal_eval,k=nlev+1) 
pred<-predict(model,x) 
l<-union(pred,y)
mtab<-table(factor(pred,l),factor(y,l))
cm[[10]]<-c("Naive Bayes","NBAYES",confusionMatrix(mtab))
cm[[10]]$table
cm[[10]]$overall[1]

#3.C Non-Linear Classification with Decision Trees
#3.C1 Classification and Regression Trees(CART)
library(rpart) 
library(rpart.plot)
model<-rpart(fmla,data=tal_eval) 
# prp(model, faclen=3)
rpart.plot(model)
pred<-predict(model, x ,type="class") 
l<-union(pred,y)
mtab<-table(factor(pred,l),factor(y,l))
cm[[11]]<-c("classification and Regression Trees","CART",confusionMatrix(mtab))
cm[[11]]$table
cm[[11]]$overall[1]

#3.C2 OneR
library(RWeka)
model<-OneR(fmla,data=tal_eval) 
pred<-predict(model,x,type="class") 
l<-union(pred,y)
mtab<-table(factor(pred,l),factor(y,l))
cm[[12]]<-c("One R","ONE-R",confusionMatrix(mtab))
cm[[12]]$table
cm[[12]]$overall[1]

#3.C3 C4.5
library(RWeka) 
model<-J48(fmla,data=tal_eval) 
pred<-predict(model,x) 
l<-union(pred,y)
mtab<-table(factor(pred,l),factor(y,l))
cm[[13]]<-c("C4.5","C45",confusionMatrix(mtab))
cm[[13]]$table
cm[[12]]$overall[1]

#3.C4 PART
library(RWeka) 
model<-PART(fmla,data=tal_eval) 
pred<-predict(model,x) 
l<-union(pred,y)
mtab<-table(factor(pred,l),factor(y,l))
cm[[14]]<-c("PART","PART",confusionMatrix(mtab))
cm[[14]]$table
cm[[14]]$overall[1]

#3.C5 Bagging CART
library(ipred) 
model<-bagging(fmla,data=tal_eval) 
pred<-predict(model,x) 
l<-union(pred,y)
mtab<-table(factor(pred,l),factor(y,l))
cm[[15]]<-c("Bagging CART","BAG-CART",confusionMatrix(mtab))
cm[[15]]$table
cm[[15]]$overall[1]

#3.C6 Random Forest
library(randomForest) 
model<-randomForest(fmla,data=tal_eval) 
pred<-predict(model,x) 
l<-union(pred,y)
mtab<-table(factor(pred,l),factor(y,l))
cm[[16]]<-c("Random Forest","RF",confusionMatrix(mtab))
cm[[16]]$table

cm[[16]]$overall[1]

#C7 Gradient Boosted Machine
library(gbm) 
model<-gbm(fmla,data=tal_eval,n.trees=5000,interaction.depth=nlev,shrinkage=0.001,bag.fraction=0.8,distribution="multinomial",verbose=FALSE,n.cores=4) 
prob<-predict(model,x,n.trees=5000,type="response")
pred<-apply(prob,1,which.max)
pred[which(pred=="1")]<-levels(y)[1]
pred[which(pred=="2")]<-levels(y)[2]
pred[which(pred=="3")]<-levels(y)[3]
pred[which(pred=="4")]<-levels(y)[4]
pred<-as.factor(pred)
l<-union(pred,y)
mtab<-table(factor(pred,l),factor(y,l))
cm[[17]]<-c("Gradient Boosted Machine","GBM",confusionMatrix(mtab))
cm[[17]]$table
cm[[17]]$overall[1]

#C8 Boosted C5.0
library(C50) 
model<-C5.0(fmla,data=tal_eval,trials=10) 
# tree <- rpart(model,data=tal_eval,control=rpart.control(minsplit=20,cp=0,digits=6))
# prp(tree,faclen=3)
pred<-predict(model,x) 
l<-union(pred,y)
mtab<-table(factor(pred,l),factor(y,l))
cm[[18]]<-c("Boosted C5.0","BOOST-C50",confusionMatrix(mtab))
cm[[18]]$table
cm[[18]]$overall[1]

#3.C9 JRip
library(RWeka)
model<-JRip(fmla,data=tal_eval)
pred<-predict(model,x)
l<-union(pred,y)
mtab<-table(factor(pred,l),factor(y,l))
cm[[19]]<-c("JRip","JRIP",confusionMatrix(mtab))
cm[[19]]$table
cm[[19]]$overall[1]

#3.C10 Deep Learning
ne<-6
library(h2o)
localH2O=h2o.init(nthreads=-1)
h2o.no_progress()
tal_eval.hex=h2o.uploadFile(path=paste0("/home/peopleanalytics/Desktop/tal_eval1.csv"))
model<-h2o.deeplearning(x=1:(ne-1), y=ne, training_frame = tal_eval.hex, variable_importances =TRUE)
prob<-h2o.predict(model,tal_eval.hex)
prob1<-as.matrix(prob[,1])
table(prob1)
l<-union(prob1,y)
mtab<-table(factor(prob1,l),factor(y,l))
cm[[17]]<-c("h2o Deep learning","DPL",confusionMatrix(mtab))
cm[[17]]$table
cm[[17]]$overall[1]
h2o.shutdown(prompt = FALSE)

#3.C10 XGB
set.seed(42)
library(xgboost)
model_xgb <- caret::train(class ~ .,
                          data = tal_eval,
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
table(predict(model_xgb, x), y)
confusionMatrix(predict(model_xgb, x), y)
results <- data.frame(actual = y,predict(model_xgb, x, type = "prob"))

mtab<-table(predict(model_xgb, x), y)
cm[[20]]<-c("XGB","XGB",confusionMatrix(mtab))
cm[[20]]$table
cm[[20]]$overall[1]

#Step 4. Performance Comparison
library(microbenchmark)
mbm<-microbenchmark(
  m1<-multinom(fmla, data = tal_eval, maxit = 500, trace=FALSE),
  m2<-vglm(fmla, family = "multinomial", data = tal_eval, maxit = 100),
  m3<-lda(fmla,data=tal_eval),
  m4<-mda(fmla,data=tal_eval),
  m5<-rda(fmla,data=tal_eval,gamma = 0.05,lambda = 0.01),  
  m6<-nnet(fmla,data=tal_eval,size = 4, decay = 0.0001, maxit = 700,trace=FALSE),
  m7<-fda(fmla,data=tal_eval),
  m8<-ksvm(fmla,data=tal_eval), 
  m9<-knn3(fmla,data=tal_eval,k=nlev+1), 
  m10<-naiveBayes(fmla,data=tal_eval,k=nlev+1), 
  m11<-rpart(fmla,data=tal_eval), 
  m12<-OneR(fmla,data=tal_eval), 
  m13<-J48(fmla,data=tal_eval), 
  m14<-PART(fmla,data=tal_eval), 
  m15<-bagging(fmla,data=tal_eval), 
  m16<-randomForest(fmla,data=tal_eval), 
  m18<-C5.0(fmla,data=tal_eval,trials=10), 
  m19<-JRip(fmla,data=tal_eval))
  
mbm<-microbenchmark(
  m1<-multinom(fmla, data = tal_eval, maxit = 500, trace=FALSE),
  m2<-vglm(fmla, family = "multinomial", data = tal_eval, maxit = 100),
  m3<-lda(fmla,data=tal_eval),
  m4<-mda(fmla,data=tal_eval),
  m5<-rda(fmla,data=tal_eval,gamma = 0.05,lambda = 0.01))

mbm2<-microbenchmark(
m6<-nnet(fmla,data=tal_eval,size = 4, decay = 0.0001, maxit = 700,trace=FALSE),
m7<-fda(fmla,data=tal_eval),
m8<-ksvm(fmla,data=tal_eval), 
m9<-knn3(fmla,data=tal_eval,k=nlev+1), 
m10<-naiveBayes(fmla,data=tal_eval,k=nlev+1))

mbm3<-microbenchmark(
m11<-rpart(fmla,data=tal_eval), 
m12<-OneR(fmla,data=tal_eval), 
m13<-J48(fmla,data=tal_eval), 
m14<-PART(fmla,data=tal_eval), 
m15<-bagging(fmla,data=tal_eval))
m17<-gbm(fmla,data=tal_eval,n.trees=5000,interaction.depth=nlev,shrinkage=0.001,bag.fraction=0.8,distribution="multinomial",verbose=FALSE,n.cores=4),
mbm4<-microbenchmark(
m15<-bagging(fmla,data=tal_eval), 
m16<-randomForest(fmla,data=tal_eval), 
m18<-C5.0(fmla,data=tal_eval,trials=10), 
m19<-JRip(fmla,data=tal_eval))
m17<-gbm(fmla,data=tal_eval,n.trees=5000,interaction.depth=nlev,shrinkage=0.001,bag.fraction=0.8,distribution="multinomial",verbose=FALSE,n.cores=4)


library(dplyr)
models<-length(cm)
mbm$expr<-rep(sapply(1:models, function(i) {cm[[i]][[2]]}),5)
mbm<-aggregate(x=mbm$time,by=list(Model=mbm$expr),FUN=mean)
mbm$x<-mbm$x/min(mbm$x)
results<-sapply (1:models, function(i) {c(cm[[i]][[1]],cm[[i]][[2]],mbm$x[i],cm[[i]]$overall[1:6])})
row.names(results)<-c("Description","Model","Model_Time_X",names(cm[[1]]$overall[1:6]))
results<-as.data.frame(t(results))
results[,3:9]<-sapply(3:9,function(i){results[,i]<-as.numeric(levels(results[,i])[results[,i]])})
results<-results[,-(8:9)]
results<-arrange(results,desc(Accuracy))
results

library(ggplot2)
library(gridExtra)
res<-data.frame(Model=results$Model,Accuracy=results$Accuracy,Speed=1/results$Model_Time_X,Overall=results$Accuracy/results$Model_Time_X)
library(RColorBrewer)
myPalette <- colorRampPalette(rev(brewer.pal(12, "Set3")))
sc <- scale_colour_gradientn(colours = myPalette(256), limits=c(0.8, 1))

kable(res)
g<-ggplot(res,aes(x=reorder(Model,-Accuracy),y=Accuracy,fill=Model))+
  geom_bar(stat="identity")+
  coord_polar(theta="x",direction=1)+
  labs(x="Machine Learning Model",y="Prediction Accuracy")+
  theme(legend.position="bottom",legend.box="horizontal")+
  ggtitle('Car Evaluation Dataset Accuracy Performance')
g



