get.error <- function(class,pred){
	cont.tab <- table(class,pred)
	print(cont.tab)
	return((cont.tab[2,1]+cont.tab[1,2])/(sum(cont.tab)))
}

get.sensitivity <- function(class,pred){
	cont.tab <- table(class,pred)
	return((cont.tab[2,2])/(sum(cont.tab[2,])))
}


get.specificity <- function(class,pred){
	cont.tab <- table(class,pred)
	return((cont.tab[1,1])/(sum(cont.tab[1,])))
}

########################
### 1. Data Set
########################

require("ISLR")
require("class")
require("MASS")
data(Default)

names(Default)

summary(Default$default)

plot(Default$balance,Default$income,col=Default$default,pch=19)

########################
### 2. lda and qda (+ logistic + knn1 and knn20)
########################

grid.balance <- seq(min(Default$balance), max(Default$balance), length.out = 1000)
grid.income <- seq(min(Default$income), max(Default$income), length.out = 1000)
data.grid <- expand.grid(balance = grid.balance, income = grid.income)
plot(data.grid,col=4,pch=20)
# points(Default$balance,Default$income,col=Default$default,pch=19)

####
#
n <- nrow(Default)
n.train <- n/4
n.test <- n-n.train
ind.train <- sample(1:nrow(Default),n.train)
data.train <- Default[ind.train,]
data.test <- Default[-ind.train,]

####
#### lda
# 

mod.lda <- lda(default ~ balance+income,data=data.train)
pred.lda.test <- predict(mod.lda,newdata=data.test)$class
get.error(data.test$default,pred.lda.test)
get.specificity(data.test$default,pred.lda.test)
get.sensitivity(data.test$default,pred.lda.test)

pred.lda.grid <- predict(mod.lda,newdata=data.grid)$class
plot(data.grid,col=as.numeric(pred.lda.grid)+1)

plot(c(min(Default$balance), max(Default$balance)),c(min(Default$income), max(Default$income)),col=0,xlab="balance",ylab="income")
contour(grid.balance, grid.income,matrix(as.numeric(pred.lda.grid),ncol=length(grid.income)),nlevels=1,add=TRUE,drawlabels=FALSE,col="black",lwd=1)
points(data.train$balance,data.train$income,pch=19,col=(2*as.numeric(data.train$default)+1))


####
#### qda
# 

mod.qda <- qda(default ~ balance+income,data=data.train)
pred.qda.test <- predict(mod.qda,newdata=data.test)$class
get.error(data.test$default,pred.qda.test)
get.specificity(data.test$default,pred.qda.test)
get.sensitivity(data.test$default,pred.qda.test)

pred.qda.grid <- predict(mod.qda,newdata=data.grid)$class

contour(grid.balance, grid.income,matrix(as.numeric(pred.qda.grid),ncol=length(grid.income)),nlevels=1,add=TRUE,drawlabels=FALSE,col="blue",lwd=1)


####
#### Logistic

mod.logistic <- glm(default ~ balance+income,data=data.train,family="binomial")

pred.logistic.test <- predict(mod.logistic,newdata=data.test,type="response") > 0.5
get.error(data.test$default,pred.logistic.test)
get.specificity(data.test$default,pred.logistic.test)
get.sensitivity(data.test$default,pred.logistic.test)

pred.logistic.grid <- predict(mod.logistic,newdata=data.grid,type="response") > 0.5

contour(grid.balance, grid.income,matrix(as.numeric(pred.logistic.grid),ncol=length(grid.income)),nlevels=1,add=TRUE,drawlabels=FALSE,col="red",lwd=1)

####
#### knn:1 

pred.test.knn.1 <- knn(train=data.train[,3:4],test=data.test[,3:4],cl=data.train$default,k=1)

get.error(data.test$default,pred.test.knn.1)
get.specificity(data.test$default,pred.test.knn.1)
get.sensitivity(data.test$default,pred.test.knn.1)

pred.grid.knn.1 <- knn(train=data.train[,3:4],test=data.grid,cl=data.train$default,k=1)

contour(grid.balance, grid.income,matrix(as.numeric(pred.grid.knn.1),ncol=length(grid.income)),nlevels=1,add=TRUE,drawlabels=FALSE,col="cyan",lwd=1)

####
#### knn:20

pred.test.knn.20 <- knn(train=data.train[,3:4],test=data.test[,3:4],cl=data.train$default,k=20)

get.error(data.test$default,pred.test.knn.20)
get.specificity(data.test$default,pred.test.knn.20)
get.sensitivity(data.test$default,pred.test.knn.20)

pred.grid.knn.20 <- knn(train=data.train[,3:4],test=data.grid,cl=data.train$default,k=20)

contour(grid.balance, grid.income,matrix(as.numeric(pred.grid.knn.20),ncol=length(grid.income)),nlevels=1,add=TRUE,drawlabels=FALSE,col=grey(0.5),lwd=5)


########################
### 3 Hyperparameters in knn
########################

########################
### 3.1 knn20 vs knn1
########################

library(caret)

cM.knn20 <- confusionMatrix(pred.test.knn.20,reference=data.test$default,mod="everything")
cM.knn1 <- confusionMatrix(pred.test.knn.1,reference=data.test$default,mod="everything")

cM.knn20$table
cM.knn1$table

tab.All <- data.frame(
  knn20=c(cM.knn20$overall[c(1,2)],cM.knn20$byClass),
  knn1=c(cM.knn1$overall[c(1,2)],cM.knn1$byClass)
)

tab.All[c(1,2,3,4,7,8,9,13),]

########################
### 3.2 hyperparameter selection with accuracy, kappa, ROC, Spec, F
########################

fitControl <- trainControl(method = "LGOCV",
                           number=10)

fitControl.TwoClass <- trainControl(method = "LGOCV",
                                    number=10,
                                    classProbs = TRUE,
                                    summaryFunction = twoClassSummary)
fitControl.prS <- trainControl(method = "LGOCV",
                               number=10,
                               classProbs = TRUE,
                               summaryFunction = prSummary)

model_knn.acc <- caret::train(form=default ~ balance+income,
                              data = data.train,
                              method = "knn",
                              trControl = fitControl,
                              tuneGrid=data.frame(k=1:15),
                              metric="Accuracy")

model_knn.kappa <- caret::train(default ~ balance+income,
                                data = data.train,
                                method = "knn",
                                trControl = fitControl,
                                tuneGrid=data.frame(k=1:15),
                                metric="Kappa")

model_knn.AUC <- caret::train(default ~ balance+income,
                              data = data.train,
                              method = "knn",
                              trControl = fitControl.TwoClass,
                              tuneGrid=data.frame(k=1:15),
                              metric="ROC")
model_knn.Spec <- caret::train(default ~ balance+income,
                               data = data.train,
                               method = "knn",
                               trControl = fitControl.TwoClass,
                               tuneGrid=data.frame(k=1:15),
                               metric="Spec")
model_knn.F <- caret::train(default ~ balance+income,
                            data = data.train,
                            method = "knn",
                            trControl = fitControl.prS,
                            tuneGrid=data.frame(k=1:15),
                            metric="F")

########################
### 3.3 hyperparameter selection with other sampling strategy
########################

mod.Acc.down <- train(default ~ balance+income, data = data.train,
                      method = "knn",
                      metric = "Accuracy",
                      tuneGrid = expand.grid(k = 1:20),
                      trControl = trainControl(method = "LGOCV",
                                               number = 10,
                                               sampling="down"))

mod.Acc.up <- train(default ~ balance+income, data = data.train,
                    method = "knn",
                    metric = "Accuracy",
                    tuneGrid = expand.grid(k = 1:20),
                    trControl = trainControl(method = "LGOCV",
                                             number = 10,
                                             sampling="up"))

mod.Acc.rose <- train(default ~ balance+income, data = data.train,
                      method = "knn",
                      metric = "Accuracy",
                      tuneGrid = expand.grid(k = 1:20),
                      trControl = trainControl(method = "LGOCV",
                                               number = 10,
                                               sampling="rose"))

mod.Acc.smote <- train(default ~ balance+income, data = data.train,
                       method = "knn",
                       metric = "Accuracy",
                       tuneGrid = expand.grid(k = 1:20),
                       trControl = trainControl(method = "LGOCV",
                                                number = 10,
                                                sampling="smote"))

########################
### 4. Thresholding for logistic
########################


pred.logistic.test.2 <- predict(mod.logistic,newdata=data.test,type="response") > 0.2
get.error(data.test$default,pred.logistic.test.2)
get.specificity(data.test$default,pred.logistic.test.2)
get.sensitivity(data.test$default,pred.logistic.test.2)

thresh.vec <- seq(0.01,0.8,by=0.02)
acc.vec <- rep(NA,times=length(thresh.vec))
spe.vec <- rep(NA,times=length(thresh.vec))
sen.vec <- rep(NA,times=length(thresh.vec))
pred.prob <- pred.logistic.test.2 <- predict(mod.logistic,newdata=data.test,type="response")

for (i in 1:length(thresh.vec)){
	pred.cur <- pred.prob > thresh.vec[i]
	acc.vec[i] <- get.error(data.test$default, pred.cur)
	spe.vec[i] <- 1-get.specificity(data.test$default, pred.cur)
	sen.vec[i] <- 1-get.sensitivity(data.test$default, pred.cur)
}

plot(thresh.vec,acc.vec,type="b",ylim=c(0,max(acc.vec,spe.vec,sen.vec)))
lines(thresh.vec,spe.vec,type="b",col=2,pch=2)
lines(thresh.vec,sen.vec,type="b",col=3,pch=3)

library(pROC)

roc.logistic <- roc(data.test$default,predict(mod.logistic,newdata=data.test,type="response"))
auc(roc.logistic)
plot(roc.logistic)

coords(roc.logistic,x="best",best.method="closest.topleft",transpose=TRUE)

roc.lda <- roc(data.test$default,predict(mod.lda,newdata=data.test)$posterior[,2])
plot(roc.lda)
auc(roc.lda)

coords(roc.lda,x="best",best.method="closest.topleft",transpose=TRUE)

roc.qda <- roc(data.test$default,predict(mod.qda,newdata=data.test)$posterior[,2])
plot(roc.qda)
auc(roc.qda)

coords(roc.qda,x="best",best.method="closest.topleft",transpose=TRUE)

