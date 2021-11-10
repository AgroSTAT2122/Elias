###
### Data - 3 classes

library(caret)

data(iris)


names(iris)

plot(iris$Sepal.Length,iris$Sepal.Width,col=as.numeric(iris$Species),pch=19)


#################
#################
### kNN
#################
#################

method <- "knn"

##################
### Validation set approach
##################


trainIndex <- createDataPartition(iris$Species, p = .6,times = 1,list=FALSE)
iris.train <- iris[ trainIndex,]
iris.test  <- iris[-trainIndex,]

fitControl <- trainControl(method = "none")

modTT <- train(Species~ Sepal.Length+Sepal.Width,data=iris.train,method=method,trControl = fitControl)
pred.train1 <- predict(modTT,newdata=iris.test)

CM1 <- confusionMatrix(data = pred.train1, reference = iris.test$Species)

cat("Accuracy is :",CM1$overall["Accuracy"],"\n")

### Resampling by hand

n.resample <- 10

Acc <- rep(NA,times=n.resample)


for (i in 1:n.resample){
	print(i)
	trainIndex <- createDataPartition(iris$Species, p = .6,list = FALSE,times = 1)
	iris.train <- iris[ trainIndex,]
	iris.test  <- iris[-trainIndex,]


	modTT1 <- train(Species~ Sepal.Length+Sepal.Width,data=iris.train,method=method,trControl = fitControl)
	pred.train1 <- predict(modTT1,newdata=iris.test)

	CM1 <- confusionMatrix(data = pred.train1, reference = iris.test$Species)

	Acc[i] <- CM1$overall["Accuracy"]
}

dd <- data.frame(Acc=Acc)
boxplot(dd)

names(CM1)





?predict.train


tmp.dta <- data.frame(obs=iris.test$Species,pred=pred.train1)

multiClassSummary(tmp.dta, lev = levels(iris$Species), model = NULL)
confusionMatrix(data = pred.train1, reference = iris.test$Species)


##################
##################
## Resampling using the \code{trainControl}
##################
##################

##################
## LGOCV
##################

fitControl.LGOCV <- trainControl(method = "LGOCV",number=100,p=0.6)
mod.LGOCV <- train(Species~ Sepal.Length+Sepal.Width,data=iris.train,method=method,trControl = fitControl.LGOCV)
mod.LGOCV
mod.LGOCV$bestTune


fitControl.LGOCV <- trainControl(method = "LGOCV",number=100,p=0.6)
mod.LGOCV <- train(Species~ Sepal.Length+Sepal.Width,data=iris.train,method=method,trControl = fitControl.LGOCV,tuneLength=10)
mod.LGOCV
mod.LGOCV$bestTune

grid <- data.frame(k=seq(1,50,by=2))
fitControl.LGOCV <- trainControl(method = "LGOCV",number=100,p=0.6)
mod.LGOCV <- train(Species~ Sepal.Length+Sepal.Width,data=iris.train,method=method,trControl = fitControl.LGOCV,tuneGrid=grid)
mod.LGOCV
mod.LGOCV$bestTune


plot(mod.LGOCV,metric="Accuracy")
ggplot(mod.LGOCV,metric="Accuracy")

mod.LGOCV$results$ymin <- mod.LGOCV$results$Accuracy-2*(mod.LGOCV$results$AccuracySD/sqrt(mod.LGOCV$control$number))
mod.LGOCV$results$ymax <- mod.LGOCV$results$Accuracy+2*(mod.LGOCV$results$AccuracySD/sqrt(mod.LGOCV$control$number))
ggplot(mod.LGOCV)+geom_errorbar(data= mod.LGOCV$results,mapping=aes(x=k,ymin=ymin,ymax=ymax),width=0.2, size=1, color="blue")


fitControl.LGOCV <- trainControl(method = "LGOCV",number=100,p=0.6,classProbs=TRUE,summaryFunction= multiClassSummary)
grid <- data.frame(k=seq(1,50,by=2))
mod.LGOCV <- train(Species~ Sepal.Length+Sepal.Width,data=iris.train,method=method,trControl = fitControl.LGOCV,tuneGrid=grid,metric="ROC")
mod.LGOCV$results
mod.LGOCV$bestTune
ggplot(mod.LGOCV,metric="AUC")

mod.LGOCV <- train(Species~ Sepal.Length+Sepal.Width,data=iris.train,method=method,trControl = fitControl.LGOCV,tuneGrid=grid,metric="Mean_Balanced_Accuracy")
ggplot(mod.LGOCV,metric="AUC")

##################
## LOOCV
##################

fitControl.LOOCV <- trainControl(method = "LOOCV")
mod.LOOCV <- train(Species~ Sepal.Length+Sepal.Width,data=iris.train,method=method,trControl = fitControl.LOOCV)
mod.LOOCV

##################
## repeatedCV
##################

fitControl.repeatedcv <- trainControl(method = "repeatedcv",number=10,repeats=20)
mod.repeatedcv <- train(Species~ Sepal.Length+Sepal.Width,data=iris.train,method=method,trControl = fitControl.repeatedcv)
mod.repeatedcv

##################
## boot
##################


fitControl.boot <- trainControl(method = "boot",number=50)
mod.boot <- train(Species~ Sepal.Length+Sepal.Width,data=iris.train,method=method,trControl = fitControl.boot)
mod.boot

#################
#################
### Multinomial
#################
#################



##
##
fitControl.repeatedcv <- trainControl(method = "repeatedcv",number=10,repeats=20)

mod.repeatedcv.multinom <- train(Species~ Sepal.Length,data=iris.train,method="multinom",trControl =fitControl.repeatedcv,tuneLength=1)
mod.repeatedcv.multinom
plot(mod.repeatedcv.multinom)

#################
#################
### Model comparaison 
#################
#################

fitControl.repeatedcv <- trainControl(method = "repeatedcv",number=10,repeats=50,classProbs=TRUE,summaryFunction= multiClassSummary)

mod.repeatedcv.knn <- train(Species~ Sepal.Length+Sepal.Width,data=iris.train,method="knn",trControl = fitControl.repeatedcv, tuneLength=9,metric="AUC")

mod.repeatedcv.lda <- train(Species~ Sepal.Length+Sepal.Width,data=iris.train,method="lda",trControl = fitControl.repeatedcv,metric="AUC")

mod.repeatedcv.qda <- train(Species~ Sepal.Length+Sepal.Width,data=iris.train,method="qda",trControl = fitControl.repeatedcv,metric="AUC")


mod.repeatedcv.multinom <- train(Species~ Sepal.Length,data=iris.train,method="multinom",trControl =fitControl.repeatedcv,tuneLength=1,metric="AUC")

cvValues <- resamples(list(knn=mod.repeatedcv.knn,lda=mod.repeatedcv.lda,qda=mod.repeatedcv.qda,multinom=mod.repeatedcv.multinom))

summary(cvValues,metric="AUC")

## Vizualizing
splom(cvValues,metric="AUC")
bwplot(cvValues,metric="AUC")
dotplot(cvValues,metric="AUC")
parallelplot(cvValues,metric="AUC")
xyplot(cvValues,metric="AUC")


## Predicting
Aucdiff <- diff(cvValues,metric="AUC")
summary(Aucdiff)
dotplot(Aucdiff)