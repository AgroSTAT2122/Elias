####################
####################
#### Packages
####################
####################


require(mlbench) ## Dataset
require(nnet) ## NN
require(NeuralNetTools) ## plotnet function
require(caret)
require(rpart) ## CART
require(randomForest) ## RF
require(gbm) ## Boosting via the gbm function
require(adabag) ## Boosting vie the boosting function

require(proxy) ## dist with Jaccard
require(networkD3) ## network

####################
####################
#### Data
####################
####################

data(LetterRecognition)
data <- LetterRecognition
n.train <- 1000

w <- sample(1:nrow(LetterRecognition),n.train)
length(w)
data.train <- LetterRecognition[w,]
data.test <- LetterRecognition[-w,]

mod.RF <- randomForest(lettr ~ .,  data = data.train,importance=TRUE)
mod.RF$mtry
#500 arbres
#4 variables candidats par arbre

pred.RF <- predict(mod.RF,newdata=data.test, type = "response")

cM <- caret::confusionMatrix(factor(pred.RF,levels=levels(data.test$lettr)),reference=data.test$lettr)
cM$overall["Accuracy"]

mod.RF2000 <- randomForest(lettr~., data = data.train, importance=TRUE, ntree=2000)

pred.RF2000 <- predict(mod.RF2000,newdata=data.test, type = "response")
cM2000 <- caret::confusionMatrix(factor(pred.RF2000,levels=levels(data.test$lettr)),reference=data.test$lettr)
cM2000$overall["Accuracy"]
varImpPlot(mod.RF2000)

####################
####################
#### Neural Network - nnet
####################
####################

?nnet

Neur1 <- nnet(lettr ~ ., data = data.train, size = 1, maxit= 1000)
Neur4 <- nnet(lettr ~ ., data = data.train, size = 4, maxit=1000)
Neur10 <- nnet(lettr ~ ., data = data.train, size = 10, maxit = 2000)
Neur18 <- nnet(lettr ~ ., data = data.train, size = 18, maxit= 5000)
# Neur30 <- nnet(lettr ~ ., data = data.train, size = 30)

PredN1 <- predict(Neur1, newdata = data.test, type = "class")
PredN4 <- predict(Neur4, newdata = data.test, type = "class")
PredN10 <- predict(Neur10, newdata = data.test, type = "class")
PredN18 <- predict(Neur18, newdata = data.test, type = "class")
# PredN30 <- predict(Neur30, newdata = data.test, type = "class")

table(PredN1)
table(PredN4)
table(PredN10)
table(PredN18)

cM1 <- caret::confusionMatrix(factor(PredN1,levels=levels(data.test$lettr)),reference=data.test$lettr)
cM1$overall["Accuracy"]
cM4 <- caret::confusionMatrix(factor(PredN4,levels=levels(data.test$lettr)),reference=data.test$lettr)
cM4$overall["Accuracy"]
cM10 <- caret::confusionMatrix(factor(PredN10,levels=levels(data.test$lettr)),reference=data.test$lettr)
cM10$overall["Accuracy"]
cM18 <- caret::confusionMatrix(factor(PredN18,levels=levels(data.test$lettr)),reference=data.test$lettr)
cM18$overall["Accuracy"]

plotnet(Neur1)
plotnet(Neur4)
plotnet(Neur10)
plotnet(Neur18)
