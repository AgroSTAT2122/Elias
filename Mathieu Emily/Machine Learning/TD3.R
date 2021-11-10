#Initialisation

require(ISLR)

##1

#1

data <- ISLR::Default

#2

data.train <- data.frame()
n <- nrow(data)
n.train <- n/4
sample <- sample(1:nrow(data),n.train)
data.train <- data[sample,]
data.test <- data[-sample,]

plot(data$income ~ data$balance, pch = 16, col = data$default)

##2

#1

require(MASS)

balanc <- seq(0,3000,length=100)
grid.balance <- seq(min(data$balance),max(data$balance))
incom <- seq(0,80000,length=100)
grid.income <- seq(min(data$income),max(data$income))
data.grid <- expand.grid(income=grid.income,balance=grid.balance)

Linear <- lda(default~income+balance, data = data.train)

pred.grid.Linear <- predict(Linear,newdata=data.grid,type="class")

data.grid$defaultLinear <- integer(nrow(data.grid))
for (i in 1:nrow(data.grid)){
  if (pred.grid.Linear$posterior[i,1] < 0.5){
    data.grid$defaultLinear[i] <- "Yes"
  }
  else {
    data.grid$defaultLinear[i] <- "No"
  }
}


Quadra <- qda(default ~ income+balance, data = data.train)

pred.grid.Quadra <- predict(Quadra,newdata=data.grid,type="class")

data.grid$defaultQuad <- integer(nrow(data.grid))
for (i in 1:nrow(data.grid)){
  if (pred.grid.Logistic$posterior[i,1] < 0.5){
    data.grid$defaultQuad[i] <- "Yes"
  }
  else {
    data.grid$defaultQuad[i] <- "No"
  }
}

table(data.grid$defaultLinear,data.grid$defaultQuad)

linear.data <- data.frame(income=data.grid$income,balance=data.grid$balance,default=data.grid$defaultLinear)
linear.data$default <- as.factor(logi.data$default)
linear.data$default <- as.numeric(logi.data$default)
plot(income ~ balance,data=linear.data,col=2*linear.data$default+1, main = "Logi data")


quad.data <- data.frame(income=data.grid$income,balance=data.grid$balance,default=data.grid$defaultQuad)
quad.data$default <- as.factor(quad.data$default)
quad.data$default <- as.numeric(quad.data$default)
plot(income ~ balance,data=quad.data,col=2*quad.data$default+1, main = "Quad data")


data$default <- as.factor(data$default)
mod <- glm(default ~ income + balance, data, family = binomial)

pred <- predict(mod, newdata = data.grid, type = "response") 


data.grid$defaultLogistique <- integer(nrow(data.grid))
for (i in 1:nrow(data.grid)){
  if (pred[i] < 0.5){
    data.grid$defaultLogistique[i] <- "Yes"
  }
  else {
    data.grid$defaultLogistique[i] <- "No"
  }
}

linear.data <- data.frame(income=data.grid$income,balance=data.grid$balance,default=data.grid$defaultLinear)
linear.data$default <- as.factor(linear.data$default)
linear.data$default <- as.numeric(linear.data$default)
plot(income ~ balance,data=linear.data,col=2*linear.data$default+1, main = "Linear data")


#2

get.error <- function(class,pred){
  cont.tab <- table(class,pred)
  return((cont.tab[2,1]+cont.tab[1,2])/(sum(cont.tab)))
}

require(class)

pred.grid.knn1 <- knn(data.train[,3:4],data.grid[,1:2],cl=data.train$default,k=1)
pred.grid.knn20 <- knn(data.train[,3:4],data.grid[,1:2],cl=data.train$default,k=2)
table(pred.grid.knn20)

plot(c(0,80000),c(0,3000),col=0,xlab="Income",ylab="Balance",main="Logistic")
contour(incom,balanc,matrix(as.numeric(pred.grid.knn1),ncol=length(balanc)),nlevels=1,add=TRUE,drawlabels=FALSE,col=2,lwd=1)
contour(incom,balanc,matrix(as.numeric(pred.grid.knn20),ncol=length(balanc)),nlevels=1,add=TRUE,drawlabels=FALSE,col=4,lwd=1)
