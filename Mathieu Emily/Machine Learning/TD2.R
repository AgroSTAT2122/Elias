#Machine learning

#TD 2
library(lme4)
#Générer un jeu de données

n.points.train = 200

my.f <- function(x){
  x[2]-((3+cos(4*pi*x[1]))/8 + (x[1]^2)/2)
}

data.train <- data.frame(x1=runif(n.points.train),x2=runif(n.points.train))

prob <- apply(data.train,1,FUN=my.f)

class <- runif(n.points.train) < prob

data.train$class <- as.factor(class)

levels(data.train$class) <- c(0,1)

n.points.train2 = 5000

data.test <- data.frame(x1=runif(n.points.train2), x2 = runif(n.points.train2))

#2

mod <- glm(class ~ ., data = data.train,family = "binomial")

data.frame <- data.frame(x1 = 0.5, x2 = 0.5)###----

predict.glm(mod,newdata = data.frame,type = "response")

x <- seq(0,1,0.01)

my.pch <- rep(21,times=n.points.train)
w <- which(data.train$class)
my.pch[w] <- 24
par(mar=c(4,4,0.1,0.1))
plot(data.train$x1,data.train$x2,col=2*as.numeric(1+class),pch=my.pch,xlim=c(0,1),ylim=c(0,1),bg="grey",xlab="X1",ylab="X2",cex=2)
lines(x,(3+cos(4*pi*x))/8+(x^2)/2,type="l",ylim=c(0,1),lty=1,lwd=2)


Bayes <- function(x){x[2]-((3+cos(4*pi*x[1]))/8 + (x[1]^2)/2)>0}

get.error <- function(class,pred){
  cont.tab <- table(class,pred)
  print(cont.tab)
  return((cont.tab[2,1]+cont.tab[1,2])/(sum(cont.tab)))
}

pred.train.Bayes <- apply(data.train[,1:2],1,FUN=Bayes)

table(pred.train.Bayes,data.train$class)
err.Bayes <- get.error(data.train$class,pred.train.Bayes)
err.Bayes

KNN3 <- knn(train=data.train[,1:2],test=data.train[,1:2],cl=data.train[,3],k = 3)

err.KNN3 <- get.error(data.train$class,KNN3)

err.k <- rep(0,20)
k.vek <- seq(1,20)
for (i in (1:20)){
  knn <- knn(train=data.train[,1:2],test=data.train[,1:2],cl=data.train[,3],k = i)
  err.k[i] <- get.error(data.train$class,pred.train.Bayes)
}
plot(err.k~k.vek)#----

hist(predict(mod,newdata = data.train,type = "response"))

table(data.train$class)

glm