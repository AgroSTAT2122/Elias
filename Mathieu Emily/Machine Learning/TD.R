#Machine learning

#Session 1 - TD 1

#Générer un jeu de données

x <- seq(0,1,0.01)

plot(x,(3+cos(4*pi*x))/8+(x^2)/2,type="l",ylim=c(0,1),lty=2) #Plot

n.points.train = 100

x1=runif(n.points.train)
x2=runif(n.points.train)

plot(x1,x2,ylim=c(0,1))

#Mettre l'appartenance à une classe sous forme de probabilité d'appartenir à cette classe

#Passage par un "intermédiaire" pour le faire
#P(Y=1|X=x)

#Un point sur la courbe, on lui donne une probabiltié de 0,5
#Plus on s'éloigne de la courbe plus la proba va tendre vers 0 (vers le bas) ou vers 1 (vers le haut)

my.f <- function(x,deg=0.5){
  tmp <- (x[2]-((3+cos(4*pi*x[1]))/8+(x[1]^2)/2))
  d <- sign(tmp)*abs(tmp)^(deg)
  return(0.5+d)
}

n.points.train <- 10000

data.train <- data.frame(x1=runif(n.points.train),x2=runif(n.points.train))
#data.train <- data.frame(x1=rnorm(n.points.train,0.5,sd=0.2),x2=rnorm(n.points.train,0.5,sd=0.2))
prob <- apply(data.train,1,FUN=my.f,deg=1)
class <- runif(n.points.train) < prob
data.train$class <- class

my.pch <- rep(21,times=n.points.train)
w <- which(data.train$class)
my.pch[w] <- 24
par(mar=c(4,4,0.1,0.1))
plot(data.train$x1,data.train$x2,col=2*as.numeric(1+class),pch=my.pch,xlim=c(0,1),ylim=c(0,1),bg="grey",xlab="X1",ylab="X2",cex=2)
lines(x,(3+cos(4*pi*x))/8+(x^2)/2,type="l",ylim=c(0,1),lty=1,lwd=2)

#Le d (degré) permet de dire comment la "frontière se casse". Plus il est petit, plus ce sera brutal en terme de proba
#Le d permet d'affiner la distinction.

#On a utilisé un modèle pour simuler les données, pour simuler des choses plus ou moins réelles

#Bayes 

#Definition 1 The Bayes classifier assigns each observation to the most likely class, given its predictor value
#Je connais "f"

get.error <- function(class,pred){
  cont.tab <- table(class,pred)
  print(cont.tab)
  return((cont.tab[2,1]+cont.tab[1,2])/(sum(cont.tab)))
}

pred.train.Bayes <- apply(data.train,1,FUN=function(x){(x[2]-((3+cos(4*pi*x[1]))/8+(x[1]^2)/2))>0})

table(pred.train.Bayes,data.train$class)
err.Bayes <- get.error(data.train$class,pred.train.Bayes)

library(class)

KNN3 <- knn(train=data.train[,1:2],test=data.train[,1:2],cl=data.train[,3],k = 3)

err.KNN3 <- get.error(data.train$class,KNN3)

err.k <- rep(0,20)
k.vek <- seq(1,20)
for (i in (1:20)){
  knn <- knn(train=data.train[,1:2],test=data.train[,1:2],cl=data.train[,3],k = i)
  err.k[i] <- get.error(data.train$class,pred.train.Bayes)
}
plot(err.k~k.vek)
