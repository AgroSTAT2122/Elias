x <- seq(0,1,0.01)

plot(x,(3+cos(4*pi*x))/8+(x^2)/2,type="l",ylim=c(0,1),lty=2)


get.error <- function(class,pred){
	cont.tab <- table(class,pred)
	print(cont.tab)
	return((cont.tab[2,1]+cont.tab[1,2])/(sum(cont.tab)))
}

############################
############################
# Simulation dataset
############################
############################


my.f <- function(x,deg=0.5){
	tmp <- (x[2]-((3+cos(4*pi*x[1]))/8+(x[1]^2)/2))
	d <- sign(tmp)*abs(tmp)^(deg)
	return(0.5+d)
}


n.points.train <- 100

data.train <- data.frame(x1=runif(n.points.train),x2=runif(n.points.train))
#data.train <- data.frame(x1=rnorm(n.points.train,0.5,sd=0.2),x2=rnorm(n.points.train,0.5,sd=0.2))
prob <- apply(data.train,1,FUN=my.f,deg=0.5)
class <- runif(n.points.train) < prob
data.train$class <- class

my.pch <- rep(21,times=n.points.train)
w <- which(data.train$class)
my.pch[w] <- 24
par(mar=c(4,4,0.1,0.1))
plot(data.train$x1,data.train$x2,col=2*as.numeric(1+class),pch=my.pch,xlim=c(0,1),ylim=c(0,1),bg="grey",xlab="X1",ylab="X2",cex=2)
lines(x,(3+cos(4*pi*x))/8+(x^2)/2,type="l",ylim=c(0,1),lty=1,lwd=2)

############################
############################
# Bayes classifier
############################
############################


pred.train.Bayes <- apply(data.train,1,FUN=function(x){
	(x[2]-((3+cos(4*pi*x[1]))/8+(x[1]^2)/2)) > 0
	})

err.Bayes <- get.error(data.train$class,pred.train.Bayes)


############################
############################
# kNN classifier
############################
############################

require(class)

k.vec <- seq(20,1)

err.k <- rep(NA,times=length(k.vec))

for (i in 1:length(k.vec)){
	k <- k.vec[i]
	pred.train.knn <- knn(data.train[,1:2],data.train[,1:2],cl=data.train$class,k=k)
	err.k[i] <- get.error(data.train$class, pred.train.knn)
}

plot(k.vec,err.k,type="b",ylim=c(0,max(max(err.k),err.Bayes)))
abline(h=err.Bayes,col=2)


### Mean evaluation

n.sim <- 100

err.Bayes.sim <- rep(NA,times=n.sim)
err.knn.sim <- matrix(NA,ncol=length(k.vec),nrow=n.sim)

for (i in 1:n.sim){
	print(i)
	data.train <- data.frame(x1=runif(n.points.train),x2=runif(n.points.train))
	#data.train <- data.frame(x1=rnorm(n.points.train,0.5,sd=0.2),x2=rnorm(n.points.train,0.5,sd=0.2))
	prob <- apply(data.train,1,FUN=my.f)
	class <- runif(n.points.train) < prob
	data.train$class <- class
	
	pred.train.Bayes <- apply(data.train,1,FUN=function(x){
	(x[2]-((3+cos(4*pi*x[1]))/8+(x[1]^2)/2)) > 0
	})
	err.Bayes.sim[i] <- get.error(data.train$class,pred.train.Bayes)

	err.k <- rep(NA,times=length(k.vec))

	for (j in 1:length(k.vec)){
		k <- k.vec[j]
		pred.train.knn <- knn(data.train[,1:2],data.train[,1:2],cl=data.train$class,k=k)
		err.k[j] <- get.error(data.train$class, pred.train.knn)
	}
	err.knn.sim[i,] <- err.k

}


err.knn.sim.mean <- apply(err.knn.sim,2,mean)
plot(k.vec,err.knn.sim.mean,type="b",ylim=c(0,max(max(err.knn.sim.mean),mean(err.Bayes.sim))))
abline(h=mean(err.Bayes.sim),col=2)

############################
############################
# test data set
############################
############################


n.points.test <- 500

data.test <- data.frame(x1=runif(n.points.test),x2=runif(n.points.test))
#data.test <- data.frame(x1=rnorm(n.points.test,0.5,sd=0.2),x2=rnorm(n.points.test,0.5,sd=0.2))
prob <- apply(data.test,1,FUN=my.f)
class <- runif(n.points.test) < prob
data.test$class <- class

# Bayes
pred.test.Bayes <- apply(data.test,1,FUN=function(x){
	(x[2]-((3+cos(4*pi*x[1]))/8+(x[1]^2)/2)) > 0
	})

err.Bayes.test <- get.error(data.test$class,pred.test.Bayes)

# knn
k.vec <- seq(20,1)

err.k.test <- rep(NA,times=length(k.vec))

for (i in 1:length(k.vec)){
	k <- k.vec[i]
	pred.test.knn <- knn(data.train[,1:2],data.test[,1:2],cl=data.train$class,k=k)
	err.k.test[i] <- get.error(data.test$class, pred.test.knn)
}

plot(k.vec,err.k.test,type="b",ylim=c(0,max(max(err.k.test),err.Bayes.test)))
abline(h=err.Bayes.test,col=2)


### Mean evaluation

n.sim <- 100

err.Bayes.test.sim <- rep(NA,times=n.sim)
err.knn.test.sim <- matrix(NA,ncol=length(k.vec),nrow=n.sim)

for (i in 1:n.sim){
	print(i)
	data.test <- data.frame(x1=runif(n.points.test),x2=runif(n.points.test))
	#data.train <- data.frame(x1=rnorm(n.points.train,0.5,sd=0.2),x2=rnorm(n.points.train,0.5,sd=0.2))
	prob <- apply(data.test,1,FUN=my.f)
	class <- runif(n.points.test) < prob
	data.test$class <- class
	
	pred.test.Bayes <- apply(data.test,1,FUN=function(x){
	(x[2]-((3+cos(4*pi*x[1]))/8+(x[1]^2)/2)) > 0
	})
	err.Bayes.test.sim[i] <- get.error(data.test$class,pred.test.Bayes)

	err.k <- rep(NA,times=length(k.vec))

	for (j in 1:length(k.vec)){
		k <- k.vec[j]
		pred.test.knn <- knn(data.train[,1:2],data.test[,1:2],cl=data.train$class,k=k)
		err.k[j] <- get.error(data.test$class, pred.test.knn)
	}
	err.knn.test.sim[i,] <- err.k

}


err.knn.test.sim.mean <- apply(err.knn.test.sim,2,mean)
plot(k.vec,err.knn.test.sim.mean,type="b",ylim=c(0,max(max(err.knn.test.sim.mean),mean(err.Bayes.test.sim))))
abline(h=mean(err.Bayes.test.sim),col=2)

plot(k.vec,err.knn.test.sim.mean,type="b",ylim=c(0,max(max(err.knn.sim.mean),max(err.knn.test.sim.mean),mean(err.Bayes.test.sim))))
lines(k.vec,err.knn.sim.mean,type="b",col=4)
abline(h=mean(err.Bayes.test.sim),col=2)



###########
###########
# Irreducible errors
###########
###########

make.data <- function(n.points.train=100,deg=0.5){
	data.train <- data.frame(x1=runif(n.points.train),x2=runif(n.points.train))
	prob <- apply(data.train,1,FUN=my.f,deg=deg)
	class <- runif(n.points.train) < prob
	data.train$class <- class
	return(data.train)
}

getBayes.err <- function(dta.train,dta){
	pred.train.Bayes <- apply(dta,1,FUN=function(x){
		(x[2]-((3+cos(4*pi*x[1]))/8+(x[1]^2)/2)) > 0
	})
	err.Bayes <- get.error(dta$class,pred.train.Bayes)
	return(err.Bayes)
}

n.points <- 20000

deg.vec <- seq(0,3,by=0.2)
err.vec.Bayes <- rep(NA,times=length(deg.vec))
for (i in 1:length(deg.vec)){
	deg <- deg.vec[i]
	dta <- make.data(n.points,deg=deg)
	err.vec.Bayes[i] <- getBayes.err(dta,dta)
}

plot(deg.vec,err.vec.Bayes,type="b")