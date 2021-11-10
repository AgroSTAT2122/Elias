x <- seq(0,1,0.01)

plot(x,(3+cos(4*pi*x))/8+(x^2)/2,type="l",ylim=c(0,1),lty=2)


get.error <- function(class,pred){
	cont.tab <- table(class,pred)
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


n.points.train <- 200

data.train <- data.frame(x1=runif(n.points.train),x2=runif(n.points.train))
prob <- apply(data.train,1,FUN=my.f)
class <- runif(n.points.train) < prob
data.train$class <- class

n.points.test <- 5000

data.test <- data.frame(x1=runif(n.points.test),x2=runif(n.points.test))
prob <- apply(data.test,1,FUN=my.f)
class <- runif(n.points.test) < prob
data.test$class <- class

mod <- glm(class ~ x1+x2,data=data.train, family="binomial")
nd <- data.frame(x1=0.5,x2=0.5)
predict(mod,newdata=nd,type="response")
#predict(mod,newdata=nd,type="link")
#predict(mod,newdata=nd,type="terms")

class.test <- predict(mod,newdata=data.test,type="response")
plot(data.test$x1,data.test$x2,col=2*(1+class.test))
points(x,(3+cos(4*pi*x))/8+(x^2)/2,type="l",ylim=c(0,1),lty=6,col=1,lwd=4)
points(0.5,0.5,col=4,pch=19,cex=2)

#Mauvaise classe de modèle choisie, on voit que la classification ne se fait pas
#selon la courbe noire

############################
# Contour - Boundary
############################


mod.deg1 <- glm(class ~ x1+x2,data=data.train,family="binomial")

ss <- seq(0,1,by=0.005)

#x.grid <- rep(ss,times=length(ss))
#y.grid <- rep(ss,each=length(ss))
#data.grid <- data.frame(x1=x.grid,x2=y.grid)

data.grid <- expand.grid(x1=ss,x2=ss)
pred.grid.deg1 <- predict(mod.deg1,newdata=data.grid,type="response") > 0.5

##
tmp.data <- data.frame(x1=data.grid$x1,x2=data.grid$x2,y=pred.grid.deg1)
plot(x2 ~ x1,data=tmp.data,col=2*tmp.data$y+1)

plot(c(0,1),c(0,1),col=0,xlab="X1",ylab="X2",main="Logistic")
lines(x,(3+cos(4*pi*x))/8+(x^2)/2,type="l",ylim=c(0,1),lty=2,lwd=3)
contour(ss,ss,matrix(as.numeric(pred.grid.deg1),ncol=length(ss)),nlevels=1,add=TRUE,drawlabels=FALSE,col=2,lwd=1)

############################
# Comparison of the test error
############################

pred.test.Bayes <- apply(data.test,1,FUN=function(x){
	(x[2]-((3+cos(4*pi*x[1]))/8+(x[1]^2)/2)) > 0
	})

err.Bayes <- get.error(data.test$class,pred.test.Bayes)


require(class)

k.vec <- seq(20,1)

err.k <- rep(NA,times=length(k.vec))

for (i in 1:length(k.vec)){
	k <- k.vec[i]
	pred.test.knn <- knn(data.train[,1:2],data.test[,1:2],cl=data.train$class,k=k)
	err.k[i] <- get.error(data.test$class, pred.test.knn)
}

mod.deg1 <- glm(class ~ x1+x2,data=data.train,family="binomial")
pred.logit.deg1 <- as.numeric(predict(mod.deg1,newdata=data.test,type="response") > 0.5)
err.logit.deg1 <- get.error(data.test$class,pred.logit.deg1)

plot(k.vec,err.k,type="b",ylim=c(0,max(max(err.k),err.Bayes,err.logit.deg1)))
abline(h=err.Bayes,col=2)
abline(h=err.logit.deg1,col=3)
legend("bottomright",legend=c("kNN","Bayes","Logistic"),col=c(1,2,3),lty=1)
############################
# U-shape
############################

make.modele.old <- function(deg){
	form <- "class ~ x1+x2"
	if (deg > 1){
		for (i in 2:deg){
			form <- paste(form,"+I(x1^",i,")","+I(x2^",i,")",sep="")
		}
	}
	form <- as.formula(form)
	mod <- glm(form,data=data.train,family="binomial")
	return(mod)
}

make.modele <- function(deg){
	mod <- glm(class ~ poly(x1,deg)+poly(x2,deg),data=data.train,family="binomial")
	return(mod)
}

###
# Contour
vec.deg <- 1:10

plot(c(0,1),c(0,1),col=0,xlab="X1",ylab="X2",main="Polynomial")
lines(x,(3+cos(4*pi*x))/8+(x^2)/2,type="l",ylim=c(0,1),lty=2,lwd=3)
for (i in 1:length(vec.deg)){
	deg <- vec.deg[i]
	mod <- make.modele(deg)
	pred.grid.deg <- predict(mod,newdata=data.grid,type="response") > 0.5
	contour(ss,ss,matrix(as.numeric(pred.grid.deg),ncol=length(ss)),nlevels=1,add=TRUE,drawlabels=FALSE,col=i,lwd=1)
}


vec.pred.train <- rep(NA, times=length(vec.deg))
vec.pred.test <- rep(NA, times=length(vec.deg))
for (i in 1:length(vec.deg)){
	deg <- vec.deg[i]
	mod <- make.modele(deg)
	pred.deg.train <- predict(mod,newdata=data.train,type="response") > 0.5
	pred.deg.test <- predict(mod,newdata=data.test,type="response") > 0.5
	vec.pred.train[i] <- get.error(data.train$class,pred.deg.train)
	vec.pred.test[i] <- get.error(data.test$class,pred.deg.test)
}


plot(vec.deg,vec.pred.train,type="b",ylim=c(0.1,0.15),col=0)
lines(vec.deg,vec.pred.test,type="b",col=2)


plot(k.vec,err.k,type="b",ylim=c(0,max(max(err.k),err.Bayes,err.logit.deg1)))
abline(h=err.Bayes,col=2)
abline(h= min(vec.pred.test),col=3)
legend("bottomright",legend=c("kNN","Bayes","Logistic"),col=c(1,2,3),lty=1)

