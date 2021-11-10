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
