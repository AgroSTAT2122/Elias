setwd("E:/Master 2/David Causeur/Session 4")

##### 


require(leaps)             # For variable selection
require(glmnet)            # For penalized regression procedures
require(fields)            # For image.plot
require(pls)               # For cvsegments, plsr, ...
require(MASS)              # For LDA
require(viridis)           # For nice colors
require(groupdata2)        # For balanced cross-validation


#####


nir <- read.table("SLBDnirstozeur.txt",header=TRUE)
carb <- read.table("SLBDcarbontozeur.txt",header=TRUE)

# SNV-transformed NIRS 
# SNV : Standard Normal Variate

nir_snv <- t(scale(t(nir)))

wn = seq(400,2500,1)           # Wavenumbers
matplot(wn,t(nir_snv),type="l",lwd=2,col="orange",lty=1,
        bty="l",xlab=expression(Wave~numbers~(nm^-1)),
        ylab="SNV-transformed NIRS",main="NIRS data of soil samples",
        cex.main=1.25,cex.lab=1.25,cex.axis=1.25)
abline(h=0,lty=3)

#2 

#1500 - 2000 nm

data <- cbind.data.frame(nir,CO=carb$Carbone)

#Graphique simple
#graph des covariances après avoir centré réduit --> corrélation 

x <- nir
y <- carb$Carbone

## Matrix of scaled explanatory variables

## Vector of response values

## Squared covariances between scaled xs and y
Correlation = cor(nir_snv,y)

plot(Correlation, type = 'l',lwd=2, xlab = expression(lambda)) #On voit deux pics, un aux alentours 
abline(h=0,lty=3)

#3

mod <- glm(CO~.,data=data)

#4 ## PLS fit with one component

lmp.pls = plsr(CO~.,data=data,ncomp=1,scale=TRUE)

## Extract latent variable and corresponding loadings
lv = scores(lmp.pls)
alpha = loadings(lmp.pls)


mod = lm(CO~LV,data=data.frame(CO=y,LV=lv[,1]))
fitted(mod)[1:6]

dim(fitted(lmp.pls))
fitted(lmp.pls)[1:6,1,1]

# MSEP of the model
lmp.pls = plsr(CO~.,data=data,ncomp=1,scale=TRUE,validation="CV",segments=10)
MSEP(lmp.pls)
#Deux valeurs, on prend celle qu'on veut comme elles se ressemblent beaucoup

# Percentage of explained variance
explvar(lmp.pls)

#Explique plus de 70% de la var de x

### Adding a 2nd latent variable
lmp.pls2 = plsr(CO~.,data=data,ncomp=2,scale=TRUE)
lv2 = scores(lmp.pls2)
cor(lv2)

# Percentage of explained variance
explvar(lmp.pls2)

#5
#Deux composantes expliquent 97% de la variance

# Score plot
scoreplot(lmp.pls2,pch=16,ylim=range(lv2[,1]))

# Does it improve the fit?
R2(lmp.pls2)

# Does it improve the prediction accuracy?
lmp.pls2 = plsr(CO~.,data=data,ncomp=2,scale=TRUE,validation="CV",segments=10)
RMSEP(lmp.pls2)

### How many latent variables to introduce in the model?
lmp.pls = plsr(CO~.,data=data,ncomp=100,scale=TRUE,validation="CV",segments=10)

# Percentage of explained variance
explvar(lmp.pls)

# Selecting the optimal number of latent variables
selectNcomp(lmp.pls,method="onesigma",plot=TRUE)

#Plot

plot(0:100,R2(lmp.pls)$val[1,1,],
     type="b",
     pch=16,
     ylim=c(0,1),
     xlab = "nombre de composantes PLS",
     ylab = "R2 (validation croisée)")
abline(v=6,lty=3)

### Implementation of a complete 10-fold CV procedure 

n = nrow(data)
cvpred = rep(0,n)

segs = cvsegments(n,10)
cvpred = rep(0,n)


for (k in 1:10) {
  cvtrain <- plsr(CO~.,data=data[-segs[[k]],],ncomp=20,scale=TRUE,validation="CV",segments=10)
  bestncomp = selectNcomp(lmp.pls,method="onesigma")
  cvpred[segs[[k]]] = predict(cvtrain,newdata=data[segs[[k]],])[,,bestncomp]
  print(k)
}

#autre manière de l'écrire :

n = nrow(data)
cvpred = rep(0,n)

segs = cvsegments(n,10)
cvpred = rep(0,n)

for (k in 1:10) {
  train <- data[-segs[[k]],]
  test <- data[segs[[k]],]
  cvtrain <- plsr(CO~.,data=data[-segs[[k]],],ncomp=20,scale=TRUE,validation="CV",segments=10)
  bestncomp = selectNcomp(lmp.pls,method="onesigma")
  plstrain <- plsr(CO~.,data=train,ncomp=bestncomp,validation="none")
  cvpred[segs[[k]]] = predict(plstrain,newdata=test)[,,bestncomp]
  print(k)
}


PRESS.pls = sum((data$CO-cvpred)^2) ; PRESS.pls
MSEP.pls <- PRESS.pls/nrow(data)
MSEP.pls

plot(data$CO,cvpred,pch=16)
abline(a=0,b=1, lwd=3)

#Coeff beta différent selon segments

####

corrplot(lmp.pls)

loadingplot(lmp.pls)
