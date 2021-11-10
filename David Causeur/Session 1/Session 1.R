#Setup

setwd("E:/Master 2/David Causeur/Session 1")


#données

datapig <- read.table("https://dcauseur.netlify.app/teaching/data/SLBDinvasive.txt", header = TRUE)
str(datapig) #définition des colonnes (numeric,factor,...)
dim(datapig) #dimensions lignes/colonnes

summary(datapig) #"description" du jeu de données

#LMP est un pourcentage de gras (C'est aussi Y)
#Les autres variables sont des épaisseurs de gras ou de muscles
#Ce qui suit la lettre, sont des sites anatomiques ex: F34LV 
#On a dans le profil x : on a des épaisseurs de muscles et des épaisseurs de gras (muscle ~= 7-8cm; gras ~= 1cm)

#But : constuire Y à partir de X

x = datapig[,-1] #On retire la première colonne
y = datapig$LMP # On garde que la première colonne

Sx= var(x)
sxy = cov(x,y)

Sx[3,4]/(sqrt(Sx[3,3])*(sqrt(Sx[4,4]))) #Coefficient de corrélation (covariance/racine des variances)

beta.calc = solve(Sx,sxy) # Estimated regression coefficients

#Utilisation du GLM

mod = glm(LMP~.,data = datapig)
data.frame(beta.calc=beta.calc,beta.mod=coef(mod)[-1])

### Using RSS and R2

RSS = sum(residuals(mod)^2)  # Residuals sum-of-squares

mod0 = glm(LMP~1,data=datapig) # LS fit of the null model
coef(mod0)
RSS0 = sum(residuals(mod0)^2)  # Residuals sum-of-squares of null model
R2=(RSS0-RSS)/RSS0 # R2
R2

fitted.lmp = fitted(mod) # Fitted LMP values
observed.lmp = datapig$LMP   # Observed LMP values
cor(observed.lmp,fitted.lmp)^2   # Same

### Scatterplot of fitted against observed LMP values

plot(observed.lmp,fitted.lmp,type="p",pch=16,bty="n",xlab="Observed LMP",
     ylab="Fitted LMP",main="Fitted versus observed LMP values",
     cex.lab=1.25,cex.axis=1.25,cex.main=1.25)
abline(0,1)
text(50,65,paste("R2=",round(R2,3)),cex=1.25)

#############

#Coffee

Cafe <- read.table("SLBDcoffee.txt", header = TRUE)
dim(Cafe)       # Number of rows and columns in data
str(Cafe)       # Overview of data
Cafe$Localisation = factor(Cafe$Localisation) # Convert 'Localisation' into a factor
summary(Cafe)   # Provides a columnwise summary of the data table (8 first columns)

## ML fit the most complete model for 'Localisation'

mod = multinom(Localisation~.,data=Cafe) # ML fit of the logistic model
coef(mod)

mod = multinom(Localisation~.,data=Cafe,maxit=200) # ML fit of the logistic model
coef(mod)

probas = predict(mod, type = "probs")
round(head(probas), digits = 2)

### Assessment of the fit

deviance(mod)          # Residual deviance

mod0 = multinom(Localisation~1,data=Cafe) # ML fit of the null model
deviance(mod0)-deviance(mod)                # Explained deviance

### Observed versus fitted values

proba = fitted(mod)            # Estimated probabilities of each class
observed_class = Cafe$Localisation
head(data.frame(round(proba,3),observed_class))

# Confusion matrix

fitted_class = predict(mod,type="class")  # Bayes rule
head(fitted_class)

confusion = table(observed_class,fitted_class)
confusion

rowSums(confusion)
confusion_percentage = 100*confusion/outer(rowSums(confusion),rep(1,7))
round(confusion_percentage,3)


######


#Exercice 

dataExo <- read.table("SLBDscanner.txt",header=TRUE)

#Question 1 

summary(dataExo)

#Le LMP est la variable réponse, variable quantitative. Les variables explicatives sont les 137 observations conjointes, chacune correspondant au taux de muscle d'un individu à un endroit donné, toutes quantitatives.

#Question 2 

dim(dataExo)

#La population concernée par cette problématique est l'ensemble des entreprises porcines.

#Question 3 

#glm (LMP ~ .)
## Least-squares fit the most complete model

X = dataExo[,-138] # 60 x 11 matrix with measurements of explanatory variables   
y = dataExo$LMP  # 60-vector of response values


#Question 4

modLMP <- glm(LMP ~ ., data = dataExo)

data.frame(beta.calc=beta.calc,beta.mod=coef(modLMP)[-138])

RSS = sum(residuals(modLMP)^2)  # Residuals sum-of-squares

mod0 = glm(LMP~1,data=dataExo) # LS fit of the null model
coef(mod0)
RSS0 = sum(residuals(mod0)^2)  # Residuals sum-of-squares of null model
R2=(RSS0-RSS)/RSS0 # R2
R2


#il y autant de paramètres dans le modèle que d'individus. Le modèle est donc saturé, R2 = 1

fitted.lmp = fitted(modLMP) # Fitted LMP values
observed.lmp = dataExo$LMP   # Observed LMP values
cor(observed.lmp,fitted.lmp)^2   # Same

#Question 5 

plot(observed.lmp,fitted.lmp,type="p",pch=16,bty="n",xlab="Observed LMP",
     ylab="Fitted LMP",main="Fitted versus observed LMP values",
     cex.lab=1.25,cex.axis=1.25,cex.main=1.25)
abline(0,1)
text(50,65,paste("R2=",round(R2,3)),cex=1.25)

#Question 6

test = sample (1:117,10)
train = setdiff(1:117,test)

modtrain <- glm(LMP ~ ., data = dataExo[train,])
deviance(modtrain)

prediction <- predict (modtrain, newdata=dataExo[test,])
cor(prediction,dataExo[test,"LMP"])^2

plot(dataExo[test,"LMP"],prediction, pch=16)


install.package(pls)
require(pls)

segs = cvsegments(N=117,k=10)
class(segs)

segs[[1]] #C'est une liste donc il faut des doubles crochets

predictions = rep(0, length = 117)

for (k in 1:10) {
  train = dataExo[-segs[[k]],]
  test = dataExo[segs[[k]],]
  modtrain <- glm(LMP ~ ., data = train)
  deviance(modtrain)
  prediction[segs[[k]]] = predict (modtrain,newdata=test)
}

cor(prediction,dataExo$LMP)^2

#Non, la prédiction est catastrophique.

#Question 7 

#La variable ayant le coefficient de correlation avec LMP le plus élevé.

corxy = cor(dataExo[,-138],dataExo[,138])

which.max(abs(corxy))
corxy[85,]

#Il s'agit de la variable X92 avec un coefficient de corrélation 0,95

#Question 8 

mod1 = glm (LMP ~ X92, data = dataExo)

RSS = sum(residuals(mod1)^2)  # Residuals sum-of-squares

R2=(RSS0-RSS)/RSS0 # R2
R2

fitted.lmp = fitted(mod1) # Fitted LMP values
observed.lmp = dataExo$LMP   # Observed LMP values
cor(observed.lmp,fitted.lmp)^2   # Same

plot(observed.lmp,fitted.lmp,type="p",pch=16,bty="n",xlab="Observed LMP",
     ylab="Fitted LMP",main="Fitted versus observed LMP values",
     cex.lab=1.25,cex.axis=1.25,cex.main=1.25)
abline(0,1)
text(50,65,paste("R2=",round(R2,3)),cex=1.25)

## predict

test = sample (1:117,10)
train = setdiff(1:117,test)

modtrain <- glm(LMP ~ X92, data = dataExo[train,])
deviance(modtrain)

prediction <- predict (modtrain, newdata=dataExo[test,])
cor(prediction,dataExo[test,"LMP"])^2

plot(dataExo[test,"LMP"],prediction, pch=16)

segs = cvsegments(N=117,k=10)
class(segs)

segs[[1]] #C'est une liste donc il faut des doubles crochets

predictions = rep(0, length = 117)

for (k in 1:10) {
  train = dataExo[-segs[[k]],c(85,138)]
  test = dataExo[segs[[k]],c(85,138)]
  modtrain <- glm(LMP ~ X92, data = train)
  deviance(modtrain)
  prediction[segs[[k]]] = predict (modtrain,newdata=test)
}

PRESS = sum((prediction-dataExo$LMP)^2)
PRESS
  
cor(prediction,dataExo$LMP)^2


  
  