#Exercice 3

#Initialisation

setwd("E:/Master 2/David Causeur/Session 3")

require(leaps)             # For variable selection
require(glmnet)            # For penalized regression procedures
require(fields)            # For image.plot
require(pls)               # For cvsegments, plsr, ...
require(RcmdrMisc)       # For Stepwise


train <- read.table("bacttrain.txt",header=TRUE)

test <- read.table("bacttest.txt",header=TRUE)

#1

names(train)

#La variable r�ponse est la variable "TYPE" prenant les valeurs : Positif ou N�gatif
#Elle est de type cat�gorielle

#Les variables explicatives sont des variables quantitatives
#Il y a : diam�tre, �longation, luminosit� moyenne/m�diane/�cart-type...

#2 

train$Type <- as.factor(train$Type)
test$Type <- as.factor(test$Type)

dim(train)

x <- train[,-31]
y <- train[,31]



mod <- glm(Type~.,data=train,family="binomial")

#3

test_x <- test[,-31]

proba = predict(mod,newdata=test_x, type="response")
predictions = ifelse(proba > 0.5, "Positif","N�gatif")

table(test$Type,predictions)



#4

mean(test$Type==predictions) #78%

#5

loglambda = seq(10,-10,length=100) 


train_cv = cv.glmnet(x=as.matrix(x),y,family="binomial",type.measure="deviance",
                                lambda=exp(loglambda))

plot(train_cv)

modnet <- glmnet(x=as.matrix(x),y,family="binomial",type.measure="deviance",
                 lambda=exp(loglambda))

predictions = predict(modnet,newx=as.matrix(test_x),
                type="class")[,which.min(train_cv$cvm)]

table(test$Type,predictions)

mean(test$Type==predictions) #78%
#On a pas gagn� d'accuracy en effectuant un mod�le p�nalis�

#D� au fait que la m�thode LASSO a utilis� un lambda petit ce qui revient � utiliser la maximum de vraisemblance, d�j� utilis� dans le glm

#6

modnet$beta[,which.min(train_cv$cvm)]
which(abs(modnet$beta[,which.min(train_cv$cvm)])>1e-08)

#Donn�es artificelles, donc en pratique, on se doute qu'il n'y a pas d'�quilibre entre positif et n�gatif. Donc on ne peut pas appliquer Bayes si
#il n'y a pas 50-50 de chaque. 
#En pratique, on calcule la probabilit� qu'un ev soit une bact�rie, puis apr�s apprentissage avec seuil choisi automatiquement