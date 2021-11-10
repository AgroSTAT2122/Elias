BiocManager::install('mixOmics')
data("vac18",package="mixOmics")

length(vac18)
names(vac18)
length(vac18$genes)
length(vac18$stimulation)
length(vac18$sample)
dim(vac18$tab.prob.gene)
vac18$tab.prob.gene[1,]
dim(vac18$genes)


###
### CART
library(rpart)
VAC18 <- data.frame(vac18$genes,stimu=vac18$stimulation)
VacTreeDef <- rpart(stimu~.,data=VAC18)
plot(VacTreeDef)
text(VacTreeDef,use.n=TRUE,xpd=TRUE)


set.seed(788182)
VacTreeMax <- rpart(stimu~.,data=VAC18,minsplit=2,cp=0)
plot(VacTreeMax)
text(VacTreeMax,use.n=TRUE,xpd=TRUE)

## L'argument xval permet de régler le paramètre de validation croisée
set.seed(413745)
VacTreeMaxLoo <- rpart(stimu~.,data=VAC18,minsplit=2,cp=0,xval=nrow(VAC18))
plotcp(VacTreeMax)
plotcp(VacTreeMaxLoo)

VacIndcpOpt <- which.min(VacTreeMaxLoo$cptable[,4])
VaccpOpt <- VacTreeMaxLoo$cptable[VacIndcpOpt,1]
VacTreeOpt <- prune(VacTreeMaxLoo,cp=VaccpOpt)
plot(VacTreeOpt)
text(VacTreeOpt,use.n=TRUE,xpd=TRUE)



########
########
## RandomForest
library(randomForest)
geneExpr <- vac18$genes
stimu <- vac18$stimulation

VacRFpsur3 <- randomForest(x=geneExpr,y=stimu,mtry=ncol(geneExpr)/3)
VacRFpsur3
plot(VacRFpsur3)


# avec ranger
library(ranger)

ranger(stimu~.,data=VAC18)


# Importance des variables

vacRFDefImp <- randomForest(x=geneExpr,y=stimu,mtry=ncol(geneExpr)/3,importance=TRUE)
varImpPlot(vacRFDefImp,type=1,sclae=FALSE,cex=0.8)


## Selection de variables avec VSURF

library(VSURF)
set.seed(481933)
vacVSURF <- VSURF(x=geneExpr,y=stimu)

summary(vacVSURF)
plot(vacVSURF)
