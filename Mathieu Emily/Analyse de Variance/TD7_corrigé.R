setwd("E:/Master 2/Mathieu Emily/Analyse de variance")

get.pval <- function(sum.aov,i,j){
	Fobs <- sum.aov[i,3]/sum.aov[j,3]
	pval <- 1-pf(Fobs,df1=sum.aov[i,1],df2=sum.aov[j,1])
	return(list(Fobs=Fobs,pval=pval))
}

get.pval.Anova <- function(Anova.Table,i,j){
	Fobs <- (Anova.Table[i,1]/Anova.Table[i,2])/(Anova.Table[j,1]/Anova.Table[j,2])
	pval <- 1-pf(Fobs,df1=Anova.Table[i,2],df2=Anova.Table[j,2])
	return(list(Fobs=Fobs,pval=pval))
}

######################
######################
### Exo 1
######################
######################

amines <- read.table("amines.txt",header=TRUE,dec=",",sep="\t", stringsAsFactors = TRUE)

summary(amines)

table(amines$espece,amines$sous.famille,amines$traitement)
table(amines$espece,amines$sous.famille)
table(amines$espece,amines$traitement)
table(amines$sous.famille,amines$traitement)

# sous.famille subordonnee à espece
# espece et traitement sont croisees
# sous.famille et traitement sont croisees

# espece: Fixe
# Traitement: Fixe
# Sous.famille: aleatoire

## Schema 3 du cours



######################
### Question Phe
######################

require("car")
options(contrasts=c("contr.sum","contr.sum"))


###########
### Pour l'anova de type III

## Test qui ne marche
mod.Phe.Anova <- Anova(lm(Phe ~ espece+traitement+sous.famille:espece+traitement:espece+traitement:sous.famille:espece,data=amines),type="III")

amines2 <- amines
nchar(as.character(amines2$sous.famille[1]))
amines2$sous.famille <- factor(sapply(amines2$sous.famille,FUN=function(x){return(substr(x,2,nchar(as.character(x))))}))

mod.Phe.Anova <- Anova(lm(Phe ~ espece+traitement+sous.famille%in%espece+traitement:espece+traitement:(sous.famille%in%espece),data=amines2),type="III")

#On enlève ce qui n'est pas significatif (interaction ordre 3)

# mod.Phe.Anova.Test <- Anova(lm(Phe ~ espece+traitement+sous.famille%in%espece+traitement:espece+traitement:(sous.famille%in%espece),data=amines2),type="III")

# mod.Phe.Anova.Test.Ordre2 <- Anova(lm(Phe ~ traitement+espece+sous.famille%in%espece+traitement:espece+traitement:(sous.famille%in%espece),data=amines2),type="III")

mod.Phe.Anova.2 <- Anova(lm(Phe ~ espece+traitement+sous.famille%in%espece+traitement:espece,data=amines2),type="III") #On a enlevé une interaction d'ordre 2 pas signficiative

mod.Phe.Anova.3 <- Anova(lm(Phe ~ espece+traitement+sous.famille%in%espece,data=amines2),type="III") 
#On enlève l'effet traitement qui n'est pas signficiatif 

mod.Phe.Anova.4 <- Anova(lm(Phe ~ espece+sous.famille%in%espece,data=amines2),type="III")
#On regarde s'il y a un 
get.pval.Anova(mod.Phe.Anova.4,2,3)


###########

mod.Phe <- Anova(Phe ~ espece+traitement+sous.famille:espece+traitement:espece+traitement:(sous.famille:espece),data=amines,type="III")

###########
### Pour l'anova de type I
mod.Phe <- aov(Phe ~ espece+traitement+sous.famille%in%espece+traitement:espece+traitement:(sous.famille%in%espece),data=amines)
aov.mod.Phe <- summary(mod.Phe)

mod.Phe.Ordre2 <- aov(Phe ~ traitement+espece+sous.famille%in%espece+traitement:espece+traitement:(sous.famille%in%espece),data=amines)
summary(mod.Phe.Ordre2)

mod.Phe.lm <- lm(Phe ~ espece+traitement+sous.famille%in%espece+traitement:espece+traitement:(sous.famille%in%espece),data=amines)
summary(mod.Phe.lm)
anova.mod.Phe <- anova(mod.Phe)

###########

## Effet espece / sous.famille %in% espece
get.pval(aov.mod.Phe[[1]],1,3)
get.pval.Anova(mod.Phe.Anova,2,4)
## Effet traitement / traitement:sous.famille %in% espece
get.pval(aov.mod.Phe[[1]],2,5)
get.pval.Anova(mod.Phe.Anova,3,6)
## Effet sous-famille / residuelle
get.pval(aov.mod.Phe[[1]],3,6)
get.pval.Anova(mod.Phe.Anova,4,7)
## Effet espece:traitement / traitement:sous.famille %in% espece
get.pval(aov.mod.Phe[[1]],4,5)
## Effet traitement:sous.famille %in% espece / residuelle
get.pval(aov.mod.Phe[[1]],5,6)

## On enleve traitement:sous.famille %in% espece
mod.Phe.2 <- aov(Phe ~ espece+traitement+sous.famille%in%espece+traitement:espece,data=amines)
aov.mod.Phe.2 <- summary(mod.Phe.2)
get.pval(aov.mod.Phe.2[[1]],4,5)
get.pval(aov.mod.Phe.2[[1]],3,5)

## On enleve espece:traitement
mod.Phe.3 <- aov(Phe ~ espece+traitement+sous.famille%in%espece,data=amines)
aov.mod.Phe.3 <- summary(mod.Phe.3)
get.pval(aov.mod.Phe.3[[1]],2,4) ## Non Significatif

## On enleve traitement
mod.Phe.4 <- aov(Phe ~ espece+sous.famille%in%espece,data=amines)
aov.mod.Phe.4 <- summary(mod.Phe.4)
get.pval(aov.mod.Phe.4[[1]],1,2) ## Significatif

mod.Phe.T <- aov(Phe ~ traitement,data=amines)
summary(mod.Phe.T)

######################
### Question Phe avec lmer
######################

require(lmerTest)

mod.lmer.1 <- lmer(Phe ~ espece+traitement+(1|espece:sous.famille)+espece:traitement+(1|traitement:espece:sous.famille),data=amines)
anova(mod.lmer.1)
lmerTest::rand(mod.lmer.1)

mod.lmer.2 <- lmer(Phe ~ espece+traitement+(1|espece:sous.famille)+espece:traitement,data=amines)
anova(mod.lmer.2)
lmerTest::rand(mod.lmer.2)

mod.lmer.3 <- lmer(Phe ~ espece+traitement+(1|espece:sous.famille),data=amines)
anova(mod.lmer.3)
lmerTest::rand(mod.lmer.3)

mod.lmer.4 <- lmer(Phe ~ espece+(1|espece:sous.famille),data=amines)
anova(mod.lmer.4)
lmerTest::rand(mod.lmer.4)

######################
### Question Dap
######################

mod.Dap <- aov(Dap ~ espece+traitement+sous.famille%in%espece+traitement:espece+traitement:(sous.famille%in%espece),data=amines)
aov.mod.Dap <- summary(mod.Dap)

## Effet traitement:sous.famille %in% espece / residuelle
get.pval(aov.mod.Dap[[1]],5,6)

mod.Dap.2 <- aov(Dap ~ espece+traitement+sous.famille%in%espece+traitement:espece,data=amines)
aov.mod.Dap.2 <- summary(mod.Dap.2)

## Effet sous-famille / residuelle
get.pval(aov.mod.Dap.2[[1]],3,5)

mod.Dap.3 <- aov(Dap ~ espece+traitement+traitement:espece,data=amines)
aov.mod.Dap.3 <- summary(mod.Dap.3)

######################
### Question Spd
######################

mod.Spd <- aov(Spd ~ espece+traitement+sous.famille%in%espece+traitement:espece+traitement:(sous.famille%in%espece),data=amines)
aov.mod.Spd <- summary(mod.Spd)

## Effet traitement:sous.famille %in% espece / residuelle
get.pval(aov.mod.Spd[[1]],5,6)

mod.Spd.2 <- aov(Spd ~ espece+traitement+sous.famille%in%espece+traitement:espece,data=amines)
aov.mod.Spd.2 <- summary(mod.Spd.2)

## On enleve espece:traitement
mod.Spd.3 <- aov(Spd ~ espece+traitement+sous.famille%in%espece,data=amines)
aov.mod.Spd.3 <- summary(mod.Spd.3)

## On enleve traitement
mod.Spd.4 <- aov(Spd ~ espece+sous.famille%in%espece,data=amines)
aov.mod.Spd.4 <- summary(mod.Spd.4)

## On enleve espece:sous.famille
mod.Spd.5 <- aov(Spd ~ espece,data=amines)
aov.mod.Spd.5 <- summary(mod.Spd.5)



######################
######################
### Exo 2
######################
######################

bota <- read.table(paste(path,"bota.csv",sep=""),sep=",",header=TRUE,dec=".",na.strings="")
names(bota)
bota$Quadrat <- factor(bota$Quadrat)
bota$Plante <- factor(bota$Plante)


mod.bota <- aov(Fecondite ~ Type.de.cage+Quadrat%in%Type.de.cage+Plante%in%(Quadrat%in%Type.de.cage),data=bota)
summary(mod.bota)
## Impossible car pas de residuelle!!


mod.bota <- aov(Fecondite ~ Type.de.cage+Quadrat%in%Type.de.cage,data=bota)
aov.mod.bota <- summary(mod.bota)

## On garde Type.de.cage
get.pval(aov.mod.bota[[1]],1,2)

require("GAD")
A <- as.fixed(bota$Type.de.cage)
B <- as.random(bota$Quadrat)
mod.bota.gad <- lm(Fecondite ~ A+B%in%A,data=bota)
gad(mod.bota.gad)

######
## avec lmer
require(lmerTest)

# Modele complet
mod.lmer.complet <- lmer(Fecondite ~ Type.de.cage+(1|Quadrat:Type.de.cage)+(1|Plante:Quadrat:Type.de.cage),data=bota)
## On obtient un message d'erreur

mod.lmer <- lmer(Fecondite ~ Type.de.cage+(1|Type.de.cage:Quadrat),data=bota)
summary(mod.lmer)
anova(mod.lmer)
rand(mod.lmer)