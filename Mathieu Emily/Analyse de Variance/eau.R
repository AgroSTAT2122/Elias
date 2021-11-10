#set wd

setwd("E:/Master 2/Mathieu Emily/Analyse de Variance")

eau <- read.table("fich_eau.txt", sep = "\t", header = TRUE)

eau$juge <- as.factor(eau$juge)
eau$produit <- as.factor(eau$produit)

plot(eau$produit,eau$intensite.de.crepitement)


par(mfrow=c(3,1))

boxplot(eau$intensite.de.crepitement ~ produit,data = eau)
boxplot(saveur.sucree ~ produit, data = eau)
boxplot(intensite.gustative.globale ~ produit, data = eau)

# Correction

##################
##################
## Exo 1
##################
##################

Eau <- read.table(paste("fich_eau.txt",sep=""),sep="\t",header=TRUE)

Eau$produit <- as.factor(Eau$produit)
Eau$juge <- as.factor(Eau$juge)

plot(Eau$produit,Eau$intensite.de.crepitement)

par(mfrow=c(3,1))
boxplot(intensite.de.crepitement ~ produit,Eau,main="intensite.de.crepitement")
boxplot(saveur.sucree ~ produit,Eau,main="saveur.sucree")
boxplot(intensite.gustative.globale ~ produit,Eau,main="intensite.gustative.globale")

# Q3
# Test Global
mod.Global <- lm(intensite.de.crepitement ~ produit,Eau)
mod.Null <- lm(intensite.de.crepitement ~ 1,Eau)
anova(mod.Global,mod.Null)
anova(lm(intensite.de.crepitement ~ produit,Eau))
anova(lm(saveur.sucree ~ produit,Eau))
summary(lm(intensite.gustative.globale ~ produit,Eau))
anova(lm(intensite.gustative.globale ~ produit,Eau))
summary(Eau$intensite.de.crepitement)

table(Eau$produit[which(!is.na(Eau$intensite.gustative.globale))])

contrasts(Eau$produit) <- contr.sum(n=8)

require(car)
Anova(lm(intensite.gustative.globale ~ produit,Eau),type="III")

# Q3a

sort(by(Eau$intensite.gustative.globale,Eau$produit,mean,na.rm=TRUE))

## Reoordonnancement des facteurs
Eau$produit <- factor(Eau$produit,levels=order(by(Eau$intensite.gustative.globale,Eau$produit,mean,na.rm=TRUE)))


require(multcomp)
modele.1way <- lm(intensite.gustative.globale ~ produit,data=Eau)
tuk <- glht(modele.1way,linfct=mcp(produit="Tukey"))
summary(tuk)
tuk.cld <- cld(tuk)
par(mfrow=c(1,1))
par(mar=c(5,4,6,2)+0.1)
plot(tuk.cld) 

require(emmeans)
mod.IGG <- lm(intensite.gustative.globale ~ produit,Eau)
emmeans(mod.IGG, pairwise ~ produit)


# Q4
contrasts(Eau$juge) <- contr.sum(n=16)
boxplot(intensite.de.crepitement ~ juge,Eau)

summary(lm(intensite.de.crepitement ~ produit+juge,Eau))
anova(lm(intensite.de.crepitement ~ produit+juge,Eau),lm(intensite.de.crepitement ~ 1,Eau))

anova(lm(intensite.de.crepitement ~ produit+juge,Eau))
anova(lm(saveur.sucree ~ produit+juge,Eau))
boxplot(intensite.gustative.globale ~ juge,Eau)
anova(lm(intensite.gustative.globale ~ produit+juge,Eau))
anova(lm(intensite.gustative.globale ~ produit,Eau))

modele.2way <- lm(intensite.gustative.globale ~ produit+juge,data=Eau)
tuk2 <- glht(modele.2way,linfct=mcp(produit="Tukey"))
summary(tuk2)
plot(tuk2)
tuk.cld2 <- cld(tuk2)
plot(tuk.cld2)

# Q5

require(car)
anova(lm(saveur.sucree ~ produit+juge,Eau))
Anova(lm(saveur.sucree ~ produit+juge,Eau),type="III")

summary(Eau$intensite.gustative.globale)
anova(lm(intensite.gustative.globale ~ produit+juge,Eau))
Anova(lm(intensite.gustative.globale ~ produit+juge,Eau),type="III")


anova(lm(intensite.gustative.globale ~ produit+juge,Eau))
anova(lm(intensite.gustative.globale ~ juge+produit,Eau))

Anova(lm(intensite.gustative.globale ~ produit+juge,Eau),type="III")
Anova(lm(intensite.gustative.globale ~ juge+produit,Eau),type="III")

anova(lm(intensite.gustative.globale ~ juge+produit,Eau))
## Données équibilibrés Type I et Type III sont identiques

# Q6

SCT <- sum((Eau$intensite.gustative.globale-mean(Eau$intensite.gustative.globale,na.rm=TRUE))^2,na.rm=TRUE)
SCT_Modele <- sum(Anova.T3.IGG[2:4,1])
## On voit un écart entre SCT et SCT_Modele

#Q7
contrasts(Eau$produit) <- contr.sum(8)
summary.lm(lm(saveur.sucree ~ produit+juge,Eau))

#Q8
modInter <- lm(intensite.gustative.globale ~ produit*juge,data=Eau)
summary(modInter) ## Etape 1
anova(mod.Null,modInter) ## Etape 2



mod.Null <- lm (intensite.gustative.globale ~ 1, data = Eau)

Anova(lm(intensite.gustative.globale ~ produit*juge,data=Eau),type = "III")

contrasts(Eau$produit)


modele.3way <- lm(intensite.gustative.globale ~ produit+juge+produit:juge,data=Eau)

summary(modele.3way)
tuk3 <- glht(modele.3way,linfct=mcp(produit="Tukey"))
summary(tuk3)
tuk.cld3 <- cld(tuk3)


contrasts(Eau$produit) <- contr.sum(8)
Anova(lm(intensite.gustative.globale ~ produit*juge,Eau),type="III")
### Attention !!!
contrasts(Eau$produit) <- contr.treatment(8,1)
Anova(lm(intensite.gustative.globale ~ produit*juge,Eau),type="III")

########
## Interactions avec phia
########

# Calcul des moyennes ajustées
library(phia)
mod.inter <- lm(intensite.gustative.globale ~ produit*juge,Eau)
interactionMeans(mod.inter) ## Toutes les moyennes d'interaction
interactionMeans(mod.inter,factors="produit") ## Uniquement les moyennes marginales pour le facteur produit

# test post-hoc - Effets simples
testInteractions(mod.inter,fixed="juge",across="produit") ## On teste l'effet produit sachant le juge

# test post-hoc - Effets multiples
testInteractions(mod.inter) 

# test post-hoc - Effets simples pairwise
testInteractions(mod.inter,pairwise="juge",across="produit") 

interaction.plot(Eau$juge,Eau$produit,Eau$intensite.de.crepitement)

##################
##################
## Exo 2
##################
##################

# Q1 Importation+transformation en disjonctif

Poussins <- read.table(paste(path,"poussins.txt",sep=""),sep="\t",header=TRUE)

Poussins$Trait <- factor(Poussins$Trait)
Poussins$Sexe <- factor(Poussins$Sexe)

# Q2
options(contrasts = c("contr.treatment", "contr.treatment"))
summary(lm(Rdt ~ Trait,data=Poussins))
options(contrasts = c("contr.sum", "contr.sum"))
summary(lm(Rdt ~ Trait,data=Poussins))

# Q3
require(FactoMineR)
my.poussins <- data.frame(cbind(Poussins$Rdt),tab.disjonctif(cbind(Poussins$Trait,Poussins$Sexe)))
names(my.poussins) <- c("Rdt","Trait1","Trait2","Trait3","Sexe1","Sexe2")

# Q4
###
summary(lm(Rdt ~ Trait1+Trait2+Trait3,my.poussins))
# Trait1, Trait2 et Trait3 sont liées!! Donc le design n'est pas de rang plein

summary(lm(Rdt ~ Trait2+Trait3,my.poussins)) ## Ici ca revient à affecter 0 comme coefficient pour Trait1

my.poussins2 <- my.poussins
my.poussins2$Trait1 <- my.poussins$Trait1-my.poussins$Trait3
my.poussins2$Trait2 <- my.poussins$Trait2-my.poussins$Trait3
summary(lm(Rdt ~ Trait1+Trait2,data=my.poussins2))

# Q5
Y <- my.poussins2$Rdt

X <- cbind(rep(1,times=length(Y)),my.poussins2$Trait1,my.poussins2$Trait2) ## Ecriture de la matrice de design sous forme matricielle
solve(t(X)%*%X)%*%t(X)%*%Y ## Formule d'estimation par moindres carrés d'un modele lineaire (XX')^(-1)X'Y

# Q6

contrasts(Poussins$Trait) <- contr.sum(3)
contrasts(Poussins$Sexe) <- contr.treatment(2,base=2)
summary(lm(Rdt ~ Trait+Sexe,data=Poussins))
X <- cbind(rep(1,times=length(Y)),my.poussins$Trait1-my.poussins$Trait3,my.poussins$Trait2-my.poussins$Trait3,my.poussins$Sexe1)

solve(t(X)%*%X)%*%t(X)%*%Y

# Q7
by(Poussins$Rdt,Poussins$Trait,mean)
