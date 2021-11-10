#Initialisation

setwd("E:/Master 2/Mathieu Emily/Analyse de Variance")

compote <- read.table("compote.txt", sep = "\t", header = TRUE)
Senso <- read.table("ansenso.txt",sep = "\t", dec = ",", header = TRUE)

require(car)
require(emmeans)
require(multcomp)

#Exercice 1 

#1

Senso$Produit <- as.factor(Senso$Produit)
Senso$Jour <- as.factor(Senso$Jour)

mod0 <- lm(Note ~ 1, data = Senso)


anova(mod0,mod1)
#Pas significatif, on garde le modèle nul

mod2 <- lm(Note ~ Jour, data = Senso)
summary(mod2)
anova(mod0,mod2)
#Significatif on garde le modèle le plus complexe

mod3 <- lm(Note ~ Produit+Jour+Produit:Jour, data = Senso)
summary(mod3)
anova(mod0,mod3)
#Significatif on garde le modèle le plus complexe

#L'interaction des deux signifie que l'effet produit dépend du jour, selon le jour la note du produit ne sera pas la même

#2

#Formule de l'estimateur 


#3

contrasts(Senso$Produit) <- contr.sum(n=3)
contrasts(Senso$Jour) <- contr.sum(n=2)
mod3 <- lm(Note ~ Produit+Jour+Produit:Jour, data = Senso)
Anova(mod3, type = "III")

#Il y a un effet du Jour, un faible effet produit, mais aucun effet de l'interaction.

#4

mod1 <- lm(Note ~ Produit, data = Senso)
meanu<-coef(mod1)
#4.56000000 -0.09333333  0.45333333 
Prod1 <- meanu[1]
Prod2 <- meanu[1]+meanu[2]
Prod3 <- meanu[3]+meanu[1]

Mean <- aggregate(Note~Produit, data = Senso, FUN=mean) ; Mean
#La moyenne des notes des produits et les valeurs prises par ^?? + ^??i sont les mêmes.
#Le produit préféré est le 3 d'après la moyenne des notes.

#5
levels(Senso$Produit)

summary(glht(mod1,linfct=mcp(Produit=c(-2,1,1))))
#On rejette l'hypothèse nulle
