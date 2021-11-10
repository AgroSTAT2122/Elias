#Mathieu Emily

setwd("E:/Master 2/Mathieu Emily/Analyse de Variance")

cow <- read.table("charolais.txt",header=TRUE)

cow$Milieu <- factor(cow$Milieu,c("Mediocre","Moyen","Bon")) #Réordonne les données dans l'ordre donné
cow$Genotype <- factor(cow$Genotype,c("Croise","Pur"))

contrasts(cow$Milieu) #permet de visualiser la contrainte du facteur, des coefficients du facteurs sur les coefficients utilisés par le modèle.

mod <- lm (GMQ ~ Milieu, data = cow) #Modèle linéaire
coef(mod)

#Modification de la contrainte pour Milieu en contrainte de somme
contrasts(cow$Milieu) <- contr.sum(n=3)
mod.sum <- lm (GMQ ~ Milieu, data = cow)
coef(mod.sum)

#Modification de la contrainte pour Milieu en mettant le milieu Moyen comme référence
contrasts(cow$Milieu) <- contr.treatment(n=3,base=2)
mod.treat <- lm (GMQ ~ Milieu, data = cow)
coef(mod.treat)

#### Vraisemblance

logLik(mod)
logLik(mod.sum)
logLik(mod.treat)
#C'est la même valeur

mod.null <- lm(GMQ ~ 1, data = cow)
logLik(mod.null)

#On prend le modèle ayant la valeur la plus élevée de vraisemblance

#Test avec une variable créée 
cow$X <- runif(nrow(cow))
mod.stupid <- lm(GMQ~Milieu + X, data = cow)
logLik(mod.stupid)

#Pourtant le modèle stupid obtient une meilleure vraisemblance
#Dès lors qu'on rajoute une variable explicative, le modèle va plus s'ajuster avec les données
#nous donnant l'impression que le modèle devient meilleur. Il faut donc aussi regarder le ddl
#(qui correspond au nombre de fois où on a été regarder le modèle), ici le nombre de ddl
#est plus élevé pour le modèle stupid pour une différence minime.

#Il y a une formule pour pouvoir savoir si la différence entre deux modèles est significative

#On peut aussi utiliser l'anova :

#Fisher
anova(mod.treat,mod.stupid,test="F")

anova(mod.treat,mod.stupid,test = "Chisq")

#Préconiser le test anova avec le Chisq, sort un meilleur résultat, plus fiable


anova(mod.null,mod.sum,test="Chisq")

#Significatif, il y a bien un effet du milieu sur le GMQ

