#Mathieu Emily

setwd("E:/Master 2/Mathieu Emily/Analyse de Variance")

cow <- read.table("charolais.txt",header=TRUE)

cow$Milieu <- factor(cow$Milieu,c("Mediocre","Moyen","Bon")) #R�ordonne les donn�es dans l'ordre donn�
cow$Genotype <- factor(cow$Genotype,c("Croise","Pur"))

contrasts(cow$Milieu) #permet de visualiser la contrainte du facteur, des coefficients du facteurs sur les coefficients utilis�s par le mod�le.

mod <- lm (GMQ ~ Milieu, data = cow) #Mod�le lin�aire
coef(mod)

#Modification de la contrainte pour Milieu en contrainte de somme
contrasts(cow$Milieu) <- contr.sum(n=3)
mod.sum <- lm (GMQ ~ Milieu, data = cow)
coef(mod.sum)

#Modification de la contrainte pour Milieu en mettant le milieu Moyen comme r�f�rence
contrasts(cow$Milieu) <- contr.treatment(n=3,base=2)
mod.treat <- lm (GMQ ~ Milieu, data = cow)
coef(mod.treat)

#### Vraisemblance

logLik(mod)
logLik(mod.sum)
logLik(mod.treat)
#C'est la m�me valeur

mod.null <- lm(GMQ ~ 1, data = cow)
logLik(mod.null)

#On prend le mod�le ayant la valeur la plus �lev�e de vraisemblance

#Test avec une variable cr��e 
cow$X <- runif(nrow(cow))
mod.stupid <- lm(GMQ~Milieu + X, data = cow)
logLik(mod.stupid)

#Pourtant le mod�le stupid obtient une meilleure vraisemblance
#D�s lors qu'on rajoute une variable explicative, le mod�le va plus s'ajuster avec les donn�es
#nous donnant l'impression que le mod�le devient meilleur. Il faut donc aussi regarder le ddl
#(qui correspond au nombre de fois o� on a �t� regarder le mod�le), ici le nombre de ddl
#est plus �lev� pour le mod�le stupid pour une diff�rence minime.

#Il y a une formule pour pouvoir savoir si la diff�rence entre deux mod�les est significative

#On peut aussi utiliser l'anova :

#Fisher
anova(mod.treat,mod.stupid,test="F")

anova(mod.treat,mod.stupid,test = "Chisq")

#Pr�coniser le test anova avec le Chisq, sort un meilleur r�sultat, plus fiable


anova(mod.null,mod.sum,test="Chisq")

#Significatif, il y a bien un effet du milieu sur le GMQ

