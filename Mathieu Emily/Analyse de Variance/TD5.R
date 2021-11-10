######################
######################
### Exo 1
######################
######################

setwd("E:/Master 2/Mathieu Emily/Analyse de variance")

viande <- read.table("viande.txt",header=TRUE, dec=",")


require(car)
require(lmerTest)
require(GAD)

viande$Fournisseur <- as.factor(viande$Fournisseur)
viande$Consommateur <- as.factor(viande$Consommateur)
viande$Seance <- as.factor(viande$Seance)
summary(viande)

table(viande$Fournisseur,viande$Consommateur,viande$Seance)
table(viande$Fournisseur,viande$Consommateur)
table(viande$Fournisseur,viande$Seance)
table(viande$Consommateur,viande$Seance)

## Effet Fournisseur: fixe
## Effet Consommateur: aléatoire
## Effet Séance: aléatoire

boxplot(Note ~ Fournisseur,data=viande)


#### Methode 1: à la main

options(contrasts=c("contr.sum","contr.sum"))
mod.viande <- lm(Note ~ Fournisseur+Consommateur+Seance+Fournisseur:Consommateur+Fournisseur:Seance+Consommateur:Seance,data=viande)
summary(mod.viande)
mod.anova.viande <- Anova(mod.viande,type="III")
print(mod.anova.viande)

## Tous les effes d'interaction sont significatifs (lisible directement dans le tableau d'ANOVA= comparaison à la residuelle)

F.Consommateur <- (mod.anova.viande[[1]][3]/mod.anova.viande[[2]][3])/(mod.anova.viande[[1]][7]/mod.anova.viande[[2]][7])
1-pf(F.Consommateur,mod.anova.viande[[2]][3],mod.anova.viande[[2]][7])

F.Seance <- (mod.anova.viande[[1]][4]/mod.anova.viande[[2]][4])/(mod.anova.viande[[1]][7]/mod.anova.viande[[2]][7])
1-pf(F.Seance,mod.anova.viande[[2]][4],mod.anova.viande[[2]][7])

## Pour l'effet fournisseur, c'est plus complique

CM.F <- (mod.anova.viande[[1]][2]/mod.anova.viande[[2]][2]) #Somme des carrés des écarts
ddl.F <- mod.anova.viande[[2]][2] #degré de liberté
CM.F.C <- (mod.anova.viande[[1]][5]/mod.anova.viande[[2]][5])
ddl.F.C <- mod.anova.viande[[2]][5]
CM.F.S <- (mod.anova.viande[[1]][6]/mod.anova.viande[[2]][6])
ddl.F.S <- mod.anova.viande[[2]][6]
CM.R <- (mod.anova.viande[[1]][8]/mod.anova.viande[[2]][8])
ddl.R <- mod.anova.viande[[2]][8]

F.Fournisseur <- CM.F/( CM.F.C+CM.F.S-CM.R)
ddl.denom <- (CM.F.C+CM.F.S-CM.R)^2/
  (CM.F.C^2/ddl.F.C+CM.F.S^2/ddl.F.S+CM.R^2/ddl.R)
1-pf(F.Fournisseur,ddl.F,ddl.denom)

mod.f.seul <- lm(Note ~ Fournisseur,data=viande)
Anova(mod.f.seul,type="III")

#### Methode 2: avec GAD

Fo <- as.fixed(viande$Fournisseur)
Co <- as.random(viande$Consommateur)
Se <- as.random(viande$Seance)

### Modele initial
mod.gad <- lm(Note~Fo+Co+Se+Fo:Co+Fo:Se+Co:Se,data=viande)
gad(mod.gad) ## GAD ne fonctionne pas avec des donnees desequilibrees
# gad ne sait gérer les bases de comparaison multiple.
#Pas bien ici 

#### Methode 3: avec lmerTest

require(lmerTest)

mod <- lmer(Note ~ Fournisseur+(1|Consommateur)+(1|Seance)+(1|Fournisseur:Consommateur)+(1|Fournisseur:Seance)+(1|Consommateur:Seance),data=viande)
summary(mod)
anova(mod)
lmerTest::rand(mod)
step(mod)
mod.ml <- lmer(Note ~ Fournisseur+(1|Consommateur)+(1|Seance)+(1|Fournisseur:Consommateur)+(1|Fournisseur:Seance)+(1|Consommateur:Seance),data=viande,REML=FALSE)
lmerTest::rand(mod.ml) ## Visiblement utiliser le max de vraisemblance n'apporte rien


mod.f <- lmer(Note ~ Fournisseur+(1|Consommateur)+(1|Seance)+(1|Fournisseur:Seance)+(1|Consommateur:Seance),data=viande)
anova(mod.f)
difflsmeans(mod.f, test.effs="Fournisseur")
plot(difflsmeans(mod.f, test.effs="Fournisseur"))
lmerTest::rand(mod.f)


### On constate donc une difference de conclusions entre la méthode à la main et par RMLE