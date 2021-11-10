######################
######################
### Exo 1
######################
######################

######################
## Q1
######################

#Initialisation

setwd("E:/Master 2/Mathieu Emily/Analyse de variance")

Mucine <- read.table("mucine.txt",header=TRUE, dec = ",",stringsAsFactors = TRUE)

names(Mucine)

Mucine$veau <- as.factor(Mucine$veau)
Mucine$proteines <- as.factor(Mucine$proteines)
summary(Mucine)

boxplot(mucines ~ site,data=Mucine)
boxplot(mucines ~ proteines,data=Mucine)
boxplot(mucines ~ veau,data=Mucine)

table(Mucine$site,Mucine$proteines,Mucine$veau)
## Heteroscedasticite

anova(lm(mucines ~ veau*proteines*site,data=Mucine))

# site et proteines=effets fixes
# veau=effet aleatoire


######################
## Q2
######################

# Considerer la quantite de proteine en quali evite de faire une hypothese de linearite sur cette variable. A l'inverse, si la variable proteine est qualitative, nous perdons le possible ordonnacement entre les modalite: 0< 10 < 20 < 30.
 
######################
## Q3
######################

library(car)
options(contrasts=c("contr.sum","contr.sum"))
mod.mucine <- lm(mucines ~ veau+proteines+site+veau:proteines+veau:site+proteines:site,data=Mucine)
mod.anova <- Anova(mod.mucine,type="III") ## ne change pas d'une anova de type I
#mod.anova <- anova(mod.mucine) ## ne change pas d'une anova de type I
print(mod.anova)

# Facteurs aleatoires: Veau
# Facteurs fixes: proteines et site
#########
## Methode 1: "A la main"
#########

# Remarque: N'ayant pas de repetition sur les combinaisons des 3 facteurs, on ne peut pas tester l'interaction triple

## Comme il n'y pas d'interaction triple dans le modele, les interactions doubles sont testees par rapport a la residuelle:
# pas d'interaction veau:site
# pas d'interaction proteines:site 
# interaction veau:site 

## Test des effets individuels
# Effet veau: on teste par rapport a la residuelle: on peut lire directement dans le tableau d'anova
# Effet proteines: on teste par rapport à l'interaction proteines:veau
F.proteines <- (mod.anova[[1]][3]/mod.anova[[2]][3])/(mod.anova[[1]][5]/mod.anova[[2]][5])
1-pf(F.proteines,mod.anova[[2]][3],mod.anova[[2]][5])
# Effet site: on teste par rapport à l'interaction site:veau
F.site <- (mod.anova[[1]][4]/mod.anova[[2]][4])/(mod.anova[[1]][6]/mod.anova[[2]][6])
1-pf(F.site,mod.anova[[2]][4],mod.anova[[2]][6])

## On pourrait enlever les effets inutiles d'interaction non significatifs (à faire pas à pas)
mod.mucine.2 <- lm(mucines ~ veau+proteines+site+veau:site+proteines:site,data=Mucine)
mod.anova.2 <- Anova(mod.mucine.2,type="III") ## ne change pas d'une anova de type I
#mod.anova.2 <- anova(mod.mucine.2) ## ne change pas d'une anova de type I


mod.mucine.3 <- lm(mucines ~ veau+proteines+site+veau:site,data=Mucine)
mod.anova.3 <- Anova(mod.mucine.3,type="III") ## ne change pas d'une anova de type I
#mod.anova.3 <- anova(mod.mucine.3) ## ne change pas d'une anova de type I
# Effet site: on teste par rapport à l'interaction site:veau
F.site <- (mod.anova.3[[1]][4]/mod.anova.3[[2]][4])/(mod.anova.3[[1]][5]/mod.anova.3[[2]][5])
1-pf(F.site,mod.anova.3[[2]][4],mod.anova.3[[2]][5])


## CCL: Effet proteines

#########
## Methode 2: avec le package GAD
#########

require("GAD")
Prot <- as.fixed(Mucine$proteines)
Sit <- as.fixed(Mucine$site)
Vea <- as.random(Mucine$veau)

mod.mucine.1.gad <- lm(mucines ~ Prot+Sit+Vea+Prot:Sit+Prot:Vea+Sit:Vea,data=Mucine)
gad(mod.mucine.1.gad)

mod.mucine.2.gad <- lm(mucines ~ Prot+Sit+Vea+Prot:Sit+Sit:Vea,data=Mucine)
gad(mod.mucine.2.gad)

mod.mucine.final.gad <- lm(mucines ~ Prot+Sit+Vea+Sit:Vea,data=Mucine)
gad(mod.mucine.final.gad)

#########
## Methode 3: avec le package lme4 et lmerTest
#########

#require(lme4)
require(lmerTest)
mod <- lmer(mucines ~ (1|veau)+proteines+site+(1|veau:proteines)+(1|veau:site)+proteines:site,data=Mucine)
summary(mod)
anova(mod,type=3) # Test des effets fixes
difflsmeans(mod, test.effs="proteines")
plot(difflsmeans(mod, test.effs="proteines"))
rand(mod) ## Test des effets aleatoires

mod2 <- lmer(mucines ~ proteines+site+(1|veau:proteines)+(1|veau:site)+proteines:site,data=Mucine)

anova(mod,mod2,type="F")
2*(logLik(mod)[1]-logLik(mod2)[1])


step(mod)

######################
## Q4
######################

#Si les régimes alimentaires avaient été testés sur des veaux diérents, on ne pourrait ni tester l'interaction Veau:Proteine, ni l'interaction Site:Veau car pour chaque veau on aurait un seul pourcentage de protéine testé, et pour chaque veau on aurait une seule mesure par site. On prendrait donc le modèle : mucine=Proteine+site+proteine:site+Veau L'inconvénient de ce modèle par rapport au précédent est que la résiduelle correspond à l'interaction site:veau et à l'interaction protéine:veau et à la vraie résiduelle. On aurait donc une résiduelle importante et on aurait du mal à mettre en évidence les effets testés.

######################
######################
### Exo 2
######################
######################

######################
## Q1
######################

require(lmerTest)

orange <- read.table("/Users/memily/Agro/Enseignement/2014-2015/SpeStat/AnalyseVariance/FrancoisHusson/Anova - Copie/donnees/orange.txt",sep="\t",header=TRUE,dec=",",na.strings="")

orange$seance <- as.factor(orange$seance)
orange$juge <- as.factor(orange$juge)
orange$rang <- as.factor(orange$rang)
orange$premier <- as.factor(orange$premier)
orange$precedent <- as.factor(orange$precedent)
orange$produit <- as.factor(orange$produit)
summary(orange)

library(car)
options(contrasts = c("contr.sum", "contr.sum"))
for (i in 1:6) orange[,i] <- as.factor(orange[,i])
result <- matrix(0,10,3)
for (i in 1:10){
res <- Anova(aov(orange[,i+6]~produit+juge+produit:juge,data=orange),type="III")
Fobs <- (res[2,1]/res[2,2])/(res[4,1]/res[4,2])
result[i,1] <- pf(Fobs,res[2,2],res[4,2],lower.tail=FALSE)
result[i,2] <- res[2,4]
result[i,3] <- res[4,4]
}
colnames(result) <- c("Proba aléa","Proba fixe","Proba interaction")
rownames(result) <- colnames(orange)[7:16]

print(result) ## Affichage du tableau de resultat


## Exemple avec GAD pour O.Intensite
require(GAD)
P <- as.fixed(orange$produit)
J <- as.random(orange$juge)
mod <- lm(O.Intensite ~ P+J+P:J,data=orange)
gad(mod)

######################
## Q2
######################

### 
### On peut rajouter des effets qui sont potentiellement intéressants - le probleme reside dans le desequilibre des donnees (ainsi que le manque de variabilite) si on integre trop de facteurs.

### Selection de modele à la main

## Step0: modele avec tous les effets
mod.orange.test <- lm(G.Appreciation~produit+juge+seance+precedent+rang+produit:juge+produit:seance+juge:seance,data=orange)
mod.anova.orange.test <- anova(mod.orange.test) 



# Exemple de l'effet produit
CM.P <- mod.anova.orange.test[1,3]
ddl.P <- mod.anova.orange.test[1,1]
CM.P.S <- mod.anova.orange.test[7,3]
ddl.P.S <- mod.anova.orange.test[7,1]
CM.P.J <- mod.anova.orange.test[6,3]
ddl.P.J <- mod.anova.orange.test[6,1]
CM.R <- mod.anova.orange.test[9,3]
ddl.R <- mod.anova.orange.test[9,1]
 
F.Produit <- CM.P/( CM.P.S+CM.P.J-CM.R)
ddl.denom <- (CM.P.S+CM.P.J-CM.R)^2/(CM.P.S^2/ddl.P.S+CM.P.J^2/ddl.P.J+CM.R^2/ddl.R)
1-pf(F.Produit,ddl.P,ddl.denom)
## Rmq: on ne peut pas estimer les carrés de Type III : il y a trop de facteur dans le modele...Cependant, en ce qui concerne les interactions d'ordre le plus eleve, l'estimation par Type I ou Type III est identique.

## On enleve produit:seance

mod.orange.2 <- lm(G.Appreciation~produit+juge+seance+rang+precedent+produit:juge+juge:seance,data=orange)
mod.anova.orange.2 <- anova(mod.orange.2) 

## On enleve juge:seance

mod.orange.3 <- lm(G.Appreciation~produit+juge+seance+rang+produit:juge+precedent,data=orange)
mod.anova.orange.3 <- anova(mod.orange.3)

## On enleve precedent

mod.orange.4 <- lm(G.Appreciation~produit+juge+seance+rang+produit:juge,data=orange)
mod.anova.orange.4 <- anova(mod.orange.4)

## On enleve rang

mod.orange.5 <- lm(G.Appreciation~produit+juge+seance+produit:juge,data=orange)
mod.anova.orange.5 <- anova(mod.orange.5) 
mod.anova.orange.5 <- Anova(mod.orange.5,type="III") ## A partir d'ici, nous pouvons estimer les SC de Type III. Cependant, on se retrouve dans un cas equilibre donc Type 1=Type 3
mod.anova.orange.5 <- anova(mod.orange.5) 

## Pour l'effet produit, il faut tester par rapport à l'interaction
F.produit <- (mod.anova.orange.5[1,3]/mod.anova.orange.5[4,3])
1-pf(F.produit,mod.anova.orange.5[1,1],mod.anova.orange.5[4,1])

### Etude "Post-Hoc"

summary(mod.orange.5)$coefficients[2:12,]
-sum(summary(mod.orange.5)$coefficients[2:12,1]) ## coefficient du 12eme produit

## Le 11eme produit est le + apprecie (Tropicana Pulpissimo)

### Modeles avec GAD

P <- as.fixed(orange$produit)
Pr <- as.fixed(orange$precedent)
R <- as.fixed(orange$rang)
J <- as.random(orange$juge)
S <- as.random(orange$seance)

### Modele initial
mod.orange.1.gad <- lm(G.Appreciation~P+J+S+Pr+R+P:J+P:S+J:S,data=orange)
gad(mod.orange.1.gad) ## GAD ne fonctionne pas avec des donnees desequilibrees
# gad ne fonctionne pas avec des donnees desequilibrees

### Modele final
mod.orange.5.gad <- lm(G.Appreciation~P+J+S+P:J,data=orange)
gad(mod.orange.5.gad)

### Modele initial et final avec lmer

mod.lmer <- lmer(G.Appreciation~produit+precedent+rang+(1|juge)+(1|seance)+(1|produit:juge)+(1|produit:seance)+(1|juge:seance),data=orange)
summary(mod.lmer)
anova(mod.lmer,type=3)
difflsmeans(mod.lmer, test.effs="produit")
plot(difflsmeans(mod.lmer, test.effs="produit"))
rand(mod.lmer) ### On retrouve les memes conclusions qu'avec la methode à la main
step(mod.lmer) ### Le modele selectionné est le meme qu'avec la methode à la main

mod.lmer.2 <- lmer(G.Appreciation~produit+(1|juge)+(1|seance)+(1|produit:juge),data=orange)

summary(mod.lmer.2)
anova(mod.lmer.2,type=3)  ## On retrouve la meme p-valeur qu'à la main ou avec GAD
difflsmeans(mod.lmer.2, test.effs="produit")
plot(difflsmeans(mod.lmer.2, test.effs="produit"))
rand(mod.lmer.2) ## Les tests sur les effets aléatoires sont differents qu'à la main ou avec GAD

