path <- "/Users/memily/AOBox/Enseignement/2017-2018/SpeDataScience/DonneesExperimentales/Data/"

#############
#############
### Exo 1
#############
#############

data <- read.table(paste(path,"ansenso.txt",sep=""),sep="\t",header=TRUE,dec=",")

### On commence par regarder les données (meme si ce n'est pas demandé)
data
summary(data)
data$Produit <- as.factor(data$Produit)
data$Jour <- as.factor(data$Jour)

summary(data)

interaction.plot(data$Produit,data$Jour,data$Note)
interaction.plot(data$Jour,data$Produit,data$Note)

# Q2: Formule générale du modèle linéaire (XX')^(-1)X'Y

# Q3:
require(car)

## Etape 1

mod0 <- lm(Note ~ 1,data=data)
mod.all <- lm(Note ~ Produit+Jour+Produit:Jour,data)

anova(mod0,mod.all)

## Etape 2

anova(mod.all) 

summary(lm(Note ~ Produit+Jour+Produit:Jour,data))

anova(lm(Note ~ Produit+Jour,data))
anova(lm(Note ~ Jour+Produit,data))
anova(lm(Note ~ Produit,data))
anova(lm(Note ~ Jour,data))
### Attention anova fait les SC de Type I

contrasts(data$Produit)
contrasts(data$Jour)

# Modification
contrasts(data$Produit) <- contr.sum(nlevels(data$Produit)) 
contrasts(data$Jour) <- contr.sum(n=2)

Anova(lm(Note ~ Produit+Jour+Produit:Jour,data),type="III") 
## On enlève l'interaction car non significative
Anova(lm(Note ~ Produit+Jour,data),type="III") 
## Le facteur produit est "devenu" significatif!
## Estimer l'interaction était inutile. Nous perdions de l'information pour
## mieux estimer l'effet du facteur produit!
## En d'autres termes : nous n'avions pas assez de degrés de liberté pour
## détecter l'effet Produit

### A partir de là, nous avons sélectionné notre modèle "final" ou
### "le meilleur modèle" ou "le moins mauvais des modèles".

### Question 4: 
### Objectif : les formules du cours ne sont valables que pour des données
### équilibrées
### Ici la moyenne observée va être différente de la moyenne attendue
by(data$Note,data$Produit,mean) ## Moyenne observée par Produit
by(data$Note,data$Jour,mean)
options(constrast=c("contr.sum","contr.sum"))
mod <- lm(Note ~ Produit+Jour,data) 
summary(mod)
mod$coefficients[1]+mod$coefficients[2] ## Moyenne attendue pour le Produit 1
mod$coefficients[1]+mod$coefficients[3] ## Moyenne attendue pour le Produit 2
mod$coefficients[1]-mod$coefficients[2]-mod$coefficients[3] ## Moyenne attendue pour le Produit 3


# Les resultats ne sont pas cohérents entre eux. Le déséquilibre avec le jour modifie la perception de l'effet produit. En fait c'est le Produit 1 le plus apprécié: il faut en effet ''corriger'' de l'effet jour.

### Question 5 et 6: Question difficile car l'effet du produit peut avoir plusieurs sens:
# Normalement on ne parle jamais d'effet d'une modalité 
## Effet du produit 1 ne veut rien dire!!!
## Il faut retraduire
#- Effet du produit 1 par rapport à un autre produit de référence (Produit 2 par exemple)?
contrasts(data$Produit) <- contr.treatment(n=3,base=2) ## Produit 2 = référence
mod <- lm(Note ~ Produit+Jour,data) 
summary(mod)
# ---> Le Produit 1 est significativement différent du Produit 2
#- Effet du produit 1 par rapport à l'ensemble des produits?
contrasts(data$Produit) <- contr.sum(n=3) ## Somme des alpha i = 0
mod <- lm(Note ~ Produit+Jour,data) 
summary(mod)
# ---> Le Produit 1 n'est pas significativement différent d'un Produit au hasard

#- Effet du produit 1 par rapport à l'ensemble des autres produits?
require(multcomp)
summary(mod)
# H0 - 1:
summary(glht(mod,linfct=mcp(Produit=c(1,-1,0)/2)))
# H0 - 2:
summary(mod)
# H0 - 3:
summary(glht(mod,linfct=mcp(Produit=c(2,-1,-1)/3)))
summary(glht(mod,linfct=mcp(Produit=c(1,-1/2,-1/2)))) ## Meme test

summary(glht(mod,linfct=mcp(Produit=rbind(
		c(1,-1,0),
		c(1,0,-1),
		c(2,-1,-1)))))

contrasts(data$Produit) <- contr.treatment(3,1)
mod2 <- lm(formula = Note ~ Produit + Jour, data = data)
summary(glht(mod2,linfct=mcp(Produit=rbind(
		c(1,-1,0),
		c(1,0,-1)))))


### Question 6

#####
#####
## Post-Hoc analysis
# Two-way

## Instabilite de TukeyHSD
amod.2w <- aov(Note ~ Produit+Jour,data=data)
TukeyHSD(amod.2w,"Produit")
amod.2w.2 <- aov(Note ~ Jour+Produit,data=data)
TukeyHSD(amod.2w.2,"Produit")
## Donc TukeyHSD dépend de l'ordre des facteurs.

modele.2way.1 <- lm(Note ~ Produit+Jour,data=data)
tuk.1 <- glht(modele.2way.1,linfct=mcp(Produit="Tukey"))
summary(tuk.1)
modele.2way.2 <- lm(Note ~ Jour+Produit,data=data)
tuk.2 <- glht(modele.2way.2,linfct=mcp(Produit="Tukey"))
summary(tuk.2)

# Contrasts (Déjà vu!!):
tuk.EffetProduit1 <- glht(modele.2way.1,linfct=mcp(Produit=c(2,-1,-1)/3))
summary(tuk.EffetProduit1)

tuk.EffetProduit2 <- glht(modele.2way.1,linfct=mcp(Produit=c(-1,2,-1)/3))
summary(tuk.EffetProduit2)

tuk.EffetProduitAll <- glht(modele.2way.1,linfct=mcp(Produit=rbind(c(2,-1,-1)/3,c(-1,2,-1)/3,c(-1,-1,2)/3)))
summary(tuk.EffetProduitAll)

tuk.EffetProduitTest <- glht(modele.2way.1,linfct=mcp(Produit=c(-1,0,1)))
summary(tuk.EffetProduitTest)


# Question 7 et 8...parce que l'on vous le demande des fois en stage...
## Personnellement je regarde très rarement ces indicateurs

### Normalité des résidus
hist(resid(modele.2way.1))
shapiro.test(resid(modele.2way.1))
## Pas significatif donc les résidus suivent une loi normale

### Homoscédasticité
boxplot(resid(modele.2way.1)~Produit,data=data)
bartlett.test(resid(modele.2way.1)~Produit,data=data)
boxplot(resid(modele.2way.1)~Jour,data=data)
bartlett.test(resid(modele.2way.1)~Jour,data=data)
## Pas significatif donc les résidus sont homoscédastiques = même variance
## dans chaque groupe




#############
#############
### Exo 2
#############
#############


compote <- read.table(paste(path,"compote.txt",sep=""),sep="\t",header=TRUE,dec=",")
names(compote)
summary(compote)
compote$juge <- as.factor(compote$juge)
compote$rang <- as.factor(compote$rang)
summary(compote)
table(compote$juge,compote$produit)
table(compote$rang,compote$produit)

require(car)
result <- matrix(0,24,6)
for (i in 1:24){
	res <- aov(compote[,i+4] ~ produit+juge,data=compote)
	result[i,1] <- Anova(res,type="III")[2,4]
	residu <- residuals(res)
	result[i,2] <- shapiro.test(residu)$p.value
	result[i,3] <- bartlett.test(residu ~ compote[!is.na(compote[,i+4]),"produit"])$p.value
	result[i,4] <- friedman.test(compote[,i+4] ~ produit|juge,data=compote)$p.value
	
	res <- aov(compote[,i+4] ~ produit,data=compote)
	result[i,5] <- Anova(res,type="III")[2,4]
	result[i,6] <- kruskal.test(compote[,i+4] ~produit,data=compote)$p.value
}
result <- as.data.frame(result)
dimnames(result)[[1]] <- dimnames(compote)[[2]][5:28]
names(result) <- c("Aov2","Shapiro","Bartlett","Friedman","Aov1","Kruskal")

result



