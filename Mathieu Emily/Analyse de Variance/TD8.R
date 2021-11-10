setwd("E:/Master 2/Mathieu Emily/Analyse de variance")



get.pval <- function(sum.aov,i,j){
	Fobs <- sum.aov[i,3]/sum.aov[j,3]
	pval <- 1-pf(Fobs,df1=sum.aov[i,1],df2=sum.aov[j,1])
	return(pval)
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

chocolat <- read.table("choc_centre.txt",sep="\t",header=TRUE,dec=",",na.strings="")

chocolat$Centre <- factor(chocolat$Centre)
contrasts(chocolat$Centre) <- contr.sum(nlevels(chocolat$Centre))
chocolat$Seance <- factor(chocolat$Seance)
contrasts(chocolat$Seance) <- contr.sum(nlevels(chocolat$Seance))
chocolat$Juge <- factor(chocolat$Juge)
contrasts(chocolat$Juge) <- contr.sum(nlevels(chocolat$Juge))
chocolat$Produit <- factor(chocolat$Produit)
contrasts(chocolat$Produit) <- contr.sum(nlevels(chocolat$Produit))

table(chocolat$Centre)
table(chocolat$Centre,chocolat$Juge,chocolat$Produit)


#########################
##### Differents facteurs de variabilite
## Produit: Fixe
## Centre: Aleatoire
## Juge: Aleatoire
## Seance: Aleatoire

#########################
##### Differents effets 
## Produit
## Centre
## Seance%in%Centre
## Juge%in%Centre
#### Produit:Centre
#### Produit:(Juge%in%Centre)
#### Produit:(Seance%in%Centre)
#### (Seance%in%Centre):(Juge%in%Centre)


## Modele avec aov
mod.Choc <- aov(S.Cacao ~ Produit+Centre+Seance%in%Centre+Juge%in%Centre+Produit:Centre+Produit:(Juge%in%Centre)+Produit:(Seance%in%Centre)+(Seance%in%Centre):(Juge%in%Centre),data=chocolat)
aov.choc <- summary(mod.Choc)
aov.choc

## Modele avec Anova - Ne fonctionne pas à cause du déséquilibre entre le nombre de juges par centre
mod.Choc.Anova <- Anova(lm(S.Cacao ~ Produit+Centre+Centre:Seance+Centre:Juge+Produit:Centre+Produit:Centre:Juge+Produit:Centre:Seance+Centre:Seance:Juge,data=chocolat),type="III")


## Effet (Seance%in%Centre):(Juge%in%Centre) teste par rapport à residuelle
get.pval(aov.choc[[1]],8,9) ## Significatif

## Effet Produit:(Seance%in%Centre) teste par rapport à residuelle
get.pval(aov.choc[[1]],7,9) ## Non Significatif

## Effet Produit:(Juge%in%Centre) teste par rapport à residuelle
get.pval(aov.choc[[1]],6,9) ## Significatif

## Effet Produit:Centre teste par rapport à Produit:(Seance%in%Centre)+Produit:(Juge%in%Centre)-(Seance%in%Centre):(Juge%in%Centre). -> plus complique
## Comme Produit:(Seance%in%Centre) n'est pas significatif, on l'enleve du modele

mod.Choc.2 <- aov(S.Cacao ~ Produit+Centre+Seance%in%Centre+Juge%in%Centre+Produit:Centre+Produit:(Juge%in%Centre)+(Seance%in%Centre):(Juge%in%Centre),data=chocolat)
aov.choc.2 <- summary(mod.Choc.2)
aov.choc.2

## Les effets (Seance%in%Centre):(Juge%in%Centre) et Produit:(Juge%in%Centre) restent significatifs
## Pour Produit:Centre on compare à Produit:(Juge%in%Centre)
get.pval(aov.choc.2[[1]],5,6) ## Significatif

## Effet Juge%in%Centre teste par rapport à (Seance%in%Centre):(Juge%in%Centre)
get.pval(aov.choc.2[[1]],4,7) ## Significatif

## Effet Seance%in%Centre teste par rapport à (Seance%in%Centre):(Juge%in%Centre)
get.pval(aov.choc.2[[1]],3,7) ## Non Significatif

## Effet Centre teste par rapport à (Seance%in%Centre)+(Juge%in%Centre)-(Seance%in%Centre):(Juge%in%Centre). -> plus complique
## Comme (Seance%in%Centre) n'est pas significatif, on est tente de l'enlever du modele...Mais cet effet present dans l'interaction (Seance%in%Centre):(Juge%in%Centre). Je dois donc le garder dans le modele.

CM.C <- (aov.choc.2[[1]][2,3])
ddl.C <- aov.choc.2[[1]][2,1]
CM.SiC <- (aov.choc.2[[1]][3,3])
ddl.SiC <- aov.choc.2[[1]][3,1]
CM.JiC <- (aov.choc.2[[1]][4,3])
ddl.JiC <- aov.choc.2[[1]][4,1]
CM.SJiC <- (aov.choc.2[[1]][7,3])
ddl.SJiC <- aov.choc.2[[1]][7,1]


F.Centre <- CM.C/( CM.SiC+CM.JiC-CM.SJiC)
ddl.denom <- (CM.SiC+CM.JiC-CM.SJiC)^2/(CM.SiC^2/ddl.SiC+CM.JiC^2/ddl.JiC+CM.SJiC^2/ddl.SJiC)
1-pf(F.Centre,ddl.C,ddl.denom) ## non significatif

## Effet Produit teste par rapport à Centre:Produit

get.pval(aov.choc.2[[1]],1,5)

######
######
## Interpretation des effets

## Le fort effet Produit signife qu'au moins un produit à une saveur signifcativement différente des autres. Effet très important à noter. Cela signifie que la gamme de produit est différente pour cette variable (ce descripteur) saveur de cacao.


## pas d'effet Centre (mais le test donne une probabilité critique limite, et avec plus de données on mettrait peut-être en évidence cet effet) : le test mis en place est peu puissant et on est à la limite de détection. Mais l'effet Centre n'est pas très intéressant car il veut juste dire que d'un centre à l'autre l'utilisation de l'échelle de note n'est pas la même.

## une interaction Produit-Centre significative signifie que les produits ne sont pas évalués de la même façon d'un centre à l'autre. Interaction très importante à noter. Ceci est problématique car cela signifie d'un centre à l'autre, l'évaluation de la saveur cacao n'est pas la même.
 
names(chocolat)
interaction.plot(chocolat$Produit,chocolat$Centre,chocolat[,12],col=1:6,xlab="Produit",ylab="S.Cacao")

### Le centre 4 a l'air different des autres
