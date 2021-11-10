path <- "/Users/memily/AOBox/Enseignement/2017-2018/SpeDataScience/DonneesExperimentales/Data/"

get.pval <- function(sum.aov,i,j){
	Fobs <- sum.aov[i,3]/sum.aov[j,3]
	pval <- 1-pf(Fobs,df1=sum.aov[i,1],df2=sum.aov[j,1])
	return(pval)
}

Tor <- read.table(paste(path,"torref.txt",sep=""),sep="\t",header=TRUE,dec=",",na.strings="")

Tor$Torrefacteur <- factor(Tor$Torrefacteur)
Tor$Type <- factor(Tor$Type)
Tor$Marque <- factor(Tor$Marque)
Tor$Moment <- factor(Tor$Moment)

options(contrast=c("contr.sum","contr.sum"))

###
# Tous les effets peuvent etre interessants a etudier pour savoir sur quels parametres jouer pour reduire les taux d'humidité. Cependant, l'interaction Moment:Marque est interessante car si elle est significative cela signifie que l'on peut adapter la production et fabriquer certaines marques plutot le matin, d'autres plutot l'apres-midi et d'autres plutot la nuit. S'il y a un effet moment important, cela signifie qu'il faut peut-etre faire des adaptations sur le site pour que le taux d'humidite reste stable (meilleure isolation, ...). Il est attendu que le type de torrefaction lent soit plus interessant (taux d'humidite plus faibles) mais il est moins rentable (car plus lent!). S'il y a un effet torréfacteur, cela signifie peut-etre qu'un torrefacteur (d'un type de torréfaction) est mal regle.
###


mod.Tor <- aov(Humidite ~ Type+Moment+Torrefacteur%in%Type+Marque%in%Type+Type:Moment+Moment:(Torrefacteur%in%Type)+Moment:(Marque%in%Type)+(Torrefacteur%in%Type):(Marque%in%Type),data=Tor)

aov.Tor <- summary(mod.Tor)

## Effet (Torrefacteur%in%Type):(Marque%in%Type) teste par rapport a la residuelle
get.pval(aov.Tor[[1]],8,9) ## Significatif
## Effet Moment:(Marque%in%Type) teste par rapport a la residuelle
get.pval(aov.Tor[[1]],7,9) ## Non Significatif 
## Effet Moment:(Torrefacteur%in%Type) teste par rapport a la residuelle
get.pval(aov.Tor[[1]],6,9) ## Significatif 

## On enleve Moment:(Marque%in%Type)
mod.Tor.2 <- aov(Humidite ~ Type+Moment+Torrefacteur%in%Type+Marque%in%Type+Type:Moment+Moment:(Torrefacteur%in%Type)+(Torrefacteur%in%Type):(Marque%in%Type),data=Tor)
aov.Tor.2 <- summary(mod.Tor.2)

## Effet Type:Moment teste par rapport a Moment:(Torrefacteur%in%Type)
get.pval(aov.Tor.2[[1]],5,6) ## Non Significatif (Mais on ne peut pas l'enlever du modele)

## Effet Marque%in%Type par rapport a (Torrefacteur%in%Type):(Marquer%in%Type)
get.pval(aov.Tor.2[[1]],4,7) ## Non Significatif  (Mais on ne peut pas l'enlever)

## Effet Torrefacteur%in%Type par rapport a (Torrefacteur%in%Type):(Marque%in%Type)
get.pval(aov.Tor.2[[1]],3,7) ## Non Significatif (Mais on ne peut pas l'enlever)

## Effet Moment par rapport a Moment:(Torrefacteur%in%Type)
get.pval(aov.Tor.2[[1]],2,6) ## Significatif 

## Effet Type par rapport a Torrefacteur%in%Type+Marque%in%Type-(Torrefacteur%in%Type):(Marquer%in%Type)

CM.Ty <- aov.Tor.2[[1]][1,3]
ddl.Ty <- aov.Tor.2[[1]][1,1]
CM.TiTy <- aov.Tor.2[[1]][3,3]
ddl.TiTy <- aov.Tor.2[[1]][3,1]
CM.MaiTy <- aov.Tor.2[[1]][4,3]
ddl.MaiTy <- aov.Tor.2[[1]][4,1]
CM.TMaiTy <- aov.Tor.2[[1]][7,3]
ddl.TMaiTy <- aov.Tor.2[[1]][7,1]


F.Type <- CM.Ty/(CM.TiTy+CM.MaiTy-CM.TMaiTy)
ddl.denom <- (CM.TiTy+CM.MaiTy-CM.TMaiTy)^2/(CM.TiTy^2/ddl.TiTy+CM.MaiTy^2/ddl.MaiTy+CM.TMaiTy^2/ddl.TMaiTy)
1-pf(F.Type,ddl.Ty,ddl.denom) ## non significatif

