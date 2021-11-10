#TD7

setwd("E:/Master 2/Mathieu Emily/Analyse de variance")

require(car)
require(lmerTest)
require(GAD)

get.pval.Anova <- function(Anova.Table,i,j){
  Fobs <- (Anova.Table[i,1]/Anova.Table[i,2])/(Anova.Table[j,1]/Anova.Table[j,2])
  pval <- 1-pf(Fobs,df1=Anova.Table[i,2],df2=Anova.Table[j,2])
  return(list(Fobs=Fobs,pval=pval))
}

Amine <- read.table("amines.txt",header=TRUE,dec=",",sep="\t", stringsAsFactors = TRUE)

Amine$plante <- as.factor(Amine$plante)


contrasts(Amine$traitement)<- contr.sum(n=nlevels(Amine$traitement))
contrasts(Amine$sous.famille)<- contr.sum(n=nlevels(Amine$sous.famille))
contrasts(Amine$espece)<- contr.sum(n=nlevels(Amine$espece))


modPhe <- lmer(Phe ~ traitement + (1|sous.famille:espece) + espece + traitement:espece + (1|traitement:espece:sous.famille),data=Amine)
modSpd <- lmer(Spd ~ traitement + (1|sous.famille:espece) + espece + traitement:espece + (1|traitement:espece:sous.famille),data=Amine)
modDap <- lmer(Dap ~ traitement + (1|sous.famille:espece) + espece + traitement:espece + (1|traitement:espece:sous.famille),data=Amine)

modPhe <- lmer(Phe ~ traitement + (1|sous.famille:espece) + espece + traitement:espece,data=Amine)
modSpd <- lmer(Spd ~ traitement + (1|sous.famille:espece) + espece + traitement:espece + (1|traitement:espece:sous.famille),data=Amine)
modDap <- lmer(Dap ~ traitement + (1|sous.famille:espece) + espece + traitement:espece + (1|traitement:espece:sous.famille),data=Amine)

anova(modPhe)
summary(modPhe)
ranova(modPhe)

anova(modSpd)
summary(modSpd)
ranova(modSpd)

anova(modDap)
summary(modDap)
ranova(modDap)

#D'après cette méthode, les espèces répondent différemment 

mod2Phe <- lm(Phe~ espece, data=Amine)
mod2Spd <- lm(Spd ~ espece, data=Amine)
mod2Dap <- lm(Dap ~ espece, data = Amine)

anova(mod2Phe)
anova(mod2Spd)
anova(mod2Dap)

#Selon l'espèce, la quantité d'amines n'est pas la même

mod3Phe <- lm(Phe~ espece:traitement, data=Amine)
mod3Spd <- lm(Spd ~ espece:traitement, data=Amine)
mod3Dap <- lm(Dap ~ espece:traitement, data = Amine)

anova(mod3Phe)
anova(mod3Spd)
anova(mod3Dap)

#Il y a des  différences significatives entre les espèces à la réponse au stress

mod4Phe <- lm(Phe ~ sous.famille%in%espece:traitement,data=Amine)
mod4Spd <- lm(Spd ~ sous.famille%in%espece:traitement,data=Amine)
mod4Dap <- lm(Dap ~ sous.famille%in%espece:traitement,data=Amine)

anova(mod4Phe) #Les caractéristiques de résistance sont différentes au sein d'une même espèce
anova(mod4Spd) #Les caractéristiques de résistance ne sont pas différentes au sein d'une même espèce
anova(mod4Dap) #Les caractéristiques de résistance sont différentes au sein d'une même espèce
