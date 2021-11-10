#exemple sur package rat

require(GAD)
require(stats)
require(base)

get.pval.Anova <- function(Anova.Table,i,j){
  Fobs <- (Anova.Table[i,1]/Anova.Table[i,2])/(Anova.Table[j,1]/Anova.Table[j,2])
  pval <- 1-pf(Fobs,df1=Anova.Table[i,2],df2=Anova.Table[j,2])
  return(list(Fobs=Fobs,pval=pval))
}


data(rats)

rats$treat <- as.factor(rats$treat)
rats$rat <- as.factor(rats$rat)
rats$liver <- as.factor(rats$liver)

contrasts(rats$treat)<- contr.sum(n=nlevels(rats$treat))
contrasts(rats$rat)<- contr.sum(n=nlevels(rats$rat))
contrasts(rats$liver)<- contr.sum(n=nlevels(rats$liver))

# methode 1

require(car)

mod.A <- lm(glycog~ treat + rat%in%treat + liver%in%rat%in%treat,data=rats)

tab.A <- Anova(mod.A,type="III")
get.pval.Anova(tab.A,4,5) #Liver effect
get.pval.Anova(tab.A,3,4) #Rat effect
get.pval.Anova(tab.A,2,3) #treat effect

# methode 2
A<-as.fixed(rats$treat)
B<-as.random(rats$rat)
C<-as.random(rats$liver)

mod.gad<-lm(glycog ~ A + B%in%A + C%in%B%in%A, data = rats)
gad(mod.gad)

# methode 3

mod.lmer<-lmer(glycog~treat+(1|treat:rat)+(1|liver:rat:treat),data=rats)
anova(mod.lmer)
summary(mod.lmer)
library(lmerTest)
ranova(mod.lmer)
