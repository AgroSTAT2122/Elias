#Initialisation

setwd("E:/Master 2/Mathieu Emily/Analyse de variance")

require(car)
# require(lme4)
require(lmerTest)

viande <- read.table("viande.txt",header=TRUE, dec=",")

viande$Fournisseur <- as.factor(viande$Fournisseur)
viande$Consommateur <- as.factor(viande$Consommateur)
viande$Seance <- as.factor(viande$Seance)

boxplot(Note ~ Fournisseur,data=viande)
boxplot(Note ~ Consommateur,data=viande)
boxplot(Note ~ Seance,data=viande)

table(viande$Fournisseur,viande$Consommateur,viande$Seance)
## Heteroscedasticite

anova(lm(Note ~ Fournisseur*Consommateur*Seance,data=viande))

mod0 <- lm(Note~ 1, data = viande)
mod <- lmer(Note ~ (1|Consommateur) + Fournisseur + (1|Seance) + (1|Consommateur:Fournisseur) + (1|Consommateur:Seance) + (1|Fournisseur:Seance),data = viande)

#Le modèle retenu est "mod"

#2

require("GAD")
Cons <- as.random(viande$Consommateur)
Four <- as.fixed(viande$Fournisseur)
Sea <- as.random(viande$Seance)

mod.viande.1.gad <- lm(Note ~ Four+Sea+Cons+Four:Sea+Sea:Cons+Four:Cons,data=viande)
gad(mod.viande.1.gad)

summary(mod)
anova(mod,type=3) # Test des effets fixes
difflsmeans(mod, test.effs="Fournisseur")
plot(difflsmeans(mod, test.effs="Fournisseur"))
rand(mod) ## Test des effets aleatoires

mod2 <- lmer(Note ~ (1|Consommateur) + Fournisseur + Seance + (1|Consommateur:Seance) + Fournisseur:Seance,data = viande)
anova(mod,mod2,type="F")
2*(logLik(mod)[1]-logLik(mod2)[1])

step(mod)

#3
#Le fournisseur 4 est meilleur significativement que le 3 sur l'appréciation globale mais le 1 et le 2 ne sont pas significativement différents que le numéro 4.

#4 
#Selon d'autres variables qui ne sont pas implémentées pour l'appréciation gloable (ex : le prix, l'impact écologique) on va peut être privilégier le 1 ou le 2 plutôt que le 4
#puisque le 1 ou le 2 ne sont pas significativement moins bons que le 4 en terme d'appréciation globale

############


