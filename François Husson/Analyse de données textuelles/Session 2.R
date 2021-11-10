#Session 2

#Exercices

#Question 1

library(FactoMineR)
dataset <- matrix(rnorm(1000),ncol = 200)

PCA(dataset)

#On voit des liaisons entre les variables pourtant on a inclu que des données indépendantes
#Si on a très peu d'individus, il est très facile d'observer des corrélations même dues au hasard.
#Il faut donc éviter d

#Question 2

data(decathlon)
don <- decathlon[1:7,1:10]

DON<-PCA(don)
DON$eig

#On a gardé 61% de l'information, l'inertie n'est pas suffisante pour pouvoir interpréter le graphique.

Simul <- function(nbsimul,nind,nvar){
  Inertie <- rep (0,nbsimul)
  for (i in 1:nbsimul){
    Randdata <- matrix(rnorm(nind*nvar),ncol=nvar)
    ResPCA<-PCA(Randdata,graph=F)
    Inertie[i]<-ResPCA$eig[2,3]
  }
  quantile(Inertie,0.95)
}

Simul(nbsimul = 100,nind = 5, nvar = 10)

simul <- 100
PCAnew <- rep (0,length(Simul))
Diff <- rep (0, 10)
for (j in 1:10){
for (i in 1:simul){
permuteLigne <- function(v) {return(v[sample(1:length(v),replace=FALSE)])}
donnew <- apply(don,2,permuteLigne)
PCAnew[i] <- PCA(donnew,graph=F)$eig[2,3]
}
Diff[j] <- abs(quantile(PCA(don,graph=F)$eig[2,3],0.95)-quantile(PCAnew,0.95))
}
Diff
#PS : pour la boucle, ça sert à rien, faut surtout regarder le permuteligne et donnew (avec les PCAnew mais sans les i)
#Permuter les lignes ça casse complétement la structure du jeu de données (ça casse les corrélations)
#Autre intérêt : en ACM puisque les variables sont qualitatives
#On garde donc les variables avec les modalités de chaque variable mais on casse la structure




############
#Exercice 2#
############

orange <- read.table("https://husson.github.io/img/orange_chimie_senso.csv",
                     header=TRUE,sep=";",row.names=1)
PCA(orange[,1:8])

#Glucose/Fructose corrélés, acide citrique et titre corrélés
#PH avant et après centri similaires
#Saccharose opposé à fructose et glucose

#91.4 d'inertie

PCA(orange[,-(1:8)])

#Amer Acide et intensité du goût corrélés
#Intensité odeur et pulpeux corrélés
#Sucré opposé Amer et Acide

#86.82 d'inertie

#Soit on met tout dans la même ACP
#Soit on met l'un ou l'autre en supplementary

PCA(orange, quanti.sup = 9:13)
PCA(orange, quanti.sup = 1:8)

#On aurait fait une ACM

#Comment faire les deux groupes de variables simultanément en actif si on a les var sensorielle en var qualitatives