#Session 2

#Exercices

#Question 1

library(FactoMineR)
dataset <- matrix(rnorm(1000),ncol = 200)

PCA(dataset)

#On voit des liaisons entre les variables pourtant on a inclu que des donn�es ind�pendantes
#Si on a tr�s peu d'individus, il est tr�s facile d'observer des corr�lations m�me dues au hasard.
#Il faut donc �viter d

#Question 2

data(decathlon)
don <- decathlon[1:7,1:10]

DON<-PCA(don)
DON$eig

#On a gard� 61% de l'information, l'inertie n'est pas suffisante pour pouvoir interpr�ter le graphique.

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
#PS : pour la boucle, �a sert � rien, faut surtout regarder le permuteligne et donnew (avec les PCAnew mais sans les i)
#Permuter les lignes �a casse compl�tement la structure du jeu de donn�es (�a casse les corr�lations)
#Autre int�r�t : en ACM puisque les variables sont qualitatives
#On garde donc les variables avec les modalit�s de chaque variable mais on casse la structure




############
#Exercice 2#
############

orange <- read.table("https://husson.github.io/img/orange_chimie_senso.csv",
                     header=TRUE,sep=";",row.names=1)
PCA(orange[,1:8])

#Glucose/Fructose corr�l�s, acide citrique et titre corr�l�s
#PH avant et apr�s centri similaires
#Saccharose oppos� � fructose et glucose

#91.4 d'inertie

PCA(orange[,-(1:8)])

#Amer Acide et intensit� du go�t corr�l�s
#Intensit� odeur et pulpeux corr�l�s
#Sucr� oppos� Amer et Acide

#86.82 d'inertie

#Soit on met tout dans la m�me ACP
#Soit on met l'un ou l'autre en supplementary

PCA(orange, quanti.sup = 9:13)
PCA(orange, quanti.sup = 1:8)

#On aurait fait une ACM

#Comment faire les deux groupes de variables simultan�ment en actif si on a les var sensorielle en var qualitatives