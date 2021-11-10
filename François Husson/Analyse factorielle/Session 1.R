#Cours François Husson Analyse Factorielle


library(raster) # raster et rgdal à installer
library(rgdal)
photo <- raster("https://husson.github.io/img/Lena.png")
photo <- as.matrix(photo)
dim(photo)

n=nrow(photo)
p=ncol(photo)


#SVD sur les données

SVD<-svd(photo,nu=min(512,512),nv=min(512,512))
u<-SVD$u
d<-SVD$d
v<-SVD$v



#Correction

#Comparaison originale avec rang matrice

par(mar=c(0,0,2,0),mfrow=c(2,3),xaxt="n",yaxt="n")
image(photo,col = grey(seq(0,1,length=256)),asp=1)
title(paste("Original, 100%"), cex.main=.9)
r=1 ; image((u[,1]*d[1])%*%t(v[,1]), col = grey(seq(0,1 ,length = 256)), asp = 1)
title(paste("r=",r, " ",round(((n+1+p)*r)/(n*p)*100,1),"%"),cex.main=0.9)

for(r in c(10,20,50,100)){
  image(u[,1:r]%*%diag(d[1:r])%*%t(v[,1:r]), col = grey (seq(0,1, length=256)), asp=1)
  title(paste("r=",r, " ",round(((n+1+p)*r)/(n*p)*100,1),"%"),cex.main=0.9)
}

for (r in c(10, 20 ,50, 100)){
  print((r*5)+5+(5*r))
}



# AFC vs ACP

fichier <- "https://husson.github.io/MOOC_AnaDo/AnaDo_JeuDonnees_Nobel_avecMaths.csv"
Nobel <- read.table(fichier, header=TRUE, sep=";", row.names=1, check.names=FALSE)
Nobel <- Nobel[1:8,]

CA(Nobel)
PCA(Nobel)

#ACM vs AFC

library(FactoMineR)
data(tea)
names(tea)
don <- tea[, c(14,18)] ; MCA(don)
res <- MCA(tea,quanti.sup = 19,quali.sup = c(1:18,20:24))
plot(res,invisible=c("ind","quali.sup")) #Cache les supplementary
plot(res,invisible=c("ind","var")) #Cache les variables et laisse les supplementary
#Séparation des gens qui boivent le thé de façon différente
#Ensuite, est-ce que ça coincide avec qui ils sont ? (Homme / Femme par ex, thé différents)

TabCont <- table(don) ; CA(TabCont)
