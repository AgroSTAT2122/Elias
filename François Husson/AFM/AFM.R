comp <- read.table("http://factominer.free.fr/more/gene.csv",sep=";",header=T,row.names=1)


library(Factoshiny)



##### Jeu de données et carte----

paleo <- read.table("https://husson.github.io/img/paleo_climato.csv", header=TRUE,
                    sep=";", row.names=1)
paleo <- cbind.data.frame(paleo,present=as.factor(c(rep("Present",700),rep("Passe",128))))

library(leaflet)
pal <- colorNumeric(palette=c(low="blue",high="red"),domain=paleo[1:700,"tann"])
m <- leaflet() %>% addTiles() %>%
  addCircles(paleo[1:700,"long"],paleo[1:700,"lati"],
             color=pal(paleo[1:700,"tann"]),fillOpacity=1,opacity=1) %>%
  addCircles(8.3147,47.07028,color="black",fillOpacity=1,opacity=1) %>%
  addPopups(8.3147,47.07028,"Lac Rotsee")
m

##### 1 ----

library(FactoMineR)
Factoshiny::Factoshiny(paleo)

ACP <- PCA(paleo[1:700,-c(41,43)])


#Test avec RandomForest ----

install.packages("randomForest")
library(randomForest)


Rf <- randomForest(biome~.,data=Present[,c(1:31,41)], ntree = 200, mtry = 3)

y_pred <- predict(Rf, newdata = Present[,1:31],type ="response")

sum(y_pred==Present$biome)

y_pred <- predict(Rf, newdata = Passe[,1:31],type ="response")


#Essai avec une régression logistique multinomiale ----


library(nnet)

Present <- paleo[1:700,c(1:31,41)]
Passe <- paleo[701:828,c(1:31,41)]

Present$biome <- as.factor(Present$biome)
Passe$biome <- as.factor(Passe$biome)

multi <- multinom(biome~.,data = Present, maxit = 1050)


multi2 <- step(multi)

multi2


class(Present$biome)

n = nrow(Present)                   # Sample size
segments = cvsegments(k=10,N=n) # Defines a list of 10 random segments
segments
cvpredictions = rep(0,n)

for (j in 1:10) {
  train = Present[-segments[[j]],]
  test = Present[segments[[j]],]
  submod = multinom(formula(multi2),data=train,trace=FALSE,maxit=1000) 
  cvpredictions[segments[[j]]] = predict(submod,newdata=test,type="class")
}

table(cvpredictions,Present$biome)

cvpredictions <- as.factor(cvpredictions)
levels(cvpredictions) <- levels(Present$biome)

table(cvpredictions==Present$biome)

586/(586+114) #84% de bonnes prédictions
#Attention pas équilibré donc à prendre avec des pincettes car une modalité ultra majoritaire

table(cvpredictions)
table(Present$biome)

table(cvpredictions,Present$biome)


### Prediction sur le passé avec le modèle multinomial----


Prediction <- as.data.frame(predict(multi2, type = "probs", newdata = Passe))

y_predict <- integer(128)

biome <- colnames(Prediction)

str(Prediction)
for (i in 1:nrow(Prediction)){
  for (j in biome){
    if (Prediction[i,j]== 1){
      y_predict[i]=j
    }
  }
  print(y_predict[i])
}

y_predict <- as.factor(y_predict)
levels(y_predict) <- levels(Present$biome)

Passe$biome <- as.factor(Passe$biome)
levels(Passe$biome) <- levels(Present$biome)
Passe$biome[1] <- y_pred[2]

Value <- nrow(Passe)
i = 1
while (i != Value+1){
  Passe$biome[i] <- y_predict[i]
  message("Progress : ", i, "/",nrow(Passe))
  i=i+1
}
#On a les biomes prédits par le modèle aux différentes périodes


paleo_present <- paleo[1:700,-42:-43]
MFA = MFA(paleo_present, group = c(31,6,3,1), type = c("s","s","s","n"), name.group=c("Pollens","Weather","Localisation","Biome"),num.group.sup=3:4)

res_pres <- MFA(paleo[,1:37], ind.sup = 701:828, group = c(31,6), type = c("s","s"))
