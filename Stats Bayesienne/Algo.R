n <- 100
mu <- 10
var <- 4

data <- rnorm(n, mean = mu, sd = sqrt(var))

mean(data) #Moyenne
mean(data^2)- mean(data)^2 #Variance

library(ggplot2)
library(tidyverse)

data %>% as_tibble() %>% ggplot() + geom_histogram(aes(x=value, y = ..density..))

sort(data)[c(3,97)]

#############

#Loi de proposition

Zi <- rnorm(n)

#calcule des poids non normalisés
#le poids pour la réalisation i
#si zi < 0 ou zi > 1 poids c'est 0
#sinon numérateur i = zi^4 (1-zi)^6

poids_numerateur <- Zi^4 * (1-Zi)^6 * ifelse(Zi<0 | Zi > 1, 0, 1)

poids_denom <- dnorm(Zi, mean=0,sd=1)

poids_non_norm <- poids_numerateur/poids_denom

sum(poids_non_norm) #Pas égal à 1 puisque non normalisés

poids_norm <- poids_non_norm/sum(poids_non_norm)
sum(poids_norm) #égal à 1 car normalisés

hist(poids_norm)

sum(poids_norm==0) #Pas un échantillon de taille 100 puisque beaucoup de valeurs ont un poids de 0

weighted.mean(Zi,w=poids_norm)

#On réechantillonne avec les nouvelles probas (poids) pour surreprésenter nos valeurs comprises dans la loi Beta

Xi <- sample(Zi, size= length(Zi), replace = TRUE, prob = poids_norm)

hist(Xi)
