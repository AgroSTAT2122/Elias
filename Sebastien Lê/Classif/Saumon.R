saumon <- read.table("saumon.txt", sep = "\t", header = TRUE, dec = ",", fill = TRUE)
str(saumon)
X1 <- saumon[1:1063, 3:32]

require(FactoMineR)
res.PCA <- PCA(saumon,
               ncp = 2,
               ind.sup = 1064:1088,
               quali.sup =  c(1:2, 33:100), graph = FALSE)

res.hcpc <- HCPC(res.PCA, graph = FALSE)

require(dplyr)
test <- res.hcpc$data.clust %>% 
  group_by(clust) %>% 
  dplyr::summarize(Mean = mean(res.hcpc ,na.rm=TRUE))
