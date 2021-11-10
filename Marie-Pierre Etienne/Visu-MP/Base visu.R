library(tidyverse)

n <- 8

data.frame(x=sample(1:10, size = n, replace =TRUE),
           y= 3*rnorm(n)) %>% #CTL + shift + M
  mutate(xplusy = x + y)  %>% 
  filter(xplusy > 4) %>% 
  filter(x < 2) %>% 
  select(-xplusy)

#Vieille version :
# dta <- data.frame(x=sample(1:10, size = n, replace =TRUE),
#                    y= 3*rnorm(n))
# dta$xplusy <- dta$x + dta$y
# dta[ dta$xplusy >4, c('x','y')]



remotes::install_github('https://github.com/MarieEtienne/coursesdata')
data(bats,package="coursesdata")
bats <- bats %>% as_tibble()


bats %>% 
  mutate(Diet_fact = as.factor(Diet)) %>% 
  group_by(Diet_fact,Clade) %>% 
  summarise(bodyW_diet_clade=mean(BOW), n=n()) %>% 
  ungroup(Clade) %>% 
  mutate(mean_BOW_diet= mean(bodyW_diet_clade))


gg <- bats %>% 
  mutate(Diet_fact = as.factor(Diet)) %>% 
  ggplot() +
  aes( x= BOW, y=BRW) +
  geom_point() + 
  aes(col = Diet_fact)+
  geom_smooth(method="lm")

gg

bats %>%
  mutate(Diet_fact = as.factor(Diet)) %>% 
  group_by(Diet_fact) %>% 
  ggplot() + aes(x = BOW) + geom_histogram(binwidth = 200) +
  labs( x = 'Body Weight in g') +
  aes(fill= Diet_fact) 

bats %>%
  mutate(Diet_fact = as.factor(Diet)) %>%
  ggplot() + aes(x = BOW, fill = Diet_fact) +
  geom_histogram() +
  facet_wrap(~Diet_fact) +
  labs( x = 'Body Weight in g') +
  scale_fill_manual(values = wesanderson::wes_palette('Darjeeling1'))

bats %>%  
  mutate(Diet_fact = as.factor(Diet)) %>%
  ggplot() + aes(x = BOW, y = ..density..) +
  facet_wrap(~Diet_fact) +
  geom_histogram(alpha=0.5, aes( fill = Diet_fact)) +
  geom_density(aes(col = Diet_fact)) +
  labs( x = 'Bill length in mm')

bats %>% 
  mutate(Diet_fact = as.factor(Diet)) %>%
  ggplot() + aes(x = Diet_fact,  y = BOW) +
  geom_boxplot(alpha=0.5, aes( fill = Diet_fact)) + ylim(0,200) +
  geom_jitter(color="black", size=0.2, alpha=0.8) +
  labs( y = 'Bill length in mm') +
  scale_fill_manual(values = wesanderson::wes_palette('Darjeeling1'))




