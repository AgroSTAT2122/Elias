library("sf")
library("rnaturalearth")
library("rnaturalearthdata")
library(tidyverse)

world <- ne_countries(scale = "medium", returnclass = "sf")

world %>% select(admin, income_grp, wikipedia, economy, geometry) %>% print(n=5)

st_crs(world )

world %>%  filter(brk_name == "United States") %>% ggplot() + geom_sf()
world %>%   filter(brk_name == "United States") %>% st_transform(crs = 2163) %>% ggplot() + geom_sf()

#Utiliser epsg.io pour trouver les systeme 

france_dta <-   world  %>% filter(name == 'France')
st_crs(france_dta)
france_dta%>% ggplot() + geom_sf()

france_dta %>% st_crop(xmin = -58, xmax = -45, ymin = 0, ymax= 10) %>% 
  ggplot() + geom_sf() 

france_dta %>%  
  ggplot() + geom_sf() 


metro_dta <- france_dta %>% st_crop( xmin = -5, xmax = 11, ymin= 40.6, ymax = 52) 
metro_dta %>% ggplot() + geom_sf()

#Avec epsg.io regarder EPSG

metro_dta %>% st_transform( crs = 2154 ) %>%
  ggplot() + geom_sf()

metro_dta %>% st_transform( crs = 32631 ) %>%
  ggplot() + geom_sf()
