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


x <- st_point(c(1,2))
x
class(x)

x %>% ggplot() + geom_sf()

p <- rbind( c(3.2,4), c(3,4.6), c(3.8,4.4), c(3.5,3.8), c(3.4,3.6), c(3.9,4.5))
(mp <- st_multipoint(p))
mp
class(mp)
mp %>% ggplot() + geom_sf()

line_sfg <- st_linestring(p)
line_sfg
class(line_sfg)
line_sfg%>% ggplot() + geom_sf()

p1 <- rbind(c(0,0), c(1,0), c(3,2), c(2,4), c(1,4), c(0,0))
p2 <- rbind(c(1,1), c(1,2), c(2,2), c(1,1))
poly_sfg <- st_polygon(list(p1, p2))
poly_sfg 
class(poly_sfg )
poly_sfg %>% ggplot() + geom_sf()


idf_shape <- st_read(dsn = 'ile_de_france_shape')
idf_shape %>% print(n=10)

dpt_shape <-  st_read(dsn = 'depts')
dpt_shape %>% print(n=10)

dpt_shape %>% 
  ggplot() + geom_sf()

population <- read_delim("population.csv", 
                         ";", escape_double = FALSE,  col_types = cols(`1999` = col_double(), 
                                                                       `2007` = col_double(), `2012` = col_double(), 
                                                                       `2017` = col_double()), trim_ws = TRUE, 
                         skip = 3) %>% 
  rename(CODE_DEPT = ...1, DEPT_NAME=...2, p2020 = '2020 (p)', p2017 = '2017', p2012 = '2012',
         p2007 = '2007', p1999 = '1999')

dpt_complete <- dpt_shape %>% inner_join( y = population, by = "CODE_DEPT")

dpt_complete %>%  ggplot() + geom_sf(aes(fill = `Part dans la France (p) (en %)`))
  
