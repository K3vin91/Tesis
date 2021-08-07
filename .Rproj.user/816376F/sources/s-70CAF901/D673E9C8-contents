library(raster)
r <- raster("C:/Users/KEVIN/Documents/LCTIG/Informacion Geografica/sombreado/sombreado_cuencas.tif")
plot(r)
###########################################################################################################
library(sf)

sh <- st_read("C:/Users/KEVIN/Documents/LCTIG/2019 PAC II/Analisis y Modelamiento Espacial/Unidad III/Practica IV (2)/Visibilidad_IV/Fmorazan.shp") 
plot(sh)

fm_cor <- crop(r,sh)
rmask <- mask(fm_cor,sh) 
plot(rmask)

hn <- st_read("C:/Users/KEVIN/Documents/LCTIG/R/R como GIS/Dep_hn.shp")
summary(hn)
plot(st_geometry(hn))
hn$NOMBRE

#################################################################################################3
library(maps)
r[r<0]<-0

col.pal<-colorRampPalette(c(rgb(255,255,255,m=255),rgb(32,126,245,m=255),rgb(24,106,94,m=255),rgb(145,196,133,m=255),rgb(241,186,111,m=255),rgb(201,88,74,m=255)))(255)

plot(r,col=col.pal,interpolate=T)
map("world",add=TRUE)

########################### primer intento real de crear un mapa
########################### es necesario importar todas las librerias anteriores menos maps 
##############
library(tidyverse)
library(ggplot2)
library(ggspatial)
library(cowplot)
prep_fm <- raster("C:/Users/KEVIN/Documents/LCTIG/R/R como GIS/fm_pre1.tif") 

dep_hn <- st_read("C:/Users/KEVIN/Documents/LCTIG/R/R como GIS/Dep_hn.shp") 
fm_prep <- rasterToPoints(prep_fm) %>% as_tibble() %>% setNames(c("x","y","z")) #para graficar un raster es necesario este paso (convertirlo en tabla) toma mucho tiempo y no funciona si el archivo es grande
bfm <- st_bbox(sh) %>% st_as_sfc()            #este es para crear una caja entorno al shape                             #


map_1 <- ggplot() +                                   #mapa de ubicacion, nada mas
  geom_sf(data = dep_hn, fill= NA, col= "green") +
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank())


map_2 <- ggplot() +
  #annotation_map_tile(zoom = 7, cachedir = NULL) +
  geom_sf(data = sh, col = "red") +
  geom_raster(data = fm_prep, mapping = aes(x, y, fill = z)) +
  scale_fill_gradientn(colours = terrain.colors(9, alpha = 0.9)) +          #alpha es transparencia
  #geom_sf(data = bfm, color = "blue", fill = NA, linetype ="longdash", size = 0.5)
  ggtitle("Mapa de precipitacion de Francisco Morazan", 
          subtitle = "Por Kevin Irias en R") +
  labs(x="",y="", fill= "milimetros cubicos", caption = "Fuente: WorldClim") +
  theme(axis.title = element_blank(),
        axis.title.y = element_text(hjust = 0.5),
        legend.title = element_text(hjust = 0.5,size = 11),
        legend.key = element_rect(fill = "gray", color = "blue"),
        legend.text = element_text(size = 9),
        plot.title = element_text(hjust = 0.5, size = 12)) +
  annotation_scale()+
  annotation_north_arrow(location= "tr", which_north= "true",
                         style = north_arrow_nautical,
                         height = unit(1.5, "cm"),
                         with = unit(2, "cm")) 
#picture in picture
map_3 <- ggdraw() +
  draw_plot(map_2) +
  draw_plot(map_1, hjust = -0.36, vjust= 0.3, scale = 0.28)
ggsave(plot = map_3, "mapa de pruebas.png", units = "cm", dpi= 300)


