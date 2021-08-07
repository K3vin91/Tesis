rm(list = ls())
library(pacman)
p_load(tidyverse, sf, raster, rgdal, ggpubr, cowplot, extrafont, ggspatial)

cnc <- st_read('cuencasEcu.shp')
stc <- read_csv('estaciones.csv')
stc <- dplyr::filter(stc, stc$COD_CUE == 130)
#coordinates(stc) <- c("x","y")
coordinates(stc) <- ~ x + y
crs(stc) <- crs(cnc)
stc <- st_as_sf(stc)
#reproyectar las capas
stc <- st_transform(stc, crs = 4326)
cnc <- st_transform(cnc, crs = 4326)
cnc <- dplyr::filter(cnc, cnc$COD_CUE == 130)
# lnd <- st_read('landCover.shp')
# lnd <- st_intersection(lnd, cnc)#demora aproximadamente 4 minutos
# disolver shp y recodificar nombres
# lndis <- lnd %>% dplyr::group_by(nivel1) %>% summarise()# 17
# lndis$nivel1 <- dplyr::recode(lndis$nivel1, "BOSQUE" = "Bosque",
#                              "CUERPO DE AGUA" = "Cuerpo de agua",
#                              "OTRAS TIERRAS" = "Otras tierras",
#                              "TIERRA AGROPECUARIA" = "Tierra agropecuaria",
#                              "VEGETACION ARBUSTIVA Y HERBACEA" = "Arbustiva y herbácea",
#                              "ZONA ANTROPICA" = "Zona antrópica")
#st_write(lndis,'landCoverDis.shp')
#cargar capa disuelta y cambiado los campos
lndis <- st_read('landCoverDis.shp')
#rdb <- dplyr::filter(lndis, lndis$nivel1 == "Cuerpo de agua")
rio <- st_read('riosEc.shp')
rio <- st_intersection(rio, cnc)
emb <- st_read('CuerpoDeAgua.shp')
emb <- st_transform(emb, crs = 4326)
emb <- st_intersection(emb,cnc)
dem <- raster('dem90m.tif')
cne <- st_read('CuencasVertientes.shp')
ecu <- dplyr::group_by(cne)%>%summarise()
dem <- raster::crop(dem,cne) %>% raster::mask(cne)
mde <- rasterToPoints(dem) %>% as_tibble() %>% setNames(c('x','y','z'))
bcn <- st_bbox(cnc) %>% st_as_sfc()
map1 <- ggplot()+
  geom_raster(data = mde, mapping = aes(x,y, fill = z), show.legend = FALSE)+
  scale_fill_gradientn(colours = terrain.colors(6, alpha = 0.8))+
  geom_sf(data = cne, color = "white", fill = NA, show.legend = FALSE)+
  geom_sf(data = ecu, color = "gray50", fill = NA, show.legend = FALSE)+
  geom_sf(data = bcn, fill = NA, color = "red", linetype = 5, size = 0.4)+
  geom_sf(data = cnc, fill = NA)+
  theme(panel.background = element_rect(fill = "white", color = "gray50"),
        plot.background = element_rect(fill = NA),
        panel.grid = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank())

cue <- st_read('cuencasEcu.shp')
cue <- st_transform(cue, crs = 4326)
loadfonts(device = "win")
rango_x <- seq(-81, -78.5, 0.5)
rango_y <- seq(-2.5, 0, 0.5)
map2 <- ggplot()+
  geom_sf(data=lndis, mapping = aes(color = nivel1, fill = nivel1))+
  scale_fill_manual(values = c("yellow2","chartreuse4","dodgerblue3","orange",
                               "yellowgreen","gray50"))+
  scale_color_manual(values = c("yellow2","chartreuse4","dodgerblue3","orange",
                                "yellowgreen","gray50"))+
  scale_x_continuous(limits = c(-81,-78.5), breaks = rango_x, 
                     labels = sprintf("%.2f", rango_x))+
  scale_y_continuous(limits = c(-2.5,0), breaks =rango_y, 
                     labels = sprintf("%.2f", rango_y))+
  geom_sf(data = cue, color = "gray70", fill= NA)+
  geom_sf(data = cnc, color = "gray50", fill = NA)+
  geom_sf(data = emb, color = "dodgerblue3", fill = "dodgerblue3")+
  geom_sf(data = rio, color = "dodgerblue3", lwd = 0.3)+
  geom_sf(data = stc, mapping = aes(size = FACTOR), shape = 21, color = "gray40",
          fill = "gray90")+
  coord_sf(xlim = c(-81, -78.5), ylim = c(-2.5, 0))+
  ggtitle("Mapa de Ubicación",
          subtitle = "Realizado por: Angel Luna-Romero")+
  labs(x="", y="", fill = "Uso de suelo", size = "Datos (%)\n1981-2014",
       caption ="Fuente: Ministerio de Agricultura")+
  theme(panel.background = element_rect(color = "gray50", fill = "white"),
        panel.grid = element_line(color = "gray90", linetype = 3),
        axis.title = element_blank(),
        text = element_text(family = "Arial"),
        axis.text.y = element_text(vjust = 0.5),
        legend.title = element_text(hjust = 0, size = 11),
        legend.key = element_rect(fill = "white", color = "white"),
        legend.text = element_text(size = 9),
        plot.title = element_text(hjust = 0.5, size = 12, family = "Arial"))+
  guides(color = "none")+
  annotation_scale()+
  annotation_north_arrow(location = "tr", which_north = "true",
                         style = north_arrow_nautical, 
                         height = unit(2.5, "cm"),
                         width = unit(2.5, "cm"))+
  guides(shape = guide_legend(override.aes = list(size = 10)))

map3 <- ggdraw() +
  draw_plot(map2) +
  draw_plot(map1, x = 0.13, y = 0.55, width = 0.37, height = 0.37)+#con 
  #titulo y subtitulo y caption
  draw_plot_label(c("A","B"), c(0.22, 0.73), c(0.9, 0.12),size = 14,
                  family = "Arial")
  #draw_plot(map1, x = 0.11, y = 0.6, width = 0.37, height = 0.37)

ggsave(plot = map3, 'MapUbicacion7.png', units = "cm", width = 23, 
       height = 15, dpi = 300) 

  
  
