############# Modelo de distribucion del gorgojo descortezador de pino utilizando MAXENT ###############
############## Kevin Gerardo Irias Padilla ################

options(scipen = 0000)
######################### Librerias ##########################
library(raster)
library(XML)
library(dismo)
library(jsonlite)
library(graphics)
library(maps)
library(maptools)
library(sf)
library(rgeos)
library(rgdal)
library(virtualspecies)
library(corrplot)
library(usdm)
library(gtools)
library(tidyverse)
library(Hmisc)
library(rJava)
library(ENMeval)
library(MODIStsp)
library(readxl)

mod09a1 <- MODIStsp()
rm(list = ls())
######################## Carpeta de trabajo ################################
setwd('C:/Tesis/capas_procesadas')
gorg_utm <- read_excel("gorg.xls")
####################### plot de area de estudio ############################
x <- gorg_geo[,1]
y <- gorg_geo[,2]
xmin=min(x)-3
xmax=max(x)+3
ymin=min(y)-1
ymax=max(y)+2

maps::map(database = "world", xlim=c(xmin,xmax), ylim=c(ymin,ymax), col = "blue", fill = TRUE)
box(which = "plot", lty = "solid", lxd=0.25)
axis(side = 1,cex.axis=0.7,lwd = 0.25)
axis(side=2,cex.axis=0.7, lwd = 0.25)

coord_gorg <- SpatialPoints(gorg_utm)

###################### listar raster #######################################
list.rasters <- (list.files('C:/Tesis/Nueva carpeta', full.names = TRUE,
                            pattern = ".tif"))
list.rasters

rasters <- stack(list.rasters)
rasters

projection(rasters) <-CRS("+proj=longlat +datum=WGS84")   ####

##################### eliminar colineridad #################################
rasters.crop <- rasters
rasters.crop.reduced <-removeCollinearity(rasters.crop, multicollinearity.cutoff = 0.85,
                                          select.variables = TRUE, sample.points = FALSE, plot = TRUE)
rasters.crop.reduced

rasters.selected <- subset(rasters.crop, c("DEM_de_Honduras1","incend_20191","LAI_2019","LST1","NMDI1","Precipitacion_anual1","Slope_Honduras","VCF1"))

#################### extraer valores de rasters #############################
presvals <- raster::extract(rasters.selected, coord_gorg)
presvals

#################### Seleccionar semilla ####################################
set.seed(1500)

#################### Background #############################################
background <- randomPoints(rasters.selected, 10000)
absvals <- raster::extract(rasters.selected, background)
pb <- c(rep(1,nrow(presvals)), rep(0, nrow(absvals)))
sdmdata.present <- data.frame(cbind(pb, rbind(presvals, absvals)))

#################### entrenar el modelo #####################################
model.maxent <-maxent(x=rasters.selected, p=coord_gorg, a=background,
                      args=c('randomtestpoints=20','betamultiplier=1',
                             'linear=true','quadratic=true','product=true',
                             'threshold=true','hinge=true','threads=2',
                             'responsecurves=true','jackknife=true',
                             'askoverwrite=true'
                             )
                      )
model.maxent
################### mapa de prediccion ######################################
map.model.maxent <- predict(object = model.maxent, x=rasters.crop, na.rm=TRUE,
                            format='GTiff', filename='C:/Tesis/Resultado',
                            overwrite='TRUE', progress='text')

plot(map.model.maxent, main='modelo')

















