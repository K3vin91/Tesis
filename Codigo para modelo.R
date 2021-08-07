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

mod09a1 <- MODIStsp()
rm(list = ls())
######################## Carpeta de trabajo ################################
setwd('C:/Tesis/capas_procesadas')

x <- gorg[,1]
y <- gorg[,2]
xmin=min(x)-5
xmax=max(x)+5
ymin=min(y)-5
ymax=max(y)+5

maps::map(database = "world", xlim = c(xmin,xmax), ylim = c(ymin,ymax),
          col = "red", fill = TRUE, resolution = 0)


