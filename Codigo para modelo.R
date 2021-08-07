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

plot(gorg)


