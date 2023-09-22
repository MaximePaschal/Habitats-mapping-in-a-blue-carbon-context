## Réalisé par Maxime Paschal, maxime.paschal@gmail.com##

#vider l'espace de travail
rm(list=ls())

#packages et librairies

#install.packages("terra")
#install.packages("sf")
#install.packages("FactoMineR")
#install.packages("randomForest")
#install.packages("caret")
# install.packages("landscapemetrics")

# install.packages("ggplot2")
# install.packages("sp")
# install.packages("dtplyr")
# install.packages("raster")
# install.packages("lattice")
# install.packages("rasterVis")
# install.packages("RColorBrewer")
# install.packages("paletteer")
# install.packages('ggthemes')


library(ggplot2)
library(sp)
library(dtplyr)
library(raster)
library(lattice)
library(rasterVis)
library(RColorBrewer)
library(paletteer)
library(ggthemes)





library(terra)
library(sf)
library(randomForest)
library(caret)
library(FactoMineR)
library(landscapemetrics)

#répertoire  de travail

WD="D:/sentinel_serie_temporelle_ndvi_esnandes/S2_esnandes_ndvi_moyen_mensuel"
setwd(WD)
getwd()


#importer les données 

champs=st_read("D:/station_esnandes/mask_esnandes_vasiere_pre_sale_980m.shp")

ndvi_fevrier=rast("esnandes_ndvi_moyen_mensuel_fevrier.tif")
ndvi_mars=rast("esnandes_ndvi_03_04.tif")
ndvi_avril=rast("esnandes_ndvi_moyen_mensuel_avril.tif")
ndvi_mai=rast("esnandes_ndvi_05_03.tif")
ndvi_juin=rast("esnandes_ndvi_moyen_mensuel_juin.tif")

fevrier=st_read("D:/Footprints_mensuels/Esnandes/footprint_esnandes_fevrier/esnandes_fevrier_secteurs_vent.shp")
mars=st_read("D:/Footprints_mensuels/Esnandes/footprint_esnandes_mars/esnandes_mars_secteurs_vent.shp")
avril=st_read("D:/Footprints_mensuels/Esnandes/footprint_esnandes_avril/esnandes_avril_secteurs_vent.shp")
mai=st_read("D:/Footprints_mensuels/Esnandes/footprint_esnandes_mai/esnandes_mai_secteurs_vent.shp")
juin=st_read("D:/Footprints_mensuels/Esnandes/footprint_esnandes_juin/esnandes_juin_secteurs_vent.shp")

#changer les sustÃ¨mes de projection
champs=st_transform(champs, 32630)
fevrier=st_transform(fevrier, 32630)
mars=st_transform(mars, 32630)
avril=st_transform(avril, 32630)
mai=st_transform(mai, 32630)
juin=st_transform(juin, 32630)

#masquer les emprises des ndvi
fevrier_c=crop(ndvi_fevrier, champs)
ndvi_fevrier=mask(fevrier_c, champs)

mars_c=crop(ndvi_mars, champs)
ndvi_mars=mask(mars_c, champs)

avril_c=crop(ndvi_avril, champs)
ndvi_avril=mask(avril_c, champs)

mai_c=crop(ndvi_mai, champs)
ndvi_mai=mask(mai_c, champs)


# mai_av_c=crop(ndvi_mai_av, champs)
# ndvi_mai_av=mask(mai_av_c, champs)
# 
# mai_ap_c=crop(ndvi_mai_ap, champs)
# ndvi_mai_ap=mask(mai_ap_c, champs)

juin_c=crop(ndvi_juin, champs)
ndvi_juin=mask(juin_c, champs)


plot(ndvi_fevrier)
plot(fevrier$geometry, add=T)

#plot ndvi time series



par(mfrow= c(3,2))
plot(ndvi_fevrier, main="ndvi fevrier", axes=F, range=c(0,1), col=paletteer_c("grDevices::Greens 3", 30, direction=-1))
plot(fevrier$geometry, add=T)
plot(ndvi_mars, main="ndvi mars", axes=F, range=c(0,1), col=paletteer_c("grDevices::Greens 3", 30, direction=-1))
plot(mars$geometry, add=T)
plot(ndvi_avril, main="ndvi avril", axes=F, range=c(0,1), col=paletteer_c("grDevices::Greens 3", 30, direction=-1))
plot(avril$geometry, add=T)
plot(ndvi_mai, main="ndvi mai", axes=F, range=c(0,1), col=paletteer_c("grDevices::Greens 3", 30, direction=-1))
plot(mai$geometry, add=T)
plot(ndvi_juin, main="ndvi juin", axes=F, range=c(0,1), col=paletteer_c("grDevices::Greens 3", 30, direction=-1))
plot(juin$geometry, add=T)



