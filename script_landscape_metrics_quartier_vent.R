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

library(terra)
library(sf)
library(randomForest)
library(caret)
library(FactoMineR)
library(landscapemetrics)

#rÃ©pertorie de travail

WD="D:/Marans_S2_multidates/secteurs_vent_marans/juin_secteurs_vent_marans"
setwd(WD)
getwd()


carte=rast("D:/marans_S2_multidates/classifs_SRFmd/Classif_SRFmd7_marans_0_9773.tif")
plot(carte)


####################DÃ©couper les cartes par rapport aux quartiers de vent###############################

nne=st_read("D:/Footprints_mensuels/Marans/footprint_marans_juin/NNE_marans_juin.shp")
ene=st_read("D:/Footprints_mensuels/Marans/footprint_marans_juin/ENE_marans_juin.shp")
ese=st_read("D:/Footprints_mensuels/Marans/footprint_marans_juin/ESE_marans_juin.shp")
sse=st_read("D:/Footprints_mensuels/Marans/footprint_marans_juin/SSE_marans_juin.shp")
sso=st_read("D:/Footprints_mensuels/Marans/footprint_marans_juin/SSO_marans_juin.shp")
oso=st_read("D:/Footprints_mensuels/Marans/footprint_marans_juin/OSO_marans_juin.shp")
ono=st_read("D:/Footprints_mensuels/Marans/footprint_marans_juin/ONO_marans_juin.shp")
nno=st_read("D:/Footprints_mensuels/Marans/footprint_marans_juin/NNO_marans_juin.shp")


crs(carte)
crs(nne)

#changer les projections
nne=st_transform(nne,32630)
ene=st_transform(ene,32630)
ese=st_transform(ese,32630)
sse=st_transform(sse,32630)
sso=st_transform(sso,32630)
oso=st_transform(oso,32630)
ono=st_transform(ono,32630)
nno=st_transform(nno,32630)

#Isoler les secteurs de vent

carte_nne_crop=crop(carte, nne)
carte_nne=mask(carte_nne_crop, nne)

carte_ene_crop=crop(carte, ene)
carte_ene=mask(carte_ene_crop, ene)

carte_ese_crop=crop(carte, ese)
carte_ese=mask(carte_ese_crop, ese)

carte_sse_crop=crop(carte, sse)
carte_sse=mask(carte_sse_crop, sse)

carte_sso_crop=crop(carte, sso)
carte_sso=mask(carte_sso_crop, sso)

carte_oso_crop=crop(carte, oso)
carte_oso=mask(carte_oso_crop, oso)

carte_ono_crop=crop(carte, ono)
carte_ono=mask(carte_ono_crop, ono)

carte_nno_crop=crop(carte, nno)
carte_nno=mask(carte_nno_crop, nno)





plot(carte)

plot(sse$geometry,add=T)

plot(carte_sse)

#enregistrer les rasters de secteurs de vent

writeRaster(carte_nne,"carte_nne_marans_juin.tif",datatype="FLT8S", overwrite=T)
writeRaster(carte_ene,"carte_ene_marans_juin.tif",datatype="FLT8S", overwrite=T)
writeRaster(carte_ese,"carte_ese_marans_juin.tif",datatype="FLT8S", overwrite=T)
writeRaster(carte_sse,"carte_sse_marans_juin.tif",datatype="FLT8S", overwrite=T)
writeRaster(carte_sso,"carte_sso_marans_juin.tif",datatype="FLT8S", overwrite=T)
writeRaster(carte_oso,"carte_oso_marans_juin.tif",datatype="FLT8S", overwrite=T)
writeRaster(carte_ono,"carte_ono_marans_juin.tif",datatype="FLT8S", overwrite=T)
writeRaster(carte_nno,"carte_nno_marans_juin.tif",datatype="FLT8S", overwrite=T)


#####################Calculer les pourcentages d'OS par quartier de vent################################

liste_metrics=list_lsm() # listes des mÃ©triques qui existent
as.data.frame(liste_metrics)
liste_metrics #afficher les mÃ©triques disponibles 





im1=rast("carte_nne_marans_juin.tif")
im2=rast("carte_ene_marans_juin.tif")
im3=rast("carte_ese_marans_juin.tif")
im4=rast("carte_sse_marans_juin.tif")
im5=rast("carte_sso_marans_juin.tif")
im6=rast("carte_oso_marans_juin.tif")
im7=rast("carte_ono_marans_juin.tif")
im8=rast("carte_nno_marans_juin.tif")

liste_im=list(im1,im2,im3,im4,im5,im6,im7,im8)
plot(im3)


Num_image=8#nbr image que tu as 

vent=c("NNE", "ENE", "ESE", "SSE", "SSO", "OSO", "ONO", "NNO")


area_metrics_all_df=NULL # va recevoir toutes les metrics 
#attention entre plusieurs utilisations il faut remettre à 0 le résultats sinon ça s'accumule

for(i in 1:Num_image){
  
  
  #compute classe metrics
  area_metrics_tmp_i=lsm_c_ca (liste_im[i], directions=8)
  as.data.frame((area_metrics_tmp_i))
  
  #ajouter le vent
  area_metrics_tmp_i$vent=vent[i]
  
  #sauvegarder les metrics dans un data frame rÃ©sultats
  area_metrics_all_df=rbind(area_metrics_all_df,area_metrics_tmp_i)
  
}

as.data.frame(area_metrics_all_df[1:Num_image,])

area_metrics_all_df # afficher le rÃ©sultats 

OS_vent=area_metrics_all_df[,c(3,6,7)] #on garde les colonnes class, value et vent
OS_vent

write.csv(OS_vent, "OS_vent_SRFmd7_marans_juin.csv", row.names=FALSE)









