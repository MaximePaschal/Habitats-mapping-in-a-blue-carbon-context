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


carte=rast("D:/sentinel_serie_temporelle_ndvi_Marans/S2_marans_ndvi_moyen_mensuel/marans_ndvi_moyen_mensuel_juin.tif")
plot(carte)


####################DÃ©couper les cartes par rapport aux quartiers de vent###############################

mask=st_read("D:/station_marans/mask_marans_champs.shp")
mask=st_transform(mask,32630)
carte_c=crop(carte, mask)
carte=mask(carte_c, mask)
plot(carte)


nne=st_read("D:/Footprints_mensuels/marans/footprint_marans_juin/NNE_marans_juin.shp")
ene=st_read("D:/Footprints_mensuels/marans/footprint_marans_juin/ENE_marans_juin.shp")
ese=st_read("D:/Footprints_mensuels/marans/footprint_marans_juin/ESE_marans_juin.shp")
sse=st_read("D:/Footprints_mensuels/marans/footprint_marans_juin/SSE_marans_juin.shp")
sso=st_read("D:/Footprints_mensuels/marans/footprint_marans_juin/SSO_marans_juin.shp")
oso=st_read("D:/Footprints_mensuels/marans/footprint_marans_juin/OSO_marans_juin.shp")
ono=st_read("D:/Footprints_mensuels/marans/footprint_marans_juin/ONO_marans_juin.shp")
nno=st_read("D:/Footprints_mensuels/marans/footprint_marans_juin/NNO_marans_juin.shp")


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


#isoler les secteurs de vent

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

#enregistrer les rasters des secteurs de vent

writeRaster(carte_nne,"S2_ndvi_juin_nne_marans.tif",datatype="FLT8S", overwrite=T)
writeRaster(carte_ene,"S2_ndvi_juin_ene_marans.tif",datatype="FLT8S", overwrite=T)
writeRaster(carte_ese,"S2_ndvi_juin_ese_marans.tif",datatype="FLT8S", overwrite=T)
writeRaster(carte_sse,"S2_ndvi_juin_sse_marans.tif",datatype="FLT8S", overwrite=T)
writeRaster(carte_sso,"S2_ndvi_juin_sso_marans.tif",datatype="FLT8S", overwrite=T)
writeRaster(carte_oso,"S2_ndvi_juin_oso_marans.tif",datatype="FLT8S", overwrite=T)
writeRaster(carte_ono,"S2_ndvi_juin_ono_marans.tif",datatype="FLT8S", overwrite=T)
writeRaster(carte_nno,"S2_ndvi_juin_nno_marans.tif",datatype="FLT8S", overwrite=T)


#####################Calculer les pourcentages d'OS par quartier de vent################################

liste_metrics=list_lsm() # listes des mÃ©triques qui existent
as.data.frame(liste_metrics)
liste_metrics #afficher les mÃ©triques disponibles 





im1=rast("S2_ndvi_juin_nne_marans.tif")
im2=rast("S2_ndvi_juin_ene_marans.tif")
im3=rast("S2_ndvi_juin_ese_marans.tif")
im4=rast("S2_ndvi_juin_sse_marans.tif")
im5=rast("S2_ndvi_juin_sso_marans.tif")
im6=rast("S2_ndvi_juin_oso_marans.tif")
im7=rast("S2_ndvi_juin_ono_marans.tif")
im8=rast("S2_ndvi_juin_nno_marans.tif")




liste_im=list(im1,im2,im3,im4,im5,im6,im7,im8)



Num_image=8#nbr image que tu as (secteurs de vent)


vent=c("NNE", "ENE", "ESE", "SSE", "SSO", "OSO", "ONO", "NNO")

mean_ndvi_df=NULL #attention entre plusieurs utilisations il faut remettre à 0 le résultats sinon ça s'accumule

for(i in 1:Num_image){
  im=liste_im[[i]]
  val=values(im, na.rm=TRUE)
  ndvi_m=mean(val)
  as.data.frame((ndvi_m))
  # ndvi_m_hiver$vent=vent[i]
  
  mean_ndvi_df=rbind(mean_ndvi_df,ndvi_m)
}

as.data.frame(mean_ndvi_df[1:Num_image,])

mean_ndvi_df # afficher le rÃ©sultats 
row.names(mean_ndvi_df)=vent
colnames(mean_ndvi_df)="ndvi_moyen"
mean_ndvi_df



write.csv(mean_ndvi_df, "ndvi_vent_S2_marans_juin.csv", row.names=TRUE)









