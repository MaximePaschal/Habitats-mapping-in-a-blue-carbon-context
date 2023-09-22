## Réalisé par Maxime Paschal, maxime.paschal@gmail.com##

#vider l'espace de travail
rm(list=ls())

#packages et librairies

#install.packages("terra")
#install.packages("sf")
#install.packages("FactoMineR")
#install.packages("randomForest")
#install.packages("caret")

library(terra)
library(sf)
library(randomForest)
library(caret)
library(FactoMineR)

#rÃ©pertorie de travail

WD="D:/img_sentinel_juillet/S2_20230705"
setwd(WD)
getwd()


#######################################################   PRE-TRAITE SENTINEL2    #################################################

#ouvrir les bandes 


B2=rast("SENTINEL2B_20230705-111820-237_L2A_T30TXS_C_V1-0/SENTINEL2B_20230705-111820-237_L2A_T30TXS_C_V1-0_FRE_B2.tif")
B3=rast("SENTINEL2B_20230705-111820-237_L2A_T30TXS_C_V1-0/SENTINEL2B_20230705-111820-237_L2A_T30TXS_C_V1-0_FRE_B3.tif")
B4=rast("SENTINEL2B_20230705-111820-237_L2A_T30TXS_C_V1-0/SENTINEL2B_20230705-111820-237_L2A_T30TXS_C_V1-0_FRE_B4.tif")
B5=rast("SENTINEL2B_20230705-111820-237_L2A_T30TXS_C_V1-0/SENTINEL2B_20230705-111820-237_L2A_T30TXS_C_V1-0_FRE_B5.tif")
B6=rast("SENTINEL2B_20230705-111820-237_L2A_T30TXS_C_V1-0/SENTINEL2B_20230705-111820-237_L2A_T30TXS_C_V1-0_FRE_B6.tif")
B7=rast("SENTINEL2B_20230705-111820-237_L2A_T30TXS_C_V1-0/SENTINEL2B_20230705-111820-237_L2A_T30TXS_C_V1-0_FRE_B7.tif")
B8=rast("SENTINEL2B_20230705-111820-237_L2A_T30TXS_C_V1-0/SENTINEL2B_20230705-111820-237_L2A_T30TXS_C_V1-0_FRE_B8.tif")
B8A=rast("SENTINEL2B_20230705-111820-237_L2A_T30TXS_C_V1-0/SENTINEL2B_20230705-111820-237_L2A_T30TXS_C_V1-0_FRE_B8A.tif")
B11=rast("SENTINEL2B_20230705-111820-237_L2A_T30TXS_C_V1-0/SENTINEL2B_20230705-111820-237_L2A_T30TXS_C_V1-0_FRE_B11.tif")
B12=rast("SENTINEL2B_20230705-111820-237_L2A_T30TXS_C_V1-0/SENTINEL2B_20230705-111820-237_L2A_T30TXS_C_V1-0_FRE_B12.tif")


vect_sf=st_read("D:/station_esnandes/mask_esnandes_vasiere_pre_sale_980m.shp")
crs(vect_sf)
crs(B2)
vect_sf=st_transform(vect_sf,32630)



#resample des bandes à 10 m  puis crop et mask,5;6;7;8A;11;12

bande_a_resample=B5
bande_pix = bande_a_resample
res(bande_pix) = 10
bande_resample_pix= resample(bande_a_resample, bande_pix ,method = "near") 
bande_crop = crop(bande_resample_pix, vect_sf)
bande_mask = mask(bande_crop,vect_sf)
writeRaster(bande_mask, "B5.tif",datatype = "FLT8S", overwrite=T)

bande_a_resample=B6
bande_pix = bande_a_resample
res(bande_pix) = 10
bande_resample_pix= resample(bande_a_resample, bande_pix ,method = "near") 
bande_crop = crop(bande_resample_pix, vect_sf)
bande_mask = mask(bande_crop,vect_sf)
writeRaster(bande_mask, "B6.tif",datatype = "FLT8S", overwrite=T)

bande_a_resample=B7
bande_pix = bande_a_resample
res(bande_pix) = 10
bande_resample_pix= resample(bande_a_resample, bande_pix ,method = "near") 
bande_crop = crop(bande_resample_pix, vect_sf)
bande_mask = mask(bande_crop,vect_sf)
writeRaster(bande_mask, "B7.tif",datatype = "FLT8S", overwrite=T)

bande_a_resample=B8A
bande_pix = bande_a_resample
res(bande_pix) = 10
bande_resample_pix= resample(bande_a_resample, bande_pix ,method = "near") 
bande_crop = crop(bande_resample_pix, vect_sf)
bande_mask = mask(bande_crop,vect_sf)
writeRaster(bande_mask, "B8A.tif",datatype = "FLT8S", overwrite=T)

bande_a_resample=B11
bande_pix = bande_a_resample
res(bande_pix) = 10
bande_resample_pix= resample(bande_a_resample, bande_pix ,method = "near") 
bande_crop = crop(bande_resample_pix, vect_sf)
bande_mask = mask(bande_crop,vect_sf)
writeRaster(bande_mask, "B11.tif",datatype = "FLT8S", overwrite=T)


bande_a_resample=B12
bande_pix = bande_a_resample
res(bande_pix) = 10
bande_resample_pix= resample(bande_a_resample, bande_pix ,method = "near") 
bande_crop = crop(bande_resample_pix, vect_sf)
bande_mask = mask(bande_crop,vect_sf)
writeRaster(bande_mask, "B12.tif",datatype = "FLT8S", overwrite=T)



#crop et mask des auters bandes2,3,4,8
bande=B2
bande_crop = crop(bande, vect_sf)
bande_mask = mask(bande_crop,vect_sf)
writeRaster(bande_mask, "B2.tif",datatype = "FLT8S", overwrite=T)

bande=B3
bande_crop = crop(bande, vect_sf)
bande_mask = mask(bande_crop,vect_sf)
writeRaster(bande_mask, "B3.tif",datatype = "FLT8S", overwrite=T)

bande=B4
bande_crop = crop(bande, vect_sf)
bande_mask = mask(bande_crop,vect_sf)
writeRaster(bande_mask, "B4.tif",datatype = "FLT8S", overwrite=T)

bande=B8
bande_crop = crop(bande, vect_sf)
bande_mask = mask(bande_crop,vect_sf)
writeRaster(bande_mask, "B8.tif",datatype = "FLT8S", overwrite=T)




#############################################################################################################################


B2=rast("B2.tif")
B3=rast("B3.tif")
B4=rast("B4.tif")
B5=rast("B5.tif")
B6=rast("B6.tif")
B7=rast("B7.tif")
B8=rast("B8.tif")
B8A=rast("B8A.tif")
B11=rast("B11.tif")
B12=rast("B12.tif")



many_rst=list(B2, B3, B4, B5, B6, B7, B8, B8A, B11, B12)

Stack_im=rast(many_rst)

names(Stack_im)=c("B2", "B3", "B4", "B5", "B6", "B7", "B8", "B8A", "B11", "B12")


writeRaster(Stack_im, "esnandes_S2_20230705_stack_980.tif", datatype="FLT8S", overwrite=T)

plotRGB(Stack_im, r=3, g=2, b=1, stretch='lin')








