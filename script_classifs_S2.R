#vider l'espace de travail
rm(list=ls())
.rs.restartR()

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

WD="D:/Esnandes_S2_multidates"
setwd(WD)
getwd()


# ouvrir l'image

Stack_im2=rast("esnandes_S2_20230212_stack_980.tif")

Stack_im4=rast("esnandes_S2_20230408_stack_980.tif")

Stack_im5=rast("esnandes_S2_202305016_stack_980.tif")

Stack_im65=rast("esnandes_S2_20230605_stack_980.tif")

Stack_im67=rast("esnandes_S2_20230607_stack_980.tif")



#les bandes des dates

#si on fait image [1] -> on accède au pixel qui a la position 1 par contre image[[1]] permet d'accéder à la première bande du stack


B2_2=Stack_im2[[1]]
B3_2=Stack_im2[[2]]
B4_2=Stack_im2[[3]]
B5_2=Stack_im2[[4]]
B6_2=Stack_im2[[5]]
B7_2=Stack_im2[[6]]
B8_2=Stack_im2[[7]]
B8A_2=Stack_im2[[8]]
B11_2=Stack_im2[[9]]
B12_2=Stack_im2[[10]]


B2_4=Stack_im4[[1]]
B3_4=Stack_im4[[2]]
B4_4=Stack_im4[[3]]
B5_4=Stack_im4[[4]]
B6_4=Stack_im4[[5]]
B7_4=Stack_im4[[6]]
B8_4=Stack_im4[[7]]
B8A_4=Stack_im4[[8]]
B11_4=Stack_im4[[9]]
B12_4=Stack_im4[[10]]


B2_5=Stack_im5[[1]]
B3_5=Stack_im5[[2]]
B4_5=Stack_im5[[3]]
B5_5=Stack_im5[[4]]
B6_5=Stack_im5[[5]]
B7_5=Stack_im5[[6]]
B8_5=Stack_im5[[7]]
B8A_5=Stack_im5[[8]]
B11_5=Stack_im5[[9]]
B12_5=Stack_im5[[10]]


B2_65=Stack_im65[[1]]
B3_65=Stack_im65[[2]]
B4_65=Stack_im65[[3]]
B5_65=Stack_im65[[4]]
B6_65=Stack_im65[[5]]
B7_65=Stack_im65[[6]]
B8_65=Stack_im65[[7]]
B8A_65=Stack_im65[[8]]
B11_65=Stack_im65[[9]]
B12_65=Stack_im65[[10]]

B2_67=Stack_im67[[1]]
B3_67=Stack_im67[[2]]
B4_67=Stack_im67[[3]]
B5_67=Stack_im67[[4]]
B6_67=Stack_im67[[5]]
B7_67=Stack_im67[[6]]
B8_67=Stack_im67[[7]]
B8A_67=Stack_im67[[8]]
B11_67=Stack_im67[[9]]
B12_67=Stack_im67[[10]]



#calculer des indices 

B_2=B2_2
G_2=B3_2
R_2=B4_2
NIR_2=B8_2

B_4=B2_4
G_4=B3_4
R_4=B4_4
NIR_4=B8_4

B_5=B2_5
G_5=B3_5
R_5=B4_5
NIR_5=B8_5

B_65=B2_65
G_65=B3_65
R_65=B4_65
NIR_65=B8_65

B_67=B2_67
G_67=B3_67
R_67=B4_67
NIR_67=B8_67




L=0.5

Ge_2=(2*(NIR_2^(2)-R_2^(2))+1.5*NIR_2+0.5*R_2)/(NIR_2+R_2+0.5)
Ge_4=(2*(NIR_4^(2)-R_4^(2))+1.5*NIR_4+0.5*R_4)/(NIR_4+R_4+0.5)
Ge_5=(2*(NIR_5^(2)-R_5^(2))+1.5*NIR_5+0.5*R_5)/(NIR_5+R_5+0.5)
Ge_65=(2*(NIR_65^(2)-R_65^(2))+1.5*NIR_65+0.5*R_65)/(NIR_65+R_65+0.5)
Ge_67=(2*(NIR_67^(2)-R_67^(2))+1.5*NIR_67+0.5*R_67)/(NIR_67+R_67+0.5)

#NDVI
ndvi_2=(NIR_2-R_2)/(NIR_2+R_2)
ndvi_4=(NIR_4-R_4)/(NIR_4+R_4)
ndvi_5=(NIR_5-R_5)/(NIR_5+R_5)
ndvi_65=(NIR_65-R_65)/(NIR_65+R_65)
ndvi_67=(NIR_67-R_67)/(NIR_67+R_67)

#GNDVI
gndvi_2=(NIR_2-G_2)/(NIR_2+G_2)
gndvi_4=(NIR_4-G_4)/(NIR_4+G_4)
gndvi_5=(NIR_5-G_5)/(NIR_5+G_5)
gndvi_65=(NIR_65-G_65)/(NIR_65+G_65)
gndvi_67=(NIR_67-G_67)/(NIR_67+G_67)


# #SAVI
savi_2=((NIR_2 - R_2) / (NIR_2 + R_2 + L)) * (1 + L) 
savi_4=((NIR_4 - R_4) / (NIR_4 + R_4 + L)) * (1 + L) 
savi_5=((NIR_5 - R_5) / (NIR_5 + R_5 + L)) * (1 + L) 
savi_65=((NIR_65 - R_65) / (NIR_65 + R_65 + L)) * (1 + L) 
savi_67=((NIR_67 - R_67) / (NIR_67 + R_67 + L)) * (1 + L)

# #ARVI
arvi_2=(NIR_2-2*R_2+B_2)/(NIR_2+2*R_2-B_2)
arvi_4=(NIR_4-2*R_4+B_4)/(NIR_4+2*R_4-B_4)
arvi_5=(NIR_5-2*R_5+B_5)/(NIR_5+2*R_5-B_5)
arvi_65=(NIR_65-2*R_65+B_65)/(NIR_65+2*R_65-B_65)
arvi_67=(NIR_67-2*R_67+B_67)/(NIR_67+2*R_67-B_67)


# #MSAVI
msavi_2=((2*NIR_2+1)-((2*NIR_2+1)^(2)-8*(NIR_2-2*R_2))^(0.5))/2
msavi_4=((2*NIR_4+1)-((2*NIR_4+1)^(2)-8*(NIR_4-2*R_4))^(0.5))/2
msavi_5=((2*NIR_5+1)-((2*NIR_5+1)^(2)-8*(NIR_5-2*R_5))^(0.5))/2
msavi_65=((2*NIR_65+1)-((2*NIR_65+1)^(2)-8*(NIR_65-2*R_65))^(0.5))/2
msavi_67=((2*NIR_67+1)-((2*NIR_67+1)^(2)-8*(NIR_67-2*R_67))^(0.5))/2

# #ASVI
asvi_2=((2*NIR_2+1)-((2*NIR_2+1)^(2)-8*(NIR_2-2*R_2+B_2))^(0.5))/2
asvi_4=((2*NIR_4+1)-((2*NIR_4+1)^(2)-8*(NIR_4-2*R_4+B_4))^(0.5))/2
asvi_5=((2*NIR_5+1)-((2*NIR_5+1)^(2)-8*(NIR_5-2*R_5+B_5))^(0.5))/2
asvi_65=((2*NIR_65+1)-((2*NIR_65+1)^(2)-8*(NIR_65-2*R_65+B_65))^(0.5))/2
asvi_67=((2*NIR_67+1)-((2*NIR_67+1)^(2)-8*(NIR_67-2*R_67+B_67))^(0.5))/2


#GEMI
gemi_2=Ge_2*(1-0.25*Ge_2)-(R_2-0.125)/(1-R_2)
gemi_4=Ge_4*(1-0.25*Ge_4)-(R_4-0.125)/(1-R_4)
gemi_5=Ge_5*(1-0.25*Ge_5)-(R_5-0.125)/(1-R_5)
gemi_65=Ge_65*(1-0.25*Ge_65)-(R_65-0.125)/(1-R_65)
gemi_67=Ge_67*(1-0.25*Ge_67)-(R_67-0.125)/(1-R_67)


#DVI
dvi_2= NIR_2-R_2
dvi_4= NIR_4-R_4
dvi_5= NIR_5-R_5
dvi_65= NIR_65-R_65
dvi_67= NIR_67-R_67


#RVI
rvi_2= NIR_2/R_2
rvi_4= NIR_4/R_4
rvi_5= NIR_5/R_5
rvi_65= NIR_65/R_65
rvi_67= NIR_67/R_67


#CRVI
crvi_2=(R_2-G_2)/NIR_2
crvi_4=(R_4-G_4)/NIR_4
crvi_5=(R_5-G_5)/NIR_5
crvi_65=(R_65-G_65)/NIR_65
crvi_67=(R_67-G_67)/NIR_67


# #EVI
evi_2=2.5*((NIR_2-R_2)/(NIR_2+6*R_2-7*B_2+1))
evi_4=2.5*((NIR_4-R_4)/(NIR_4+6*R_4-7*B_4+1))
evi_5=2.5*((NIR_5-R_5)/(NIR_5+6*R_5-7*B_5+1))
evi_65=2.5*((NIR_65-R_65)/(NIR_65+6*R_65-7*B_65+1))
evi_67=2.5*((NIR_67-R_67)/(NIR_67+6*R_67-7*B_67+1))


# #WAVI
wavi_2=(1+L)*((NIR_2-B_2)/(NIR_2+B_2+L))
wavi_4=(1+L)*((NIR_4-B_4)/(NIR_4+B_4+L))
wavi_5=(1+L)*((NIR_5-B_5)/(NIR_5+B_5+L))
wavi_65=(1+L)*((NIR_65-B_65)/(NIR_65+B_65+L))
wavi_67=(1+L)*((NIR_67-B_67)/(NIR_67+B_67+L))







## Sélection des variables##

# créer un stack avec l'image et les indices


many_rst=list(B7_2, B8_2, B8A_2, B11_2, B12_2, ndvi_2, B8_4, B8A_4, B11_4, B12_4, gndvi_4, arvi_4, B2_5, B5_5, B6_5, B8A_5, arvi_5, B8_65, B12_65, savi_65, B5_67, B6_67, B12_67, gndvi_67, savi_67)


Stack_im=rast(many_rst)

names(Stack_im)= c("B7_2", "B8_2", "B8A_2", "B11_2", "B12_2", "ndvi_2", "B8_4", "B8A_4", "B11_4", "B12_4", "gndvi_4", "arvi_4", "B2_5", "B5_5", "B6_5", "B8A_5", "arvi_5", "B8_65", "B12_65", "savi_65", "B5_67", "B6_67", "B12_67", "gndvi_67", "savi_67")





######################### Random Forest #######################################

plotRGB(Stack_im,r=4,g=3,b=2,stretch='lin')



# open training samples
CLC_poly_AOI= read_sf("echantillons_esnandes_multidates_0505/echantillon_esnandes_vege_multidates_0505.shp")


crs(CLC_poly_AOI)
CLC_poly_AOI=st_transform(CLC_poly_AOI,crs(Stack_im))


CLC_poly_AOI=CLC_poly_AOI[,-1] #retirer le numéro de l'échantillon

names(CLC_poly_AOI)
table(CLC_poly_AOI$classe)



# Class names
table(CLC_poly_AOI$classe)

# Extract les signatures spectrales des points
#autre méthode je retrouve à partir de mes points, les numéros des pixels correspondants 

ind_points= extract(Stack_im, CLC_poly_AOI)

ind_points$classe= as.factor(CLC_poly_AOI$classe)                       
Spectra_df=ind_points[,-1]   #retirer le numéro de l'échantillon
names(Spectra_df)= c("B7_2", "B8_2", "B8A_2", "B11_2", "B12_2", "ndvi_2", "B8_4", "B8A_4", "B11_4", "B12_4", "gndvi_4", "arvi_4", "B2_5", "B5_5", "B6_5", "B8A_5", "arvi_5", "B8_65", "B12_65", "savi_65", "B5_67", "B6_67", "B12_67", "gndvi_67", "savi_67", "classe") 
head(Spectra_df)
dim(Spectra_df)
str(Spectra_df)





#remove points with NA values
POSNA= which(is.na(Spectra_df))#[,1]
POSNA
if (length(POSNA)>0) {
  Spectra_df=Spectra_df[-POSNA,]
}
dim(Spectra_df)
# on a supprimé les points NA

# split du data set

N= nrow(Spectra_df)
TRAIN= sample(x=1:N,replace=F, size=N*0.7)
Spectra_train_df=Spectra_df[TRAIN,]
Spectra_valid_df= Spectra_df[-TRAIN,]
str(Spectra_train_df)
str(Spectra_valid_df)


cat("\t compute random forest model \n")
#ligne juste pour afficher le message dans la console 

##Spectra_train_df[,1:25] -> je travaille avec les variables 1 à 25 (la 26e est la classe)

#Tuning 
Tune_RF<-tuneRF(Spectra_train_df[,1:25],Spectra_train_df$classe, ntreeTry = 200, stepFactor = 2, trace = FALSE, plot = FALSE,improve = 0.5, doBest = FALSE)

#Calibration
calib_RF = randomForest(Spectra_train_df[,1:25],Spectra_train_df$classe, data = Spectra_train_df, mtry = Tune_RF[which.min(Tune_RF[,2]),1], 
                        ntree=200, importance = TRUE, na.action=na.omit)

plot(calib_RF$err.rate[,1], type = "l", xlab = "Tree number", ylab = "error OOB") #permet de savoir si le nombre d'arbre permet d'améliorer la précision


# Retrieve important variables
#MDA = used for RF-based regression
#The Mean Decrease Accuracy plot expresses how much accuracy the model losses by excluding each variable.
#The more the accuracy suffers, the more important the variable is for the successful classification
#The variables are presented from descending importance. 

#MDG = used for RF-based classification
#The mean decrease in Gini coefficient is a measure of how each variable contributes to the homogeneity of the nodes and leaves in the resulting random forest. 
#The higher the value of mean decrease accuracy or mean decrease Gini score, the higher the importance of the variable in the model.


varImpPlot(calib_RF)
imp_var= importance(calib_RF, type=2)
imp_var= order(imp_var, decreasing=TRUE)
imp_var
#permet de trier les varaibles les plus importantes poure lancer la classification qu'avec les variables qui nouys intéressent le plus
#une fois arrivé ici on peut relancer ce qu'on a fait avant mais en gardant que les variables qui nous intéresse le plus 
#bien penser à modifier les Spectra_valid_df[,1:n] qui arrvient après 

######penser à bien garder que les variables les plus explicatives en relançant ce qu'on a fait avant #####

# predict and compute

predic_RF= predict(calib_RF, Spectra_valid_df[,1:25], type="response") #penser à changer les variales avec lesquelles on travaille
matrice_confusion=caret::confusionMatrix(predic_RF,Spectra_valid_df$classe)
matrice_confusion

# apply model


Stack_crop_mx=as.matrix(Stack_im)
POSNA= which(is.nan(Stack_crop_mx)) 
Stack_crop_mx_na=Stack_crop_mx[-POSNA,-c(26)]#,-c(n)
#pendant la mise en place du TRAIN et TEST on voit qu'on a n varaibles (nos bandes + la classe donc -c(n) pour virer la variable classe)
Classif_rf=predict(calib_RF,Stack_crop_mx_na)
str(Classif_rf)

r= numeric (ncell(Stack_im))
r[-POSNA]=Classif_rf
Classif_rf_im=setValues(Stack_im[[1]],r)
plot(Classif_rf_im,main="classif")

writeRaster(Classif_rf_im, "Classif_SRFj3_esnandes_0_9402.tif", datatype="INT2U", overwrite=T)





#rÃ©cupÃ©rer les Ã©chantillons avec les valeurs spÃ©ctrales sous forme de csv 

csv=ind_points
csv_scale=csv
csv_scale[,2:10]=scale(csv[,2:10])
write.csv(csv_scale, "SRFj_pts_extract_scale.csv", row.names=FALSE)



# SI LA VERSION AU DESSUS FONCTIONNE PAS UTILISER CES ETAPES :

# write.csv(ind_points, "SRFj_pts_extract.csv", row.names=FALSE)
# #entre temps h'ouvre ce csv dans excel pour le rÃ©enregistrer en csv avec sÃ©parateur point virgule, peut Ãªtre qu'on pourrait sauter cette Ã©tape
# #on ferait juste une copie de nos Ã©chantillons et centre et rÃ©duit Ã§a pour directement passer Ã  la derniÃ¨re Ã©tape
# 
# #ouvrir le csv pour centrer et rÃ©duire les donnÃ©es 
# 
# csv=read.csv("SRFj_pts_extract_csv.csv",dec=".", sep = ';')
# csv_scale=csv
# csv_scale[,2:71]=scale(csv[,2:71])
# 
# write.csv(csv_scale, "SRFj_pts_extract_scale.csv", row.names=FALSE)

























