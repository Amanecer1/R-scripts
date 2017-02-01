rm(list=ls())

library(sp)
library(gstat)
library(lattice)
library(gridExtra)
library(rgdal)
library(raster)
library(maptools)

ProjMex<- "+proj=lcc +lat_1=17.5 +lat_2=29.5 +lat_0=12 +lon_0=-102 +x_0=2500000 +y_0=0 +ellps=GRS80 +units=m +no_defs" # características de mapas de México
ProjGCS_WSG84<- "+proj=longlat +datum=WGS84 +no_defs" # datum de los mapas de México

Mex_Lamb<-readShapePoly("C:\\Users\\Erandi\\Google Drive\\Erandi_GIS_PhD\\Capas_SIG\\Entidades_2013.shp", proj4string=CRS(ProjMex)) # para hacer que la información de México quede disponible para manipularla, bajo las características definidas antes
Mex_GCS<-spTransform(Mex_Lamb,CRS(ProjGCS_WSG84)) # para transformar el vector de las coordenadas en el mismo tipo de información de México

par(mfrow=c(1,1),mar=c(1,1,1,1))
plot(Mex_GCS)
box()

### Ver la base de datos proporcionada: meuse. Checar el pdf adjunto también
ach<- read.csv("Achimenes_SIG.csv", header = T, sep=",") #tuve que cambiar la coma para que leyera bien el archivo
attach(ach)
head(ach)
tail(ach)
dim(ach)
str(ach)
class(ach)
ach$Especie
anti<-subset(ach, Especie == "antirrhina")

points(anti$longitud,anti$latitud,col="red",pch=20, cex=0.5)

temp<-raster("E:\\bio1-9_30s_bil\\bio_1.bil")# datos en memoria

plot(temp)
tannu<-crop(temp,Mex_GCS)
par(mfrow=c(1,1),mar=c(1,1,1,10))
plot(tannu)

prec<-raster("E:\\bio10-19_30s_bil\\bio_12.bil")
pannu<-crop(prec,Mex_GCS)
plot(pannu)

files<- list(tannu,pannu)
pred<-stack(files) #variables predictoras
pred

coor<-ach[,1:2]
spp <- SpatialPoints(coor)
bs<- bioclim(pred, spp)
pairs(bs)
coordinates~ pred
presvals<-c(v1,v12)
pb<-predict(pred, bs)
bioclim(presvals, spp)
plot(pb, main ="Bioclim (1 y 12)")

fisio<-readShapePoly("E:\\fisiograficas\\ ",proj4string=CRS(ProjMex))
fis<-spTransform(fisio,CRS(ProjGCS_WSG84))

par(mfrow=c(1,1),mar=c(1,1,1,1))
plot(fis)

par(mfrow=c(1,1),mar=c(1,1,1,1))
plot(ecorr)
box()

eco<-readShapePoly("E:\\ecorregiones\\ecomex.shp", proj4string=CRS(ProjMex))
ecorr<-spTransform(eco,CRS(ProjGCS_WSG84))

par(mfrow=c(1,1),mar=c(1,1,1,1))
plot(ecorr)
box()


COLOR <- c("darkred", "darkblue", "darkgreen", "orange","darkcyan","darkorchid1","darkturquoise","firebrick","gold","sandybrown","yellowgreen","mistyrose3","plum","salmon","yellow","green","coral","seagreen","red","purple")
K <- 0
for (i in levels(Especie)){
K<- K+1
points(longitud[Especie == i], latitud[Especie == i], pch = 20, 
			 col = COLOR[K], cex = 0.5)	
}

legend("bottomleft",levels(Especie),title= "Especie",col=c("darkred", "darkblue", "darkgreen", "orange","darkcyan","darkorchid1","darkturquoise","firebrick","gold","sandybrown","yellowgreen","mistyrose3","plum","salmon","yellow","green","coral","seagreen","red","purple"),bty="n",pch=20, cex= 0.5)



#Darle coordenadas al data.frame, o sea, tranformarlo en un spatial points data frame
coordinates(ach) <- c("longitud", "latitud")
proj4string(ach) <- CRS(ProjGCS_WSG84) # para asociar datos espaciales con coordenadas
writeOGR(ach, dsn="Capas_SIG",layer="achshape", driver="ESRI Shapefile", overwrite=TRUE) #para vectorizar la capa de Achimenes_SIG

mrmidbi1<-raster("c:\\Users\\Erandi\\Google Drive\\Erandi_GIS_PhD\\mrmidbi1.tif") #para cada una de las capas de Bioclim
mrmidbi2<-raster("c:\\Users\\Erandi\\Google Drive\\Erandi_GIS_PhD\\mrmidbi2.tif")
mrmidbi3<-raster("c:\\Users\\Erandi\\Google Drive\\Erandi_GIS_PhD\\mrmidbi3.tif")
mrmidbi4<-raster("c:\\Users\\Erand\\Google Drive\\Erandi_GIS_PhD\\mrmidbi4.tif")
mrmidbi5<-raster("c:\\Users\\Erandi\\Google Drive\\Erandi_GIS_PhD\\mrmidbi5.tif")
mrmidbi6<-raster("c:\\Users\\Erandi Ra\\Google Drive\\Erandi_GIS_PhD\\mrmidbi6.tif")
mrmidbi7<-raster("c:\\Users\\Erandi Ra\\Google Drive\\Erandi_GIS_PhD\\mrmidbi7.tif")
mrmidbi8<-raster("c:\\Users\\Erandi RA\\Google Drive\\Erandi_GIS_PhD\\mrmidbi8.tif")
mrmidbi9<-raster("c:\\Users\\Erandi Ra\\Google Drive\\Erandi_GIS_PhD\\mrmidbi9.tif")
mrmidbi10<-raster("c:\\Users\\Erandi RA\\Google Drive\\Erandi_GIS_PhD\\mrmidbi10.tif")
mrmidbi11<-raster("c:\\Users\\Erandi RA\\Google Drive\\Erandi_GIS_PhD\\mrmidbi11.tif")
mrmidbi12<-raster("c:\\Users\\Erandi RA\\Google Drive\\Erandi_GIS_PhD\\mrmidbi12.tif")
mrmidbi13<-raster("c:\\Users\\Erandi RA\\Google Drive\\Erandi_GIS_PhD\\mrmidbi13.tif")
mrmidbi14<-raster("c:\\Users\\Erandi RA\\Google Drive\\Erandi_GIS_PhD\\mrmidbi14.tif")
mrmidbi15<-raster("c:\\Users\\Erandi RA\\Google Drive\\Erandi_GIS_PhD\\mrmidbi15.tif")
mrmidbi16<-raster("c:\\Users\\Erandi RA\\Google Drive\\Erandi_GIS_PhD\\mrmidbi16.tif")
mrmidbi17<-raster("c:\\Users\\Erandi RA\\Google Drive\\Erandi_GIS_PhD\\mrmidbi17.tif")
mrmidbi18<-raster("c:\\Users\\Erandi RA\\Google Drive\\Erandi_GIS_PhD\\mrmidbi18.tif")
mrmidbi19<-raster("c:\\Users\\Erandi RA\\Google Drive\\Erandi_GIS_PhD\\mrmidbi19.tif")



mrmidbi1_c<-crop(mrmidbi1,Mex_GCS) # para que la capa de Bioclim sólo contenga los datos de México
mrmidbi2_c<-crop(mrmidbi2,Mex_GCS)
mrmidbi3_c<-crop(mrmidbi3,Mex_GCS)
mrmidbi4_c<-crop(mrmidbi4,Mex_GCS)
mrmidbi5_c<-crop(mrmidbi5,Mex_GCS)
mrmidbi6_c<-crop(mrmidbi6,Mex_GCS)
mrmidbi7_c<-crop(mrmidbi7,Mex_GCS)
mrmidbi8_c<-crop(mrmidbi8,Mex_GCS)
mrmidbi9_c<-crop(mrmidbi9,Mex_GCS)
mrmidbi10_c<-crop(mrmidbi10,Mex_GCS)
mrmidbi11_c<-crop(mrmidbi11,Mex_GCS)
mrmidbi12_c<-crop(mrmidbi12,Mex_GCS)
mrmidbi13_c<-crop(mrmidbi13,Mex_GCS)
mrmidbi14_c<-crop(mrmidbi14,Mex_GCS)
mrmidbi15_c<-crop(mrmidbi15,Mex_GCS)
mrmidbi16_c<-crop(mrmidbi16,Mex_GCS)
mrmidbi17_c<-crop(mrmidbi17,Mex_GCS)
mrmidbi18_c<-crop(mrmidbi18,Mex_GCS)
mrmidbi19_c<-crop(mrmidbi19,Mex_GCS)




writeRaster(mrmidbi1_c, filename="Capas_SIG//mrmidbi1_c.tif", datatype="FLT4S", overwrite=TRUE) # para que la capa de Bioblim quede como archivo tif sólo con los datos para México, datatype= FLT4S muestra los datos incluyendo decimales, hace más pesado el archivo, quedaron en Documentos
writeRaster(mrmidbi2_c, filename="Capas_SIG//mrmidbi2_c.tif", datatype="FLT4S", overwrite=TRUE)
writeRaster(mrmidbi3_c, filename="Capas_SIG//mrmidbi3_c.tif", datatype="FLT4S", overwrite=TRUE)
writeRaster(mrmidbi4_c, filename="Capas_SIG//mrmidbi4_c.tif", datatype="FLT4S", overwrite=TRUE)
writeRaster(mrmidbi5_c, filename="Capas_SIG//mrmidbi5_c.tif", datatype="FLT4S", overwrite=TRUE)
writeRaster(mrmidbi6_c, filename="Capas_SIG//mrmidbi6_c.tif", datatype="FLT4S", overwrite=TRUE)
writeRaster(mrmidbi7_c, filename="Capas_SIG//mrmidbi7_c.tif", datatype="FLT4S", overwrite=TRUE)
writeRaster(mrmidbi8_c, filename="Capas_SIG//mrmidbi8_c.tif", datatype="FLT4S", overwrite=TRUE)
writeRaster(mrmidbi9_c, filename="Capas_SIG//mrmidbi9_c.tif", datatype="FLT4S", overwrite=TRUE)
writeRaster(mrmidbi10_c, filename="Capas_SIG//mrmidbi10_c.tif", datatype="FLT4S", overwrite=TRUE)
writeRaster(mrmidbi11_c, filename="Capas_SIG//mrmidbi11_c.tif", datatype="FLT4S", overwrite=TRUE)
writeRaster(mrmidbi12_c, filename="Capas_SIG//mrmidbi12_c.tif", datatype="FLT4S", overwrite=TRUE)
writeRaster(mrmidbi13_c, filename="Capas_SIG//mrmidbi13_c.tif", datatype="FLT4S", overwrite=TRUE)
writeRaster(mrmidbi14_c, filename="Capas_SIG//mrmidbi14_c.tif", datatype="FLT4S", overwrite=TRUE)
writeRaster(mrmidbi15_c, filename="Capas_SIG//mrmidbi15_c.tif", datatype="FLT4S", overwrite=TRUE)
writeRaster(mrmidbi16_c, filename="Capas_SIG//mrmidbi16_c.tif", datatype="FLT4S", overwrite=TRUE)
writeRaster(mrmidbi17_c, filename="Capas_SIG//mrmidbi17_c.tif", datatype="FLT4S", overwrite=TRUE)
writeRaster(mrmidbi18_c, filename="Capas_SIG//mrmidbi18_c.tif", datatype="FLT4S", overwrite=TRUE)
writeRaster(mrmidbi19_c, filename="Capas_SIG//mrmidbi19_c.tif", datatype="FLT4S", overwrite=TRUE)

ach_r1<-rasterize(ach, mrmidbi1_c, field="id_esp",datatype="INT2S",overwrite=TRUE) # para crear un vector tipo raster de cada capa de Bioclim, y poder cargarla en QGis, cambiar field a números por especie para poder marcarlos en el nuevo mapa
ach_r2<-rasterize(ach, mrmidbi2_c, field="id_esp",datatype="INT2S",overwrite=TRUE)
ach_r3<-rasterize(ach, mrmidbi3_c, field="id_esp",datatype="INT2S",overwrite=TRUE)
ach_r4<-rasterize(ach, mrmidbi4_c, field="id_esp",datatype="INT2S",overwrite=TRUE)
ach_r5<-rasterize(ach, mrmidbi5_c, field="id_esp",datatype="INT2S",overwrite=TRUE)
ach_r6<-rasterize(ach, mrmidbi6_c, field="id_esp",datatype="INT2S",overwrite=TRUE)
ach_r7<-rasterize(ach, mrmidbi7_c, field="id_esp",datatype="INT2S",overwrite=TRUE)
ach_r8<-rasterize(ach, mrmidbi8_c, field="id_esp",datatype="INT2S",overwrite=TRUE)
ach_r9<-rasterize(ach, mrmidbi9_c, field="id_esp",datatype="INT2S",overwrite=TRUE)
ach_r10<-rasterize(ach, mrmidbi10_c, field="id_esp",datatype="INT2S",overwrite=TRUE)
ach_r11<-rasterize(ach, mrmidbi11_c, field="id_esp",datatype="INT2S",overwrite=TRUE)
ach_r12<-rasterize(ach, mrmidbi12_c, field="id_esp",datatype="INT2S",overwrite=TRUE)
ach_r13<-rasterize(ach, mrmidbi13_c, field="id_esp",datatype="INT2S",overwrite=TRUE)
ach_r14<-rasterize(ach, mrmidbi14_c, field="id_esp",datatype="INT2S",overwrite=TRUE)
ach_r15<-rasterize(ach, mrmidbi15_c, field="id_esp",datatype="INT2S",overwrite=TRUE)
ach_r16<-rasterize(ach, mrmidbi16_c, field="id_esp",datatype="INT2S",overwrite=TRUE)
ach_r17<-rasterize(ach, mrmidbi17_c, field="id_esp",datatype="INT2S",overwrite=TRUE)
ach_r18<-rasterize(ach, mrmidbi18_c, field="id_esp",datatype="INT2S",overwrite=TRUE)
ach_r19<-rasterize(ach, mrmidbi19_c, field="id_esp",datatype="INT2S",overwrite=TRUE)

writeRaster(ach_r1, filename="Ac1.tif", datatype="INT2S", overwrite=TRUE) # para crear la nueva capa ráster en formato 
writeRaster(ach_r2, filename="Ac2.tif", datatype="INT2S", overwrite=TRUE)
writeRaster(ach_r3, filename="Ac3.tif", datatype="INT2S", overwrite=TRUE)
writeRaster(ach_r4, filename="Ac4.tif", datatype="INT2S", overwrite=TRUE)
writeRaster(ach_r5, filename="Ac5.tif", datatype="INT2S", overwrite=TRUE)
writeRaster(ach_r6, filename="Ac6.tif", datatype="INT2S", overwrite=TRUE)
writeRaster(ach_r7, filename="Ac7.tif", datatype="INT2S", overwrite=TRUE)
writeRaster(ach_r8, filename="Ac8.tif", datatype="INT2S", overwrite=TRUE)
writeRaster(ach_r9, filename="Ac9.tif", datatype="INT2S", overwrite=TRUE)
writeRaster(ach_r10, filename="Ac10.tif", datatype="INT2S", overwrite=TRUE)
writeRaster(ach_r11, filename="Ac11.tif", datatype="INT2S", overwrite=TRUE)
writeRaster(ach_r12, filename="Ac12.tif", datatype="INT2S", overwrite=TRUE)
writeRaster(ach_r13, filename="Ac13.tif", datatype="INT2S", overwrite=TRUE)
writeRaster(ach_r14, filename="Ac14.tif", datatype="INT2S", overwrite=TRUE)
writeRaster(ach_r15, filename="Ac15.tif", datatype="INT2S", overwrite=TRUE)
writeRaster(ach_r16, filename="Ac16.tif", datatype="INT2S", overwrite=TRUE)
writeRaster(ach_r17, filename="Ac17.tif", datatype="INT2S", overwrite=TRUE)
writeRaster(ach_r18, filename="Ac18.tif", datatype="INT2S", overwrite=TRUE)
writeRaster(ach_r19, filename="Ac19.tif", datatype="INT2S", overwrite=TRUE)


# radiación solar
pri<-readShapePoly("E:\\rsp16mpgw\\rsp16mpgw.shp")
vera<-readShapePoly("E:\\rsv16mpgw\\rsv16mpgw.shp")
oto<-readShapePoly("E:\\rso16mpgw\\rso16mpgw.shp")
invi<-readShapePoly("E:\\rsi16mpgw\\rsi16mpgw.shp")
#altitud


pred<-stack(files) #variables predictoras
pred
plot(pred)
#-----------------------------------XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX-------------------------
# para extraer de las coordenadas, los datos que le corresponden a las capas
ach<- read.csv("Achimenes_SIG.csv", header = T, sep=",") #tuve que cambiar la coma para que leyera bien el archivo
attach(ach)
plot(uno)
plot(siete)
plot(doce)
coor<-ach[,1:2] #selección de coordenadas

# para ver las capas climáticas
uno<- raster("C:\\Users\\Erandi\\documents\\Capas_SIG\\mrmidbi1_c.tif")
dos<- raster("C:\\Users\\Erandi\\documents\\Capas_SIG\\mrmidbi2_c.tif")
tres<- raster("C:\\Users\\Erandi\\documents\\Capas_SIG\\mrmidbi3_c.tif")
cuatro<- raster("C:\\Users\\Erandi\\documents\\Capas_SIG\\mrmidbi4_c.tif")
cinco<- raster("C:\\Users\\Erandi\\documents\\Capas_SIG\\mrmidbi5_c.tif")
seis<- raster("C:\\Users\\Erandi\\documents\\Capas_SIG\\mrmidbi6_c.tif")
siete<- raster("C:\\Users\\Erandi\\documents\\Capas_SIG\\mrmidbi7_c.tif")
ocho<- raster("C:\\Users\\Erandi\\documents\\Capas_SIG\\mrmidbi8_c.tif")
nueve<- raster("C:\\Users\\Erandi\\documents\\Capas_SIG\\mrmidbi9_c.tif")
diez<-raster("C:\\Users\\Erandi\\documents\\Capas_SIG\\mrmidbi10_c.tif")
once<-raster("C:\\Users\\Erandi\\documents\\Capas_SIG\\mrmidbi11_c.tif")
doce<-raster("C:\\Users\\Erandi\\documents\\Capas_SIG\\mrmidbi12_c.tif")
trece<-raster("C:\\Users\\Erandi\\documents\\Capas_SIG\\mrmidbi13_c.tif")
catorce<-raster("C:\\Users\\Erandi\\documents\\Capas_SIG\\mrmidbi14_c.tif")
quince<-raster("C:\\Users\\Erandi\\documents\\Capas_SIG\\mrmidbi15_c.tif")
dseis<-raster("C:\\Users\\Erandi\\documents\\Capas_SIG\\mrmidbi16_c.tif")
dsiete<-raster("C:\\Users\\Erandi\\documents\\Capas_SIG\\mrmidbi17_c.tif")
docho<-raster("C:\\Users\\Erandi\\documents\\Capas_SIG\\mrmidbi18_c.tif")
dnueve<-raster("C:\\Users\\Erandi\\documents\\Capas_SIG\\mrmidbi19_c.tif")
pred<-stack(uno, dos, tres, cuatro, cinco, seis, siete, ocho, nueve, diez, once, doce, trece, catorce, quince, dseis, dsiete, docho, dnueve)
files<- list(uno,doce)
# extraer valores por coordenadas
clims<-extract(pred, coor)
v1<-extract(uno,coor) 

#radiación solar
prim<-extract(pri,coor)
ver<-extract(vera,coor)
ot<-extract(oto,coor)
inv<-extract(invi,coor)


#datos en archivos para EXcel
write.table(v1,"C:\\Users\\Erandi RA\\Documents\\bc1.csv",sep = ",", row.names = T, col.names = NA)
write.table(clims,"C:\\Users\\Erandi RA\\Documents\\bclims_SIG.csv",sep = ",", row.names = T, col.names = NA)
write.table(ver,"C:\\Users\\Erandi RA\\Documents\\radv.csv",sep = ",", row.names = T, col.names = NA)
write.table(ot,"C:\\Users\\Erandi RA\\Documents\\rado.csv",sep = ",", row.names = T, col.names = NA)
write.table(inv,"C:\\Users\\Erandi RA\\Documents\\radi.csv",sep = ",", row.names = T, col.names = NA)
write.table(prim,"C:\\Users\\Erandi RA\\Documents\\radp.csv",sep = ",", row.names = T, col.names = NA)



#por cada una de las variables climáticas, ver TablaMapaBIS en Google Drive
plot(pb, main ="Bioclim (1 y 12)")
COLOR <- c("darkred", "darkblue", "darkgreen", "orange","darkcyan","darkorchid1","darkturquoise","firebrick","gold","sandybrown","yellowgreen","mistyrose3","plum","salmon","yellow","green","coral","seagreen","red","purple")
K <- 0
for (i in levels(Especie)){
K<- K+1
points(longitud[Especie == i], latitud[Especie == i], pch = 20,
col = COLOR[K], cex = 0.5)
}
legend("bottomleft",levels(Especie),title= "Especie",col=c("darkred", "darkblue", "darkgreen", "orange","darkcyan","darkorchid1","darkturquoise","firebrick","gold","sandybrown","yellowgreen","mistyrose3","plum","salmon","yellow","green","coral","seagreen","red","purple"),bty="n",pch=20, cex= 0.5)




