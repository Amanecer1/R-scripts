ach<-read.table("C:\\Users\\Erandi\\Documents\\achimenes_mes.csv",header=T,sep=",")

ach<-read.table("C:\\Users\\Erandi\\Documents\\ach_completa.csv",header=T,sep=",")#meses en número

ach<-read.csv("C:\\Users\\Erandi\\Documents\\Achimenes_SIG.csv",header=T,sep=",") #completa
attach(ach)

ach
class(ach)
names(ach)
plot(Especie, floracion)
feno<-range (floracion, na.rm =T)#para número de mes
feno<-table(sindrome, floracion)# tabla de frecuencias
feno
write.table(feno,"C:\\Users\\Erandi\\Documents\\fenologia.csv",sep = ",", row.names = T, col.names = NA)
mosaicplot(feno)
chisq.test(feno, simulate.p.value=F, B=5000)
plot(altitud~Especie)
alt<-tapply(altitud, Especie, mean, na.rm=T)
write.table(alt,"C:\\Users\\Erandi\\Documents\\altitudes.csv",sep = ",", row.names = T, col.names = NA)
lat<-tapply(latitud, Especie, mean, na.rm=T)
write.table(lat,"C:\\Users\\Erandi\\Documents\\latitudes.csv",sep = ",", row.names = T, col.names = NA)
lon<-tapply(longitud, Especie, mean, na.rm=T)
write.table(lon,"C:\\Users\\Erandi\\Documents\\longitudes.csv",sep = ",", row.names = T, col.names = NA)

tapply(altitud, list(sindrome, floracion), mean, na.rm=T)

#Tzararacua_ archivo feno14_15
data<-read.delim("clipboard", header= T, sep="\t")# usar la tabla con columna de especie (tratamientos) y la pestaña datos (ambas en feno14_15)
class(data)
attach(data)
names(data)
año<-as.factor(Año) #otra base de datos
tapply(flores, list(fecha, Especie), mean, na.rm=T) #cambiar Botones, Flores, Frutos.M de tabla de datos

data2<-subset(data, Especie=="flava") #cambiar las especies para hacer comparación por año
attach(data2)
detach(data)
rm(data)
data2
names(data2)

mn<-glmer(Botones ~ 1 + (1 | Planta), family= poisson, na.action= na.omit)
m<- glmer(Botones ~ año + (año| Planta), family= poisson, na.action= na.omit)
m1<- glmer(Botones ~ año + (1 | Planta), family= poisson, na.action= na.omit)
mn<-glmer(Botones ~ 1 + (1 | Planta), family= poisson, na.action= na.omit)
m<- glmer(Botones ~ Mes * año + (año| Planta), family= poisson, na.action= na.omit)
summary(m)
anova(mn, m)
anova(m, m1)
contrasts(Especie)

#cargar paquete "circular" con columna especie
library(circular)
date<-circular(fecha, type = "angles", units = "radians")
date
bot<-circular(botones, type= "angles", units="radians")
flo<-circular(flores, type= "angles", units="radians")
fm<-circular(fm, type= "angles", units="radians")
m<-aov.circular(flo, Especie, method ="LRT")# cambiar cada uno de los estadios
m
rayleigh.test(flo) #hacer por especie

watson.williams.test(flo~Especie, data = data) #por año también


#para seleccionar las variables climáticas y análisis de autocorrelación
ach<-read.csv("C:\\Users\\Erandi\\Documents\\Achimenes_SIG.csv",header=T,sep=",")
attach(ach)
ach
colnames(ach)
dim(ach)
pcclims<- ach[2:260, 14:32]
pcclims
climas<- princomp (formula = ~., data = pcclims, cor = TRUE, na.action=na.exclude, scores=T) 
summary(climas)
loadings(climas)
eig<-(climas$sdev)^2
eig
pesos<-climas$scores
pesos
write.table(pesos,"C:\\Users\\Erandi\\Documents\\climasPc.csv",sep=",",row.names = T, col.names = NA)
library(ggplot2)
library(ggfortify)
autoplot(climas, data =ach, colour ='Especie', label= F, loadings = TRUE, loadings.colour = 'black', loadings.label = TRUE)
autoplot(climas, data =ach, colour ='sindrome', label= F, loadings = TRUE, loadings.colour = 'black', loadings.label = TRUE)
biplot(climas)
#--------------------------------------------------
clim<-read.table("C:\\Users\\Erandi\\Documents\\climasPc.csv",header=T,sep=",")
attach(clim)
names(clim)
M<-lm(comp.1~ Especie, na.action = na.omit, contrasts=T)
M<-glm(Comp.1~ Especie, na.action = na.omit, contrasts=T, family=  (link = "logit"))
summary(M)
anova(M, test ='F')
plot(M)
shapiro.test (M$residuals)
ks.test(M$residuals, "pnorm")
# Variances in percentage, más sencillo que el mío (A_morfo.R)
variance <- eig*100/sum(eig)
variance
# Cumulative variances
cumvar <- cumsum(variance)
cumvar


#---------------------para conocer la relación entre altitud y floración y entre latitud y floración por especie
espe<-subset(ach, Especie=="admirabilis") #cambiar las especies
attach(espe)
espe
detach(ach)
rm(ach) 
#volver a cargar la base de datos
ach<-read.table("C:\\Users\\Erandi\\Documents\\Achimenes_SIG.csv",header=T,sep=",")
names(ach)
attach(ach)

lrv<-log(radv)
lro<-log(rado)
rad<-cbind(lrv, lro)
scatterplotMatrix(~lrv+lro|sindrome, ellipse=T, by.groups= T, diagonal= "none", smooth= F)
m<-lm(rad~sindrome)
summary(m)
mr<- anova(m)
summary(mr)

M<-manova (rad ~ sindrome, na.action = na.omit, contrasts=T)
summary(M, "Wilks")
manova(M)
summary.aov(M)
eu<- c(1,0,0,0)
me<- c(1,1,0,0)
orni<- c(1,0,1,0)
psi<- c(1,0,0,1)
eu_me<- eu-me
eu_orni<- eu-orni
eu_psi<- eu-psi
me_orni<- me-orni
me_psi<- me-psi
orni_psi<- orni-psi
s_s<- rbind(eu_me, eu_orni, eu_psi, me_orni, me_psi, orni_psi)
prueba<- sample (1:25, 15)
dr<-lda (sindrome ~ rad, data= ach, na.action = "na.omit", method= "moment", subset= prueba)
dr<-lda (sindrome ~ rad, data= ach, na.action = "na.omit", method= "moment")

plot(M)
l6<-log (bc6)
l2<-log (bc2)
l12<-log (bc12)
l13<-log (bc13)
l16<-log (bc16)
vars<- cbind(l6, l2, l12, l13, l16)
M1<-manova (vars ~ sindrome, na.action = na.omit)
summary(M1, "Wilks")#usar Wilks
summary.aov(M1)
manova(M1)


library(circular)
aa<-circular(altitud, type= "angles", units="radians")
m<-aov.circular(aa, sindrome, method ="LRT")# cambiar cada uno de los estadios
m
plot(tapply(altitud, sindrome, var, na.rm= T), tapply(altitud, sindrome, mean, na.rm= T))
boxplot(altitud~sindrome)
lalt<-log(altitud, 10)
lalt
m1<-lm(lalt~sindrome, na.action=na.omit)
plot(m1)
M<-lm (altitud ~ sindrome, na.action = na.omit, contrasts=T)
summary(M)
anova(M)
plot(M)
s1<-c(1,0,0,0) #gaEuglossa
s2<-c(1,1,0,0) #melitofilia
s3<-c(1,0,1,0) #ornitofilia
s4<-c(1,0,0,1) #psicofilia
ss<-rbind(s1,s2,s3,s4)
contrasts(sindrome)
s1_s2<-s1-s2
s1_s3<-s1-s3
s1_s4<-s1-s4
s2_s3<-s2-s3
s2_s4<-s2-s4
s3_s4<-s3-s4
s_s<-rbind(s1_s2, s1_s3, s1_s4,s2_s3,s2_s4,s3_s4)
library(multcomp)
summary(glht(M, s_s))#diferencias entre tratamientos (vector s_s), cuál tratamiento difiere de cero (vector ss)
summary(glht(M, linfct = mcp (sindrome = "Tukey"))) #"mcp: multiple comparison of means", se puede cambiar la forma de contrastar matrices, "Dunnett", "GrandMean"... ver en contrMat
glht(M,s_s) #estimable (gmodels) y glht dan lo mismo hasta el valor de t, pero dan diferentes de cálculos de probabilidad
estimable(M, s_s)
plot(M)
source("c:\\Users\\Erandi\\Google Drive\\Clase R\\FUNCIONES.r")
m<-lm(floracion~latitud)
plot(m)
ks.test(residuos,"pnorm")
summary(m)
library(boot)
stat<-function(data, indices){
  t.test<- mean (pc$Comp.1~ach$Especie, data)$stat
  t.test
}

rand.gen<-function(pc, mle){
  out<-data
  out$pc$Comp.1<- sample(out$pc$Comp.1, replace = F)
  out
}

ctrl.boot<-boot(ach$Especie, stat, R= 5000, sim ="parametric", ran.gen= rand.gen)# ran.gen es sólo para sim = parametric
summary(ctrl.boot)
print(ctrl.boot)
plot(ctrl.boot)
boot.ci(ctrl.boot, type = "all")






#para análisis bioclimático (bioclim)
K<-0
for(i in levels (Especie)){
	K<-K+1
mean(altitud, na.rm=T)}


K<-0
h<-0
for(i in levels (Especie)){
	K<-K+1
longitud (Especie==i),
latitud (Especie==i),
}


head(ach)
tail(ach)
dim(ach)
str(ach)
class(ach)
library(dismo)
library(maptools)


temp<-raster("c:\\Users\\Erandi\\Documents\\bio1-9_30s_bil\\bio_1.bil")
temp1<-raster("E:\\bio1-9_30s_bil\\bio_1.bil")#los datos están en la memoria
temp2<-raster("E:\\bio1-9_30s_bil\\bio_2.bil")
temp3<-raster("E:\\bio1-9_30s_bil\\bio_3.bil")
temp4<-raster("E:\\bio1-9_30s_bil\\bio_4.bil")
temp5<-raster("E:\\bio1-9_30s_bil\\bio_5.bil")
temp6<-raster("E:\\bio1-9_30s_bil\\bio_6.bil")
temp7<-raster("E:\\bio1-9_30s_bil\\bio_7.bil")
temp8<-raster("E:\\bio1-9_30s_bil\\bio_8.bil")
temp9<-raster("E:\\bio1-9_30s_bil\\bio_9.bil")
prec10<-raster("E:\\bio10-19_30s_bil\\bio_10.bil")
prec11<-raster("E:\\bio10-19_30s_bil\\bio_11.bil")
prec12<-raster("E:\\bio10-19_30s_bil\\bio_12.bil")
prec13<-raster("E:\\bio10-19_30s_bil\\bio_13.bil")
prec14<-raster("E:\\bio10-19_30s_bil\\bio_14.bil")
prec15<-raster("E:\\bio10-19_30s_bil\\bio_15.bil")
prec16<-raster("E:\\bio10-19_30s_bil\\bio_16.bil")
prec17<-raster("E:\\bio10-19_30s_bil\\bio_17.bil")
prec18<-raster("E:\\bio10-19_30s_bil\\bio_18.bil")
prec19<-raster("E:\\bio10-19_30s_bil\\bio_19.bil")

files<-(c())
pred<-stack(temp1,temp2, temp3, temp4, temp5, temp6, temp7, temp8, temp9, prec10, prec11, prec12, prec13, prec14,prec15,prec16, prec17,prec18,prec19)
coor<-ach[,1:2]
spp <- SpatialPoints(coor)
bs<- bioclim(pred, spp)
pairs(bs)
coordinates~ pred
presvals<-extract(pred,spp)
write.table(presvals,"C:\\Users\\Erandi\\Documents\\bioclim.csv",sep = ",", row.names = T, col.names = NA)
bioc<-read.table("C:\\Users\\Erandi\\Documents\\bioclim.csv",header=T,sep=",")
bioc
class(bioc)
clima<-princomp(formula = ~., data = bioc, cor = TRUE, na.action=na.exclude) #para que corra
summary(clima)
clima$sdev
loadings(clima)
clima$scores [,1:2] #son los dos primeros componentes principales del análisis
head(clima$scores [,1:2]) #para checar
pc1_2<-clima$scores [,1:2]
pc12<-as.data.frame(pc1_2) #buscar la forma para encontrar la relación entre el PCA y la floración
write.table(pc12,"C:\\Users\\Erandi\\Documents\\pc_1_2.csv",sep = ",", row.names = T, col.names = NA)
pc<-read.table("C:\\Users\\Erandi\\Documents\\pc_1_2.csv",sep = ",", header=T)
qqnorm(pc$Comp.1)
ach<-read.table("C:\\Users\\Erandi\\Documents\\achimenes_mes.csv",header=T,sep=",")# sin attach
plot(pc$Comp.1~ach$Especie)
plot(pc$Comp.2~ach$Especie)
plot(ach$altitud~ach$Especie)
mf<-lm(pc$Comp.1~ach$floracion)
summary(mf)
ma<-lm(pc$Comp.1~ach$altitud)
summary(ma)

read.table("C:\\Users\\Erandi\\Documents\\climasPc.csv",header=T,sep=",")
plot(climasPc$Comp.1~ach$Especie)
plot(ClimasPc$Comp.2~ach$Especie)
#para usar bootstrap
stat<-function(data, indices){
	t.test<- mean (pc$Comp.1~ach$Especie, data)$stat
	t.test
}

rand.gen<-function(pc, mle){
  out<-data
  out$pc$Comp.1<- sample(out$pc$Comp.1, replace = F)
  out
}

ctrl.boot<-boot(ach$Especie, stat, R= 5000, sim ="parametric", ran.gen= rand.gen)# ran.gen es sólo para sim = parametric
summary(ctrl.boot)
print(ctrl.boot)
plot(ctrl.boot)
boot.ci(ctrl.boot, type = "all")


