datos<-read.delim("clipboard", header= T, sep="\t")# de la base de datos feno14_15 por mes
dat<-data.frame(datos)
rm(datos)
names (dat)
data2<-subset(dat, Especie=="flava") #cambiar por especie
attach(data2)
data2
rm(dat)

tapply(Flores, list(Mes, Año), mean) #para visualización, cambiar las variables
flo<-glm(Flores~Año*Mes)
summary(flo)
