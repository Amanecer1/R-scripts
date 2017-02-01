datos<-read.table("C:\\Users\\Erandi\\Documents\\nectar_achimenes.csv",header=T,sep=",")# de la base nectar_inv2016 por especie
class(datos)
attach(datos)
names(datos)
datos
source("c:\\Users\\Erandi\\Google Drive\\Clase R\\FUNCIONES.r")
bosque 
EE<-function(x){sqrt(var(x,na.rm=T)/length(x))}
resumen(azucar)# bases de datos por especie
resumen(BRIX.REAL) #bases de datos por especie
tapply(ultotal,Especie,mean,na.rm=T)
tapply(ultotal,Especie,EE)
tapply(BRIX.REAL,Especie,mean,na.rm=T)
tapply(BRIX.REAL,Especie,EE) # hace el error con length, hay que cambiarlo por especie
levels (Especie)
espe<-subset(datos, Especie=="skinnerii") #cambiar las especies
attach(espe)
summary.data.frame(espe)
espe
detach(datos)
rm(datos) 
EE<-function(x){sqrt(var(x,na.rm=T)/ 11)}# cambiar el denominador cuando haya NA's por factor
mean(BRIX.REAL,na.rm=T)
EE(BRIX.REAL)
rm(espe)


plot(ultotal,BRIX.REAL)

plot(logul,BRIX.REAL)
mean (ultotal, na.rm=T)
mean(BRIX.REAL,na.rm = T)
var(ultotal, na.rm=T)
var(BRIX.REAL,na.rm = T)
summary(ultotal,na.rm=T)
summary(BRIX.REAL,na.rm=T)
EE(BRIX.REAL)
EE(ultotal)
logul<- log10(ultotal+1)
logul

library(lme4)
mn<-lmer(logul~ 1 + (1|Especie), na.action = na.omit)
m1<-lmer(logul~ polinizador +  (1| Especie), na.action= na.omit, contrasts=T)
library(nlme)
mn<-lme(logul~ 1, random= ~1|Especie, na.action = na.omit, weights = varPower())
m1<-lme(logul~ polinizador, random = ~1| Especie, na.action= na.omit, contrasts=T, weights = varPower())
#m1<-lme(ultotal~ fase, random= ~fase| Planta, na.action= na.omit) no son normales
#m2<-lmer(ultotal~ fase +  (1| fase/Planta), na.action= na.omit) #fase/Planta: model is nearly unidentifiable
anova(m1,mn)#para lmer
plot(m1)
anova(m1)
anova(mn)
summary(m1)
summary(mn)
contrasts(polinizador)
plot(ultotal ~ polinizador)
p1<-c(1,0,0,0,0) #abeja
p2<-c(1,1,0,0,0) #abeja*
p3<-c(1,0,1,0,0) #colibri
p4<-c(1,0,0,1,0) #mariposa
p5<-c(1,0,0,0,1) #mixto
ps<-rbind(p1,p2,p3,p4,p5)
contrasts(polinizador)
p1_p2<-p1-p2
p1_p3<-p1-p3
p1_p4<-p1-p4
p1_p5<-p1-p5
p2_p3<-p2-p3
p2_p4<-p2-p4
p2_p5<-p2-p5
p3_p4<-p3-p4
p3_p5<-p3-p5
p4_p5<-p4-p5
p_s<-rbind(p1_p2, p1_p3,p1_p4,p1_p5,p2_p3,p2_p4,p2_p5,p3_p4,p3_p5,p4_p5)
library(multcomp)
summary(glht(m1, p_s))#diferencias entre tratamientos (vector s_s), cuál tratamiento difiere de cero (vector ss)
summary(glht(m1, linfct = mcp (polinizador = "Tukey")))
#fit<-fitted.values(m1)
#residuo<-resid(m1)
#plot(fit,residuo)

#cambiar en el modelo la variable dependiente: Vol, mg, Brix
day<- as.factor(dia)
plot(ultotal~day, xlab= "Anthesis day", ylab="Volume (ul)")
MEDIA<-tapply(ultotal,day,mean, na.rm=T)
MEDIA
summary.data.frame(datos)
dim.data.frame(datos)
# restar numero de columnas menos na's del factor por cada especie
EE<-function(x){sqrt(var(x,na.rm=T)/ 5)}# cambiar el denominador cuando haya NA's por factor
EE<-function(x){sqrt(var(x,na.rm=T)/length(x))} #para vol
SE<-tapply(ultotal, day, EE)
barplot.error(MEDIA, SE, main= "A. patens", xlab= "Anthesis day", ylab="Volume (ul)", ylim=c(0,10), axes=T , cex.main= 1.5, cex.lab =1.5, cex.axis=1.5, cex=1.5, axis.lty=1)# antirrhina (lim 10), flava (1), patens (10)
