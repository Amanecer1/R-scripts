data<-read.delim("clipboard", header= T, sep="\t")# con cada base de datos de Achimenes
dat<-data.frame(data)
attach(dat)
dat
rm(data)
z<-asin(sqrt(fruitset))
trat<-as.factor(trat)
EE<-function(x){sqrt(var(x,na.rm=T)/length(x))}
DE<-function(x){sqrt(var(x,na.rm=T))}
source("c:\\Users\\ERANDI\\Documents\\Clase R\\FUNCIONES.r")

tapply(z,trat,mean,na.rm=T)
tapply(fruitset,trat,mean,na.rm=T)
tapply(fruitset,trat,length)
vv<-cbind(tapply(fruitset,trat,var, na.rm=T))
tapply(fruitset,trat,DE)
tapply(fruitset,trat,EE)

ff<-flores
MN<-glmer (fruitset ~ 1+ (1|planta),na.action=na.omit, contrasts=T, weights= ff, family=binomial (link="logit"))
M2<-glmer (fruitset ~ trat + (1|planta),na.action=na.omit, contrasts=T, weights= ff, family=binomial (link="logit")) #para oinochrophylla advertencias y pide re-escalar los datos
anova(MN,M2)
summary(M2, test ="Chi")

t1<-c(1,0,0,0) #handcross
t2<-c(1,1,0,0) #autonoma
t3<-c(1,0,1,0) #exo
t4<-c(1,0,0,1) #natural
tt<-rbind(t1,t2,t3,t4)
contrasts(trat) # es igual a mi vector tt
# cargar paquete multcomp, ver más abajo para contrastes
summary(glht(M2, tt))#diferencias entre tratamientos (vector ts), cuál tratamiento difiere de cero (vector tt)
glht(M2,tt) #para medias del modelo
coef(glht(M2,tt))
lsm<-cbind(coef(glht(M2,tt)))
mm<-exp(lsm)/(1+exp(lsm))#medias calculadas del modelo
vv<-cbind(tapply(fruitset,trat,var, na.rm=T))
mm*(1-mm)*vv

est<-lsmeans(M2,"ttt")

