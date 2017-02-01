#análisis para conocer diferencias entre tratamientos de polinización

data<-read.table("c:\\Users\\Erandi RA\\Documents\\reprotodo.csv",header=T,sep=",",na.strings="NA")
data<-read.table("c:\\Users\\Erandi RA\\Documents\\reproductiva.csv",header=T,sep=",")#sin valores ausentes
data<-read.table("c:\\Users\\Erandi RA\\Documents\\reproductiva2.csv",header=T,sep=",") # para gráficaen orden creciente
attach(data)
data
data2<-subset(data, Especie=="D.strigosa")
attach(data2)
detach(data)
rm(data)
data2
names(data2)
detach(data2)
rm(data2)
data<-read.table("c:\\Users\\Erandi\\Documents\\reprotodo.csv",header=T,sep=",")
data<-read.table("c:\\Users\\Erandi\\Documents\\reproductiva.csv",header=T,sep=",")
attach(data)
data
data3<-subset(data, Especie=="D.oinochrophylla")
attach(data3)
detach(data)
rm(data)
data3
names(data3)
class(flores)

z<-asin(sqrt(Fruit.set))
trat<-as.factor(Tratamiento)
EE<-function(x){sqrt(var(x,na.rm=T)/length(x))}
DE<-function(x){sqrt(var(x,na.rm=T))}
source("c:\\Users\\ERANDI RA\\Documents\\Clase R\\FUNCIONES.r")

tapply(z,trat,mean,na.rm=T)
tapply(fruitset,trat,mean,na.rm=T)
tapply(fruitset,trat,length)
vv<-cbind(tapply(fruitset,trat,var, na.rm=T))
tapply(fruitset,trat,DE)
tapply(fruitset,trat,EE)

data<-read.table("C:\\Users\\Erandi RA\\Documents\\srepro.csv", header=T, sep=",")
data<-read.table("C:\\Users\\Erandi RA\\Documents\\orepro.csv", header=T, sep=",")
# estas bases son para binomial
#*********************************tienen flor, fruto y fruitset:
data<-read.table("C:\\Users\\Erandi RA\\Documents\\DS_reprobis.csv", header=T, sep=",")
data<-read.table("C:\\Users\\Erandi RA\\Documents\\DO_reprobis.csv", header=T, sep=",")
attach(data)
data
str(data)
names(data)
dat1<-subset(data, trat != "apomixis" & trat != "natural")
dat1<-subset(data, trat != "apomixis")
attach(dat1)
detach(data)
rm(data)
#paquete lme4
MM<-lmer(z ~ Tratamiento + (1|Planta),na.action=na.omit, contrasts=T, REML=T)
M1<-lmer(fruitset ~ trat +(1|planta))
MN<-lmer(fruitset ~ 1 + 1| planta)

plot(M2)
anova(M2,MN)
anova(M1)
plot(M1)
summary(M1)

Y<-cbind(flores, frutos)# para binomial
ESTOS <- which(Y[, 1] > 0 & Y[, 2] > 0)
Y <- Y[ESTOS, ]
ttt<-trat[ESTOS]
ppp<-planta[ESTOS]
M2<-glmer (Y ~ ttt + (1|ppp),na.action=na.omit, family=binomial (link="logit"), contrasts=T)
plot(M2)
m2<-glm(Y ~ trat + planta, family=binomial (link="logit"))

#******************************este es el bueno*****************************
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

#paquete nlme
M<-lme(fruitset~trat,random=~1|planta,na.action=na.omit, weights=varPower())
M<-lme(Fruit.set~Tratamiento,random=~1|Planta,na.action=na.omit)
, weights=varPower())
anova(M2,type="marginal")
summary(M)
BETAS <- summary(M)$coefficients$fixed
#coeficientes; base de datos reproductiva.csv /srepro.csv, orepro.csv
t1<-c(1,0,0,0,0) #handcross /apomixis
t2<-c(1,1,0,0,0) #handself /autonoma
t3<-c(1,0,1,0,0) #autonomous /handcross
t4<-c(1,0,0,1,0) #apomixis /handself
t5<-c(1,0,0,0,1) #natural /natural
tt<-rbind(t1,t2,t3,t4,t5)

BETAS <- summary(M2)$coefficients[,1]
BETAS
TABLA <- cbind(BETAS,t2 , BETAS * t2)
sum(TABLA[, 3])

t1_2<-t1-t2
t1_3<-t1-t3
t1_4<-t1-t4
t1_5<-t1-t5
t2_3<-t2-t3
t2_4<-t2-t4
t2_5<-t2-t5
t3_4<-t3-t4
t3_5<-t3-t5
t4_5<-t4-t5
ts<-rbind(t1_2,t1_3,t1_4,t1_5,t2_3,t2_4,t2_5,t3_4,t3_5,t4_5)
library(gmodels)
estimable (M2,ts)
lsmeans(M2,"ttt")


residuos<-resid(M2)
plot(M)
plot(residuos)
plot(M1,resid(.,scaled=T) ~ trat|Planta, abline=0)
qqplot(residuos)
qqplot((resid(M1))
qqnorm(M1$residuals)
qqnorm(resid(M2))
qqline(residuos)
hist(M$residuals)
hist(resid(M2))
ks.test(M$residuals,"pnorm")
ks.test(residuos,"pnorm")

lsmeans(M2,"Tratamiento")


M1<-lme(z~Tratamiento,random=~1|Planta,na.action=na.omit,weights=varPower()) # Tratamiento es "integer", no tiene sentido así
anova(M1,type="marginal") #
summary(M1)
plot(M1)

M<-lme(z~trat*Planta,random=~1|Planta,na.action=na.omit,weights=varPower())
anova(M,type="marginal") # error
summary(M)
plot(M)

M<-lme(z~trat,random=~1|trat/Planta,na.action=na.omit,weights=varPower())
anova(M,type="marginal") #Error
summary(M)
plot(M)

M<-lme(z~trat,random=~1|Planta/trat,na.action=na.omit,weights=varPower()) #error
anova(M,type="marginal") 
summary(M)
plot(M)

M<-lme(z~trat,random=~trat|Planta,na.action=na.omit,weights=varPower()) #
anova(M,type="marginal")
summary(M)
plot(M)
#---------------------------------------------------------------

tapply(Fruit.set,Tratamiento,mean, na.rm=T)
MEAN<-tapply(Fruit.set,Tratamiento,mean, na.rm=T)
SE<-tapply(Fruit.set,Tratamiento,EE)
par(mfrow= c(1,1), mar=c(2,5,2,1))
barplot.error(MEAN,SE, main="Drymonia strigosa",ylab="Fruit set (fruits/flowers)",xlab="Treatment",names.arg=c("Apomixis","Hand cross","Hand self","Autonomous","Natural"),col.b="gray25", density=25,angle=c(90,130,60,20,180),cex.axis=0.9,cex.lab=1,ylim=c(0,1),cex=0.9, cex.main=1)
barplot.error(MEAN,SE,ylab="Fruit set (frutos/flores)",xlab="Tratamiento",names.arg=c("Exocruza","Geitonogamia","Autónoma","Apomixis","Natural"),col.b=c("palevioletred", "salmon", "tomato","lavender","violetred"),cex.axis=0.9,cex.lab=1,ylim=c(0,1),cex.main=1)
			 
			 #en orden
barplot.error(MEAN,SE,ylab="Fruit set (frutos/flores)",xlab="Tratamiento",names.arg=c("Apomixis","Exocruza","Geitonogamia","Autónoma","Natural"),col.b=c("lavender","palevioletred", "salmon", "tomato","violetred"),cex.axis=0.9,cex.lab=1,ylim=c(0,1),cex.main=1)
       
       
       
par(mfrow= c(1,1), mar=c(2,5,2,1))
MEAN<-tapply(Fruit.set,Tratamiento,mean, na.rm=T)
EE<-function(x){sqrt(var(x,na.rm=T)/length(x))}
SE<-tapply(Fruit.set,Tratamiento,EE)
barplot.error(MEAN,SE, main="Drymonia oinochrophylla",ylab="Fruit set (fruits/flowers)",xlab="Treatment",names.arg=c("Apomixis","Hand cross","Hand self","Autonomous","Natural"),col.b="gray56",density=25,angle=c(90,130,60,20,180),cex.axis=0.9,cex.lab=1, ylim=c(0,1),cex=0.9,cex.main=1)
barplot.error(MEAN,SE,ylab="Fruit set (frutos/flores)",xlab="Tratamiento",names.arg=c("Exocruza","Geitonogamia","Autónoma","Apomixis","Natural"),col.b=c("palevioletred", "salmon", "tomato","lavender","violetred"),cex.axis=0.9,cex.lab=1,ylim=c(0,1),cex.main=1)


z<-asin(sqrt(Fruit.set))
trat<-as.factor(Tratamiento)
EE<-function(x){sqrt(var(x,na.rm=T)/length(x))}
source("c:\\Users\\ERANDI\\Documents\\Clase R\\FUNCIONES.r")

