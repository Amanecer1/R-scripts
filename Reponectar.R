#respuesta a la remoción sum vs ctrl

data<-read.table("c:\\Users\\Erandi\\Documents\\necmodf.csv",header=T,sep=",")
data<-read.table("c:\\Users\\Erandi\\Documents\\necmodfbis.csv",header=T,sep=",")#con "staminate" y "pistilate"
attach(data)
data
data2<-subset(data, Especie=="D. strigosa" & Dia== "pistilate") #cambiar a día 2 o "pistilate"
data2<-subset(data, Especie=="D. strigosa" & Trat != "sum")
data2<-subset(data, Especie=="D. strigosa")
attach(data2)
detach(data)
rm(data)
data2
dia<-as.factor(Dia)
detach(data2)
rm(data2)
data<-read.table("c:\\Users\\Erandi\\Documents\\necmodf.csv",header=T,sep=",")# para volver a cargar la base completa de néctar volver a hacer un subset
attach(data)
data
detach(data)
rm(data)
data3
rm(data2)

MN<-lme(Vol~ 1, random=~1|Planta)
M<-lme(Vol~Trat, random=~1|Planta)
MM<-lme(Vol+mg ~ Trat, random=~1|Planta)
summary(MM)
anova(MM, type="marginal")
plot(MM)
anova(MN,M)
M<-lme(Vol~Trat, random=~1|Planta) #cambiar variable dep
summary(M)
anova(M, type="marginal")
plot(M)
M<-lme(mg~dia, random=~1|Planta)#cambiar variable dep
summary(M1)
anova(M1)
plot(M)
summary(M)
resid<-residuals(M)
ks.test(M$resid,"pnorm")
shapiro.test(M$resid)
library(gmodels)
cl<-c(1,0) # o dia 1
sm<-c(1,1) # dia 2
tt<-rbind(cl,sm)
tc_s<-cl-sm
estimable (M,tt)
MEDIA <- estimable(M, tt)[, 1]
SE <- estimable(M, tt)[, 2]
barplot.error #abajo, para graficar los valores calculados del modelo



MX<-lme(Vol+mg ~ Trat*Dia, random=~1|Planta)
summary(MX)
anova(MX, type="marginal")

data2<-subset(data, Especie=="D. strigosa" & Trat !="sum") #para comparar entre fases


MF<-lme(Vol+mg ~ dia, random=~1|Planta, na.action=na.omit)
summary(MF)
anova(MF, type="marginal")
plot(MF)
MF<-lme(mg ~ dia, random=~1|Planta,na.action=na.omit, weights=varPower())

source("c:\\Users\\ERANDI\\Documents\\Clase R\\FUNCIONES.r")
EE<-function(x){sqrt(var(x,na.rm=T)/length(x))}
names(data2)

#para comparar entre tratamientos por sexo
par(mfrow=c(1,1),mar=c(2,4,2,2))
MEDIA<-tapply(Vol,list(Trat,Dia),mean, na.rm=T)
SE<-tapply(Vol,list(Trat,Dia),EE)
barplot.error(MEDIA,SE,main="Total volume",ylab="Volume (microliters)",xlab="Treatment",names.arg=c("Control","Sum"),col.b=(c("gray","gray50")),cex.main=1, ylim=c(0,100) )
legend("topright","10,5",levels(Dia),title="Phase",col=c("gray","gray50"),bty="n",pch=19)

MEDIA1<-tapply(mg,list(Trat,Dia),mean,na.rm=T)
SE1<-tapply(mg,list(Trat,Dia),EE)
barplot.error(MEDIA1,SE1,main="Total sugar",ylab="Sugar (miligrames)",xlab="Treatment",names.arg=c("Control","Sum"),col.b=(c("gray","gray50")),cex.main=1,ylim=c(0,40) )
legend("topright","10,5",levels(Dia),title="Phase",col=c("gray","gray50"),bty="n",pch=19)

#********************para comparar entre sexos por tratamiento*****************************
par(mfrow=c(1,1),mar=c(5,4,4,4))#abajo, izquierda, arriba, derecha
MEDIA<-tapply(Vol,list(Dia,Trat),mean, na.rm=T)
SE<-tapply(Vol,list(Dia,Trat),EE)# para datos crudos


barplot.error(MEDIA,SE,ylab="Volume (microliters)",xlab="Sexual phase", names.arg=c("Pistilate","Staminate"),col.b= c("black","white"),cex.main=1, ylim=c(0,300) )
legend("topright","10,5",levels(Trat),title="Trat",bty="n", col=c("black", "white"), pch=19, fill=c("black","white"), border="black")

par(mfrow=c(1,1),mar=c(5,4,4,4))
MEDIA1<-tapply(mg,list(Dia,Trat),mean, na.rm=T)
SE1<-tapply(mg,list(Dia,Trat),EE)
barplot.error(MEDIA1,SE1,ylab="Sugar (miligrams)",names.arg=c("Pistilate","Staminate"),col.b= c("black","white"), ylim=c(0,150) )
legend("topright","10,5",levels(Trat),title="Trat",col=c("black", "white"),bty="n", pch=19,fill=c("black","white"), border="black")





tapply(Vol,list(Trat,dia),mean,na.rm=T)
tapply(Vol,list(Trat,dia),var, na.rm=T)
tapply (mg,dia,mean.na.rm=T)
tapply(mg,list(Trat,dia),mean, na.rm=T)
tapply(mg,list(Trat,dia),var, na.rm=T)
CV<-function (x){sqrt(var(x,na.rm=T))/mean(x,na.rm=T)}
tapply(mg,list(Trat,dia),CV)
tapply(Vol,list(Trat,dia),CV)
tapply(Vol,list(Trat,dia),length)
tapply(mg,list(Trat,dia),length)
tapply(Vol,list(Trat,dia), EE)
tapply(mg,list(Trat,dia),EE)
