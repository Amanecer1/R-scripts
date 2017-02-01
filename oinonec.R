data<-read.table("c:\\Users\\Erandi\\Documents\\Necoino10.csv",header=T,sep=",")# todo
data<-read.table("c:\\Users\\Erandi\\Documents\\123oino.csv",header=T,sep=",")

data<-read.table("c:\\Users\\Erandi\\Documents\\oinosum.csv",header=T,sep=",")
attach (data)
names(data)
data
data2<-subset(data, Fase=="pistilate") #cambiar las fases para hacer comparación por fase
data2<-subset(data, trat !="sum") # comparación entre fases
attach(data2)
detach(data)
rm(data)
data2

source("c:\\Users\\ERANDI\\Google Drive\\Clase R\\FUNCIONES.r")
EE<-function(x){sqrt(var(x,na.rm=T)/length(x))}

tapply(vol,list(trat,Fase), mean,na.rm=T)
tapply(azu,list(trat,Fase), mean,na.rm=T)
tapply(vol,list(trat,Fase), EE)
tapply(azu,list(trat,Fase), EE)
MM<-lme(vol+azu ~ trat, random=~1|Fase/Planta, na.action=na.omit)

MM<-lme(vol+azu ~ trat + Fase, random=~1|Planta, na.action=na.omit)
summary(MM)
anova(MM, type="marginal")
plot(MM)

M<-lme(vol + azu ~ trat, random=~1|Planta, na.action=na.omit)
summary(M)
anova(M, type="marginal")
plot(M1)
MN<-lme(vol~ 1, random=~1|Planta)
anova(MN,M1)
M1<-lme(azu ~ trat, random=~1|Planta, na.action=na.omit)#cambiar para cada variable
M1<-lme(azu ~ Fase, random=~1|Planta, na.action=na.omit)#cambiar para cada variable
summary(M1)
anova(M1)
plot(M1)
residuos<-resid(M1)
ks.test(resid(M1),"pnorm")
qqnorm(resid(M1))
qqline(residuos)
library(gmodels)
cl<-c(1,0)# o pistilada
sm<-c(1,1) #o estaminada
tt<-rbind(cl,sm)
tc_s<-cl-sm
estimable (M1,tt)
estimable (M1,tc_s)
MEDIA <- estimable(M, tt)[, 1]
SE <- estimable(M, tt)[, 2]

lM1<-lmer(azu ~ trat + (1|Planta),na.action=na.omit)
lM2<-glmer(azu ~ Fase + (1|Planta),na.action=na.omit, family=poisson, (link ="log"))
summary(lM2)
anova(lM2)
plot(lM2)
residuos<-resid(lM2)
ks.test(resid(lM2),"pnorm")
qqnorm(resid(M1))
qqline(residuos)

M2<-glm(vol~trat, random)

MX<-lme(Vol+mg ~ Trat*Dia, random=~1|Planta)
summary(MX)
anova(MX, type="marginal")

#fases por tratamiento
par(mfrow=c(1,1),mar=c(5,4,4,4))#abajo, izquierda, arriba, derecha
MEDIA<-tapply(vol,list(Fase,trat),mean, na.rm=T)
SE<-tapply(vol,list(Fase,trat),EE)# para datos crudos
barplot.error(MEDIA,SE,ylab="Volume (microliters)", names.arg=c("Pistilate","Staminate"),col.b=c("black","white"),cex.main=1, ylim=c(0,300) )
legend("topright","10,5",levels(Trat),title="Trat",col= c("black","white"),bty="n", fill=c("black","white"), border="black")

par(mfrow=c(1,1),mar=c(5,4,4,4))
MEDIA1<-tapply(azu,list(Fase,trat),mean, na.rm=T)
SE1<-tapply(azu,list(Fase,trat),EE)
barplot.error(MEDIA1,SE1,ylab="Sugar (miligrams)",names.arg=c("Pistilate","Staminate"),col.b=c("black","white"),cex.main=1, ylim=c(0,150))																																																																																	
legend("topright","10,5",levels(Trat),title="Trat",col= c("black","white"),bty="n", fill=c("black","white"), border="black")





par(mfrow=c(1,1),mar=c(2,4,2,2))
MEDIA<-tapply(vol,list(trat,Fase),mean, na.rm=T)
SE<-tapply(vol,list(trat,Fase),EE)
barplot.error(MEDIA,SE,main="D. oinochrophylla",ylab="Volume (microliters)",xlab="Treatment",names.arg=c("Control","Sum"),col.b=(c("black","white")),cex.main=0.75, ylim=c(0,300) )
legend("topright","10,5",levels(Fase),title="Phase",col=c("gray","gray50"),bty="n",pch=15)

MEDIA1<-tapply(azu,list(trat,Fase),mean,na.rm=T)
SE1<-tapply(azu,list(trat,Fase),EE)
barplot.error(MEDIA1,SE1,main="D. oinochrophylla",ylab="Sugar (miligrames)",xlab="Treatment",names.arg=c("Control","Sum"),col.b=(c("gray","gray50")),cex.main=0.75,ylim=c(0,130) )
legend("topright","10,5",levels(Fase),title="Phase",col=c("gray","gray50"),bty="n",pch=19)
