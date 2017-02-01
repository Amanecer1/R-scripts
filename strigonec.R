data<-read.table("c:\\Users\\Erandi\\Documents\\Necoino10.csv",header=T,sep=",")# todo
data<-read.table("c:\\Users\\Erandi\\Documents\\123oino.csv",header=T,sep=",")

controles<-read.delim("clipboard", header= T, sep="\t")# de hoja de nectartodo2 ctrl para strigosa
attach(controles)

data<-read.table("c:\\Users\\Erandi\\Documents\\strigorepo.csv",header=T,sep=",")
attach(data)
names(data)
data2<-subset(data, fase=="staminate") #cambiar las fases para hacer comparación por fase
attach(data2)
detach(data)
rm(data)
data2

data
source("c:\\Users\\ERANDI RA\\Google Drive\\Clase R\\FUNCIONES.r")
EE<-function(x){sqrt(var(x,na.rm=T)/length(x))}
tapply(Vol,list(Trat,fase), mean,na.rm=T)
tapply(mg,list(Trat,fase), mean,na.rm=T)
tapply(Vol,list(Trat,fase), EE)
tapply(mg,list(Trat,fase), EE)

#para BRIX de controles
cientos<-cien
MN<-glmer (bx ~ 1+ (1|Planta),na.action=na.omit, contrasts=T, weights= cientos, family=binomial (link="logit"))
M2<-glmer (bx ~ Fase + (1|Planta),na.action=na.omit, contrasts=T, weights= cientos, family=binomial (link="logit")) 
anova(MN,M2)
summary(M2, test ="Chi")
plot(M2)

#para controles
plot(controles$mg~controles$Fase, xlab= "Sexual phase", ylab="Volume (ul)", names.arg=c("Pistilate", "Staminate"))
plot(controles$mg~controles$Fase, xlab= "Sexual phase", ylab="Volume (ul)", names.arg=c("Pistilate", "Staminate"))

stat<-function(data, indices){
	t.test<- t.test (Vol~Fase, data)$stat
	t.test
}

#cambiar en el modelo la variable dependiente: Vol, mg, Brix

rand.gen<-function(data, mle){
	out<-data
	out$Vol<- sample(out$Vol, replace = F)
	out
}

ctrl.boot<-boot(controles, stat, R= 5000, sim ="parametric", ran.gen= rand.gen)# ran.gen es sólo para sim = parametric
summary(ctrl.boot)
print(ctrl.boot)
plot(ctrl.boot)
boot.ci(ctrl.boot, type = "all")

#***************************************para las otras bases de datos
class(Trat) #para "controles" no hay Trat

tapply(Vol,list(Trat,fase), mean,na.rm=T)
tapply(mg,list(Trat,fase), mean,na.rm=T)

MM<-lme(Vol+mg ~ Trat, random=~1|fase/Planta, na.action=na.omit)
MM<-lme(Vol+mg ~ Trat + fase, random=~1|Planta, na.action=na.omit)
summary(MM)
anova(MM, type="marginal")
plot(MM)

MN<-lme(Vol+azu~Trat, random=~1|Planta)
anova(MN)
plot(MN)
ks.test(resid(M1),"pnorm")
qqnorm(resid(M1))
qqline(residuos)
M<-lme(Vol~Trat, random=~1|Planta)
summary(M)
anova(M, type="marginal")
plot(M)
M1<-lme(Azu ~Trat, random=~1|Planta)
summary(M1)
anova(M1)
plot(M1)

# para controles, comparación por fase
#con paquete nlme
M<-lme(Vol~Fase, random=~1|Planta)
summary(M)
anova(M, type="marginal")
plot(M)
M1<-lme(Azu ~Fase, random=~1|Planta)
summary(M1)
anova(M1, type="marginal")
plot(M1)


par(mfrow=c(1,1),mar=c(2,4,2,2))
MEDIA<-tapply(Vol,list(Trat,fase),mean, na.rm=T)
SE<-tapply(Vol,list(Trat,fase),EE)
barplot.error(MEDIA,SE,main="D. strigosa",ylab="Volume (microliters)",xlab="Treatment",names.arg=c("Control","Sum"),col.b=(c("gray","gray50")),cex.main=0.75, ylim=c(0,300) )
legend("topright","10,5",levels(fase),title="Phase",col=c("gray","gray50"),bty="n",pch=19)

MEDIA1<-tapply(mg,list(Trat,fase),mean,na.rm=T)
SE1<-tapply(mg,list(Trat,fase),EE)
barplot.error(MEDIA1,SE1,main="D. strigosa",ylab="Sugar (miligrames)",xlab="Treatment",names.arg=c("Control","Sum"),col.b=(c("gray","gray50")),cex.main=0.75,ylim=c(0,130) )
legend("topright","10,5",levels(fase),title="Phase",col=c("gray","gray50"),bty="n",pch=19)

M1<-lmer(azu ~ trat + (1|planta),na.action=na.omit)