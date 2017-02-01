data<-read.table("c:\\Users\\Erandi\\Documents\\conc_oino_ctrl.csv",header=T,sep=",")
attach(data)
data
names(data)

controles<-read.delim("clipboard", header= T, sep="\t")# para strigosa: hoja de nectartodo2 ctrl para strigosa. Para oinochrophylla Nec_oino10ul

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


source("c:\\Users\\ERANDI\\Google Drive\\Clase R\\FUNCIONES.r")
EE<-function(x){sqrt(var(x,na.rm=T)/length(x))}

tapply(ml,Fase, mean,na.rm=T)
tapply(Brix,Fase, mean,na.rm=T)# cambiar para strigosa a Brix
mn<-lme(Brix_ctrl~ 1, random = ~1|Planta, na.action=na.omit)
plot(mn)
m<-lme(Brix_ctrl~ Fase, random = ~1|Planta, na.action= na.omit)
m<-lme(Vol~ Fase, random = ~1|Planta, na.action= na.omit)# para strigosa
plot(m)
anova(m)
summary(m)
anova(mn,m)
res<-resid(m)
ks.test(res, "pnorm")
library(gmodels)
masc<-c(1,0)# o pistilada
fem<-c(1,1) #o estaminada
tt<-rbind(masc,fem)
tc_s<-cl-sm
estimable (m,tt)
estimable (M1,tc_s)
MEDIA <- estimable(M, tt)[, 1]
SE <- estimable(M, tt)[, 2]

attach(controles)
plot(Vol~Fase)
plot(Brix~Fase)
plot(mg~Fase)
#######################################################
stat<-function(data, indices){
	t.test<- t.test (Brix~Fase, data, na.action = na.omit)$stat
	t.test
}   

#cambiar en el modelo la variable dependiente: Vol, mg, Brix, para oinochrophylla quitar la fila con NA's

rand.gen<-function(data, mle){
	out<-data
	out$Brix<- sample(out$Brix, replace = F)
	out
}

ctrl.boot<-boot(controles, stat, R= 5000, sim ="parametric", ran.gen= rand.gen)
summary(ctrl.boot)
print(ctrl.boot)
plot(ctrl.boot)

boot.ci(ctrl.boot, type ="basic") #en escala original























