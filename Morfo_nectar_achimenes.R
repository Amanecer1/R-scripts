datos<-read.delim("clipboard", header= T, sep="\t")# de la base morfo_inv2016
datos
dat<-data.frame(datos)
rm(datos)
names(dat)
attach(dat)

plot(ul, loncorola)
lonen<-cor.test(ul,lonentrada, method="pearson")
lonen
plot(ul, lonentrada)
boxplot(ul~loncorola)
uno<-princomp(~., data=dat, cor=T, na.action=na.omit, scores=T)#sólo los valores de morfología
summary(uno)
uno$sdev
loadings(uno)
cp1<-cbind(lonentrada, anentrada, longpet, anpet, anfrente)
cprin1<-cor(ul, cp1)
shapiro.test(baseesp)

cor.test(ul~ cp1, method="pearson")
cor.test(ul,loncorola, method="pearson")#antirrhina
cor.test(ul,ancorola, method="pearson")
cor.test(ul,lonentrada, method="pearson")#antirrhina
cor.test(ul,anentrada, method="pearson")
cor.test(ul,longfrente, method="pearson") #p= 0.0445 (flava)
cor.test(ul,anfrente, method="pearson")
cor.test(ul,longpet, method="pearson")
cor.test(ul,anpet, method="pearson")#antirrhina
cor.test(ul, baseesp, method="pearson")



