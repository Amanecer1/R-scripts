data<-read.table("c:\\Users\\Erandi\\Documents\\asegstrigosa2.csv",header=T,sep=",")
attach(data)
data
names(data)
source("c:\\Users\\ERANDI\\Documents\\Clase R\\FUNCIONES.r")
v1<-var(fs.emasc)
v2<-var(fs.noemasc)
v2/v1
shapiro.test(fs.emasc)
var.test(fs.emasc, fs.noemasc)
wilcox.test(fs.emasc, fs.noemasc, paired=T, conf.int=T, var.equal=T)
kruskal.test(fs.emasc,fs.noemasc, paired=T, conf.int=T, var.equal=T)
#---------------------------------------------------------------
data<-read.table("c:\\Users\\Erandi\\Documents\\asegreprostrigo.csv",header=T,sep=",")
attach(data)
data
names(data)
z<-(asin(sqrt(fruitset)))
bartlett.test(z~Trat)
wilcox.test(z~Trat, paired=T, var.equal=F)
#nlme/ lme4
ff<-flores
M<- glmer (fruitset ~ Trat + (1|Planta), na.action=na.omit, contrasts=T, weights= ff, family=binomial (link="logit"))
MN<- glmer (fruitset~1 + (1|Planta), na.action=na.omit, contrasts=T, weights= ff, family=binomial (link="logit"))
anova(MN,M)
anova(M)
summary(M)
plot(M)
t1<-c(1,0) #emasc
t2<-c(1,1) 
tt<-rbind(t1,t2)
#multcomp
summary(glht(M, tt))
glht(M,tt)
coef(glht(M,tt))
lsm<-cbind(coef(glht(M,tt)))
lsm
mm<-exp(lsm)/(1+exp(lsm))
mm
summary(glht(M, tt))
#lme4
M<-glmer(fruitset~Trat + (1|Planta), family=poisson(link="log"))
M<-glmer(z~Trat + (1|Planta), family = poisson (link="1/mu^2"))
anova(M)
summary(M)
plot(M)
