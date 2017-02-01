data<-read.delim("clipboard", header= T, sep="\t")
data<-read.table("clipboard", header= T, sep="\t")#usar éste
class(data)
attach(data)
names(data)
levels(PFG)
polis<-table(data$PFG)

C_P<-table(PFG, visitor.spp, dnn=c("PFG","Corolla.Color"))#cambiar a especies o cáliz
C_P<-table(PFG, reward, dnn=c("PFG","Scoro.f"))# cambiar por corola o recompensa
C_P<-table(PFG, MPn.spp, dnn=c("PFG","Scoro.f"))# cambiar por MPn.spp
C_P<-table(PFG, ul.day, dnn=c("PFG","Nectar"))
tapply(AFI,corolla.form, mean, na.rm=T)#para autocompatibilidad por polinizador
tapply(ul.day,PFG, mean, na.rm=T)
C_P<-chisq.test(C_P)#para caracteristicas florales y polinizadores
C_P
C_P$observed
Es<-C_P$expected
C_P$residuals
mosaicplot(t(Es), shade = TRUE)

#anova
m<-lm(AFI~PFG)# cambiar otros factores: corolla.form y variable dependiente por SCI
anova(m)
summary(m)
plot(m) #no cumple con los supuestos de normalidad
#-----------------
mn<-glm(AFI~ 1 + 1|Species)# cambiar otros factores: corolla.form y variable dependiente por SCI
m<-glm(AFI~ PFG, family= Gamma (link = inverse) )
anova(m)
summary(m)
plot(m)


#paquete multcomp
glht(m,tt) #matriz de dummies tt

#otras formas de hacer la tabla
table(dat$PFG, dat$corolla.form, dnn=c("PFG","corolla")) #cambiar corola dependiendo de la base de datos y de la característica floral
table(dat[,c("PFG","Corola")])
countdf <- as.data.frame(table(dat), stringsAsFactors=TRUE)

#para hacer un prueba de Chi cuadrada a mano, a partir de los datos de la tabla de contigencia de corolas y polinizadores
write.table(C_P,"C:\\Users\\Erandi\\Documents\\PVisit.csv",sep = ",", row.names = T, col.names = NA)
write.table(C_P,"C:\\Users\\Erandi\\Documents\\freqR.csv",sep = ",", row.names = T, col.names = NA)

A1<-read.table("C:\\Users\\Erandi\\Documents\\freqP.csv",sep = ",", header= T, na.strings="NA")
attach(A1)

a1<-sum(bat) # cambiar para cada polinizador
a2<-sum(Bee)
a3<-sum(bird)
a4<-sum(hbird)
a5<-sum(ltfly)
a6<-sum(mixed)
as<-sum(a1,a2,a3,a4,a5,a6)
c1<-sum(A1[1,2:7]) # para sumas por filas
c2<-sum(A1[2,2:7])
c3<-sum(A1[3,2:7])
c4<-sum(A1[4,2:7])
c5<-sum(A1[5,2:7])
c6<-sum(A1[6,2:7])
c7<-sum(A1[7,2:7])
c8<-sum(A1[8,2:7])
cs<-sum(c1,c2,c3,c4,c5,c6,c7,c8)
