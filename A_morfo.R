source("c:\\Users\\ERANDI RA\\Google Drive\\Clase R\\FUNCIONES.r")# estadística descriptiva
data<-read.delim("clipboard", header= T, sep="\t")# con cada base de datos de Achimenes
data<-read.table("clipboard", header= T, sep="\t")
class(data)
dim(data)
attach(data)
data
names(data)

source("c:\\Users\\Erandi\\Google Drive\\Clase R\\FUNCIONES.r")
dt<-log(data[, 2:11]) #todas 3:11 y medias x especie 3:11
dt
shapiro.test(dt$ancorola)
skewness(dt$ancorola)
kurtosis(dt$ancorola)
qqnorm(dt$ancorola)
qqline(dt$ancorola,col="red")

dat<-data[, 2:12] #para la base de datos con especie, sino no funciona "autoplot",[,1:8]Tzararacua
dat
uno<- princomp (formula = ~., data = dat, cor = TRUE, na.action=na.exclude, scores=T)# menor precisión numérica, este usa la descomposición por valor espectral++++++++ usé este
uno<-prcomp( na.omit(dat), center = TRUE, scale = TRUE)#varianzas calculadas con N-1, pero al calcular el índice da lo mismo/ print (uno) para ver los loadings como rotation
predict(uno)
uno
summary(uno)
uno$sdev
loadings(uno, cutoff = 0.01, sort =T)
eig<-uno$sdev*uno$sdev #da lo mismo que eigenval de Vegan o (uno$sdev)^2
eig

variance <- eig*100/sum(eig)
variance
uno$scores
biplot(uno, arrow.len =0 )
library(ggplot2)
library(ggfortify) # ver página que explica ggfortify
autoplot(uno)
autoplot(uno, data =data, colour ='polinizador', label= F, loadings = F, loadings.colour = 'black', loadings.label = F)


library (Hmisc)
dat1<-as.matrix(dat)
dim(dat1)
#x<-dat[1:30, ]
#y<-dat[ , 1:6]
rcorr(dat1, type ="pearson") #checar que den las mismas correlaciones que con la función cor
#------------o uno por uno para significancia de las correlaciones
mcov<-cov(data, method="pearson")
mcor<-cor(dat1, method="pearson")#hacer matrices por especie
mcor
lower.tri(mcor)
upper<-mcor
upper[upper.tri(mcor)]<-""
upper<-as.data.frame(upper)
upper

names(data)
cor.test(data$LCE, data$AA, method="pearson") #calcular un par de variables a la vez
#mi pequeño análisis de autovalores
eigen(mcor)
autos<-eigen(mcor)$values#también así se obtienen autovalores del PCA
autos
sum(autos)# varianza total

# para hacer las matrices de distancias entre de las especies
vcov<- cov(dat)
vcov
medias<-colMeans(dat)
medias
mah<- mahalanobis(dat, medias, vcov) # distancias al cuadrado
mah
raiz<-sqrt(mah)
as.matrix(raiz)
mahal<-as.matrix(mah)
mahal
library(StatMatch)
md<-mahalanobis.dist(dat, vc=NULL) 
md
msrt<-sqrt(md)
msrt
# cálculo de la gran media de cada variable morfológica con las medias de cada especie
#c cálculo de la matriz de covarianza a partir de la media de cada variable por especie
datm<-as.matrix(dat)
clus<-kmeans (datm, 3)
clus
table(clus,3)

write.table(mahal, "C:\\Users\\Erandi\\Google Drive\\ Excel\\Achimenes\\dist_patens.csv",sep = ",", row.names = T, col.names = NA)  # cambiar el nombre del archivo por especie
write.table(mahal, "C:\\Users\\Erandi\\documentos\\dist_patens.csv",sep = ",", row.names = T, col.names = NA) 


# INT
length(data) #número de factores
cf<-(length(data)-1)/uno$n.obs #factor de corrección
cf
IRosas<-(var(eig)-cf)/length(data)*100 #R calcula la varianza de la muestra, y quita un grado de libertad, divide entre n-1, para calcular la varianza de los autovalores hay que usar N, no N-1,porque no es poblacional ver Pavlicev et al 2009. Fórmula,según Rosas-Guerrero
IRosas #infla los valores por usar el denominador poblacional en la función var

# comparar con
library (PHENIX)
cor.par(data)
pint(data) # que según mis cálculos (abajo) sólo difiere en los decimales porque redondea
pint.boot(data)

IPhen<-var(eig)*(length (data)-1)*(1/length(data))# integración fenotípica según Wagner, dada como la varianza de los autovalores, con el denominador N------Es igual al calculado en PHENIX
IPhen
IPHEN1<-var(eig)*(sum(eig)-1)*(1/sum(eig))# es igual a Wagner e igual a mi otra ecuación con length
IPHEN1
IPhco<-(var(eig)*(length (data)-1)*(1/length(data)))-cf # INT corregido
IPhco







#intento de crear mi matriz vcov a mano
length(dat) #para conocer el tamaño de la matrix, descomponer el número con row
row(mdat)# para conocer el número de observaciones
m1<-cbind(rep(1, times= 30)) #cambiar el número de veces, dependiendo del número de observaciones por carácter medido
m1
class(m1)
m2<-t(m1)
m2
unos<-outer (m1,m2)

vcm<-mdat-(unos*mdat) 1/30

#checar este script para armar la matriz de varcov
covr <- function(r, i, j, k, l, n){
  if(i==k && j==l)
    return((1-r[i,j]^2)^2/n)
  ( 0.5 * r[i,j]*r[k,l]*(r[i,k]^2 + r[i,l]^2 + r[j,k]^2 + r[j,l]^2) +
    r[i,k]*r[j,l] + r[i,l]*r[j,k] - (r[i,j]*r[i,k]*r[i,l] +
                                       r[j,i]*r[j,k]*r[j,l] + r[k,i]*r[k,j]*r[k,l] + r[l,i]*r[l,j]*r[l,k]) )/n
}
vcovr <- function(r, n){
  p <- combn(nrow(r), 2)
  q <- seq(ncol(p))
  outer(q, q, Vectorize(function(x,y) covr(r, p[1,x], p[2,x], p[1,y], p[2,y], n)))
}
vcovr(correlation_matrix_input, 66)
vcovr(mcor, 30)

