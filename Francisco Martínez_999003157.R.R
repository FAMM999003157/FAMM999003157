#examen final 
#Francisco Martínez 999003157
#pregunta 11,12
media <- 173.47
n <-15
desv <- 4
alfa <- 0.05/2
nivelconfianza <- 1- alfa

normal<- qnorm(nivelconfianza,0,1)
normal
#o
normal<- qnorm(alfa,0,1)
normal

error<-  desv/sqrt(n)
error
margen <- abs(normal)*error
margen

limInf<- media - margen
limsup <- media +margen
limInf
limsup

#ejercicio 13
media <- 173.47
n <-15
desv <- 4
alfa <- 0.2/2
nivelconfianza <- 1- alfa

normal<- qnorm(nivelconfianza,0,1)
normal
#o
normal<- qnorm(alfa,0,1)
normal

error<-  desv/sqrt(n)
error
margen <- abs(normal)*error
margen

limInf<- media - margen
limsup <- media +margen
limInf
limsup


#pregunta 14
#respuesta 172.15 , 174.79

#pregunta 15
media <- 173.47
n <-15
desv <- 4
alfa <- 0.05/2
nivelconfianza <- 1- alfa

normal<- qnorm(nivelconfianza,0,1)
normal
#o
normal<- qnorm(alfa,0,1)
normal

error<-  desv/sqrt(n)
error
margen <- abs(normal)*error
margen

limInf<- media - margen
limsup <- media +margen
limInf
limsup

#Pregunta 16,17,18,19
#prueba de hipotesis para valores mayores
#1 h0: MU < 800
#2 h1:  Mu >= 800

#3- Alfa
alfa <- 0.01

n=50
media= 750
sd= 120
mu=800

#4- estadistico
z0<- (media-mu)/(sd/sqrt(n))
z0

#5- valor de la normal al ser mas de 30 elementos en la muestra con alfa = 1%
zAlfa <- qnorm(alfa, 0,1)
zAlfa

#6 analizando el criterio de rechazo. 
z0 <  zAlfa


#pregunta 20,21,22,23,24
#hipotesis de varianza. 
#definimos la hipótesis. 
##1. h0: sigma <= 4
#2  h1: sigma > 4

#3 y 4 se definen las siguientes variables. 
n=24
s2 <- 4.9  #(varianza de la muestra)
sigma20<- 4  #(varianza de la población)
alfa <- 0.05

#5 calculamos el estadistico de pruebas
X2 <- ((n-1)*s2)/sigma20
X2

#6 se calcula chi cuadrada para alfa (ojo, alfa, no alfa/ 2, ya que se está validando una cola)
chi_sd2<- qchisq(1-alfa, n-1)
chi_sd2

#7 verificación
X2> chi_sd2
# 6.075 > 16.9188 (FALSO)
#se acepta la hipotesis nula que indica que el valor de sigma es menor o igual a 40
#no hay suficiente evidencia estadística para inferir que la hipótesis nula es falsa


#pregunta 25,26,27,28
#1 Variables del conjunto Tree $Volume y $height

#Regresión lineal
lm.trees <- lm(Volume~Height, data=trees)
summary (lm.trees)

#2 la formula generada es Girth= -6.18839 +0.25575volume
#3 el intercepto es -6.18839, el R2 es 0.2697 y el R2 ajustado es 0.2445

#4 Regresión lineal entre girth y volumen
lm.trees2 <- lm(Volume~Girth, data=trees)
summary (lm.trees2)

#pregunta29

plot(trees$Volume~trees$Girth)
abline(lm.trees2)
