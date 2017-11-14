######## Lectura de Datos

library(lmtest)
library(readxl)
datos <- read_excel("/Users/Martin/Downloads/Datos.xlsx")
View(datos)

attach(datos)

n<-132
for (i in 1:n){ ####Dejar tratamiento 4 (control) como base
  if (Tratamiento[i]==4){
    Tratamiento[i]=0
  }
}
Tratamiento

trat<-as.factor(Tratamiento)
pec<-as.factor(Estanque)

peso<-Peso
altura<-Altura
longitud<-Longitud

#######################################
#            C?lculo ?ndice           #
#######################################

lpeso<-log(peso)
lalt<-log(altura)
llong<-log(longitud)

fit<-lm(lpeso~lalt+llong)
summary(fit)

###Revision puntos de influencia y de alto leverage

source("/Users/Martin/Downloads/FuncionesEjercicio3.txt")

Leverage.normal(fit,3,"") 
qqplot.normal(fit,500,0.01,0,"")
Influence.normal(fit,3,2,0,"")

fit1<-lm(lpeso~lalt+llong, subset=-c(61))
summary(fit1)

####Se evidencia que los ebtas asociados cambian notoriamente por la influencia del punto 61

##########Indice

b<-coef(fit)[3]
c<-coef(fit)[2]

b1<-coef(fit1)[3]
c1<-coef(fit1)[2]


KM=peso/(longitud^b*altura^c)*10
KM1=peso/(longitud^b1*altura^c1)*10

########### GEE (ecuaciones de estimacion generalizadas)

library(gee)
plot(trat,KM1)

mod<-gee(KM1~trat,id=pec)
summary(mod)

mod1<-gee(KM~trat,id=pec)
summary(mod1)


library (geepack)

form<-formula(KM1~trat)
mo<-geeglm(form,id=pec)
summary(mo)

form1<-formula(KM~trat)
mo1<-geeglm(form1,id=pec)
summary(mo1)

anova(mo1)
anova(mo)

### Usando la funcion geepack se observa que los 
### parametros que involucran el efecto medio de los tratamientos
### no son significativos, conclusion que se corrobora con el 
### p-valor obtenido en el anova > 0.05 por lo que no hay 
### diferencias significativas entre tratamientos
