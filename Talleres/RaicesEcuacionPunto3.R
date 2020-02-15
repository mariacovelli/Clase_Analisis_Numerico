###############
#Encontrar la altura maxima usando metodos de ecuaciones no lineales
#Autores:
#Gabriel Niño
#Juliana Garcia
#Taller 1 - Analisis Numerico
##############
rm(list=ls())
E<-1e-6
t<-1
f<-function(t){
  6+2.13*t^2-0.0013*t^4
}

#Metodo del Punto fijo
max<-0
xi<-100
repeat{
  xi1<-f(xi)
  xi<-xi1
  if(xi1>max) max<-xi1
  print(xi1)
  if(abs(xi1-xi)<E || xi1<0) break
}
print(xi1)
#----------------------------------------------