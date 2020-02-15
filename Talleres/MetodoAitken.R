###############
#Aitken Coseno
#Autores:
#Gabriel Niño
#Juliana Garcia
#Taller 1 - Analisis Numerico
##############
rm(list=ls())
x<-1
vec<-1:22
valcoseno<-cos(1/vec)
valcos<-c()
calculos<-c()
cont<-1
repeat{
  a<-valcoseno[cont+2]-((valcoseno[cont+2]-valcoseno[cont+1])^2)/(valcoseno[cont+2]-2*valcoseno[cont+1]+valcoseno[cont])
  calculos<-c(calculos,a)
  valcos<-c(valcos,valcoseno[cont])
  cont<-cont+1
  if(cont>20) break
}
datos<-data.frame("Coseno"=valcos,"Aitken"=calculos)
print(datos)
#----------------------------------