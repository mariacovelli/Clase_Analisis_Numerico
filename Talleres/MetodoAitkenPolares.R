###############
#Aitken Polares
#Autores:
#Gabriel Niño
#Juliana Garcia
#Taller 1 - Analisis Numerico
##############
rm(list=ls())
t<-1
E<-1e-6
f<-function(t){
  3*(sin(t))^3-1
}
g<-function(t){
  4*cos(t)*sin(t)
}
valf<-c()
valg<-c()
cont<-1
repeat{
  valf<-c(valf,f(cont))
  valg<-c(valg,g(cont))
  cont<-cont+1
  if(cont>100) break
}
aitf<-c()
aitg<-c()
cont<-1
repeat{
  a<-valf[cont+2]-((valf[cont+2]-valf[cont+1])^2)/(valf[cont+2]-2*valf[cont+1]+valf[cont])
  b<-valg[cont+2]-((valg[cont+2]-valg[cont+1])^2)/(valg[cont+2]-2*valg[cont+1]+valg[cont])
  aitf<-c(aitf,a)
  aitg<-c(aitg,b)
  cont<-cont+1
  if(abs(a-b)<E || cont>100) break
}
datos<-data.frame("f"=aitf,"g"=aitg)
print(datos)
#-------------------------------------------------------