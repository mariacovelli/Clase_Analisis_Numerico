##############
#Raices de una Ecuacion 2
#Autores:
#Gabriel Niño
#Juliana Garcia
#Taller 1 - Analisis Numerico
##############
rm(list=ls())
n<-100000
valn<-c()
valn2<-c()
cont<-1000
tiempos<-c()
repeat{
  t<-proc.time()
  cont2<-1
  acum<-0
  repeat{
    acum<-acum+cont2^2
    cont2<-cont2+1
    if(cont2>cont) break
  }
  valn<-c(valn,cont)
  valn2<-c(valn2,acum)
  temp<-proc.time()-t
  tiempos<-c(tiempos,temp[["elapsed"]])
  cont<-cont+1000
  if(cont>n) break
}
datos<-data.frame("n"=valn,"Tiempos"=tiempos)
print(datos)
plot(valn,tiempos,main="Orden_Convergencia",xlab="n",ylab="Tiempo",ylim=c(0,0.05),type="l",col="red")
#---------------------------------------------------
