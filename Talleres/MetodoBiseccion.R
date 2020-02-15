###############
#Metodo de Biseccion
#Autores:
#Gabriel Niño
#Juliana Garcia
#Taller 1 - Analisis Numerico
##############
a<-1
b<-100
n<-0
nmax<-floor(1/log(2)*log(abs(a-b)/E))
x0<-a
x1<-b
repeat{
  x2<-(x0+x1)/2
  if(f(x0)*f(x2) < 0){
    x0<-x0
    x1<-x2
  }
  else{
    x0<-x2
    x1<-x1
  }
  n<-n+1
  if((abs(a-b)/(2^n))<=E) break
}
print(x2)