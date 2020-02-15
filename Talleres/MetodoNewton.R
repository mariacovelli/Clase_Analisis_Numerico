###############
#Metodo de Newton
#Autores:
#Gabriel Niño
#Juliana Garcia
#Taller 1 - Analisis Numerico
##############

df<-function(t){
  4.26*t-(0.0052)*t^3
}
xk<-1
repeat{
  dx<-f(xk)/df(xk)
  xk1<-xk-dx
  xk<-xk1
  if(dx<=E) break
}
print(xk1)
#----------------------------------