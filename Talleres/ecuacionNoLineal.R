#Encontrar la altura maxima usando metodos de ecuaciones no lineales 
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
#Metodo de Biseccion
#a<-1
#b<-100
#n<-0
#nmax<-floor(1/log(2)*log(abs(a-b)/E))
#x0<-a
#x1<-b
#repeat{
 # x2<-(x0+x1)/2
  #if(f(x0)*f(x2) < 0){
   # x0<-x0
    #x1<-x2
  #}
  #else{
    #x0<-x2
    #x1<-x1
  #}
  #n<-n+1
  #if((abs(a-b)/(2^n))<=E) break
#}
#print(x2)
#--------------------------------------
#Metodo de Newton
#df<-function(t){
 # 4.26*t-(0.0052)*t^3
#}
#xk<-1
#repeat{
 # dx<-f(xk)/df(xk)
  #xk1<-xk-dx
  #xk<-xk1
  #if(dx<=E) break
#}
#print(xk1)
#----------------------------------