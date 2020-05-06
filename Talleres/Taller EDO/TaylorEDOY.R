rm(list=ls())
options(digits = 16)
#Funcion que implementa la aproximacion Taylor donde:
#f es la expresion de la EDO, t0 y y0 son los valores iniciales de t y y respectivamente,
#h representa la magnitud del paso, a y b son el dominio de t tal que t pertenece a [a,b]
#m es el orden de la aproximacion Taylor
taylor<-function(f,t0,y0,h,m,a,b){
  #Derivadas parciales de f respecto a t y y
  fdt<-c(f,D(f,"t")); fdy<-c(f,D(f,"y"))
  for(i in 3:m){
    fdt<-c(fdt,D(fdt[[i-1]],"t")); fdy<-c(fdy,D(fdy[[i-1]],"y"))
  }
  t<-t0 ; y<-y0; E<-c(abs(y0 - g(t0))) #Error absoluto y0 y la funcion g en t0 
  valt<-c(t) ; valy<-c(y) ; valg<-c(g(t0))
  miny <- y ; maxy<-0 ; n <- (b-a)/h #n es la cantidad de pasos
  #La aproximacion de Taylor se calcula para cada paso hasta n
  for(k in 1:n){
    resp<-y + h*eval(f) #Se inicializa para aproximar Taylor
    #Se completa la aproximacion de Taylor
    for(i in 2:m){
      resp<-resp+((h^(i))/factorial(i)) * (eval(fdt[[i]]) + eval(fdy[[i]]) * eval(f))
    }
    t<-t + h ; y<-resp ; valt<-c(valt,t) ; valy<-c(valy,y) 
    E<-c(E,abs(y - g(t))) #Error absoluto entre la aproximacion de Taylor y la solucion
    valg<- c(valg, g(t)) ; if(y > maxy){ maxy<-y } ; if(y < miny){ miny<-y }
  }
  #Los puntos rojos representan la solucion del problema
  plot(valt,valg,main=paste("Taylor"),xlab="t"
       ,ylab="y",xlim = c(a,b),ylim = c(miny,maxy),col="red", pch = 16)
  #La linea azul representa la aproximacon de Taylor
  points(valt,valy, type="l", col = "blue")
  resultados<-data.frame("i"=(0:n),"t"=valt,"y"=valy, "Solucion" = valg ,"Error"=E)
  resultados
}
#Expresion de la EDO del problema
f = expression(t*exp(3*t) - 40*y)
#f = expression(y - t^2 + 1)
#f = expression(1 + y/t)
#f = expression(t - y)
#Ecuacion solucion calculada con Wolfram
g = function(t){
  exp(-40*t)*(exp(43*t)*(43*t-1)-42*exp(43)+18490*exp(40))/1849
  #(t+1)^2 - 0.5*exp(t)
  #t*(log(t) + 2)
  #t + 3*exp(-t) - 1
}

#Prueba de la funcion Taylor
taylor(f,1,10,0.2,4,1,2)
#taylor(f,0,0.5,0.2,4,0,2)
#taylor(f,1,2,0.25,2,1,2)
#taylor(f,0,2,0.2,4,0,1)