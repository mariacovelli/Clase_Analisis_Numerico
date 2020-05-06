rm(list=ls())

require(deSolve)    #se requiere de la librería deSolve para hacer uso de la función ode

###########
#Solución del ejercicio mediante la funcion ode
##########
fp = function(x,y, parms){
  s = ((x*exp(1)^(3*x))-40*y)   # (e^(3x)*x)-40y
  return(list(s)) # ode requiere salida sea una lista
}

xis = seq(from = 1, to = 2,by = 0.2)    #se establece el rango x[1,2] con tamaño de paso 0.2

sol = ode(c(1,10),xis,fp,parms = NULL, method = "rk4") #se guarda el resultado de la función ode en la variable sol para posteriormente graficar el resultado
options(digits = 16)
tabla = cbind(xis, sol[,2])
tabla
plot(xis, sol[,2], type = "p", main="Metodo de Runge-Kutta por func ODE")
lines(xis, sol[,2], col= "blue")

###########
#Solución del ejercicio haciendo Runge-Kutta de manera "manual"
##########

f<-function(x,y){x*exp(1)^(3*x)-40*y}   
x<-c(1)     #se establecen los valores iniciales de h, x, y.
y<-c(10)
h<-0.2
error<-c(0)

#como el intervalo de X varía entre 1 y 2 con incremente de 0.2, establecemos un ciclo for para 5 iteraciones
#acá se calculan los coeficientes k que nos servirán para calcular nuestro y
for(i in 1:5)
{
  k1<-h*f(x[i],y[i])
  k2<-h*f(x[i]+h/2,y[i]+(k1/2))
  k3<-h*f(x[i]+h/2,y[i]+(k2/2))
  k4<-h*f(x[i]+h, y[i]+k3)
  y<-c(y,y[i]+1/6*(k1+2*k2+2*k3+k4))
  x<-c(x,x[i]+h)
  #error<-c(error, abs(y[i]-sol[i,2]))
}
data.frame (x=x, y=y)

plot(x, y, type = "p", main="Metodo de Runge-Kutta forma manual")
lines(x, y, col= "blue")

###########
#Solución de la ecuación diferencial
##########

#se calcula la solución de la E.D mediante el programa WolframAlpha
solucionED = function(x){(exp(1)^(-40*x)*(exp(1)^(43*x)*(43*x-1)-42*exp(1)^(43)+18490*exp(1)^(40)))/1849}
options(digits = 16)
xis2 = seq(from = 1, to = 2,by = 0.2)  

y2=c(10)
h2=1
i=1

while(i <= 6){
  y2[i] = solucionED(h2)  #se evalúa la solución de la E.D en cada intervalo
  h2 = h2+0.2 #incrementamos el intervalo de acuerdo al tamaño del paso (0.2)
  i = i + 1   #incrementamos el contador i para el correcto uso del ciclo while
}
data.frame (x=xis2, y=y2)

plot(xis2, y2, type = "p", main="Metodo de Runge-Kutta con solucion ED")
lines(xis2, y2, col= "blue")





