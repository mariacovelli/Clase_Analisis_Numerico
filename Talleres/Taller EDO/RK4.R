rm(list=ls())

require(deSolve)
require(PolynomF)


fp = function(x,y, parms){
  s = ((x*exp(1)^(3*x))-40*y)
  return(list(s)) # ode requiere salida sea una lista
}

xis = seq(from = 1, to = 2,by = 0.2)

sol = ode(c(1,10),xis,fp,parms = NULL, method = "rk4")
options(digits = 16)
tabla = cbind(xis, sol[,2])
tabla
plot(xis, sol[,2], type = "p", main="Metodo de Runge-Kutta por func ODE")
lines(xis, sol[,2], col= "blue")

f<-function(x,y){x*exp(1)^(3*x)-40*y}
x<-c(1)
y<-c(10)
h<-0.2
error<-c(0)

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

plot(x, y, type = "p", main="Metodo de Runge-Kutta")
lines(x, y, col= "blue")

solucionED = function(x){(exp(1)^(-40*x)*(exp(1)^(43*x)*(43*x-1)-42*exp(1)^(43)+18490*exp(1)^(40)))/1849}
options(digits = 16)
xis2 = seq(from = 1, to = 2,by = 0.2)

y2=c()
h2=1
i=1
while(i <= 6){
  y2[i] = solucionED(h2)
  h2 = h2+0.2
  i = i + 1
}
data.frame (x=xis2, y=y2)

plot(xis2, y2, type = "p", main="Metodo de Runge-Kutta con solucion ED")
lines(xis2, y2, col= "blue")





