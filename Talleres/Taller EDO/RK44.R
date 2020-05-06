rm(list=ls())
options (digits=16)
f<-function(x,y){x*exp(1)^(3*x)-40*y}
x<-c(1)
y<-c(10)
h<-0.2
error<-c(0)

for(i in 1:5){
  cat("imprimiendo iteracion: ", i, "\n\n")
  k1<-h*f(x[i],y[i])
  cat("imprimiendo y[i]", y[i], "\n")
  cat("imprimiendo k1", k1, "\n")
  k2<-h*f(x[i]+h/2,y[i]+(k1/2))
  cat("imprimiendo k2", k2, "\n")
  k3<-h*f(x[i]+h/2,y[i]+(k2/2))
  cat("imprimiendo k3", k3, "\n")
  k4<-h*f(x[i]+h, y[i]+k3)
  cat("imprimiendo k4", k4, "\n")
  y<-c(y,y[i]+1/6*(k1+2*k2+2*k3+k4))
  x<-c(x,x[i]+h)
  #error<-c(error, abs(y[i]-sol[i, 2]))
  cat("-----------------", "\n")
  
}
cat("imprimiendo --- x: ", x, "\n")
cat("imprimiendo --- y: ", y, "\n")
data.frame (x=x, y=y)


#tabla = cbind(x, y, error)
#tabla
plot(x, y, type = "p", main="Metodo de Runge-Kutta")
lines(x, y, col= "blue")