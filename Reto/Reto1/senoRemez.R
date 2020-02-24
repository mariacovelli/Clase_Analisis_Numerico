rm(list=ls())
#Metodo de Remez
#Tomado de https://github.com/simonbyrne/Remez.jl/blob/master/src/Remez.jl
#Evaluar el polinomio
x<-0 #Punto en el que se evalua el polinomio
coef<-c() #Arreglo de coeficientes del polinomio
y<-0
poly_eval<-function(x,coef){
  y<-0
  n<-length(coef)
  if(isTRUE(n > 0)){
    y<-coef[n]
    for(i in (n-1):1){
      y<-coef[i]+x*y
    }
  }
  y
}
#Evaluar una funcion racional
ncoef<-c()#Coeficientes del numerador
dcoef<-c()#Coeficientes del denominador
x<-0#Punto en el que se evalua la funcion
eval_rac<-function(x,ncoef,dcoef){
  poly_eval(x,ncoef)/poly_eval(x,dcoef)
}
#Cuadrados Inferiores
  #Se usa para obtener un punto de inicio para comenzar con las iteraciones de Remez
f<-function(x){0}#Funcion que se va a aproximar
xvals<-c() #Coordenadas de x para evaluar f
n<-0 #Grado de del numerador polinomial de la funcion racional deseada
d<-0#Grado de del denominador polinomial de la funcion racional deseada
cuadradosMenores<-function(f,xvals,n,d){
  maxpow<-max(n,d)*2+1
  sums<-matrix(0,maxpow,3)
  for(x in xvals){
    y<-f(x)
    for(i in 1:maxpow){
      for(j in 1:3){
        sums[i,j]<-sums[i,j]+x^(i-1)*y^(j-1)
      }
    }
  }
  matriz<-matrix(0,n+d+1,n+d+1)
  vect<-array(0,c(n+d+1,1))
  for(i in 0:n){
    row<-i+1
    for(j in 0:n){
      matriz[row,1+j]<-sums[1+i+j,1]
    }
    for(j in 1:d){
      matriz[row,1+n+j]<--sums[1+i+j,2]
    }
    vect[row]<-sums[1+i,2]
  }
  for(i in 1:d){
    row<-1+n+i
    for(j in 0:n){
      matriz[row,1+j]<-sums[1+i+j,2]
    }
    for(j in 1:d){
      matriz[row,1+n+j]<--sums[1+i+j,3]
    }
    vect[row]<-sums[1+i,3]
  }
  all_coef<-matriz/vect
  ncoef<-all_coef[1:n+1]
  dcoef<-vcat(1,all_coef[n+2:n+d+1])
  return(ncoef,dcoef)
}

#Busca hallar el maximo de la funcion
a<-0#Valores donde se evalua la funcion, van en el orden a,b,c
b<-0
c<-0
golden_section<-function(f,x,a,b,c){
  E<-1e-6
  one_over_pi<-2/(1+sqrt(5))
  if(abs(b-a) < abs(c-a)){
    temp<-c
    c<-a
    a<-temp
  }
  fa<-f(a)
  fb<-f(b)
  fc<-f(c)
  repeat{
    if(abs(c-a) < 1e-6) break
      d<-a+(b-a)*one_over_pi
      fd<-f(d)
      if(fd > fb){
        b<-d
        c<-b
        fb<-fd
        fc<-fb
      }
      else{
        a<-c
        c<-d
        fa<-fc
        fc<-fd
      }
  }
  return(b,fb)
}

#Encontrar el extremo de una funcion en un intervalo
grid<-c()
find_extrema<-function(f,grid){
  len<-length(grid)
  extrema<-c()
  for(i in 1:len){
    prev<-max(1,i-1)
    nextt<-min(i+1,len)
    xp<-grid[prev]
    xi<-grid[i]
    xn<-grid[nextt]
    yp<-f(xp)
    yi<-grid(xi)
    yn<-grid(xn)
    if(yp <= yi && yi >= yn){
      extrema<-c(extrema,Tuple(golden_section(f,xp,xi,xn)))
    }
    else if(yp >= yi && yi <= yn){ #:v
      extrema<-c(extrema,Tuple(golden_section(f,xp,xi,xn)))
    }
  }
  extrema
}











