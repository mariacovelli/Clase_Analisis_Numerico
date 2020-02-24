rm(list=ls())
#Aproximacion de Taylor
x<-0
senoImp<-function(x,n){
  #Valores tabla
  valn<-c() #Valores de n
  valit<-c() #Valores de la iteracion
  valsin<-c() #Valores de la funcion Seno en x
  valtay<-c() #Valores del metodo de remez en x
  eabs<-c() #Errores Absolutos
  erel<-c()#Errores relativos
  precision <-c() #Precisionresp<-0
  #------------
  seno<-0
  dividendo<-0
  divisor<-0
  signo<-0
  for(i in 0:n){
    dividendo<-1
    for(j in 0:((2*i+1)-1)){
      dividendo<-dividendo*x
    }
    divisor<-1
    for(j in 1:(2*i+1)){
      divisor<-divisor*j
    }
    if(isTRUE(i%%2 == 0)){
      signo<-1
    }
    else{
      signo<--1
    }
    seno<-seno+(dividendo/divisor)*signo
    valn<-c(valn,n)
    valit<-c(valit,i+1)
    valsin<-c(valsin,sin(x))
    valtay<-c(valtay,seno)
    eabs<-c(eabs,abs(seno-sin(x)))
    erel<-c(erel,abs(seno-sin(x))/abs(seno)*100)
    precision <-c(precision,100-erel[i+1])
  }
  #print(seno)
  resultados<-data.frame("n"=valn,"it"=valit,"Seno"=valsin,"Taylor"=valtay,"E"=eabs,"e"=erel,"Precision"=precision)
  
}

#Pruebas de Taylor
testTaylor<-function(){
  n<-1:3#Varia
  E<-1e-6 #Constante
  x<-0.04
  #Valores tabla
  valn<-c() #Valores de n
  valit<-c() #Valores de la iteracion
  valsin<-c() #Valores de la funcion Seno en x
  valtay<-c() #Valores del metodo de remez en x
  eabs<-c() #Errores Absolutos
  erel<-c()#Errores relativos
  precision <-c() #Precision
  resultados<-data.frame("n"=valn,"it"=valit,"Seno"=valsin,"Taylor"=valtay,"E"=eabs,"e"=erel,"Precision"=precision)
  #------------
  for(i in n){
    mat<-senoImp(x,i)
    resultados<-rbind(resultados,mat)
  }
  print(resultados,"tol"=16)
}