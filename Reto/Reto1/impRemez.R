#Ejercicio 2.2
#Gabriel Niño y Juliana Garcia
rm(list=ls())
#Hallar nodos de Chebychev
n<-3 #Cantidad de Nodos de Chebychev que reducen el error de interpolacion
a<--pi/64
b<-pi/64 #Se establece el intervalo cerrado [a,b]
E<-1e-6 #Error
#Funcion que obtiene los nodos de Chebychev
nodosChebychev<-function(n,a,b){
  nodos<-c()
  for(i in 1:n){
    temp<-1/2*(a+b)+1/2*(b-a)*cos((2*i-1)/(2*n)*pi) #Se quito 1/2*(a+b) de la ecuacion ya que siempre daba 0
    nodos<-c(nodos,temp)
  }
  nodos
}

#Funcion que obtiene la matriz
obtenerMatriz<-function(nodos,n,E){
  matriz<-matrix(0,n,n)
  for(i in 1:n){
    for(j in 1:n){
      if(j == n){
        matriz[i,j]<-E*(-1)^i
      }
      else{
        matriz[i,j]<-nodos[i]^(j-1)
      }
    }
  }
  matriz
}
#Funcion que obtiene la matriz de resultados de la funcion seno
matrizSin<-function(nodos){
  n<-length(nodos)
  matriz<-matrix(0,n,1)
  for(i in 1:n){
    matriz[i,1]<-sin(nodos[i])
  }
  matriz
}

resp<-0
#Funcion que halla el polinomio minimax de seno
senoMiniMax<-function(a,b,coef){
  n<-length(coef)
  nuevosNodos<-c()
  for(i in 1:n){
    if(i == 1){
      intervalo<-a:coef[i,1]
    }
    else if(i == n){
      intervalo<-coef[i,1]:b
    }
    else{
      intervalo<-coef[i-1,1]:coef[i,1]
    }
    tam<-length(intervalo)
    for(j in 1:tam){
      interminmax<-c()
      resp<-0
      for(k in 1:n){
        resp<-resp+coef[k]*intervalo[j]^(k-1)
      }
      resp<-resp-sin(intervalo[j])
      interminmax<-c(interminmax,resp)
    }
    nuevosNodos<-c(nuevosNodos,max(interminmax)) #Aqui se hizo la modificacion
  }
  nuevosNodos
}
#Funcion que aplica el metodo de Remez
remezSin<-function(a,b,n,x,E){
  nodos<-nodosChebychev(n+2,a,b) #Paso 1 obtener los nodos de Chevychev
  #Valores tabla
  valx<-c() #Valores de x
  valit<-c() #Valores de la iteracion
  valsin<-c() #Valores de la funcion Seno en x
  valremez<-c() #Valores del metodo de remez en x
  eabs<-c() #Errores Absolutos
  erel<-c()#Errores relativos
  precision <-c() #Precisionresp<-0
  cont<-0
  coef
  
  #------------
  repeat{
    if(cont > 0){
      nodos<-senoMiniMax(a,b,coef)
    }
    matriz<-obtenerMatriz(nodos,n+2,E) #Paso 2 Generar una matriz de (n+2)*(n+2)
    vec<-matrizSin(nodos)
    coef<-solve("a"=matriz,"b"=vec)
    lon<-length(coef)
    for(i in 1:(lon-1)){
      resp<-resp+coef[i]*x^(i-1)
    }
    cont<-cont+1
    valx<-c(valx,x)
    valit<-c(valit,cont)
    valsin<-c(valsin,sin(x))
    valremez<-c(valremez,resp)
    eabs<-c(eabs,abs(resp-sin(x)))
    erel<-c(erel,abs(resp-sin(x))/abs(resp)*100)
    precision <-c(precision,100-erel[cont])
    if(abs(resp-sin(x)) < E) break #El criterio de parada es el error absoluto
  }
  resultados<-data.frame("x"=valx,"it"=valit,"Seno"=valsin,"Remez"=valremez,"E"=eabs,"e"=erel,"Precision"=precision)
  resultados
  #resp
}

#Prubas con distintos valores
testGrado<-function(){
  n<-5#Constante
  a<--pi/64 #Constante
  b<-pi/64 #Constante
  E<-1e-6 #Constante
  x<-c(-pi/64,-0.04,-0.03,-0.02,-0.01,0,0.01,0.02,0.03,0.04,pi/64) #Varia
  #Valores tabla
  valx<-c() #Valores de x
  valit<-c() #Valores de la iteracion
  valsin<-c() #Valores de la funcion Seno en x
  valremez<-c() #Valores del metodo de remez en x
  eabs<-c() #Errores Absolutos
  erel<-c()#Errores relativos
  precision <-c() #Precision
  resultados<-data.frame("x"=valx,"it"=valit,"Seno"=valsin,"Remez"=valremez,"E"=eabs,"e"=erel,"Precision"=precision)
  #------------
  for(k in x){
    mat<-remezSin(a,b,n,k,E)
    resultados<-rbind(resultados,mat)
  }
  print(resultados,"tol"=16)
  plot(resultados[,"x"],resultados[,"Seno"], main="Metodo_Remez",xlab="X",ylab="Y",xlim=c(-pi/2,pi/2),type="l",col="blue")
  points(resultados[,"x"],resultados[,"Remez"],col="red")
}


