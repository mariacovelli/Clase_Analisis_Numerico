#Teorema de Taylor
rm(list=ls()) #Borrar los datos guardados en memoria
n<-0 #Valor Inicial
x<-0.5 #Constante
y<-function(x,n){
  x^n/factorial(n)
}
valx<-c(x)
valy<-c(y(x,n)) #Valores que tomara y en cada iteracion
valtay<-c(sum(valy))#Valores de aproximacion del Teorema de Taylor
temp<-sum(valy) #Auxiliar
valn<-c(n) #Valores de n en cada iteracion
valE<-c(abs(exp(x)-mean(valtay))) #Valores del Error Absoluto en cada iteracion
vale<-c(abs(exp(x)-mean(valtay))/mean(valtay)) #Valores del error relativo en cada iteracion
n<-n+1
repeat{
  valx<-c(valx,x)
  valy<-c(valy,y(x,n))
  valtay<-c(valtay,sum(valy))
  valn<-c(valn,n)
  valE<-c(valE,abs(exp(x)-mean(valtay)))
  vale<-c(vale,abs(exp(x)-mean(valtay))/mean(valtay))
  temp<-sum(valy)
  n<-n+1
  if(n>5) break
}
precision<-100-vale
#Guardar resultados en una tabla de datos
resultados<-data.frame("X"=valx,"N"=valn,"Y"=valy,"Taylor"=valtay,"Error_Absoluto"=valE,"Error_Relativo"=vale,"Precision"=precision)
#Exportar los resultados
#write.table(resultados,file="lol.txt")
#Imprimir resultados
print(resultados,tol=1e-16)
#Graficar
#plot(valx,valy,xlab="X",ylab="Y",xlim=c(0,1),ylim=c(0,3),type="l",col="blue")
#points(valn,valtay,xlim=c(0,1),ylim=c(0,3), type="l",col="red")
#vec<-c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9)
#points(vec,exp(vec),xlim=c(0,1),ylim=c(0,3), type="l")
rm(list=ls())
x<-c(-10,-8,-6,-4,-2,0,2,4,6,8,10)
f<-function(x){
  1+x+x^2/2+x^3/factorial(3)+x^4/factorial(4)+x^5/factorial(5)
}
plot(x,f(x),main="Teorema_de_Taylor_VS_Exponencial",xlab="X",ylab="Y",type="l",col="blue")
#plot(x,exp(x), main="exp(x)",xlab="X",ylab="Y",type="l",col="red")
points(x,exp(x),type="l",col="red")
points(0.5,exp(0.5))
#--------------------------------------------