#Raiz Cuadrada de 7
#Referencias: Analisis Numerico Con Python, pag 15
rm(list=ls()) #Borrar los datos guardados en memoria
x<-3 #Valor Inicial
n<-7 #Dato
E<-1e-5 #Error Permitido
cont<-1 #Contador de iteración
valit<- c() #Valor de cada iteracion
valx<-c() #Valores que tomara x en cada iteracion
valy<-c() #Valores que tomara y en cada iteracion
valE<-c() #Valores del Error Absoluto en cada iteracion
vale<-c() #Valores del error relativo en cada iteracion
valval<-c() # Validacion de la raiz en en cada iteracion
valaprox<-c() #Valor aproximado que es la media de las diferentes medidas
f<-function(x,n,E){
    1/2*(x+n/x) #Función para calcular la raiz cuadrada
}
  y<-f(x,n,E) #Resultado de la funcion
  valit<-c(valit,cont)
  valx<-c(valx,x)
  valy<-c(valy,y)
  vale<-c(vale,(abs(y-x)/abs(y))*100)
  valval<-c(valval,y^2)
  valaprox<-c(valaprox,mean(valy))
  precision<-c() #Precision del calculo
    repeat{
      cont<-cont+1
      x<-y
      y<-f(x,n,E)
      valit<-c(valit,cont)
      valx<-c(valx,x)
      valy<-c(valy,y)
      valval<-c(valval,y^2)
      vale<-c(vale,(abs(y-x)/abs(y))*100)
      valaprox<-c(valaprox,mean(valy))
      if(abs(x-y)<E) break 
    }
  valE<-abs(y-valaprox)
  precision<-100-vale
  #Guardar resultados en una tabla de datos
  resultados<-data.frame("Iteracion"=valit,"X"=valx,"Y"=valy,"Error_Absoluto"=valE,"Error_Relativo"=vale,"Precision"=precision,"Validacion"=valval)
  #Exportar los resultados
  write.table(resultados,file="ResultadosRaiz7.txt")
  #Imprimir resultados
  print(resultados)
  #-----------------------------------------------------------------