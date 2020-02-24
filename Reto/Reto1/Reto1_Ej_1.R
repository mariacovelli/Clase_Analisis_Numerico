#Reto 1
#Ejercicio 1.1
#Gabriel Niño y Juliana Mogollon
func_horner = function(x_0, nums){
  cant = 0
  reps = 0                               #Creamos función horner
                                                #       y aplicamos el método.
  for(z in nums[1:length(nums)])  
    {
    cant = x_0*cant + z                
    reps = reps + 2
  }                                     #Retornamos resultado con total de
  cat("El resultado obtenido en: ",x_0," con un total de: \n ", reps, " operaciones. \n ", reps/2, " multiplicaciones. \n ", reps/2, " sumas.\n ", "Es: ", cant)                                      #                 operaciones y sumas.
  return(cant)
}
derivar = function(nums){
  #Creamos función para derivar un 
  #                     polinomio de grado k.
  derivada = c()  #Variable que contendrá los coeficientes de f'(x)
     flag=0    #Bandera que nos permite almacenar el grado del nuevo polinomio
  for(k in nums[1:length(nums)-1])    #Criterio de parada
    {
    flag = k*(length(nums)-1-k)                   #Derivamos
    derivada = c(derivada, flag)        #Derivamos
  }
  return (derivada)
}

x_0 = pi    #variable que nos permite probar la función con distintos valores
nums = c(2,0,-3,3,-4)     #Utilizamos el mismo polinomio del problema de horner del taller 1
resul = func_horner(x_0,derivar(nums))
cat("Coeficientes calculados: ", derivada)
print(resul, 16)      #imprimimos el resultado de la función con 16 cifras significativas

#graficamos las funciones

f = function(x) 2*x^4-3*x^2+3*x+4
f_prima = function(x) 8*x^3-6*x+3
plot(f,xlim = c(-2,2), col = 1, ylim=c(0,40),ylab = "Y" )
par(new = TRUE)           #par can be used to set or query graphical parameters 
plot(f_prima,xlim = c(-2,2),col= "red",ylim=c(0,40),xlab = "X")

epsi = .Machine$double.eps #epsilon de mi máquina
print(epsi, 16) #epsilon de mi máquina con 16 cifras significativas
1 + epsi == 1   #comprobamos que el resultado 1 + epsilon de mi maquina es diferente a 1
1 + epsi != 1   #comprobamos que el resultado 1 + epsilon de mi maquina es diferente a 1


