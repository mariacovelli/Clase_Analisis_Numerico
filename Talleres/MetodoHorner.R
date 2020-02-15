###############
#Teorema Horner
#Autores:
#Gabriel Niño
#Juliana Garcia
#Taller 1 - Analisis Numerico
##############

coef=c(2,0,-3,3,-4)  
valor=0                  
x_0=-2
i=1

numDeSumas = 0
numDeMultiplicaciones = 0

while(i<=5){
  valor=valor*x_0+coef[i]
  i=i+1;
  numDeSumas = numDeSumas + 1
  numDeMultiplicaciones = numDeMultiplicaciones +1
}
print(valor)
cat("El numero total de sumas es :",numDeSumas,"\n")
cat("El numero total de Multiplicaciones es :",numDeMultiplicaciones,"\n")
#######################################################