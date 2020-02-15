###############
#Autores:
#Gabriel Niño
#Juliana Garcia
#Taller 1 - Analisis Numerico
##############

matTriangular = function(tamanyo)
{
  suma = c()
  
  for (z in tamanyo) 
  {
    mat= matrix(1,nrow = z, ncol = z)
    cont = 0
    
    for (i in 1:z) 
    {
      for (j in 1:i) 
      {
        cont = cont + mat[i,j]
      }   
    }
    
    suma = c(suma,cont)
    cont = 0
  }
  
  return(suma)
}

val = c(5,10,15,20,25,40,50,60,65,75)

graph = matTriangular(val)

plot(val,graph,xlab = "Tamanyo",ylab = "sumasXmatriz", type = 'b',col = 4)