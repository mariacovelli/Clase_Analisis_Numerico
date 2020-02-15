###############
#Autores:
#Gabriel Niño
#Juliana Garcia
#Taller 1 - Analisis Numerico
##############


P_x = function(x)
{
  return (1 + x + x^2 + x^3 + x^4 + x^5 + x^6 + x^7 + x^8 + x^9 + x^10 
          + x^11 + x^12 + x^13 + x^14 + x^15 + x^16 + x^17 + x^18 + x^19 + 
            x^20 + x^21 +x^22 + x^23 + x^24 + x^25 + x^26 + x^27 + x^28 + x^29 + 
            x^30 + x^31 + x^32 + x^33 + x^34 + x^35 + x^36 + x^37 + x^38 + x^39 +
            x^40 + x^41 + x^42 + x^43 + x^44 + x^45 + x^46 + x^47 + x^48 + x^49 + 
            x^50)
}


cat("Este es el resultado por P(x):",
    round(P_x(1.0001), 4),
    "\n")

Q_x = function(x)
{
  return((x^51-1)/(x-1))
  
}

cat("Este es el resultado por Q(x):", 
    round(Q_x(1.0001), 4), 
  "\n")

valor_real = round(P_x(1.0001), 4)
Valor_exp = round(Q_x(1.0001), 4)

cat("El error absoluto del calculo es de: ", abs(Valor_exp - valor_real), ".\n")
