rm(list=ls())
options(digits = 10)
#Funcion que haya el divisor de beta y gamma
#x es el numero a dividir
hallarDivisor <- function(x){
  val <- 1
  repeat{
    temp <- x %/% val
    if(temp >= 0 && temp <= 10){break}
    else{val <- val*10}
  }
  return(val)
}

#Funcion que implementa la aproximacion Taylor donde:
#t es el numero de dias
# b es la tasa de infeccion/contagio/transmision beta
# g es la tasa de recuperacion gamma
#S0 es la poblacion susceptible inicial
#I0 es la poblacion infectada inicial
#k indica cada cuantos dias se calcula la prevalencia y la incidencia
taylorSIR<-function(t,b,g,S0,I0,k){
  #Hallar el divisor para ajustar beta y gamma
  divisor <- hallarDivisor(S0 + I0)
  b = b/divisor ;
  g <- g/divisor*100000
  #Tabla de datos
  dataSIR <- data.frame(t=1,S=S0,I=I0,R=0,Prevalencia=0,Incidencia=0)
  #Incializar datos
  S = S0; I = I0; R = 0 ; h = 1; acumI = I0
  #Aproximacion por Taylor de orden 3
  for(i in 2:t){
    tempS = S + h*(-b*S*I) + h^2/2*(-b*I*(-b*S*I) - b*S*(b*S*I - I*g)) + h^3/6*(-b*I*(b^2*I^2*S - b^2*S^2*I + b*S*I*g) - b*S*(-b^2*S*I^2 + b^2*S^2*I - b*S*I*g - g*b*S*I + I*g^2)) #+ h^4/24*(-b*I*(-b*I*(b^2*I^2*S - b^2*S^2*I + b*S*I*g) - b*S*(-b^2*S*I^2 + b^2*S^2*I - b*S*I*g - g*b*S*I + I*g^2)) - b*S*(b*S*(-b^2*S*I^2 + b^2*S^2*I - b*S*I*g - g*b*S*I + I*g^2) + b*I*(b^2*I^2*S - b^2*S^2*I + b*S*I*g) - g*(-b^2*S*I^2 + b^2*S^2*I - b*S*I*g - g*b*S*I + I*g^2)))
    tempI = I + h*(b*S*I - I*g) + h^2/2*(b*S*(b*S*I - I*g) + b*I*(-b*S*I) - g*(b*S*I - I*g)) + h^3/6*(b*S*(-b^2*S*I^2 + b^2*S^2*I - b*S*I*g - g*b*S*I + I*g^2) + b*I*(b^2*I^2*S - b^2*S^2*I + b*S*I*g) - g*(-b^2*S*I^2 + b^2*S^2*I - b*S*I*g - g*b*S*I + I*g^2)) #+ h^4/4*(b*S*(b*S*(-b^2*S*I^2 + b^2*S^2*I - b*S*I*g - g*b*S*I + I*g^2) + b*I*(b^2*I^2*S - b^2*S^2*I + b*S*I*g) - g*(-b^2*S*I^2 + b^2*S^2*I - b*S*I*g - g*b*S*I + I*g^2)) + b*I*(-b*I*(b^2*I^2*S - b^2*S^2*I + b*S*I*g) - b*S*(-b^2*S*I^2 + b^2*S^2*I - b*S*I*g - g*b*S*I + I*g^2)) - g*(b*S*(-b^2*S*I^2 + b^2*S^2*I - b*S*I*g - g*b*S*I + I*g^2) + b*I*(b^2*I^2*S - b^2*S^2*I + b*S*I*g) - g*(-b^2*S*I^2 + b^2*S^2*I - b*S*I*g - g*b*S*I + I*g^2)))
    tempR = R + h*(I*g) + h^2/2*(g*(b*S*I - I*g)) + h^3/6*(g*(-b^2*S*I^2 + b^2*S^2*I - b*S*I*g - g*b*S*I + I*g^2)) #+ h^4/24*(g*(b*S*(-b^2*S*I^2 + b^2*S^2*I - b*S*I*g - g*b*S*I + I*g^2) + b*I*(b^2*I^2*S - b^2*S^2*I + b*S*I*g) - g*(-b^2*S*I^2 + b^2*S^2*I - b*S*I*g - g*b*S*I + I*g^2)))
    acumI = acumI + h*(b*S*I) + h^2/2*(b*S*(b*S*I - I*g) + b*I*(-b*S*I)) + h^3/6*(b*S*(-b^2*S*I^2 + b^2*S^2*I - b*S*I*g - g*b*S*I + I*g^2) + b*I*(b^2*I^2*S - b^2*S^2*I + b*S*I*g))# + h^4/24*(b*S*(b*S*(-b^2*S*I^2 + b^2*S^2*I - b*S*I*g - g*b*S*I + I*g^2) + b*I*(b^2*I^2*S - b^2*S^2*I + b*S*I*g) - g*(-b^2*S*I^2 + b^2*S^2*I - b*S*I*g - g*b*S*I + I*g^2)) + b*I*(-b*I*(b^2*I^2*S - b^2*S^2*I + b*S*I*g) - b*S*(-b^2*S*I^2 + b^2*S^2*I - b*S*I*g - g*b*S*I + I*g^2)))
    prevalencia = 0; incidencia = 0
    S = tempS; I = tempI; R = tempR
    #Restriccion para aproximarse al modelo real
    if(S <= 0){S = 0}
    if(I <= 0){I = 0}
    if(R >= (S0+I0)){R = (S0 + I0)}
    if(i%%k == 0){
      #Calculo de prevalencia e incidencia [1][2][3]
      prevalencia = I/(S+I+R)
      incidencia = acumI/(S+I+R)
      acumI = 0
    }
    S = floor(S);R = ceiling(R)
    if(dataSIR[[i-1,"I"]] < I){I = ceiling(I)} else{I = floor(I)}
    dataSIR = rbind(dataSIR,list(i,S,I,R,prevalencia,incidencia))
  }
  #Grafico de prueba
  plot(dataSIR[["t"]],dataSIR[["S"]],main=paste("Modelo SIR"),xlab="Dias",ylab="Poblacion",xlim = c(0,t),ylim = c(0,(S0+I0)),type="l",col="blue") 
  points(dataSIR[["t"]],dataSIR[["I"]],type="l",col="red") 
  points(dataSIR[["t"]],dataSIR[["R"]],type="l",col="green") 
  legend("bottomright",c("S","I","R"),fill = c("blue","red","green"),cex = 0.5)
  
  return(dataSIR)
}

#Prueba de Taylor
diasCOVID19 = 77 #Desde 2020-03-06
poblacionColombia = (49.65*1000000) #[4]
infec = 1 #Segun la base de datos 2020-05-22.xlsx [6]
#Tasa de contagio del covid entre 2.5% y 1.5%
beta = 0.025 #[5]
#Tasa de recuperacion 
gamma = 2.16 
taylorSIR(diasCOVID19,beta,gamma,poblacionColombia-infec,infec,3)
#REFERENCIAS---------------------------------------------
#[1]
#«Incidencia y prevalencia: conceptos epidemiológicos clave», 
#EUPATI, jun. 17, 2015. https://www.eupati.eu/es/farmacoepidemiologia-es/conceptos-epidemiologicos-incidencia-y-prevalencia/ (accedido may 25, 2020).
#[2]
#«Hospital Ramón y Cajal», Hospital Universitario Ramón 
#y Cajal. https://www.comunidad.madrid/hospital/ramonycajal/ (accedido may 25, 2020).
#[3]
#«Incidencia». http://www.hrc.es/bioest/Medidas_frecuencia_3.html (accedido may 25, 2020).
#[4] Obtenido de busqueda en Google "Poblacion Colombia"
#[5]
#«Ni crecimiento exponencial ni "reglas de tres": 
#¿cómo se calcula, realmente, una epidemia?», El Confidencial
#, mar. 11, 2020. https://www.elconfidencial.com/tecnologia/ciencia/2020-03-11/matematicas-estadistica-calculo-coronavirus_2488931/ (accedido may 25, 2020).
#[6]
#«Casos positivos de COVID-19 en Colombia | Datos Abiertos 
#Colombia», la plataforma de datos abiertos del gobierno 
#colombiano. https://www.datos.gov.co/Salud-y-Protecci-n-Social/Casos-positivos-de-COVID-19-en-Colombia/gt2j-8ykr/data (accedido may 25, 2020).

