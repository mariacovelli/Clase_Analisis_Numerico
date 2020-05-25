#Se demora 5 minutos
rm(list=ls())
options(digits = 16)
library(readxl)
library(pracma)
source("TaylorSI.R")
#Lee la informacion de la base de datos 2020-05-22.xlsx [1]
ColombiaCOVID2020 <- read_excel(path = "2020-05-22.xlsx", range = "A1:Q19132", col_names = TRUE, col_types =c("numeric","date","numeric","text","text","numeric","text","text","text","text","text","date","text","date","date","date","text"))
#Fecha de inicio del COVID 19 en Colombia
inicioCOVIDColombia = as.Date("2020-03-06")
#Fecha de hace unos dias
fechaMaxima = as.Date("2020-05-22")
#Dias desde que la pandemia comenzó oficialmente
diasCOVID19 = 77
#Poblacion de Colombia
poblacionColombia = (49.65*1000000) #[2]
filas = 19131 #Filas de la base de datos [1]
#Funcion que cuenta las poblaciones S, I y R en la base de datos
contarSI <- function(){
  #Inicializar Datos
  dias<-c(1)
  fecha = inicioCOVIDColombia
  fechas <-c(as.character(fecha))
  S = (poblacionColombia - 1); I = 1
  valS<-c(S); valI<-c(I);valB<-c(I/S)
  incidenciaI<-(I/poblacionColombia)
  #Conteo
  for(i in 2:diasCOVID19){
    fecha = fecha + 1
    fechastr = as.character(fecha)
    contI = 0
    for(j in 1:filas){
      fechaI = as.character(ColombiaCOVID2020$`Fecha Not`[j])
      if(strcmp(fechastr,fechaI)){contI = contI + 1}
    }
    S = S - contI; I = I + contI
    fechas <-c(fechas,as.character(fecha))
    valS<-c(valS,S); valI<-c(valI,I)
    if(S > 0){valB<-c(valB,(I/S))} else{valB<-c(valB,0)}
    dias<-c(dias,i)
    incidenciaI<-c(incidenciaI,contI/poblacionColombia)
  }
  
  conteoSI <-data.frame("Fecha"=fechas,"t"=dias,"S"=valS,"I"=valI,"beta"=valB,"IncidenciaI"=incidenciaI)
  #Grafico de prueba
  plot(conteoSI[["t"]],conteoSI[["S"]],main=paste("Datos COVID 19 Colombia"),xlab="Dias",ylab=" ",xlim = c(0,diasCOVID19),ylim = c(0,poblacionColombia),type="l",col="blue") 
  points(conteoSI[["t"]],conteoSI[["I"]],type="l",col="red") 
  legend("bottomright",c("S","I"),fill = c("blue","red"),cex = 0.5)
  
  return(conteoSI)
}
#Funcion que calcula el error entre Taylor y lo obtenido de la base de datos
errorTaylor<-function(){
  conteoSI = contarSI()
  print(paste("Conteo Base de Datos"))
  print(conteoSI)
  #Tasa de contagio del covid entre 2.5% y 1.5% [3]
  beta = 0.025
  taylor = taylorSI(diasCOVID19,beta,(poblacionColombia - 1),1,3)
  errS<-c(); errI<-c(); dias<-c()
  InI = 0
  for(i in 1:diasCOVID19){
    #Error Absoluto
    tempS = abs(conteoSI[[i,"S"]] - taylor[[i,"S"]])
    tempI = abs(conteoSI[[i,"I"]] - taylor[[i,"I"]])
    errS<-c(errS,tempS); errI<-c(errI,tempI); dias<-c(dias,i)
    InI = InI + conteoSI[[i,"IncidenciaI"]]
  }
  print(paste("Promedio Incidencia I")); print(InI/diasCOVID19)
  result <- data.frame(dias,errS,errI)
  return(result)
}

#Prueba
resultados<-errorTaylor()
print(paste("Resultados de Error"))
print(resultados)
#REFERENCIAS----------------------------------------
#[1]
#«Casos positivos de COVID-19 en Colombia | Datos Abiertos 
#Colombia», la plataforma de datos abiertos del gobierno 
#colombiano. https://www.datos.gov.co/Salud-y-Protecci-n-Social/Casos-positivos-de-COVID-19-en-Colombia/gt2j-8ykr/data (accedido may 25, 2020).
#[2] Obtenido de busqueda en Google "Poblacion Colombia"
#[3]
#«Ni crecimiento exponencial ni “reglas de tres”: 
#¿cómo se calcula, realmente, una epidemia?», El Confidencial
#, mar. 11, 2020. https://www.elconfidencial.com/tecnologia/ciencia/2020-03-11/matematicas-estadistica-calculo-coronavirus_2488931/ (accedido may 25, 2020).
