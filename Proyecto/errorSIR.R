#Se demora 10 minutos
rm(list=ls())
options(digits = 16)
library(readxl)
library(pracma)
source("TaylorSIRX.R")
#Lee la informacion de la base de datos
ColombiaCOVID2020 <- read_excel(path = "2020-05-22.xlsx", range = "A1:Q19132", col_names = TRUE, col_types =c("numeric","date","numeric","text","text","numeric","text","text","text","text","text","date","text","date","date","date","text"))
#Fecha de inicio del COVID 19 en Colombia
inicioCOVIDColombia = as.Date("2020-03-06")
#Fecha de hace unos dias
fechaMaxima = as.Date("2020-05-22")
#Dias desde que la pandemia comenzó oficialmente
diasCOVID19 = 77
#Poblacion de Colombia
poblacionColombia = (49.65*1000000)
filas = 19131
#Funcion que cuenta las poblaciones S, I y R en la base de datos
contarSIR <- function(){
  #Inicializar Datos
  dias<-c(1)
  fecha = inicioCOVIDColombia
  fechas <-c(as.character(fecha))
  S = (poblacionColombia - 1); I = 1; R = 0
  valS<-c(S); valI<-c(I); valR<-c(R)
  valB<-c(I/S); valG<-(R/I)
  incidenciaI<-(I/poblacionColombia)
  incidenciaR<-(0)
  #Conteo
  for(i in 2:diasCOVID19){
    fecha = fecha + 1
    fechastr = as.character(fecha)
    contI = 0; contR = 0;
    for(j in 1:filas){
      fechaR = as.character(ColombiaCOVID2020$`Fecha recuperado`[j])
      fechaI = as.character(ColombiaCOVID2020$`Fecha Not`[j])
      if(strcmp(fechastr,fechaI)){contI = contI + 1}
      if(strcmp(fechastr,fechaR)){contR = contR + 1}
    }
    S = S - contI; I = I + contI - contR; R = R + contR
    fechas <-c(fechas,as.character(fecha))
    valS<-c(valS,S); valI<-c(valI,I); valR<-c(valR,R)
    if(S > 0){valB<-c(valB,(I/S))} else{valB<-c(valB,0)}
    if(I > 0){valG<-c(valG,(R/I))} else{valG<-c(valG,0)}
    dias<-c(dias,i)
    incidenciaI<-c(incidenciaI,contI/poblacionColombia)
    incidenciaR<-c(incidenciaR,contR/poblacionColombia)
  }
  
  conteoSIR <-data.frame("Fecha"=fechas,"t"=dias,"S"=valS,"I"=valI,"R"=valR,"beta"=valB,"gamma"=valG,"IncidenciaI"=incidenciaI, "IncidenciaR"=incidenciaR)
  #Grafico de prueba
  plot(conteoSIR[["t"]],conteoSIR[["S"]],main=paste("Datos COVID 19 Colombia"),xlab="Dias",ylab=" ",xlim = c(0,diasCOVID19),ylim = c(0,poblacionColombia),type="l",col="blue") 
  points(conteoSIR[["t"]],conteoSIR[["I"]],type="l",col="red") 
  points(conteoSIR[["t"]],conteoSIR[["R"]],type="l",col="green") 
  legend("bottomright",c("S","I","R"),fill = c("blue","red","green"),cex = 0.5)
  
  return(conteoSIR)
}
#Funcion que calcula el error entre Taylor y lo obtenido de la base de datos
errorTaylor<-function(){
 conteoSIR = contarSIR()
 print(paste("Conteo Base de Datos"))
 print(conteoSIR)
 #Tasa de contagio del covid entre 2.5% y 1.5%
 beta = 0.025
 #Tasa de recuperacion 
 gamma = 2.16
 taylor = taylorSIR(diasCOVID19,beta,gamma,(poblacionColombia - 1),1,3)
 errS<-c(); errI<-c(); errR<-c(); dias<-c()
 InI = 0; InR = 0;
 for(i in 1:diasCOVID19){
   #Error Absoluto
   tempS = abs(conteoSIR[[i,"S"]] - taylor[[i,"S"]])
   tempI = abs(conteoSIR[[i,"I"]] - taylor[[i,"I"]])
   tempR = abs(conteoSIR[[i,"R"]] - taylor[[i,"R"]])
   errS<-c(errS,tempS); errI<-c(errI,tempI)
   errR<-c(errR,tempR); dias<-c(dias,i)
   InI = InI + conteoSIR[[i,"IncidenciaI"]]
   InR = InR + conteoSIR[[i,"IncidenciaR"]]
 }
 print(paste("Promedio Incidencia I")); print(InI/diasCOVID19)
 print(paste("Promedio Incidencia R")); print(InR/diasCOVID19)
 result <- data.frame(dias,errS,errI,errR)
 return(result)
}

#Prueba
resultados<-errorTaylor()
print(paste("Resultados de Error"))
print(resultados)

