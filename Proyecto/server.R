#Aplicativo Shiny
#Para comparar con la base de datos 2020-05-22.xlsx [1] se mide 
#hasta el dia 77 que es la fecha 2020-05-22 
#Para extrapolar y predecir el comportamiento de la curva 
#se mide con un numero de dias mayor a 77
library(shiny)
library(pracma)
library(ggplot2)
source("aproxTaylor.R")
rm(list=ls())
options(digits = 5)
shinyServer(
  function(input,output){
    #Valores que cambian al hacer clic en el boton
    val <- reactiveValues(data = NULL, t = 0 , S0 = 0, I0 = 0, estilo = "N")
    #Evento que observa el boton y actualiza los datos 
    observeEvent(input$btn,{
          options(warn = -1)
          if(input$txtT == "" || is.na(as.numeric(input$txtT)) == TRUE || as.numeric(input$txtT) < 1){
            showNotification(paste("Los dias son invalidos"), type = "error")        
          }
          else if(input$txtB == "" || is.na(as.numeric(input$txtB)) == TRUE || as.numeric(input$txtB) < 0){
            showNotification(paste("La tasa de contagio es invalida"), type = "error")        
          }
          else if(input$txtG == "" || is.na(as.numeric(input$txtG)) == TRUE || as.numeric(input$txtG) < 0){
            showNotification(paste("La tasa de recuperacion es invalida"), type = "error")        
          }
          else if(input$txtS == "" || is.na(as.numeric(input$txtS)) == TRUE || as.numeric(input$txtS) < 1){
            showNotification(paste("La poblacion susceptible inicial es invalida"), type = "error")        
          }
          else if(input$txtI == "" || is.na(as.numeric(input$txtI)) == TRUE || as.numeric(input$txtI) < 1){
            showNotification(paste("La poblacion infectada inicial es invalida"), type = "error")        
          }
          else if(input$txtk == "" || is.na(as.numeric(input$txtk)) == TRUE || as.numeric(input$txtk) < 1){
            showNotification(paste("El periodo de prevalencia e incidencia es invalido"), type = "error")        
          }
          else{
              val$t <- as.numeric(input$txtT)
              val$S0 <- as.numeric(input$txtS)
              val$I0 <- as.numeric(input$txtI)
              val$estilo <-input$rdbtn
              if(strcmp(input$rdbtn,"SIR")){
                val$data <- taylorSIR(as.numeric(input$txtT),as.numeric(input$txtB),as.numeric(input$txtG),as.numeric(input$txtS),as.numeric(input$txtI),as.numeric(input$txtk))
              }
              else{
                val$data <- taylorSI(as.numeric(input$txtT),as.numeric(input$txtB),as.numeric(input$txtS),as.numeric(input$txtI),as.numeric(input$txtk))
              }
          }
        })
    #Generacion de la grafica a partir de los datos calculados
    output$graph <- renderPlot({
        if(is.null(val$data)){return()}
      else{
          if(strcmp(val$estilo,"SIR")){
            
            ggplot(val$data, aes(x = val$data[["t"]], y = val$data[["S"]])) + geom_line(colour = "blue") + ggtitle("MODELO SIR - SUSCEPTIBLES") + labs(x = "Dias", y = " Poblacion")
    
          }
        else{
          ggplot(val$data, aes(x = val$data[["t"]], y = val$data[["S"]])) + geom_line(colour = "blue") + ggtitle("MODELO SI - SUSCEPTIBLES") + labs(x = "Dias", y = " Poblacion")
        }
        }
      })
    #Generacion de la tabla de datos a partir de los datos calculados
    output$tabla <- renderDataTable({
      if(is.null(val$data)){return()}
      else {return(val$data)}
    })
    
    output$graph2 <- renderPlot({
      if(is.null(val$data)){return()}
      else{
        if(strcmp(val$estilo,"SIR")){
          
          ggplot(val$data, aes(x = val$data[["t"]], y = val$data[["I"]])) + geom_line(colour = "red")  + ggtitle("MODELO SIR - INFECTADOS")  + labs(x = "Dias", y = " Poblacion Infectada")
        }
        else{
          ggplot(val$data, aes(val$data[["t"]], val$data[["I"]])) + geom_line(colour = "red") + ggtitle("MODELO SI - INFECTADOS")  + labs(x = "Dias", y = " Poblacion Infectada")
  
        }
      }
    })
    
    output$graph3 <- renderPlot({
      if(is.null(val$data)){return()}
      else{
        if(strcmp(val$estilo,"SIR")){
          
          ggplot(val$data, aes(x = val$data[["t"]], y = val$data[["R"]])) + geom_line(colour = "green")  + ggtitle("MODELO SIR - RECUPERADOS")  + labs(x = "Dias", y = " Poblacion Recuperada")
          
        }
      }
    })
    
    
  }
)
#REFERENCIAS----------------------------------------
#[1]
#«Casos positivos de COVID-19 en Colombia | Datos Abiertos 
#Colombia», la plataforma de datos abiertos del gobierno 
#colombiano. https://www.datos.gov.co/Salud-y-Protecci-n-Social/Casos-positivos-de-COVID-19-en-Colombia/gt2j-8ykr/data (accedido may 25, 2020).

