rm(list=ls())
library(shiny)
shinyUI(fluidPage(
  #Titulo
    titlePanel(title = h2("Modelo SIR COVID-19", align = "center")),
  #Panel donde se ingresan los datos de entrada  
    sidebarLayout(
      sidebarPanel(
        h4("Datos de entrada", align = "center"), 
        radioButtons(inputId = "rdbtn", label = "Modelo de contagio",choices = c("SIR","SI"), selected = "SIR"),
        textInput(inputId = "txtT",label = "Dias (t)",placeholder = "100", width = "150px"),
        textInput(inputId = "txtB",label = "Tasa de contagio (beta) [0.01,0.99]", placeholder = "0.3", width = "150px"),
        textInput(inputId = "txtG",label = "Tasa de recuperacion (gamma) [0.01,0.99]",placeholder = "0.1", width = "150px"),
        textInput(inputId = "txtS",label = "Poblacion susceptible inicial (S0)",placeholder = "1000", width = "150px"),
        textInput(inputId = "txtI",label = "Poblacion infectada inicial (I0)",placeholder = "1", width = "150px"),
        textInput(inputId = "txtk",label = "Periodo de prevalencia e incidencia",placeholder = "5", width = "150px"),
        actionButton("btn","Calcular")
      ),
      #Panel donde se muestra la grafica y la tabla de datos
      mainPanel(h4(title = "Grafica de resultados", align = "center"), 
                plotOutput(outputId = "graph"),
                plotOutput(outputId = "graph2"),
                plotOutput(outputId = "graph3"),
                h4(title = "Tabla de datos", align = "center"),
                dataTableOutput(outputId = "tabla"),
                
              )
    )
  )
)
