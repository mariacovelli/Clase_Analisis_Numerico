library(deSolve)
library(phaseR)
library(flowfield)
options(warn = -1)
#tamaño poblacional
N = 49.65*1000000
#estado inicial de los compartimentos
init <- c(S = N-1,
          I = 1,
          R = 0)
#parámetros del modelo (coeficientes de las variables)
param <- c(beta = 0.025,
           gamma = 2.16)

beta <- 0.025
gamma <- 2.16
#crear la función con las ODE
sir <- function(times, init, param) {
  with(as.list(c(init, param)), {
    #ecuaciones diferenciales   
    dS <- -beta * S * I
    dI <-  beta * S * I - gamma * I
    dR <-                 gamma * I
    #resultados de las tasas de cambio    
    return(list(c(dS, dI, dR)))
  })
}
#intervalo de tiempo y resolución

scopeField <- function(t, p, param){
    k <- param[1]
    n <- param[2]
    dp <- k*(p*(n-p))
    list(dp)
  }
  scopeField.flowField <- flowField(scopeField, xlim = c(0,77),
                                    ylim = c(0,N), param = c(beta, gamma),
                                    system = "one.dim",
                                    add = FALSE, xlab = "Tiempo (en dias)", ylab = "Poblacion", 
                                    main = "Campo de pendientes")
  
  