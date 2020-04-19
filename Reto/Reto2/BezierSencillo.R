#BezierSencillo implementacion con sencilla
rm(list=ls())
#Splines de Bezier para graficar el Mortero
options(digits = 16)
library(grid)
library(gridBezier)
library(vwline)
grid.newpage()
#Funciones--------------------------------
pixelx
pixely
px<-function(pixelx){
  (pixelx - 142)*0.75/865
}
py<-function(pixely){
  (677 - pixely)*0.55/638
}
#Punto Base--------------------------------
xbase<-px(580)
ybase<-py(636)
#Punto p1
#Punto p1
xp1<-px(945)
yp1<-py(209)
#Lineas Verticales--------------------------------
#Base a p1--------------------------------
x<-c(xbase,px(794),px(941),xp1)
y<-c(ybase,py(656),py(512),yp1)
grid.Bezier(x,y)
#Reflejo en x
xrx<-c(xbase,2*xbase-x[2],2*xbase-x[3],2*xbase-x[4])
grid.Bezier(xrx,y)
#--------------------------------
#Borde superior--------------------------------
#p8 a p1--------------------------------
x<-c(px(588),px(712),px(912),xp1)
y<-c(py(346),py(357),py(273),yp1)
grid.Bezier(x,y)
#Reflejo en x
xrx<-c(2*xbase-x[1],2*xbase-x[2],2*xbase-x[3],2*xbase-x[4])
grid.Bezier(xrx,y)
#Reflejo en y
yry<-c(2*yp1-y[1],2*yp1-y[2],2*yp1-y[3],2*yp1-y[4])
grid.Bezier(x,yry)
#Reflejo xy
grid.Bezier(xrx,yry)
#p16 a p9--------------------------------
x<-c(px(580),px(689),px(849),px(859))
y<-c(py(317),py(337),py(258),py(212))
grid.Bezier(x,y)
#Reflejo en x
xrx<-c(2*xbase-x[1],2*xbase-x[2],2*xbase-x[3],2*xbase-x[4])
grid.Bezier(xrx,y)
#Reflejo en y
yry<-c(2*yp1-y[1],2*yp1-y[2],2*yp1-y[3],2*yp1-y[4])
grid.Bezier(x,yry)
#Reflejo xy
grid.Bezier(xrx,yry)