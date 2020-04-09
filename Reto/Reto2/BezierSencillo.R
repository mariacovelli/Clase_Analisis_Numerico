rm(list=ls())
#Splines de Bezier para graficar el Mortero
options(digits = 16)
library(grid)
library(gridBezier)
library(vwline)
grid.newpage()
#Punto Base----------------------------------------------------
xbase<-0.379768786
ybase<-0.035344828
#Punto p1
xp1<-0.696242775
yp1<-0.403448276
#Lineas Verticales --------------------------------------------
#Base a p1-----------------------------------------------------
x<-c(xbase,0.565317919,0.692774566,xp1)
y<-c(ybase,0.018103448,0.142241379,yp1)
grid.Bezier(x,y)
#Reflejo en x
xrx<-c(xbase,2*xbase-x[2],2*xbase-x[3],2*xbase-x[4])
grid.Bezier(xrx,y)
#--------------------------------------------------------------
#Borde superior-------------------------------------------------
#p8 a p1--------------------------------------------------------
x<-c(0.386705202,0.494219653,0.667630058,xp1)
y<-c(0.285344828,0.275862069,0.348275862,yp1)
grid.Bezier(x,y)
#Reflejo en x
xrx<-c(2*xbase-x[1],2*xbase-x[2],2*xbase-x[3],2*xbase-x[4])
grid.Bezier(xrx,y)
#Reflejo en y
yry<-c(2*yp1-y[1],2*yp1-y[2],2*yp1-y[3],2*yp1-y[4])
grid.Bezier(x,yry)
#Reflejo xy
grid.Bezier(xrx,yry)
#p16 a p9-------------------------------------------------------
x<-c(0.379768786,0.474277457,0.61300578,0.621676301)
y<-c(0.310344828,0.293103448,0.361206897,0.400862069)
grid.Bezier(x,y)
#Reflejo en x
xrx<-c(2*xbase-x[1],2*xbase-x[2],2*xbase-x[3],2*xbase-x[4])
grid.Bezier(xrx,y)
#Reflejo en y
yry<-c(2*yp1-y[1],2*yp1-y[2],2*yp1-y[3],2*yp1-y[4])
grid.Bezier(x,yry)
#Reflejo xy
grid.Bezier(xrx,yry)
#_--------------------------------------------------------------