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
#Base a p2-----------------------------------------------------
x<-c(xbase,0.523699422,0.661560694,0.65982659)
y<-c(ybase,0.049137931,0.217241379,0.364655172)
grid.Bezier(x,y)
#Reflejo en x
xrx<-c(xbase,2*xbase-x[2],2*xbase-x[3],2*xbase-x[4])
grid.Bezier(xrx,y)
#Base a p3-----------------------------------------------------
x<-c(xbase,0.519364162,0.615606936,0.611271676)
y<-c(ybase,0.08362069,0.215517241,0.334482759)
grid.Bezier(x,y)
#Reflejo en x
xrx<-c(xbase,2*xbase-x[2],2*xbase-x[3],2*xbase-x[4])
grid.Bezier(xrx,y)
#Base a p4-----------------------------------------------------
x<-c(xbase,0.501156069,0.553179191,0.556647399)
y<-c(ybase,0.127586207,0.206896552,0.311206897)
grid.Bezier(x,y)
#Reflejo en x
xrx<-c(xbase,2*xbase-x[2],2*xbase-x[3],2*xbase-x[4])
grid.Bezier(xrx,y)
#Base a p5-----------------------------------------------------
x<-c(xbase,0.478612717,0.497687861,0.502023121)
y<-c(ybase,0.152586207,0.204310345,0.293965517)
grid.Bezier(x,y)
#Reflejo en x
xrx<-c(xbase,2*xbase-x[2],2*xbase-x[3],2*xbase-x[4])
grid.Bezier(xrx,y)
#Base a p6-----------------------------------------------------
x<-c(xbase,0.43699422,0.450867052,0.442196532)
y<-c(ybase,0.137068966,0.200862069,0.285344828)
grid.Bezier(x,y)
#Reflejo en x
xrx<-c(xbase,2*xbase-x[2],2*xbase-x[3],2*xbase-x[4])
grid.Bezier(xrx,y)
#Base a p7-----------------------------------------------------
x<-c(xbase,0.410115607,0.417919075,0.411849711)
y<-c(ybase,0.129310345,0.18362069,0.284482759)
grid.Bezier(x,y)
#Reflejo en x
xrx<-c(xbase,2*xbase-x[2],2*xbase-x[3],2*xbase-x[4])
grid.Bezier(xrx,y)
#---------------------------------------------------------------
#Lineas Horizontales--------------------------------------------
#p24 a p17------------------------------------------------------
x<-c(0.387572254,0.464739884,0.684104046,0.691907514)
y<-c(0.250862069,0.238793103,0.344827586,0.356034483)
grid.Bezier(x,y)
#Reflejo en x
xrx<-c(2*xbase-x[1],2*xbase-x[2],2*xbase-x[3],2*xbase-x[4])
grid.Bezier(xrx,y)
#p25 a p18------------------------------------------------------
x<-c(0.386705202,0.472543353,0.631213873,0.686705202)
y<-c(0.218103448,0.2,0.256896552,0.317241379)
grid.Bezier(x,y)
#Reflejo en x
xrx<-c(2*xbase-x[1],2*xbase-x[2],2*xbase-x[3],2*xbase-x[4])
grid.Bezier(xrx,y)
#p26 a p19------------------------------------------------------
x<-c(0.383236994,0.43699422,0.638150289,0.671965318)
y<-c(0.181034483,0.164655172,0.221551724,0.262068966)
grid.Bezier(x,y)
#Reflejo en x
xrx<-c(2*xbase-x[1],2*xbase-x[2],2*xbase-x[3],2*xbase-x[4])
grid.Bezier(xrx,y)
#p27 a p20------------------------------------------------------
x<-c(0.38150289,0.464739884,0.613872832,0.649421965)
y<-c(0.143103448,0.131896552,0.173275862,0.206896552)
grid.Bezier(x,y)
#Reflejo en x
xrx<-c(2*xbase-x[1],2*xbase-x[2],2*xbase-x[3],2*xbase-x[4])
grid.Bezier(xrx,y)
#p28 a p21------------------------------------------------------
x<-c(0.378901734,0.443930636,0.552312139,0.631213873)
y<-c(0.10862069,0.095689655,0.118965517,0.171551724)
grid.Bezier(x,y)
#Reflejo en x
xrx<-c(2*xbase-x[1],2*xbase-x[2],2*xbase-x[3],2*xbase-x[4])
grid.Bezier(xrx,y)
#p29 a p22------------------------------------------------------
x<-c(0.378034682,0.496820809,0.549710983,0.60867052)
y<-c(0.08362069,0.076724138,0.093965517,0.139655172)
grid.Bezier(x,y)
#Reflejo en x
xrx<-c(2*xbase-x[1],2*xbase-x[2],2*xbase-x[3],2*xbase-x[4])
grid.Bezier(xrx,y)
#p30 a p23------------------------------------------------------
x<-c(0.375433526,0.409248555,0.485549133,0.54017341)
y<-c(0.060344828,0.046551724,0.046551724,0.074137931)
grid.Bezier(x,y)
#Reflejo en x
xrx<-c(2*xbase-x[1],2*xbase-x[2],2*xbase-x[3],2*xbase-x[4])
grid.Bezier(xrx,y)
#---------------------------------------------------------------
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
#p9 a p1--------------------------------------------------------
x<-c(0.621676301,0.632947977,0.667630058,xp1)
y<-c(0.400862069,0.417241379,0.419827586,yp1)
grid.Bezier(x,y)
#Reflejo en x
xrx<-c(2*xbase-x[1],2*xbase-x[2],2*xbase-x[3],2*xbase-x[4])
grid.Bezier(xrx,y)
#Reflejo en y
yry<-c(2*yp1-y[1],2*yp1-y[2],2*yp1-y[3],2*yp1-y[4])
grid.Bezier(x,yry)
#Reflejo xy
grid.Bezier(xrx,yry)
#p10 a p2-------------------------------------------------------
x<-c(0.611271676,0.625144509,0.656358382,0.65982659)
y<-c(0.376724138,0.387068966,0.382758621,0.364655172)
grid.Bezier(x,y)
#Reflejo en x
xrx<-c(2*xbase-x[1],2*xbase-x[2],2*xbase-x[3],2*xbase-x[4])
grid.Bezier(xrx,y)
#Reflejo en y
yry<-c(2*yp1-y[1],2*yp1-y[2],2*yp1-y[3],2*yp1-y[4])
grid.Bezier(x,yry)
#Reflejo xy
grid.Bezier(xrx,yry)
#p11 a p3-------------------------------------------------------
x<-c(0.576589595,0.589595376,0.606069364,0.611271676)
y<-c(0.350862069,0.356034483,0.35,0.334482759)
grid.Bezier(x,y)
#Reflejo en x
xrx<-c(2*xbase-x[1],2*xbase-x[2],2*xbase-x[3],2*xbase-x[4])
grid.Bezier(xrx,y)
#Reflejo en y
yry<-c(2*yp1-y[1],2*yp1-y[2],2*yp1-y[3],2*yp1-y[4])
grid.Bezier(x,yry)
#Reflejo xy
grid.Bezier(xrx,yry)
#p12 a p4-------------------------------------------------------
x<-c(0.53150289,0.549710983,0.560115607,0.556647399)
y<-c(0.332758621,0.335344828,0.325862069,0.311206897)
grid.Bezier(x,y)
#Reflejo en x
xrx<-c(2*xbase-x[1],2*xbase-x[2],2*xbase-x[3],2*xbase-x[4])
grid.Bezier(xrx,y)
#Reflejo en y
yry<-c(2*yp1-y[1],2*yp1-y[2],2*yp1-y[3],2*yp1-y[4])
grid.Bezier(x,yry)
#Reflejo xy
grid.Bezier(xrx,yry)
#p13 a p5-------------------------------------------------------
x<-c(0.482080925,0.494219653,0.502023121,0.502023121)
y<-c(0.319827586,0.315517241,0.306896552,0.293965517)
grid.Bezier(x,y)
#Reflejo en x
xrx<-c(2*xbase-x[1],2*xbase-x[2],2*xbase-x[3],2*xbase-x[4])
grid.Bezier(xrx,y)
#Reflejo en y
yry<-c(2*yp1-y[1],2*yp1-y[2],2*yp1-y[3],2*yp1-y[4])
grid.Bezier(x,yry)
#Reflejo xy
grid.Bezier(xrx,yry)
#p14 a p6-------------------------------------------------------
x<-c(0.434393064,0.440462428,0.443930636,0.442196532)
y<-c(0.312068966,0.310344828,0.300862069,0.285344828)
grid.Bezier(x,y)
#Reflejo en x
xrx<-c(2*xbase-x[1],2*xbase-x[2],2*xbase-x[3],2*xbase-x[4])
grid.Bezier(xrx,y)
#Reflejo en y
yry<-c(2*yp1-y[1],2*yp1-y[2],2*yp1-y[3],2*yp1-y[4])
grid.Bezier(x,yry)
#Reflejo xy
grid.Bezier(xrx,yry)
#p15 a p7-------------------------------------------------------
x<-c(0.402312139,0.414450867,0.418786127,0.411849711)
y<-c(0.310344828,0.307758621,0.295689655,0.284482759)
grid.Bezier(x,y)
#Reflejo en x
xrx<-c(2*xbase-x[1],2*xbase-x[2],2*xbase-x[3],2*xbase-x[4])
grid.Bezier(xrx,y)
#Reflejo en y
yry<-c(2*yp1-y[1],2*yp1-y[2],2*yp1-y[3],2*yp1-y[4])
grid.Bezier(x,yry)
#Reflejo xy
grid.Bezier(xrx,yry)
#p8 a p16-------------------------------------------------------
x<-c(0.386705202,0.391040462,0.388439306,0.379768786)
y<-c(0.285344828,0.293965517,0.304310345,0.310344828)
grid.Bezier(x,y)
#Reflejo en x
xrx<-c(2*xbase-x[1],2*xbase-x[2],2*xbase-x[3],2*xbase-x[4])
grid.Bezier(xrx,y)
#Reflejo en y
yry<-c(2*yp1-y[1],2*yp1-y[2],2*yp1-y[3],2*yp1-y[4])
grid.Bezier(x,yry)
#Reflejo xy
grid.Bezier(xrx,yry)
#---------------------------------------------------------------
