rm(list=ls())
options(digits=16)
#Interpolacion Mortero por Splines
#x------------------------------------------------------------------
puntosx<-c(0.379768786,0.696242775,0.65982659,0.611271676
           ,0.556647399,0.502023121,0.442196532,0.411849711
           ,0.386705202,0.621676301,0.611271676,0.576589595
           ,0.53150289,0.482080925,0.434393064,0.402312139
           ,0.379768786,0.691907514,0.686705202,0.671965318
           ,0.649421965,0.631213873,0.60867052,0.54017341)

puntosy<-c(0.035344828,0.403448276,0.364655172,0.334482759
           ,0.311206897,0.293965517,0.285344828,0.284482759
           ,0.285344828,0.400862069,0.376724138,0.350862069
           ,0.332758621,0.319827586,0.312068966,0.310344828
           ,0.310344828,0.356034483,0.317241379,0.262068966
           ,0.206896552,0.171551724,0.139655172,0.074137931)

plot(puntosx,puntosy, main=paste("Mortero"),xlim=c(0,0.75),ylim=c(0,0.55),col="red")
#Punto Base----------------------------------------------------
xbase<-0.379768786
ybase<-0.035344828
#Punto p1
xp1<-0.696242775
yp1<-0.403448276
#Lineas Verticales --------------------------------------------
#Base a p1-----------------------------------------------------
x<-c(xbase,puntosx[18],puntosx[19],puntosx[20],puntosx[21],puntosx[22],puntosx[23],puntosx[24],xp1)
y<-c(ybase,puntosy[18],puntosy[19],puntosy[20],puntosy[21],puntosy[22],puntosy[23],puntosy[24],yp1)
lines(spline(x, y), col = "blue")
#Reflejo en x
xrx<-c(xbase,2*xbase-x[2],2*xbase-x[3],2*xbase-x[4],2*xbase-x[5],2*xbase-x[6],2*xbase-x[7],2*xbase-x[8],2*xbase-x[9])
lines(spline(xrx,y), col = "blue")
#--------------------------------------------------------------
#Borde superior-------------------------------------------------
#p8 a p1--------------------------------------------------------
x<-c(puntosx[2],puntosx[3],puntosx[4],puntosx[5],puntosx[6],puntosx[7],puntosx[8],puntosx[9])
y<-c(puntosy[2],puntosy[3],puntosy[4],puntosy[5],puntosy[6],puntosy[7],puntosy[8],puntosy[9])
lines(spline(x,y), col = "blue")
#Reflejo en x
xrx<-c(2*xbase-x[1],2*xbase-x[2],2*xbase-x[3],2*xbase-x[4],2*xbase-x[5],2*xbase-x[6],2*xbase-x[7],2*xbase-x[8])
lines(spline(xrx,y), col = "blue")
#Reflejo en y
yry<-c(2*yp1-y[1],2*yp1-y[2],2*yp1-y[3],2*yp1-y[4],2*yp1-y[5],2*yp1-y[6],2*yp1-y[7],2*yp1-y[8])
lines(spline(x,yry), col = "blue")
#Reflejo xy
lines(spline(xrx,yry), col = "blue")
#p16 a p9-------------------------------------------------------
x<-c(puntosx[10],puntosx[11],puntosx[12],puntosx[13],puntosx[14],puntosx[15],puntosx[16],puntosx[17])
y<-c(puntosy[10],puntosy[11],puntosy[12],puntosy[13],puntosy[14],puntosy[15],puntosy[16],puntosy[17])
lines(spline(x,y), col = "blue")
#Reflejo en x
xrx<-c(2*xbase-x[1],2*xbase-x[2],2*xbase-x[3],2*xbase-x[4],2*xbase-x[5],2*xbase-x[6],2*xbase-x[7],2*xbase-x[8])
lines(spline(xrx,y), col = "blue")
#Reflejo en y
yry<-c(2*yp1-y[1],2*yp1-y[2],2*yp1-y[3],2*yp1-y[4],2*yp1-y[5],2*yp1-y[6],2*yp1-y[7],2*yp1-y[8])
lines(spline(x,yry), col = "blue")
#Reflejo xy
lines(spline(xrx,yry), col = "blue")