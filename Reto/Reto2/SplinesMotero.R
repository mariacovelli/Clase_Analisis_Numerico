#SplinesMortero implementacion sencilla
rm(list=ls())
options(digits=16)
#Interpolacion Mortero por Splines
#Funciones--------------------------------
pixelx
pixely
px<-function(pixelx){
  (pixelx - 142)*0.75/865
}
py<-function(pixely){
  (677 - pixely)*0.55/638
}

#Pixeles en x--------------------------------
pixelesx<-c(580,945,903,847,784,721,652,617
            ,588,859,847,807,755,698,643,606
            ,580,940,934,917,891,870,844,765)
#Pixeles en y--------------------------------
pixelesy<-c(636,209,254,289,316,336,346,347
            ,346,212,240,270,291,306,315,317
            ,317,264,309,373,437,478,515,591)
#x------------------------------------------
puntosx<-px(pixelesx)
#y-----------------------------------------
puntosy<-py(pixelesy)
#Grafica base
plot(puntosx,puntosy, main=paste("Mortero")
  ,xlim=c(0,0.75),ylim=c(0,0.55),col="red")
#Punto Base--------------------------------
xbase<-px(580)
ybase<-py(636)
#Punto p1
xp1<-px(945)
yp1<-py(209)
#Lineas Verticales --------------------------------
#Base a p1--------------------------------
x<-c(xbase,puntosx[18],puntosx[19],puntosx[20]
     ,puntosx[21],puntosx[22],puntosx[23]
     ,puntosx[24],xp1)
y<-c(ybase,puntosy[18],puntosy[19],puntosy[20]
     ,puntosy[21],puntosy[22],puntosy[23]
     ,puntosy[24],yp1)
lines(spline(x, y), col = "blue")
#Reflejo en x
xrx<-c(xbase,2*xbase-x[2],2*xbase-x[3],2*xbase-x[4]
       ,2*xbase-x[5],2*xbase-x[6],2*xbase-x[7]
       ,2*xbase-x[8],2*xbase-x[9])
lines(spline(xrx,y), col = "blue")
#--------------------------------
#Borde superior--------------------------------
#p8 a p1--------------------------------
x<-c(puntosx[2],puntosx[3],puntosx[4],puntosx[5]
     ,puntosx[6],puntosx[7],puntosx[8],puntosx[9])
y<-c(puntosy[2],puntosy[3],puntosy[4],puntosy[5]
     ,puntosy[6],puntosy[7],puntosy[8],puntosy[9])
lines(spline(x,y), col = "blue")
#Reflejo en x
xrx<-c(2*xbase-x[1],2*xbase-x[2],2*xbase-x[3]
       ,2*xbase-x[4],2*xbase-x[5],2*xbase-x[6]
       ,2*xbase-x[7],2*xbase-x[8])
lines(spline(xrx,y), col = "blue")
#Reflejo en y
yry<-c(2*yp1-y[1],2*yp1-y[2],2*yp1-y[3],2*yp1-y[4]
       ,2*yp1-y[5],2*yp1-y[6],2*yp1-y[7],2*yp1-y[8])
lines(spline(x,yry), col = "blue")
#Reflejo xy
lines(spline(xrx,yry), col = "blue")
#p16 a p9--------------------------------
x<-c(puntosx[10],puntosx[11],puntosx[12],puntosx[13]
     ,puntosx[14],puntosx[15],puntosx[16],puntosx[17])
y<-c(puntosy[10],puntosy[11],puntosy[12],puntosy[13]
     ,puntosy[14],puntosy[15],puntosy[16],puntosy[17])
lines(spline(x,y), col = "blue")
#Reflejo en x
xrx<-c(2*xbase-x[1],2*xbase-x[2],2*xbase-x[3],2*xbase-x[4]
       ,2*xbase-x[5],2*xbase-x[6],2*xbase-x[7],2*xbase-x[8])
lines(spline(xrx,y), col = "blue")
#Reflejo en y
yry<-c(2*yp1-y[1],2*yp1-y[2],2*yp1-y[3],2*yp1-y[4],2*yp1-y[5]
       ,2*yp1-y[6],2*yp1-y[7],2*yp1-y[8])
lines(spline(x,yry), col = "blue")
#Reflejo xy
lines(spline(xrx,yry), col = "blue")