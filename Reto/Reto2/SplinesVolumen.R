#SplinesVolumen implementacion con Volumen
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
            ,580,940,934,917,891,870,844,765
            ,589,588,584,582,579,578,575,900
            ,895,879,848,814,760,682,850,844
            ,823,789,749,703,633,779,772,753
            ,724,680,648,608,714,708
            ,694,672,646,626,600,652,651,646
            ,635,619,606,591,618,618,616,608
            ,602,593,583,905,880,832,776,711
            ,651,618,587)
#Pixeles en y--------------------------------
pixelesy<-c(636,209,254,289,316,336,346,347
            ,346,212,240,270,291,306,315,317
            ,317,264,309,373,437,478,515,591
            ,386,424,467,511,551,580,607,299
            ,336,398,462,511,566,614,331,367
            ,426,485,537,579,617,361,399,449
            ,500,550,580,613,379,416
            ,461,510,554,580,611,386,422,467
            ,511,553,581,611,387,425,467,512
            ,553,579,610,201,240,274,298,317
            ,326,329,330)
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
#Lineas Verticales --------------------------------------------
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
#Base a p2--------------------------------
x<-c(xbase,puntosx[38],puntosx[37],puntosx[36],puntosx[35]
     ,puntosx[34],puntosx[33],puntosx[32],puntosx[3])
y<-c(ybase,puntosy[38],puntosy[37],puntosy[36],puntosy[35]
     ,puntosy[34],puntosy[33],puntosy[32],puntosy[3])
lines(spline(x[1:4],y[1:4]), col = "blue")
linea = spline(y[4:9],x[4:9])
l = linea$x
linea$x = linea$y
linea$y = l
lines(linea, col = "blue")
#Reflejo en x
xrx<-c(2*xbase-x[1],2*xbase-x[2],2*xbase-x[3]
       ,2*xbase-x[4],2*xbase-x[5],2*xbase-x[6]
       ,2*xbase-x[7],2*xbase-x[8],2*xbase-x[9])
lines(spline(xrx[1:4],y[1:4]), col = "blue")
linea = spline(y[4:9],xrx[4:9])
l = linea$x
linea$x = linea$y
linea$y = l
lines(linea, col = "blue")
#Base a p3--------------------------------
x<-c(xbase,puntosx[45],puntosx[44],puntosx[43],puntosx[42]
     ,puntosx[41],puntosx[40],puntosx[39],puntosx[4])
y<-c(ybase,puntosy[45],puntosy[44],puntosy[43],puntosy[42]
     ,puntosy[41],puntosy[40],puntosy[39],puntosy[4])
#lines(spline(x,y), col = "blue")
linea = spline(y,x)
l = linea$x
linea$x = linea$y
linea$y = l
lines(linea, col = "blue")
#Reflejo en x
xrx<-c(2*xbase-x[1],2*xbase-x[2],2*xbase-x[3]
       ,2*xbase-x[4],2*xbase-x[5],2*xbase-x[6]
       ,2*xbase-x[7],2*xbase-x[8],2*xbase-x[9])
linea = spline(y,xrx)
l = linea$x
linea$x = linea$y
linea$y = l
lines(linea, col = "blue")
#Base a p4--------------------------------
x<-c(xbase,puntosx[52],puntosx[51],puntosx[50],puntosx[49]
     ,puntosx[48],puntosx[47],puntosx[46],puntosx[5])
y<-c(ybase,puntosy[52],puntosy[51],puntosy[50],puntosy[49]
     ,puntosy[48],puntosy[47],puntosy[46],puntosy[5])
linea = spline(y,x)
l = linea$x
linea$x = linea$y
linea$y = l
lines(linea, col = "blue")
#Reflejo en x
xrx<-c(2*xbase-x[1],2*xbase-x[2],2*xbase-x[3]
       ,2*xbase-x[4],2*xbase-x[5],2*xbase-x[6]
       ,2*xbase-x[7],2*xbase-x[8],2*xbase-x[9])
linea = spline(y,xrx)
l = linea$x
linea$x = linea$y
linea$y = l
lines(linea, col = "blue")
#Base a p5--------------------------------
x<-c(xbase,puntosx[58],puntosx[57],puntosx[56],puntosx[55]
     ,puntosx[54],puntosx[53],puntosx[52],puntosx[6])
y<-c(ybase,puntosy[58],puntosy[57],puntosy[56],puntosy[55]
     ,puntosy[54],puntosy[53],puntosy[52],puntosy[6])
linea = spline(y,x)
l = linea$x
linea$x = linea$y
linea$y = l
lines(linea, col = "blue")
#Reflejo en x
xrx<-c(2*xbase-x[1],2*xbase-x[2],2*xbase-x[3]
       ,2*xbase-x[4],2*xbase-x[5],2*xbase-x[6]
       ,2*xbase-x[7],2*xbase-x[8],2*xbase-x[9])
linea = spline(y,xrx)
l = linea$x
linea$x = linea$y
linea$y = l
lines(linea, col = "blue")
#Base a p6--------------------------------
x<-c(xbase,puntosx[65],puntosx[64],puntosx[63],puntosx[62]
     ,puntosx[61],puntosx[60],puntosx[59],puntosx[7])
y<-c(ybase,puntosy[65],puntosy[64],puntosy[63],puntosy[62]
     ,puntosy[61],puntosy[60],puntosy[59],puntosy[7])
linea = spline(y,x)
l = linea$x
linea$x = linea$y
linea$y = l
lines(linea, col = "blue")
#Reflejo en x
xrx<-c(2*xbase-x[1],2*xbase-x[2],2*xbase-x[3]
       ,2*xbase-x[4],2*xbase-x[5],2*xbase-x[6]
       ,2*xbase-x[7],2*xbase-x[8],2*xbase-x[9])
linea = spline(y,xrx)
l = linea$x
linea$x = linea$y
linea$y = l
lines(linea, col = "blue")
#Base a p7--------------------------------
x<-c(xbase,puntosx[72],puntosx[71],puntosx[70],puntosx[69]
     ,puntosx[68],puntosx[67],puntosx[66],puntosx[8])
y<-c(ybase,puntosy[72],puntosy[71],puntosy[70],puntosy[69]
     ,puntosy[68],puntosy[67],puntosy[66],puntosy[8])
linea = spline(y,x)
l = linea$x
linea$x = linea$y
linea$y = l
lines(linea, col = "blue")
#Reflejo en x
xrx<-c(2*xbase-x[1],2*xbase-x[2],2*xbase-x[3]
       ,2*xbase-x[4],2*xbase-x[5],2*xbase-x[6]
       ,2*xbase-x[7],2*xbase-x[8],2*xbase-x[9])
linea = spline(y,xrx)
l = linea$x
linea$x = linea$y
linea$y = l
lines(linea, col = "blue")
#--------------------------------
#Lineas Horizontales--------------------------------
#p17 a p24--------------------------------
x<-c(puntosx[18],puntosx[32],puntosx[39],puntosx[46]
     ,puntosx[53],puntosx[60],puntosx[67],puntosx[25])
y<-c(puntosy[18],puntosy[32],puntosy[39],puntosy[46]
     ,puntosy[53],puntosy[60],puntosy[67],puntosy[25])
lines(spline(x,y), col = "blue")
#Reflejo en x
xrx<-c(2*xbase-x[1],2*xbase-x[2],2*xbase-x[3]
       ,2*xbase-x[4],2*xbase-x[5],2*xbase-x[6]
       ,2*xbase-x[7],2*xbase-x[8])
lines(spline(xrx,y), col = "blue")
#p18 a p25--------------------------------
x<-c(puntosx[19],puntosx[33],puntosx[40],puntosx[47]
     ,puntosx[54],puntosx[61],puntosx[68],puntosx[26])
y<-c(puntosy[19],puntosy[33],puntosy[40],puntosy[47]
     ,puntosy[54],puntosy[61],puntosy[68],puntosy[26])
lines(spline(x,y), col = "blue")
#Reflejo en x
xrx<-c(2*xbase-x[1],2*xbase-x[2],2*xbase-x[3]
       ,2*xbase-x[4],2*xbase-x[5],2*xbase-x[6]
       ,2*xbase-x[7],2*xbase-x[8])
lines(spline(xrx,y), col = "blue")
#p19 a p26--------------------------------
x<-c(puntosx[20],puntosx[34],puntosx[41],puntosx[48]
     ,puntosx[55],puntosx[62],puntosx[69],puntosx[27])
y<-c(puntosy[20],puntosy[34],puntosy[41],puntosy[48]
     ,puntosy[55],puntosy[62],puntosy[69],puntosy[27])
lines(spline(x,y), col = "blue")
#Reflejo en x
xrx<-c(2*xbase-x[1],2*xbase-x[2],2*xbase-x[3]
       ,2*xbase-x[4],2*xbase-x[5],2*xbase-x[6]
       ,2*xbase-x[7],2*xbase-x[8])
lines(spline(xrx,y), col = "blue")
#p20 a p27--------------------------------
x<-c(puntosx[21],puntosx[35],puntosx[42],puntosx[49]
     ,puntosx[56],puntosx[63],puntosx[70],puntosx[28])
y<-c(puntosy[21],puntosy[35],puntosy[42],puntosy[49]
     ,puntosy[56],puntosy[63],puntosy[70],puntosy[28])
lines(spline(x,y), col = "blue")
#Reflejo en x
xrx<-c(2*xbase-x[1],2*xbase-x[2],2*xbase-x[3]
       ,2*xbase-x[4],2*xbase-x[5],2*xbase-x[6]
       ,2*xbase-x[7],2*xbase-x[8])
lines(spline(xrx,y), col = "blue")
#p21 a p28--------------------------------
x<-c(puntosx[22],puntosx[36],puntosx[43]
     ,puntosx[50],puntosx[57]
     ,puntosx[64],puntosx[71],puntosx[29])
y<-c(puntosy[22],puntosy[36],puntosy[43]
     ,puntosy[50],puntosy[57]
     ,puntosy[64],puntosy[71],puntosy[29])
lines(spline(x,y), col = "blue")
#Reflejo en x
xrx<-c(2*xbase-x[1],2*xbase-x[2],2*xbase-x[3]
       ,2*xbase-x[4],2*xbase-x[5],2*xbase-x[6]
       ,2*xbase-x[7],2*xbase-x[8])
lines(spline(xrx,y), col = "blue")
#p22 a p29--------------------------------
x<-c(puntosx[23],puntosx[37],puntosx[44]
     ,puntosx[51],puntosx[58]
     ,puntosx[65],puntosx[72],puntosx[30])
y<-c(puntosy[23],puntosy[37],puntosy[44]
     ,puntosy[51],puntosy[58]
     ,puntosy[65],puntosy[72],puntosy[30])
lines(spline(x,y), col = "blue")
#Reflejo en x
xrx<-c(2*xbase-x[1],2*xbase-x[2],2*xbase-x[3]
       ,2*xbase-x[4],2*xbase-x[5],2*xbase-x[6]
       ,2*xbase-x[7],2*xbase-x[8])
lines(spline(xrx,y), col = "blue")
#p23 a p30--------------------------------
x<-c(puntosx[24],puntosx[38],puntosx[45]
     ,puntosx[52],puntosx[59]
     ,puntosx[66],puntosx[73],puntosx[31])
y<-c(puntosy[24],puntosy[38],puntosy[45]
     ,puntosy[52],puntosy[59]
     ,puntosy[66],puntosy[73],puntosy[31])
lines(spline(x,y), col = "blue")
#Reflejo en x
xrx<-c(2*xbase-x[1],2*xbase-x[2],2*xbase-x[3]
       ,2*xbase-x[4],2*xbase-x[5],2*xbase-x[6]
       ,2*xbase-x[7],2*xbase-x[8])
lines(spline(xrx,y), col = "blue")
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
x<-c(puntosx[10],puntosx[11],puntosx[12]
     ,puntosx[13],puntosx[14],puntosx[15]
     ,puntosx[16],puntosx[17])
y<-c(puntosy[10],puntosy[11],puntosy[12]
     ,puntosy[13],puntosy[14],puntosy[15]
     ,puntosy[16],puntosy[17])
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
#p1 a p9--------------------------------
x<-c(puntosx[2],puntosx[74],puntosx[10])
y<-c(puntosy[2],puntosy[74],puntosy[10])
lines(spline(x,y), col = "blue")
#Reflejo en x
xrx<-c(2*xbase-x[1],2*xbase-x[2],2*xbase-x[3])
lines(spline(xrx,y), col = "blue")
#Reflejo en y
yry<-c(2*yp1-y[1],2*yp1-y[2],2*yp1-y[3])
lines(spline(x,yry), col = "blue")
#Reflejo xy
lines(spline(xrx,yry), col = "blue")
#p2 a p10--------------------------------
x<-c(puntosx[3],puntosx[75],puntosx[11])
y<-c(puntosy[3],puntosy[75],puntosy[11])
lines(spline(x,y), col = "blue")
#Reflejo en x
xrx<-c(2*xbase-x[1],2*xbase-x[2],2*xbase-x[3])
lines(spline(xrx,y), col = "blue")
#Reflejo en y
yry<-c(2*yp1-y[1],2*yp1-y[2],2*yp1-y[3])
lines(spline(x,yry), col = "blue")
#Reflejo xy
lines(spline(xrx,yry), col = "blue")
#p3 a p11--------------------------------
x<-c(puntosx[4],puntosx[76],puntosx[12])
y<-c(puntosy[4],puntosy[76],puntosy[12])
lines(spline(x,y), col = "blue")
#Reflejo en x
xrx<-c(2*xbase-x[1],2*xbase-x[2],2*xbase-x[3])
lines(spline(xrx,y), col = "blue")
#Reflejo en y
yry<-c(2*yp1-y[1],2*yp1-y[2],2*yp1-y[3])
lines(spline(x,yry), col = "blue")
#Reflejo xy
lines(spline(xrx,yry), col = "blue")
#p4 a p12--------------------------------
x<-c(puntosx[5],puntosx[77],puntosx[13])
y<-c(puntosy[5],puntosy[77],puntosy[13])
lines(spline(x,y), col = "blue")
#Reflejo en x
xrx<-c(2*xbase-x[1],2*xbase-x[2],2*xbase-x[3])
lines(spline(xrx,y), col = "blue")
#Reflejo en y
yry<-c(2*yp1-y[1],2*yp1-y[2],2*yp1-y[3])
lines(spline(x,yry), col = "blue")
#Reflejo xy
lines(spline(xrx,yry), col = "blue")
#p5 a p13--------------------------------
x<-c(puntosx[6],puntosx[78],puntosx[14])
y<-c(puntosy[6],puntosy[78],puntosy[14])
lines(spline(x,y), col = "blue")
#Reflejo en x
xrx<-c(2*xbase-x[1],2*xbase-x[2],2*xbase-x[3])
lines(spline(xrx,y), col = "blue")
#Reflejo en y
yry<-c(2*yp1-y[1],2*yp1-y[2],2*yp1-y[3])
lines(spline(x,yry), col = "blue")
#Reflejo xy
lines(spline(xrx,yry), col = "blue")
#p6 a p14--------------------------------
x<-c(puntosx[7],puntosx[79],puntosx[15])
y<-c(puntosy[7],puntosy[79],puntosy[15])
linea = spline(y,x)
l = linea$x
linea$x = linea$y
linea$y = l
lines(linea, col = "blue")
#Reflejo en x
xrx<-c(2*xbase-x[1],2*xbase-x[2],2*xbase-x[3])
linea = spline(y,xrx)
l = linea$x
linea$x = linea$y
linea$y = l
lines(linea, col = "blue")
#Reflejo en y
yry<-c(2*yp1-y[1],2*yp1-y[2],2*yp1-y[3])
linea = spline(yry,x)
l = linea$x
linea$x = linea$y
linea$y = l
lines(linea, col = "blue")
#Reflejo xy
linea = spline(yry,xrx)
l = linea$x
linea$x = linea$y
linea$y = l
lines(linea, col = "blue")
#p7 a p15--------------------------------
x<-c(puntosx[8],puntosx[80],puntosx[16])
y<-c(puntosy[8],puntosy[80],puntosy[16])
linea = spline(y,x)
l = linea$x
linea$x = linea$y
linea$y = l
lines(linea, col = "blue")
#Reflejo en x
xrx<-c(2*xbase-x[1],2*xbase-x[2],2*xbase-x[3])
linea = spline(y,xrx)
l = linea$x
linea$x = linea$y
linea$y = l
lines(linea, col = "blue")
#Reflejo en y
yry<-c(2*yp1-y[1],2*yp1-y[2],2*yp1-y[3])
linea = spline(yry,x)
l = linea$x
linea$x = linea$y
linea$y = l
lines(linea, col = "blue")
#Reflejo xy
linea = spline(yry,xrx)
l = linea$x
linea$x = linea$y
linea$y = l
lines(linea, col = "blue")
#p8 a p16--------------------------------
x<-c(puntosx[9],puntosx[81],puntosx[17])
y<-c(puntosy[9],puntosy[81],puntosy[17])
linea = spline(y,x)
l = linea$x
linea$x = linea$y
linea$y = l
lines(linea, col = "blue")
#Reflejo en x
xrx<-c(2*xbase-x[1],2*xbase-x[2],2*xbase-x[3])
linea = spline(y,xrx)
l = linea$x
linea$x = linea$y
linea$y = l
lines(linea, col = "blue")
#Reflejo en y
yry<-c(2*yp1-y[1],2*yp1-y[2],2*yp1-y[3])
linea = spline(yry,x)
l = linea$x
linea$x = linea$y
linea$y = l
lines(linea, col = "blue")
#Reflejo xy
linea = spline(yry,xrx)
l = linea$x
linea$x = linea$y
linea$y = l
lines(linea, col = "blue")