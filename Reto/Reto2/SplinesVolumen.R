rm(list=ls())
options(digits=16)
#Interpolacion Mortero por Splines
#x------------------------------------------------------------------
puntosx<-c(0.379768786,0.696242775,0.65982659,0.611271676
           ,0.556647399,0.502023121,0.442196532,0.411849711
           ,0.386705202,0.621676301,0.611271676,0.576589595
           ,0.53150289,0.482080925,0.434393064,0.402312139
           ,0.379768786,0.691907514,0.686705202,0.671965318
           ,0.649421965,0.631213873,0.60867052,0.54017341 
           ,0.387572254,0.386705202,0.383236994,0.38150289
           ,0.378901734,0.378034682,0.375433526,0.657225434
           ,0.652890173,0.639017341,0.612138728,0.58265896
           ,0.53583815,0.468208092,0.613872832,0.60867052
           ,0.590462428,0.560982659,0.526300578,0.486416185
           ,0.425722543,0.552312139,0.546242775,0.529768786
           ,0.504624277,0.466473988,0.438728324,0.404046243
           ,0.495953757,0.490751445,0.478612717,0.459537572
           ,0.43699422,0.419653179,0.397109827,0.442196532
           ,0.44132948,0.43699422,0.427456647,0.413583815
           ,0.402312139,0.389306358,0.412716763,0.412716763
           ,0.410982659,0.404046243,0.398843931,0.391040462
           ,0.382369942,0.661560694,0.639884393,0.598265896
           ,0.549710983,0.493352601,0.44132948,0.412716763
           ,0.38583815)
#y---------------------------------------------------------                                                            
puntosy<-c(0.035344828,0.403448276,0.364655172,0.334482759
           ,0.311206897,0.293965517,0.285344828,0.284482759
           ,0.285344828,0.400862069,0.376724138,0.350862069
           ,0.332758621,0.319827586,0.312068966,0.310344828
           ,0.310344828,0.356034483,0.317241379,0.262068966
           ,0.206896552,0.171551724,0.139655172,0.074137931
           ,0.250862069,0.218103448,0.181034483,0.143103448
           ,0.10862069,0.08362069,0.060344828,0.325862069
           ,0.293965517,0.240517241,0.185344828,0.143103448
           ,0.095689655,0.054310345,0.298275862,0.267241379
           ,0.21637931,0.165517241,0.120689655,0.084482759
           ,0.051724138,0.272413793,0.239655172,0.196551724
           ,0.152586207,0.109482759,0.08362069,0.055172414
           ,0.256896552,0.225,0.186206897,0.143965517
           ,0.106034483,0.08362069,0.056896552,0.250862069
           ,0.219827586,0.181034483,0.143103448,0.106896552
           ,0.082758621,0.056896552,0.25,0.217241379
           ,0.181034483,0.142241379,0.106896552,0.084482759
           ,0.057758621,0.410344828,0.376724138,0.347413793
           ,0.326724138,0.310344828,0.302586207,0.3
           ,0.299137931)
           
                                                         
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
#Base a p2-----------------------------------------------------
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
xrx<-c(2*xbase-x[1],2*xbase-x[2],2*xbase-x[3],2*xbase-x[4],2*xbase-x[5],2*xbase-x[6],2*xbase-x[7],2*xbase-x[8],2*xbase-x[9])
lines(spline(xrx[1:4],y[1:4]), col = "blue")
linea = spline(y[4:9],xrx[4:9])
l = linea$x
linea$x = linea$y
linea$y = l
lines(linea, col = "blue")
#Base a p3-----------------------------------------------------
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
xrx<-c(2*xbase-x[1],2*xbase-x[2],2*xbase-x[3],2*xbase-x[4],2*xbase-x[5],2*xbase-x[6],2*xbase-x[7],2*xbase-x[8],2*xbase-x[9])
#lines(spline(xrx,y), col = "blue")
linea = spline(y,xrx)
l = linea$x
linea$x = linea$y
linea$y = l
lines(linea, col = "blue")
#Base a p4-----------------------------------------------------
x<-c(xbase,puntosx[52],puntosx[51],puntosx[50],puntosx[49]
     ,puntosx[48],puntosx[47],puntosx[46],puntosx[5])
y<-c(ybase,puntosy[52],puntosy[51],puntosy[50],puntosy[49]
     ,puntosy[48],puntosy[47],puntosy[46],puntosy[5])
#lines(spline(x,y), col = "blue")
linea = spline(y,x)
l = linea$x
linea$x = linea$y
linea$y = l
lines(linea, col = "blue")
#Reflejo en x
xrx<-c(2*xbase-x[1],2*xbase-x[2],2*xbase-x[3],2*xbase-x[4],2*xbase-x[5],2*xbase-x[6],2*xbase-x[7],2*xbase-x[8],2*xbase-x[9])
#lines(spline(xrx,y), col = "blue")
linea = spline(y,xrx)
l = linea$x
linea$x = linea$y
linea$y = l
lines(linea, col = "blue")
#Base a p5-----------------------------------------------------
x<-c(xbase,puntosx[58],puntosx[57],puntosx[56],puntosx[55]
     ,puntosx[54],puntosx[53],puntosx[52],puntosx[6])
y<-c(ybase,puntosy[58],puntosy[57],puntosy[56],puntosy[55]
     ,puntosy[54],puntosy[53],puntosy[52],puntosy[6])
#lines(spline(x,y), col = "blue")
linea = spline(y,x)
l = linea$x
linea$x = linea$y
linea$y = l
lines(linea, col = "blue")
#Reflejo en x
xrx<-c(2*xbase-x[1],2*xbase-x[2],2*xbase-x[3],2*xbase-x[4],2*xbase-x[5],2*xbase-x[6],2*xbase-x[7],2*xbase-x[8],2*xbase-x[9])
#lines(spline(xrx,y), col = "blue")
linea = spline(y,xrx)
l = linea$x
linea$x = linea$y
linea$y = l
lines(linea, col = "blue")
#Base a p6-----------------------------------------------------
x<-c(xbase,puntosx[65],puntosx[64],puntosx[63],puntosx[62]
     ,puntosx[61],puntosx[60],puntosx[59],puntosx[7])
y<-c(ybase,puntosy[65],puntosy[64],puntosy[63],puntosy[62]
     ,puntosy[61],puntosy[60],puntosy[59],puntosy[7])
#lines(spline(x,y), col = "blue")
linea = spline(y,x)
l = linea$x
linea$x = linea$y
linea$y = l
lines(linea, col = "blue")
#Reflejo en x
xrx<-c(2*xbase-x[1],2*xbase-x[2],2*xbase-x[3],2*xbase-x[4],2*xbase-x[5],2*xbase-x[6],2*xbase-x[7],2*xbase-x[8],2*xbase-x[9])
#lines(spline(xrx,y), col = "blue")
linea = spline(y,xrx)
l = linea$x
linea$x = linea$y
linea$y = l
lines(linea, col = "blue")
#Base a p7-----------------------------------------------------
x<-c(xbase,puntosx[72],puntosx[71],puntosx[70],puntosx[69]
     ,puntosx[68],puntosx[67],puntosx[66],puntosx[8])
y<-c(ybase,puntosy[72],puntosy[71],puntosy[70],puntosy[69]
     ,puntosy[68],puntosy[67],puntosy[66],puntosy[8])
#lines(spline(x,y), col = "blue")
linea = spline(y,x)
l = linea$x
linea$x = linea$y
linea$y = l
lines(linea, col = "blue")
#Reflejo en x
xrx<-c(2*xbase-x[1],2*xbase-x[2],2*xbase-x[3],2*xbase-x[4],2*xbase-x[5],2*xbase-x[6],2*xbase-x[7],2*xbase-x[8],2*xbase-x[9])
#lines(spline(xrx,y), col = "blue")
linea = spline(y,xrx)
l = linea$x
linea$x = linea$y
linea$y = l
lines(linea, col = "blue")
#--------------------------------------------------------------
#Lineas Horizontales-------------------------------------------
#p17 a p24-----------------------------------------------------
x<-c(puntosx[18],puntosx[32],puntosx[39],puntosx[46],puntosx[53]
     ,puntosx[60],puntosx[67],puntosx[25])
y<-c(puntosy[18],puntosy[32],puntosy[39],puntosy[46],puntosy[53]
     ,puntosy[60],puntosy[67],puntosy[25])
lines(spline(x,y), col = "blue")
#linea = spline(y,x)
#l = linea$x
#linea$x = linea$y
#linea$y = l
#lines(linea, col = "blue")
#Reflejo en x
xrx<-c(2*xbase-x[1],2*xbase-x[2],2*xbase-x[3],2*xbase-x[4],2*xbase-x[5],2*xbase-x[6],2*xbase-x[7],2*xbase-x[8])
lines(spline(xrx,y), col = "blue")
#linea = spline(y,xrx)
#l = linea$x
#linea$x = linea$y
#linea$y = l
#lines(linea, col = "blue")
#p18 a p25-----------------------------------------------------
x<-c(puntosx[19],puntosx[33],puntosx[40],puntosx[47],puntosx[54]
     ,puntosx[61],puntosx[68],puntosx[26])
y<-c(puntosy[19],puntosy[33],puntosy[40],puntosy[47],puntosy[54]
     ,puntosy[61],puntosy[68],puntosy[26])
lines(spline(x,y), col = "blue")
#linea = spline(y,x)
#l = linea$x
#linea$x = linea$y
#linea$y = l
#lines(linea, col = "blue")
#Reflejo en x
xrx<-c(2*xbase-x[1],2*xbase-x[2],2*xbase-x[3],2*xbase-x[4],2*xbase-x[5],2*xbase-x[6],2*xbase-x[7],2*xbase-x[8])
lines(spline(xrx,y), col = "blue")
#linea = spline(y,xrx)
#l = linea$x
#linea$x = linea$y
#linea$y = l
#lines(linea, col = "blue")
#p19 a p26-----------------------------------------------------
x<-c(puntosx[20],puntosx[34],puntosx[41],puntosx[48],puntosx[55]
     ,puntosx[62],puntosx[69],puntosx[27])
y<-c(puntosy[20],puntosy[34],puntosy[41],puntosy[48],puntosy[55]
     ,puntosy[62],puntosy[69],puntosy[27])
lines(spline(x,y), col = "blue")
#linea = spline(y,x)
#l = linea$x
#linea$x = linea$y
#linea$y = l
#lines(linea, col = "blue")
#Reflejo en x
xrx<-c(2*xbase-x[1],2*xbase-x[2],2*xbase-x[3],2*xbase-x[4],2*xbase-x[5],2*xbase-x[6],2*xbase-x[7],2*xbase-x[8])
lines(spline(xrx,y), col = "blue")
#linea = spline(y,xrx)
#l = linea$x
#linea$x = linea$y
#linea$y = l
#lines(linea, col = "blue")
#p20 a p27-----------------------------------------------------
x<-c(puntosx[21],puntosx[35],puntosx[42],puntosx[49],puntosx[56]
     ,puntosx[63],puntosx[70],puntosx[28])
y<-c(puntosy[21],puntosy[35],puntosy[42],puntosy[49],puntosy[56]
     ,puntosy[63],puntosy[70],puntosy[28])
lines(spline(x,y), col = "blue")
#linea = spline(y,x)
#l = linea$x
#linea$x = linea$y
#linea$y = l
#lines(linea, col = "blue")
#Reflejo en x
xrx<-c(2*xbase-x[1],2*xbase-x[2],2*xbase-x[3],2*xbase-x[4],2*xbase-x[5],2*xbase-x[6],2*xbase-x[7],2*xbase-x[8])
lines(spline(xrx,y), col = "blue")
#linea = spline(y,xrx)
#l = linea$x
#linea$x = linea$y
#linea$y = l
#lines(linea, col = "blue")
#p21 a p28-----------------------------------------------------
x<-c(puntosx[22],puntosx[36],puntosx[43],puntosx[50],puntosx[57]
     ,puntosx[64],puntosx[71],puntosx[29])
y<-c(puntosy[22],puntosy[36],puntosy[43],puntosy[50],puntosy[57]
     ,puntosy[64],puntosy[71],puntosy[29])
lines(spline(x,y), col = "blue")
#linea = spline(y,x)
#l = linea$x
#linea$x = linea$y
#linea$y = l
#lines(linea, col = "blue")
#Reflejo en x
xrx<-c(2*xbase-x[1],2*xbase-x[2],2*xbase-x[3],2*xbase-x[4],2*xbase-x[5],2*xbase-x[6],2*xbase-x[7],2*xbase-x[8])
lines(spline(xrx,y), col = "blue")
#linea = spline(y,xrx)
#l = linea$x
#linea$x = linea$y
#linea$y = l
#lines(linea, col = "blue")
#p22 a p29-----------------------------------------------------
x<-c(puntosx[23],puntosx[37],puntosx[44],puntosx[51],puntosx[58]
     ,puntosx[65],puntosx[72],puntosx[30])
y<-c(puntosy[23],puntosy[37],puntosy[44],puntosy[51],puntosy[58]
     ,puntosy[65],puntosy[72],puntosy[30])
lines(spline(x,y), col = "blue")
#linea = spline(y,x)
#l = linea$x
#linea$x = linea$y
#linea$y = l
#lines(linea, col = "blue")
#Reflejo en x
xrx<-c(2*xbase-x[1],2*xbase-x[2],2*xbase-x[3],2*xbase-x[4],2*xbase-x[5],2*xbase-x[6],2*xbase-x[7],2*xbase-x[8])
lines(spline(xrx,y), col = "blue")
#linea = spline(y,xrx)
#l = linea$x
#linea$x = linea$y
#linea$y = l
#lines(linea, col = "blue")
#p23 a p30-----------------------------------------------------
x<-c(puntosx[24],puntosx[38],puntosx[45],puntosx[52],puntosx[59]
     ,puntosx[66],puntosx[73],puntosx[31])
y<-c(puntosy[24],puntosy[38],puntosy[45],puntosy[52],puntosy[59]
     ,puntosy[66],puntosy[73],puntosy[31])
lines(spline(x,y), col = "blue")
#linea = spline(y,x)
#l = linea$x
#linea$x = linea$y
#linea$y = l
#lines(linea, col = "blue")
#Reflejo en x
xrx<-c(2*xbase-x[1],2*xbase-x[2],2*xbase-x[3],2*xbase-x[4],2*xbase-x[5],2*xbase-x[6],2*xbase-x[7],2*xbase-x[8])
lines(spline(xrx,y), col = "blue")
#linea = spline(y,xrx)
#l = linea$x
#linea$x = linea$y
#linea$y = l
#lines(linea, col = "blue")
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
#p1 a p9-------------------------------------------------------
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
#p2 a p10-------------------------------------------------------
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
#p3 a p11-------------------------------------------------------
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
#p4 a p12-------------------------------------------------------
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
#p5 a p13-------------------------------------------------------
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
#p6 a p14-------------------------------------------------------
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
#p7 a p15-------------------------------------------------------
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
#p8 a p16-------------------------------------------------------
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