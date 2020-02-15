#Silueta Perro 
rm(list=ls())

x = c(0.5, 1.1, 5.4, 7.5, 10.6, 16, 17.8, 21, 24, 27, 30, 32, 31.2, 30, 27.5, 26.5, 24.7, 22, 20.5, 18, 14, 12.5, 11.5, 9.55, 8.5, 9, 8.8, 6.96, 3.3, 0.5)
y = c(2.1, 3, 3.8, 05.5, 7.5, 6, 5.2, 7.2, 6.9, 5.4, 5.2, 4.3, 2.5, 1.50, 0.95, 00.55, 00.52, 1, 00.90, 0.97, 0.9, 1, 1.4, 1, 1.5, 2.5, 2.67, 2.2, 2.1, 2.1)
y_1= y[1:7]; x_1 = x[1:7]
y_2= y[7:12]; x_2 = x[7:12]
y_3= y[12:14]; x_3 = x[12:14]
y_4= y[14:15]; x_4 = x[14:15]
y_5= y[15:18]; x_5 = x[15:18]
y_6= y[18:20]; x_6 = x[18:20]
y_7= y[20:25]; x_7 = x[20:25]
y_8= y[25:27];  x_8 = x[25:27]
y_9= y[27:28];  x_9 = x[27:28]
y_10= y[28:30]; x_10 = x[28:30]

num_puntos = length(y)
color = 1
linea = spline(y_3,x_3)
lin = linea$x
linea$x = linea$y
linea$y = lin
plot(x, y, main = paste("Silueta Perrito: ", num_puntos, "puntos"),xlim=c(0,35),ylim=c(0,10))
lines(spline(x_1, y_1), col = color)
lines(spline(x_2, y_2), col = color)
lines(spline(x_4, y_4), col = color)
lines(spline(x_5,y_5), col = color)
lines(spline(x_6, y_6), col = color)
lines(spline(x_7, y_7), col = color)
lines(spline(x_8, y_8), col = color)
lines(spline(x_9, y_9), col = color)
lines(spline(x_10, y_10), col = color)
lines(linea, col = color)