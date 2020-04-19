#Bezier implementacion con Volumen
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
#Base a p2--------------------------------
x<-c(xbase,px(746),px(905),px(903))
y<-c(ybase,py(620),py(425),py(254))
grid.Bezier(x,y)
#Reflejo en x
xrx<-c(xbase,2*xbase-x[2],2*xbase-x[3],2*xbase-x[4])
grid.Bezier(xrx,y)
#Base a p3--------------------------------
x<-c(xbase,px(741),px(852),px(847))
y<-c(ybase,py(580),py(427),py(289))
grid.Bezier(x,y)
#Reflejo en x
xrx<-c(xbase,2*xbase-x[2],2*xbase-x[3],2*xbase-x[4])
grid.Bezier(xrx,y)
#Base a p4--------------------------------
x<-c(xbase,px(720),px(780),px(784))
y<-c(ybase,py(529),py(437),py(316))
grid.Bezier(x,y)
#Reflejo en x
xrx<-c(xbase,2*xbase-x[2],2*xbase-x[3],2*xbase-x[4])
grid.Bezier(xrx,y)
#Base a p5--------------------------------
x<-c(xbase,px(694),px(716),px(721))
y<-c(ybase,py(500),py(440),py(336))
grid.Bezier(x,y)
#Reflejo en x
xrx<-c(xbase,2*xbase-x[2],2*xbase-x[3],2*xbase-x[4])
grid.Bezier(xrx,y)
#Base a p6--------------------------------
x<-c(xbase,px(646),px(662),px(652))
y<-c(ybase,py(518),py(444),py(346))
grid.Bezier(x,y)
#Reflejo en x
xrx<-c(xbase,2*xbase-x[2],2*xbase-x[3],2*xbase-x[4])
grid.Bezier(xrx,y)
#Base a p7--------------------------------
x<-c(xbase,px(615),px(624),px(617))
y<-c(ybase,py(527),py(464),py(347))
grid.Bezier(x,y)
#Reflejo en x
xrx<-c(xbase,2*xbase-x[2],2*xbase-x[3],2*xbase-x[4])
grid.Bezier(xrx,y)
#--------------------------------
#Lineas Horizontales--------------------------------
#p24 a p17--------------------------------
x<-c(px(589),px(678),px(931),px(940))
y<-c(py(386),py(400),py(277),py(264))
grid.Bezier(x,y)
#Reflejo en x
xrx<-c(2*xbase-x[1],2*xbase-x[2],2*xbase-x[3],2*xbase-x[4])
grid.Bezier(xrx,y)
#p25 a p18--------------------------------
x<-c(px(588),px(687),px(870),px(934))
y<-c(py(424),py(445),py(379),py(309))
grid.Bezier(x,y)
#Reflejo en x
xrx<-c(2*xbase-x[1],2*xbase-x[2],2*xbase-x[3],2*xbase-x[4])
grid.Bezier(xrx,y)
#p26 a p19--------------------------------
x<-c(px(584),px(646),px(878),px(917))
y<-c(py(467),py(486),py(420),py(373))
grid.Bezier(x,y)
#Reflejo en x
xrx<-c(2*xbase-x[1],2*xbase-x[2],2*xbase-x[3],2*xbase-x[4])
grid.Bezier(xrx,y)
#p27 a p20--------------------------------
x<-c(px(582),px(678),px(850),px(891))
y<-c(py(511),py(524),py(476),py(437))
grid.Bezier(x,y)
#Reflejo en x
xrx<-c(2*xbase-x[1],2*xbase-x[2],2*xbase-x[3],2*xbase-x[4])
grid.Bezier(xrx,y)
#p28 a p21--------------------------------
x<-c(px(579),px(654),px(779),px(870))
y<-c(py(551),py(566),py(539),py(478))
grid.Bezier(x,y)
#Reflejo en x
xrx<-c(2*xbase-x[1],2*xbase-x[2],2*xbase-x[3],2*xbase-x[4])
grid.Bezier(xrx,y)
#p29 a p22--------------------------------
x<-c(px(578),px(715),px(776),px(844))
y<-c(py(580),py(588),py(568),py(515))
grid.Bezier(x,y)
#Reflejo en x
xrx<-c(2*xbase-x[1],2*xbase-x[2],2*xbase-x[3],2*xbase-x[4])
grid.Bezier(xrx,y)
#p30 a p23--------------------------------
x<-c(px(575),px(614),px(702),px(765))
y<-c(py(607),py(623),py(623),py(591))
grid.Bezier(x,y)
#Reflejo en x
xrx<-c(2*xbase-x[1],2*xbase-x[2],2*xbase-x[3],2*xbase-x[4])
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
#p9 a p1--------------------------------
x<-c(px(859),px(872),px(912),xp1)
y<-c(py(212),py(193),py(190),yp1)
grid.Bezier(x,y)
#Reflejo en x
xrx<-c(2*xbase-x[1],2*xbase-x[2],2*xbase-x[3],2*xbase-x[4])
grid.Bezier(xrx,y)
#Reflejo en y
yry<-c(2*yp1-y[1],2*yp1-y[2],2*yp1-y[3],2*yp1-y[4])
grid.Bezier(x,yry)
#Reflejo xy
grid.Bezier(xrx,yry)
#p10 a p2--------------------------------
x<-c(px(847),px(863),px(899),px(903))
y<-c(py(240),py(228),py(233),py(254))
grid.Bezier(x,y)
#Reflejo en x
xrx<-c(2*xbase-x[1],2*xbase-x[2],2*xbase-x[3],2*xbase-x[4])
grid.Bezier(xrx,y)
#Reflejo en y
yry<-c(2*yp1-y[1],2*yp1-y[2],2*yp1-y[3],2*yp1-y[4])
grid.Bezier(x,yry)
#Reflejo xy
grid.Bezier(xrx,yry)
#p11 a p3--------------------------------
x<-c(px(807),px(822),px(841),px(847))
y<-c(py(270),py(264),py(271),py(289))
grid.Bezier(x,y)
#Reflejo en x
xrx<-c(2*xbase-x[1],2*xbase-x[2],2*xbase-x[3],2*xbase-x[4])
grid.Bezier(xrx,y)
#Reflejo en y
yry<-c(2*yp1-y[1],2*yp1-y[2],2*yp1-y[3],2*yp1-y[4])
grid.Bezier(x,yry)
#Reflejo xy
grid.Bezier(xrx,yry)
#p12 a p4--------------------------------
x<-c(px(755),px(776),px(788),px(784))
y<-c(py(291),py(288),py(299),py(316))
grid.Bezier(x,y)
#Reflejo en x
xrx<-c(2*xbase-x[1],2*xbase-x[2],2*xbase-x[3],2*xbase-x[4])
grid.Bezier(xrx,y)
#Reflejo en y
yry<-c(2*yp1-y[1],2*yp1-y[2],2*yp1-y[3],2*yp1-y[4])
grid.Bezier(x,yry)
#Reflejo xy
grid.Bezier(xrx,yry)
#p13 a p5--------------------------------
x<-c(px(698),px(712),px(721),px(721))
y<-c(py(306),py(311),py(321),py(336))
grid.Bezier(x,y)
#Reflejo en x
xrx<-c(2*xbase-x[1],2*xbase-x[2],2*xbase-x[3],2*xbase-x[4])
grid.Bezier(xrx,y)
#Reflejo en y
yry<-c(2*yp1-y[1],2*yp1-y[2],2*yp1-y[3],2*yp1-y[4])
grid.Bezier(x,yry)
#Reflejo xy
grid.Bezier(xrx,yry)
#p14 a p6--------------------------------
x<-c(px(643),px(650),px(654),px(652))
y<-c(py(315),py(317),py(328),py(346))
grid.Bezier(x,y)
#Reflejo en x
xrx<-c(2*xbase-x[1],2*xbase-x[2],2*xbase-x[3],2*xbase-x[4])
grid.Bezier(xrx,y)
#Reflejo en y
yry<-c(2*yp1-y[1],2*yp1-y[2],2*yp1-y[3],2*yp1-y[4])
grid.Bezier(x,yry)
#Reflejo xy
grid.Bezier(xrx,yry)
#p15 a p7--------------------------------
x<-c(px(606),px(620),px(625),px(617))
y<-c(py(317),py(320),py(334),py(347))
grid.Bezier(x,y)
#Reflejo en x
xrx<-c(2*xbase-x[1],2*xbase-x[2],2*xbase-x[3],2*xbase-x[4])
grid.Bezier(xrx,y)
#Reflejo en y
yry<-c(2*yp1-y[1],2*yp1-y[2],2*yp1-y[3],2*yp1-y[4])
grid.Bezier(x,yry)
#Reflejo xy
grid.Bezier(xrx,yry)
#p8 a p16--------------------------------
x<-c(px(588),px(593),px(590),px(580))
y<-c(py(346),py(336),py(324),py(317))
grid.Bezier(x,y)
#Reflejo en x
xrx<-c(2*xbase-x[1],2*xbase-x[2],2*xbase-x[3],2*xbase-x[4])
grid.Bezier(xrx,y)
#Reflejo en y
yry<-c(2*yp1-y[1],2*yp1-y[2],2*yp1-y[3],2*yp1-y[4])
grid.Bezier(x,yry)
#Reflejo xy
grid.Bezier(xrx,yry)