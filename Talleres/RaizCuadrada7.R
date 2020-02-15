#Analisis Numerico Con Python, pag 15
x<-3
n<-7
E<-1e-5
f<-function(x,n,E){
  if(n>=0&x!=0){
    y<-1/2*(x+n/x)
    print(y)
    repeat{
      x<-y
      y<-1/2*(x+n/x)
      print(y)
      if(abs(x-y)<E) break#Error Absoluto
      #if(isTRUE(all.equal(x,y,tolerance=E))) break
    }
    print("Validando Convergencia del Algoritmo")
    print(y^2)
  }
  else{
    "No se puede"
  }
}
#-----------------------------------------------------------------