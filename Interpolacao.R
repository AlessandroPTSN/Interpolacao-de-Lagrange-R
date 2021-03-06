#FUNÇÃO######################################################
#Interpolação polinomial IP


x=c(0,2,4,7)
y=c(1,3,-1,4)

plot(1, type="n", xlab="x", ylab="f", xlim=c(0, 7), ylim=c(-2, 4))
abline(h=-2:4,col="gray");abline(h=0,pch=20)
abline(v=0:7,col="gray");abline(v=0,pch=20)
points(0,1,col="red",pch=20,lwd = 5);points(2,3,col="red",pch=20,lwd = 5)
points(4,-1,col="red",pch=20,lwd = 5);points(7,4,col="red",pch=20,lwd = 5)

IP=function(x,y){m=matrix(x,nrow = (length(x)),ncol=(length(x)))
(length(m))

m[,1]=1

for(i in 2:((length(m)/(sqrt(length(m)))))){
  for(j in 1:((length(m)/(sqrt(length(m)))))){
    m[j,i]=m[j,i-1]*x[j]
  }
}
z=solve(m,y)
return(z)
}

IP(x,y)#vai lhe dar a funçao a + bx + cX2 + ... + zxn
f=function(x){1+4.1952381*x-2.0214286*x^2+0.2119048*x^3}



plot(1, type="n", xlab="x", ylab="f", xlim=c(0, 7), ylim=c(-2, 4))
abline(h=-2:4,col="gray");abline(h=0,pch=20)
abline(v=0:7,col="gray");abline(v=0,pch=20)
plot(f,from = 0 ,add=T, to = 7,col="blue", lwd = 5)
points(0,1,col="red",pch=20,lwd = 5);points(2,3,col="red",pch=20,lwd = 5)
points(4,-1,col="red",pch=20,lwd = 5);points(7,4,col="red",pch=20,lwd = 5)




######################################################################################
#Utilizando método de interpolação por polinômio de Lagrange 
#para avaliar a função interpoladora em um x qualquer.


#Interpolação de Lagrange  IL
IL=function(c,x,y){
  #-----------------------------
  n=length(x)
  h=c(2:n)
  vv=matrix(nrow = n,ncol = n-1)
  for (i in 1:n){
    for (j in 1:n-1){
      vv[i,j] = h[j]
    }
    h[i] = i 
  }
  #-----------------------------
  ll=0
  for(j in 1:n){
    l=1
    for(i in vv[j,]){
      l=l*(c-x[i])/(x[j]-x[i])
    }
    ll[j]=l
  }
  #-----------------------------
  a=0
  for(i in 1:n){
    a[i]=y[i]*ll[i]
  }
  return(sum(a)) 
}


IL(1,x,y)
#3.385714


plot(1, type="n", xlab="x", ylab="f", xlim=c(0, 7), ylim=c(-2, 4))
abline(h=-2:4,col="gray");abline(h=0,pch=20)
abline(v=0:7,col="gray");abline(v=0,pch=20)
plot(f,from = 0 ,add=T, to = 7,col="blue", lwd = 5)
points(0,1,col="red",pch=20,lwd = 5);points(2,3,col="red",pch=20,lwd = 5)
points(4,-1,col="red",pch=20,lwd = 5);points(7,4,col="red",pch=20,lwd = 5)
#novo ponto
points(1,3.385714,col="red",pch=20,lwd = 5)
