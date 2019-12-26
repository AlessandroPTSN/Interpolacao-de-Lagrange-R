# Interpolacao-por-polinomio-de-Lagrange-R
Interpolando os pontos (0, 1), (2, 3), (4, -1) e (7, 4) utilizando o interpolação polinomial no R

![1](https://user-images.githubusercontent.com/50224653/71485011-ecb15b00-27ed-11ea-90e3-70cafc605d9e.PNG)

## Interpolação polinomial IP
```R
x=c(0,2,4,7)
y=c(1,3,-1,4)

IP=function(x,y){m=matrix(x,nrow = (length(x)),ncol=(length(x)))
(length(m))

m[,1]=1

for(i in 2:((length(m)/(sqrt(length(m)))))){
  for(j in 1:((length(m)/(sqrt(length(m)))))){
    m[j,i]=m[j,i-1]*x[j]
    #print(m);print(i,j)
  }
}
z=solve(m,y)
return(z)
}

IP(x,y)#vai lhe dar a funçao a + bx + cX2 + ... + zxn
f=function(x){1+4.1952381*x-2.0214286*x^2+0.2119048*x^3}
```

![2](https://user-images.githubusercontent.com/50224653/71485012-ed49f180-27ed-11ea-99f7-0ec386b155d6.PNG)

Agora, utilizando método de interpolação por polinômio de Lagrange para avaliar a função interpoladora em um x qualquer.

```R
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


plot(f,from = 0 , to = 7,col="blue", lwd = 5)
abline(h=-2:4,col="gray");abline(h=0,pch=20)
abline(v=0:7,col="gray");abline(v=0,pch=20)
points(0,1,col="red",pch=20,lwd = 5);points(2,3,col="red",pch=20,lwd = 5)
points(4,-1,col="red",pch=20,lwd = 5);points(7,4,col="red",pch=20,lwd = 5)
#novo ponto
points(1,3.385714,col="red",pch=20,lwd = 5)
```
