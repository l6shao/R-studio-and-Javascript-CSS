#1a
seq=function(x,y,tol,deg.max){
  deg=0
  dif=tol
  a=summary(lm(y~1))$r.squared 
  while(deg<deg.max & dif>=tol){
    deg=deg+1
    fit=lm(y ~ poly(x, deg, raw=T))  
    b=summary(fit)$r.squared
    dif=(b-a)/a
    a=b
    if(dif<tol)
      {deg=deg-1}
  }
  return(length(1:deg))
}

#1b
forward=function(x,y,tol,deg.max){
  X=matrix(NA,n,deg.max)
  for(i in 1:deg.max){X[,i]=x^i}
  deg=0
  temp1=c()
  dif=tol
  a=summary(lm(y~1))$r.squared
  
  while(length(temp1)<deg.max & dif>=tol){
    temp3=rep(NA,deg.max)
    
    for(deg in setdiff(1:deg.max, temp1)){
      temp3[deg]=summary( lm(y ~ X[,c(temp1,deg)]) )$r.squared
    }
    
    b=max(temp3, na.rm=T)
    dif=(b-a)/a
    temp2=which.max(temp3)
    
    if(dif>=tol) 
      {temp1=c(temp1,temp2)}
    
    a=b
  }
  return(length(temp1))
}
A=0
B=0
n=200

#simulate and compare
for(k in 1:1000){
  x=runif(n,0,2*pi)
  y=rnorm(n,sin(3*x)+x,1)
  A=A+seq(x,y,tol=0.02,20)
  B=B+forward(x,y,tol=0.02,20)
}

#method sequential
A

#method forward step
B

#comments: I performed 1000 simulations to compare the overall degree.
#If we use sequential, the degree is overall greater than using forward step method
#although the dergee difference is not large.

#2
BMP=function( x, y, steps = min(5, ncol(x)) ){
  
  
  tol=1e-6
  k1=0
  b1=y
  s=0
  temp=c()
  x=apply(x,MARGIN=2,function(x) x/sqrt(sum(x^2)))
  
  for(s in 1:steps){
    
    R=rep(NA,ncol(x))
    for(l in setdiff(1:ncol(x),k1)) {
      R[l]=abs(t(x[,l])%*%b1) 
    } 
    
    k2=which.max(R)
    b2=b1-x[,k2]%*%t(x[,k2])%*%b1
    
    if(abs(sqrt(sum(b2^2))-sqrt(sum(b1^2)))<=tol) 
    {
      break
      }
    if(k2 %in% temp == F)
    {
      temp=c(temp,k2) 
      }
    b1=b2; k1=k2
  }
  
  return(temp)
}


MMP=function( x, y, steps = min(5, ncol(x)) ){
  
  tol=1e-6
  k=0
  temp=c()
  P=matrix(0,length(y),length(y))
  b2=y
  x=apply(x,MARGIN=2,function(x) x/sqrt(sum(x^2)))
  
  for(s in 1:steps){
    b1=b2
    R=rep(NA,ncol(x))
    for(l in setdiff(1:ncol(x),temp)){ 
      R[l]=abs(t(x[,l])%*%b1)
    } 
    
    k=which.max(R)
    Q=-qr.Q(qr(x[,c(temp,k)]))
    P=P+Q[,s]%*%t(Q[,s])
    b2=y-P%*%y
    
    if( sqrt( sum((b2-b1)^2) ) <= tol) 
      break
    
    temp=c(temp,k)
  }
  return(temp)
}

ORMP=function( x, y, steps = min(5, ncol(x)) ){
  
  tol=1e-6
  b2=y
  temp=c()
  P=matrix(0,length(y),length(y))
  x=apply(x,MARGIN=2,function(x) x/sqrt(sum(x^2)))
  
  for (s in 1:steps){
    b1=b2
    R=rep(NA,ncol(x))
    
    for( l in setdiff(1:ncol(x),temp) ){
      a=x[,l]-P%*%x[,l]; a=a/sqrt(sum(a^2))
      R[l]=abs(t(a)%*%b1)
    }
    
    k=which.max(R)
    a=x[,k]-P%*%x[,k]; a=a/sqrt(sum(a^2))
    P=P+a%*%t(a)
    b2=b1-P%*%b1
    
    if( sqrt( sum((b2-b1)^2) ) <= tol)
      break
    temp=c(temp,k)
  }
  return(temp)
}

#now simulate and compare

bmp=rep(NA,rep.max)
mmp=rep(NA,rep.max)
ormp=rep(NA,rep.max)
for(i in 1:1000){
  A=matrix(rnorm(20*30,0,1),20,30)
  temp.true=sample(1:30, size=4, replace=F)
  y=A[,temp.true]%*%runif(4,-1,1)
  
  bmp[i]=length(BMP(A,y,20))/4
  mmp[i]=length(MMP(A,y,20))/4
  ormp[i]=length(ORMP(A,y,20))/4
}

#show frequencies of redundancy
hist(bmp, breaks=15, xlim=c(0,5),main="BMP")
hist(mmp, breaks=15, xlim=c(0,5), main="MMP")
hist(ormp, breaks=15, xlim=c(0,5) ,main="ORMP")


# We want the frequency to concentrate around 1
# Therefore MMP performs the best of the three





