#1

bootLS=function(x, y, conf=0.95, B=1000){
  fit<-lm(y~x)
  n=length(y)
  a=matrix(nrow=B,ncol=2)
  
  for(b in 1:B){
    sam=cbind(y,x)[sample(1:n,size=n,replace=T),]
    fit2=lm(sam[,1]~sam[,2])
    a[b,]=(coef(fit2)-coef(fit))/summary(fit2)$coefficients[,2]
  }
  
  t1=quantile(a, probs=1-(1-conf)/2)
  t2=quantile(a, probs=(1-conf)/2)
  se=summary(fit)$coefficients[,2]
  
  c<-cbind(coef(fit)-t1*se,coef(fit)-t2*se)
  
  return(c)
}



#2
size=c(10,50,100)


#normal
CI1=list()
#takes a while
for(i in 1:3){
  CI1[[i]]<-matrix(NA,100,8)
  for(j in 1:100){
    x=runif(size[i])
    y=x*2+rnorm(size[i])
    a<-c(t(confint(lm(y~x))),t(bootLS(x,y)))
   CI1[[i]][j,]<-a
  }
  } 

#Compare length------------------------
#for intercept
a<-cbind(CI1[[1]][,2]-CI1[[1]][,1],CI1[[1]][,6]-CI1[[1]][,5],CI1[[2]][,2]-CI1[[2]][,1],CI1[[2]][,6]-CI1[[2]][,5],CI1[[3]][,2]-CI1[[3]][,1],CI1[[3]][,6]-CI1[[3]][,5])
colnames(a)<-c("n=10","boot n=10","n=50","boot n=50","n=100","boot n=100")
boxplot(a,xlab="comparison of length for intercept with n=10,n=50,n=100")
a

#for x,ie beta1
b<-cbind(CI1[[1]][,4]-CI1[[1]][,3],CI1[[1]][,8]-CI1[[1]][,7],CI1[[2]][,4]-CI1[[2]][,3],CI1[[2]][,8]-CI1[[2]][,7],CI1[[3]][,4]-CI1[[3]][,3],CI1[[3]][,8]-CI1[[3]][,7])
colnames(b)<-c("n=10","boot n=10","n=50","boot n=50","n=100","boot n=100")
boxplot(b,xlab="comparison of length for slope with n=10,n=50,n=100")
b

#note that the length of student confidence inteval are all quite similar, 
#with less n, boot studendized confidence interval tend to have greater length


#compare confidence level-------------------

CL<-matrix(nrow = 3,ncol = 4,data=NA)
for(i in 1:3){
 
 temp<-sum(CI1[[i]][,1]<=0&CI1[[i]][,2]>=0)/100
 temp2<-sum(CI1[[i]][,5]<=0&CI1[[i]][,6]>=0)/100
  CL[i,1]<-temp
  CL[i,2]<-temp2
}

for(i in 1:3){
  temp<-sum(CI1[[i]][,3]<=2&CI1[[i]][,4]>=2)/100
  temp2<-sum(CI1[[i]][,7]<=2&CI1[[i]][,8]>=2)/100
  CL[i,3]<-temp
  CL[i,4]<-temp2
}
colnames(CL)<-c("regular intercept","boot intercept","regular x","boot x")
rownames(CL)<-c("n=10","n=50","n=100")
CL
#note that the results are quite similar 
#except for smallest n, n=10, boot confidence interval has higher confidence level

#---------------------------------------------------------------------------------------
#exponential--------------------------------
CI1=list()

#takes a while
for(i in 1:3){
  CI1[[i]]<-matrix(NA,100,8)
  for(j in 1:100){
    x=runif(size[i])
    y=x*2+scale(rexp(size[i]),center=T,scale=F)
    a<-c(t(confint(lm(y~x))),t(bootLS(x,y)))
    CI1[[i]][j,]<-a
  }
} 

#Compare length------------------------
#for intercept
a<-cbind(CI1[[1]][,2]-CI1[[1]][,1],CI1[[1]][,6]-CI1[[1]][,5],CI1[[2]][,2]-CI1[[2]][,1],CI1[[2]][,6]-CI1[[2]][,5],CI1[[3]][,2]-CI1[[3]][,1],CI1[[3]][,6]-CI1[[3]][,5])
colnames(a)<-c("n=10","boot n=10","n=50","boot n=50","n=100","boot n=100")
boxplot(a,xlab="comparison of length for intercept with n=10,n=50,n=100")
a

#for x,ie beta1
b<-cbind(CI1[[1]][,4]-CI1[[1]][,3],CI1[[1]][,8]-CI1[[1]][,7],CI1[[2]][,4]-CI1[[2]][,3],CI1[[2]][,8]-CI1[[2]][,7],CI1[[3]][,4]-CI1[[3]][,3],CI1[[3]][,8]-CI1[[3]][,7])
colnames(b)<-c("n=10","boot n=10","n=50","boot n=50","n=100","boot n=100")
boxplot(b,xlab="comparison of length for slope with n=10,n=50,n=100")
b

#note that the length of student confidence inteval are all quite similar,
#with less n, boot studendized confidence interval tend to have greater length


#compare confidence level-------------------

CL<-matrix(nrow = 3,ncol = 4,data=NA)
for(i in 1:3){
  
  temp<-sum(CI1[[i]][,1]<=0&CI1[[i]][,2]>=0)/100
  temp2<-sum(CI1[[i]][,5]<=0&CI1[[i]][,6]>=0)/100
  CL[i,1]<-temp
  CL[i,2]<-temp2
}

for(i in 1:3){
  temp<-sum(CI1[[i]][,3]<=2&CI1[[i]][,4]>=2)/100
  temp2<-sum(CI1[[i]][,7]<=2&CI1[[i]][,8]>=2)/100
  CL[i,3]<-temp
  CL[i,4]<-temp2
}
colnames(CL)<-c("regular intercept","boot intercept","regular x","boot x")
rownames(CL)<-c("n=10","n=50","n=100")
CL
#note that the results are quite similar 
#except for smallest n, n=10, boot confidence interval has higher confidence level
#except for smallest n, n=10, boot confidence interval has higher confidence level