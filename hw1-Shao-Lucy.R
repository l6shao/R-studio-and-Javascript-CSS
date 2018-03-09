# Math 282B hw1
# Lucy Shao

#1
##function
confBand<-function(x, y, conf=0.95){
  plot(x,y)
  fit <- lm(y ~ x) 
  abline(fit) 
  p<-1
  n<-length(y)
  f<-qf(conf,df1=p+1,df2=n-p-1)#f score
  
  sigma<-sum((y-(cbind(1,x))%*%matrix(fit$coefficients))^2)/(n-p-1)#sigma
  new<-cbind(1,seq(min(x),max(x),(max(x)-min(x))/100))
  
  band<-matrix(nrow=101,ncol=2)#empty confidence band vectors
  for(i in 1:101){
    temp<-new[i,]
    se<-sqrt(sigma)*sqrt(t(temp)%*%solve(t(cbind(1,x))%*%cbind(1,x))%*%(temp)) #se hat
    yFit<-t(temp)%*%matrix(fit$coefficients)
    band[i,1]<-yFit-sqrt((p+1)*f)*se #first coln lower bond
    band[i,2]<-yFit+sqrt((p+1)*f)*se #second coln upper bond
  }
  
  newx <- seq(min(x), max(x), length.out=100)
  preds <- predict(fit, newdata = data.frame(x=newx), 
                   interval = 'confidence')
  
  lines(newx, preds[ ,3], lty = 'dashed', col = 'blue')
  lines(newx, preds[ ,2], lty = 'dashed', col = 'blue')
  
  lines(new[,2],band[,1],col="red",lty=1)
  lines(new[,2],band[,2],col="red",lty=1)
}

#read data
load("04cars.rda")
dat=dat[,c("Horsepower","Highway_MPG")]
dat=dat[complete.cases(dat),]

#plot
confBand(dat$Horsepower,dat$Highway_MPG)

#2
x<-runif(100, min = 0, max = 1)
record<-c()

for(i in 1:1000){
  e<-rnorm(100, mean = 0, sd = sqrt(0.2))
  y<-rep(1,100)+x+e
  
  #start with confidence band like the first problem
  fit <- lm(y ~ x) 
  p<-1
  n<-length(y)
  
  f<-qf(0.99,df1=p+1,df2=n-p-1)
  
  sigma<-sum((y-(cbind(1,x))%*%matrix(fit$coefficients))^2)/(n-p-1)
  new<-cbind(1,seq(min(x),max(x),(max(x)-min(x))/100))
  band<-matrix(nrow=101,ncol=2)
  
  for(i in 1:101){
    temp<-new[i,]
    se<-sqrt(sigma)*sqrt(t(temp)%*%solve(t(cbind(1,x))%*%cbind(1,x))%*%(temp))
    yFit<-t(temp)%*%matrix(fit$coefficients)
    band[i,1]<-yFit-sqrt((p+1)*f)*se
    band[i,2]<-yFit+sqrt((p+1)*f)*se
  }
  
  
  trueLine<-1+new[,2]
  #record result
  record<-c(record,all(trueLine<=band[,2])&all(band[,1]<=trueLine))
}
#result
sum(record)/1000
