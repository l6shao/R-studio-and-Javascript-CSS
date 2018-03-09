#Q1
greedyAIC<-function(x, y){
  X<-matrix(NA,n,20)
  for(i in 1:20) {
    X[,i]<-x^i 
    }
  temp<-c()
  a<-AIC(lm(y~1))
  
  for( j in 1:20 ){
    b<-rep(NA,20)
    
    for( deg in setdiff(1:20,temp) )
      b[deg]<-summary( lm( y ~ X[,c(temp,deg)] ) )$r.squared
    
    temp<-c( temp, which.max(b) )
    
    a<-c( a, AIC( lm( y ~ X[,c(temp)] ) ) )
  }
  
  m<-sort( temp[1:which.min(a)] )
  
  cat("path:", paste(temp,collapse =","),"\nthe selected powers:", paste(m,collapse =","), "\n")
  
  return(list(path=temp, selected.model=m, AIC=a, AIC.min=min(a)))
}


x<-runif(200,0,2*pi)
y<-rnorm(200,sin(3*x)+x,1)

#greedyAIC
fit1<-greedyAIC(x, y)
plot(fit1$AIC, ylab="AIC", xlab="Step")

#step
fit2<-step(lm(y~1), scope= y ~ x + I(x^2) + I(x^3) + I(x^4) + I(x^5) + I(x^6) + I(x^7) + I(x^8) + I(x^9) + I(x^10) + I(x^11) + I(x^12) + I(x^13) + I(x^14) + I(x^15) + I(x^16) + I(x^17)+ I(x^18) + I(x^19) + I(x^20), direction = "forward")
coef(fit2)


#Q2
require(MASS)
require(glmnet)
require(ellipse)
# GCV

ridge.gcv=function(X,y,lamb = NULL){
  
  fit<-glmnet(X, y, alpha=0, lambda = lamb, intercept=T)
  len<-length(fit$lambda)
  
  temp<-c()
  mse<-c()
  for(l in 1:len){
    mat <- X%*%solve( t(X)%*%X + diag(fit$lambda[l], dim(X)[2] ) )%*%t(X)
    prediction<-predict(fit, s=fit$lambda[l], newx=X)
    temp<-c(temp, mean((y-prediction)^2)/(( length(y) - sum(diag(mat)) )^2) )
    mse<-c(mse,mean((y-prediction)^2))
  }
  
  lambda.min=fit$lambda[which.min(temp)]
  result=rbind(fit$lambda,temp)
  relativeIneff=mse[which.min(temp)]/min(mse)
  
  rownames(result)=c("lambda","GCV")
  return( list( GCV=result, lambda.min=fit$lambda[which.min(temp)], GVC.min=min(temp), relativeIneff=relativeIneff) )
}


# PRESS
ridge.press<-function(X, y, lamb){
  se<-matrix(NA, length(y), length(lamb))
  
  for(i in 1:length(y)){
    fit<-glmnet(X[-i,], y[-i], alpha=0, lambda = lamb, intercept=T)
    se[i,]<-( y[i] - c(1,X[i,])%*%as.matrix( coef(fit) ) )^2
  }
  
  CV<-apply(se, MARGIN=2, mean)
  lamb.min<-fit$lambda[ which.min(CV) ]
  PRESS<-rbind(fit$lambda,CV)
  rownames(PRESS)=c("lambda","PRESS")
  
  fit<-glmnet(X, y, alpha=0, lambda = lamb, intercept=T)
  prediction<-predict(fit, s=fit$lambda, newx=X)
  mse<-apply(prediction, 2, function(t) sum((t-y)^2)/length(y))
  
  relativeIneff<-mse[which.min(CV)]/min(mse)
  names(relativeIneff)<-NULL
  
  return( list( lambda.min=lamb.min, PRESS=PRESS, relativeIneff=relativeIneff ) )
}



# setting 1
y=mtcars$hp
X=subset(mtcars, select=c("mpg","wt","drat","qsec","disp"))
X=as.matrix(X)
lamb<-list()

#three lambs
lamb[[1]]=glmnet(X, y)$lambda
lamb[[2]]=10^seq(10,-2,length.out = 100)
lamb[[3]]=seq(0.1, 30, length.out = 1000)
par(mfrow=c(3,2))

for(i in 1:3){
# GCV
result=ridge.gcv(X,y,lamb[[i]] )
result$relativeIneff

plot(result$GCV[1,], result$GCV[2,],xlab="lambda", type="l", ylab="GVC",lty=1)
ll=result$lambda.min
abline(v=ll, lty=2)
text(ll, mean(result$GCV[2,]), labels=round(ll,2) )


# PRESS
result=ridge.press( X, y, lamb[[i]])
result$relativeIneff

plot(result$PRESS[1,], result$PRESS[2,], xlab="lambda", ylab="PRESS", type="l", lty=1)
abline(v=result$lambda.min, lty=2)
text(result$lambda.min, mean(result$PRESS[2,]), labels=round(result$lambda.min,2) )
}

# setting 2
n=30
p=20
X=poly(runif(n), degree=p, raw=T)
temp=sample(1:p, 5, replace=F)
temp
y=1+X[,temp]%*%5:1+rnorm(n)
kappa(t(X)%*%X, exact=TRUE)
#lambda same as setting 1

for(i in 1:3){
  # GCV
  result=ridge.gcv(X,y,lamb[[i]] )
  result$relativeIneff
  plot(result$GCV[1,], result$GCV[2,],xlab="lambda", type="l", ylab="GVC",lty=1)
  ll=result$lambda.min
  abline(v=ll, lty=2)
  text(ll, mean(result$GCV[2,]), labels=round(ll,2) )
  
  
  # PRESS
  result=ridge.press( X, y, lamb[[i]])
  result$relativeIneff
   plot(result$PRESS[1,], result$PRESS[2,], xlab="lambda", ylab="PRESS", type="l", lty=1)
  abline(v=result$lambda.min, lty=2)
  text(result$lambda.min, mean(result$PRESS[2,]), labels=round(result$lambda.min,2) )
}