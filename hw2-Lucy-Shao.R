
f<-function(n){
  record<-c()
  x<-runif(n, min=-1, max=1)
  
  for(i in 1:1000){
    y<-c()
    for(i in 1:n){
      
      y<-c(y,rnorm(1, mean = 2*x[i]+1, sd = 0.5))
    }
    record<-rbind(record,lm(y~x)$coefficients)
  }
  return(record)
}
# hist 2d
# kernel

library(plotly)

#with n=50
a<-f(50)
hist(a[,1])
hist(a[,2])
freqz <- with(data.frame(a), MASS::kde2d(a[,1], a[,2], n = 50))
with(freqz, plot_ly(x = x, y = y,z=z, type = "surface")) 

#n=100
a<-f(100)
hist(a[,1])
hist(a[,2])
freqz <- with(data.frame(a), MASS::kde2d(a[,1], a[,2], n = 100))
with(freqz, plot_ly(x = x, y = y,z=z, type = "surface")) 

#n=200
a<-f(200)
hist(a[,1])
hist(a[,2])
freqz <- with(data.frame(a), MASS::kde2d(a[,1], a[,2], n = 50))
with(freqz, plot_ly(x = x, y = y,z=z, type = "surface")) 

n=500
a<-f(500)
hist(a[,1])
hist(a[,2])
freqz <- with(data.frame(a), MASS::kde2d(a[,1], a[,2], n = 50))
with(freqz, plot_ly(x = x, y = y,z=z, type = "surface")) 

#all the graphs are approx normal


#--------------------------------
#part b

#t distribution  {2,5,10,20,50}

g<-function(n,df){
  record<-c()
  x<-rt(n, df)
  for(i in 1:1000){
    y<-c()
    for(i in 1:n){
      
      y<-c(y,rnorm(1, mean = 2*x[i]+1, sd = 0.5))
    }
    record<-rbind(record,lm(y~x)$coefficients)
  }
  return(record)
}

#n=50
#k={2,5,10,20,50}
a<-g(50,2)
hist(a[,1])
hist(a[,2])
freqz <- with(data.frame(a), MASS::kde2d(a[,1], a[,2], n = 50))
with(freqz, plot_ly(x = x, y = y,z=z, type = "surface")) 

a<-g(50,5)
hist(a[,1])
hist(a[,2])
freqz <- with(data.frame(a), MASS::kde2d(a[,1], a[,2], n = 50))
with(freqz, plot_ly(x = x, y = y,z=z, type = "surface")) 

a<-g(50,10)
hist(a[,1])
hist(a[,2])
freqz <- with(data.frame(a), MASS::kde2d(a[,1], a[,2], n = 50))
with(freqz, plot_ly(x = x, y = y,z=z, type = "surface")) 

a<-g(50,20)
hist(a[,1])
hist(a[,2])
freqz <- with(data.frame(a), MASS::kde2d(a[,1], a[,2], n = 50))
with(freqz, plot_ly(x = x, y = y,z=z, type = "surface")) 

a<-g(50,50)
hist(a[,1])
hist(a[,2])
freqz <- with(data.frame(a), MASS::kde2d(a[,1], a[,2], n = 50))
with(freqz, plot_ly(x = x, y = y,z=z, type = "surface")) 

#n=100
#k={2,5,10,20,50}
a<-g(100,2)
hist(a[,1])
hist(a[,2])
freqz <- with(data.frame(a), MASS::kde2d(a[,1], a[,2], n = 50))
with(freqz, plot_ly(x = x, y = y,z=z, type = "surface")) 

a<-g(100,5)
hist(a[,1])
hist(a[,2])
freqz <- with(data.frame(a), MASS::kde2d(a[,1], a[,2], n = 50))
with(freqz, plot_ly(x = x, y = y,z=z, type = "surface")) 

a<-g(100,10)
hist(a[,1])
hist(a[,2])
freqz <- with(data.frame(a), MASS::kde2d(a[,1], a[,2], n = 50))
with(freqz, plot_ly(x = x, y = y,z=z, type = "surface")) 

a<-g(100,20)
hist(a[,1])
hist(a[,2])
freqz <- with(data.frame(a), MASS::kde2d(a[,1], a[,2], n = 50))
with(freqz, plot_ly(x = x, y = y,z=z, type = "surface")) 

a<-g(100,50)
hist(a[,1])
hist(a[,2])
freqz <- with(data.frame(a), MASS::kde2d(a[,1], a[,2], n = 50))
with(freqz, plot_ly(x = x, y = y,z=z, type = "surface")) 

#n=200
#k={2,5,10,20,50}
a<-g(200,2)
hist(a[,1])
hist(a[,2])
freqz <- with(data.frame(a), MASS::kde2d(a[,1], a[,2], n = 50))
with(freqz, plot_ly(x = x, y = y,z=z, type = "surface")) 

a<-g(200,5)
hist(a[,1])
hist(a[,2])
freqz <- with(data.frame(a), MASS::kde2d(a[,1], a[,2], n = 50))
with(freqz, plot_ly(x = x, y = y,z=z, type = "surface")) 

a<-g(200,10)
hist(a[,1])
hist(a[,2])
freqz <- with(data.frame(a), MASS::kde2d(a[,1], a[,2], n = 50))
with(freqz, plot_ly(x = x, y = y,z=z, type = "surface")) 

a<-g(200,20)
hist(a[,1])
hist(a[,2])
freqz <- with(data.frame(a), MASS::kde2d(a[,1], a[,2], n = 50))
with(freqz, plot_ly(x = x, y = y,z=z, type = "surface")) 

a<-g(200,50)
hist(a[,1])
hist(a[,2])
freqz <- with(data.frame(a), MASS::kde2d(a[,1], a[,2], n = 50))
with(freqz, plot_ly(x = x, y = y,z=z, type = "surface")) 

#n=500
#k={2,5,10,20,50}
a<-g(500,2)
hist(a[,1])
hist(a[,2])
freqz <- with(data.frame(a), MASS::kde2d(a[,1], a[,2], n = 50))
with(freqz, plot_ly(x = x, y = y,z=z, type = "surface")) 

a<-g(500,5)
hist(a[,1])
hist(a[,2])
freqz <- with(data.frame(a), MASS::kde2d(a[,1], a[,2], n = 50))
with(freqz, plot_ly(x = x, y = y,z=z, type = "surface")) 

a<-g(500,10)
hist(a[,1])
hist(a[,2])
freqz <- with(data.frame(a), MASS::kde2d(a[,1], a[,2], n = 50))
with(freqz, plot_ly(x = x, y = y,z=z, type = "surface")) 

a<-g(500,20)
hist(a[,1])
hist(a[,2])
freqz <- with(data.frame(a), MASS::kde2d(a[,1], a[,2], n = 50))
with(freqz, plot_ly(x = x, y = y,z=z, type = "surface")) 

a<-g(500,50)
hist(a[,1])
hist(a[,2])
freqz <- with(data.frame(a), MASS::kde2d(a[,1], a[,2], n = 50))
with(freqz, plot_ly(x = x, y = y,z=z, type = "surface")) 

#all the plots are approx normal

#---------------------------------------------------
#2
library(MASS)
data(Boston)

fit<-lm(medv~crim+zn+indus+nox+rm+age+dis+tax+ptratio+black+lstat,data=Boston)
summary(fit)

# checking model accuracy and heteroscedasticity via residual plots

#a-----------

#model accuracy
library(car)
plot(fit, which=1, pch=16) # shows that the model has some curvature, non-linear
residualPlots(fit) # the curvature of the model mainly result from rm,lstat,

#heteroscedasticity
#from the plots above, we see that the  model does not has equal variance, a little bit fan shaped

#Normallity
plot(fit, which=2, cex=1, pch=16)#approx normal by qq plot

#b----------------

# checking for outliers in predictor via hatvalues
plot(hatvalues(fit), type='h', col="blue", ylab="Hat Values", main="Hat values")
p = 4; n = 387
abline(h = 2*(p+1)/n, lty=2) # threshold for suspects
#it seems there are some extreme outliers around 400, since the observations has very large hat values than other ones, and some other better outliers all over the range

#c----------------
# checking outliers in response via externally studentized residuals
plot(abs(rstudent(fit)), type='h', col="blue", ylab="Externally Studentized Residuals (in absolute value)", main="Externally Studentized Residuals (in absolute value)")
abline(h = qt(.95, n-p-2), lty=2) # threshold for suspects
#again it seems there are some extreme outliers around 400, also some better outliers around 200


#d--------------
#A point that is both an outlier in predictor and response is said to beinfluential.

# Cook's distances
plot(fit, which=4, col="blue", lwd=2)
abline(h = 1, lty=2) # threshold for suspects (not visible on this plot)
# influtial points are at 369, 366,381

# DFBETAS
par(mfrow=c(2,3))
for (j in 1:6){
  plot(abs(dfbetas(fit)[,j]), col=4, type='h', ylab='DFBETAS')
  abline(h = 2/sqrt(n), lty=2) # threshold for suspects
}
#DFBETASj(i)>2/√n is consider suspect and there are a lot around 400, and some less influential ones around 200

# DFFITS	
par(mfrow=c(1,1))
plot(abs(dffits(fit)), typ='h', col=4, ylab='DFFITS')
abline(h = 2*sqrt(p/n), lty=2) # threshold for suspects
#DFFITSi>2p(p+ 1)/n, since there are a lot of datapoints, the threshhold is small and besides obvious points around 400, there are also points around 200 to be influential


#e---------------


# checking for multicolinearity via pairwise correlations b/w predictors
round( cor(Boston[, -14]),2) # rounded to 2 digits
require(ellipse)
plotcorr(cor(Boston[, -14]))
#the predictors has high correlation especially between rad and crim, dis and indus,rad and tax, indus and nox and so on.

# checking for multicolinearity via variance inflation factors (VIF)
require(car)
plot(vif(fit), type='h', col=4)
abline(h = 10, lty=2) # threshold for suspects 
#There are no VIFs>10, so there are no suspects

# checking for multicolinearity via condition indices
C = cor(Boston[, -14]) # correlation matrix for the predictors
L = eigen(C) # eigenvalues  
K = max(L$val)/L$val # condition indices
plot(K, type='h', col=4)
abline(h = 1000, lty=2) # threshold for suspects 
#K>1000 is consider suspect, but the largest of them is 96.5, so no suspects