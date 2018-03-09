

#1
library(MASS)
medv=Boston$medv
lstat=Boston$lstat
chas=as.factor(Boston$chas)
rad=as.factor(Boston$rad)

#a

boxplot(medv~chas)
# there are difference bwtween medv when chas==1 and chas==0
# the two median are similar but the third quaitiles for two boxplot differs

fit=lm(medv~chas)
anova(fit)
# F-test tests hypothesis H0: there is no difference in means
# The p value is small and we reject null, and we conclude that there is a difference in 
#The result is consistent with plot

#b

boxplot(medv~rad)
# there are difference bwtween medv when rad is different, 
#there is a relationship between medv and rad


fit1=lm(medv~rad)
anova(fit1)
# F-test tests hypothesis H0: there is no difference in means
# The p value is small and we reject null, and we conclude that there is a difference in 
#The result is consistent with plot

#c

boxplot(medv~chas+rad)
# there are difference bwtween medv when rad and chas is different, 
#there is a relationship between medv and chas+rad


interaction.plot(chas,rad,medv)
# we should add the interaction term


fit2=lm(medv~chas*rad)
# summary(fit1c)
anova(fit2)
# F tests:
# model:medv~ 1  and medv~1+chas, null:the true means for two models are equal
# model: medv~1+chas and medv~1+chas+rad, null:the true means for two models are equal
#model: medv~1+chas+rad and medv~1+chas+rad+chas:rad, null:the true means for two models are equal
# All three predictor chas, rad, chas:rad are significant since the p value is small
#The result is consistent with plot

#d

a=Boston[chas==0,]$medv
b=Boston[chas==0,]$lstat
a1=Boston[chas==1,]$medv
b1=Boston[chas==1,]$lstat
plot(lstat, medv, type="n")
points(b, a, cex=1, pch=1)
points(b1, a1, cex=1, pch=2)
abline(lm(a~b), lty=1, lwd=2)
abline(lm(a1~b1), lty=2, lwd=2)
legend("topright", inset=.05, title="chas", c("0","1"), lty=1:2, lwd=2)
#there appear that the rate  of decrease depend on whether the area borders the Charles Rive

# null: model medv~lstat has same mean with model medv~lstat+chas
# alternative: model medv~lstat does not have  same mean with model medv~lstat+chas
fit3=lm(medv~lstat+chas)
anova(fit3)
#look at the second line of anova table, the test result is significant, both lstat and chas are significant predictors.

#2
#a
medv=Boston$medv
lstat=Boston$lstat
fit=lm(medv~poly(lstat,3,raw=FALSE)) 

#b

# LS with no outliers
fit2=lm(medv~poly(lstat,3,raw=FALSE))

# various diagnostics for detecting influentiall points
I = influence.measures(fit)
inf.sum=apply(I$is.inf,MARGIN=1,sum); inf.index=inf.sum>=1
fit3=lm(medv[-inf.index]~poly(lstat[-inf.index],3,raw=FALSE))

# L1 regression
library(quantreg)
fit4=rq(medv~poly(lstat,3,raw=FALSE))

# M-estimators
library(MASS)
fit5=rlm(medv~poly(lstat,3,raw=FALSE), maxit=50)#huber
fit6=rlm(medv~poly(lstat,3,raw=FALSE), maxit=50, psi=psi.hampel)#hampel
fit7=rlm(medv~poly(lstat,3,raw=FALSE), maxit=50, psi=psi.bisquare)#tukey

# high breakdown point methods
fit8=lmsreg(medv~poly(lstat,3,raw=FALSE))
fit9=ltsreg(medv~poly(lstat,3,raw=FALSE))

## part c ##

col=rainbow(9)
s=seq(0,40,length.out=1000)
fits=list(fit, fit2, fit4, fit5, fit6, fit7,fit8,fit9)
plot(lstat, medv, pch=1, cex=0.4, col='grey',
     xlab="population", ylab="Median", main="models")
for(i in 1:length(fits)){
  val=predict(fits[[i]], newdata=data.frame(lstat=s))
  lines(s, val, lwd=1.3, col=col[i])
}
legend("topright", title="Method", c("LS","LS no outlier","L1","Huber","Hampel","Tukey","LMS","LTS"), lty=1, lwd=1.3, col=col)






