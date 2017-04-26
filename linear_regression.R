library(MASS)
library(ISLR)

###### simple linear regression
names(Boston)
?Boston
plot(medv~lstat,Boston)
fit1=lm(medv~lstat,data=Boston)
fit1
summary(fit1)
abline(fit1,col="red")
names(fit1)
confint(fit1)
predict(fit1,data.frame(lstat=c(5,10,15)), interval = "confidence")

####### multiple linear regression
# 2 variables
fit2=lm(medv~lstat+age,data=Boston)
summary(fit2)
# all variables
fit3=lm(medv~.,Boston)
summary(fit3)
par(mfrow=c(2,2))
plot(fit3)
# all variables minus less significant ones
fit4=update(fit3,~.-age-indus)
summary(fit4)

######## non linearity and interactions
fit5=lm(medv~lstat*age,Boston)
summary(fit5)
fit6=lm(medv~lstat+I(lstat^2),Boston)
summary(fit6)
fit7=lm(medv~poly(lstat,4))
summary(fit7)

attach(Boston)
par(mfrow=c(1,1))
plot(medv~lstat)
# points(lstat,fitted(fit1),col="blue",pch=20)
# points(lstat,fitted(fit4),col="green",pch=20)
# points(lstat,fitted(fit5),col="magenta",pch=20)
points(lstat,fitted(fit6),col="red",pch=20)
points(lstat,fitted(fit7),col="blue",pch=20)

# markers
plot(1:30,1:30,pch=1:30,cex=1)

########### qualitative predictors
# to look at data ....
# fix(Carseats) 
names(Carseats)
summary(Carseats)
fit1=lm(Sales~.+Income:Advertising+Age:Price,Carseats)
summary(fit1)
contrasts(Carseats$ShelveLoc)

# function
regplot = function(x,y,...){
  fit=lm(y~x)
  plot(x,y,...)
  abline(fit,col="red")
}

attach(Carseats)
regplot(Price,Sales,xlab="Price",ylab="Sales",col="blue",pch=20)