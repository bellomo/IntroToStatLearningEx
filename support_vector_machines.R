##########
# SVM    #
##########
require(e1071)

# generate some data
set.seed(10111)
x = matrix(rnorm(40), 20, 2)
y = rep(c(-1,1), c(10,10))
x[y==1,]=x[y==1,]+1
plot(x, col=y+3, pch=19)
dat = data.frame(x,y=as.factor(y))

# Linear SVM classifier
########################
svmfit = svm(y~., data=dat, 
             kernel="linear", cost=10, scale=FALSE)
svmfit
plot(svmfit,dat)

# a better plot, maybe...
# make a grid of values...
make.grid = function(x,n=75) {
  grange = apply(x,2,range) # get range of x
  x1 = seq(from=grange[1,1], to=grange[2,1], length=n)
  x2 = seq(from=grange[1,2], to=grange[2,2], length=n)
  expand.grid(X1=x1, X2=x2) # makes the grid
}
xgrid = make.grid(x)
ygrid = predict(svmfit, xgrid)
plot(xgrid, col=c("red","blue")[as.numeric(ygrid)], pch=20, cex=.2)
points(x, col=y+3, pch=19)
points(x[svmfit$index,], pch=5, cex=2) # support points

# See ch 12 ESL....
beta0 = svmfit$rho
beta = drop(t(svmfit$coefs)%*%x[svmfit$index,])
plot(xgrid, col=c("red","blue")[as.numeric(ygrid)], pch=20, cex=.2)
points(x, col=y+3, pch=19)
points(x[svmfit$index,], pch=5, cex=2) # support points
abline(beta0/beta[2], -beta[1]/beta[2])
abline((beta0-1)/beta[2], -beta[1]/beta[2], lty=2)
abline((beta0+1)/beta[2], -beta[1]/beta[2], lty=2)


# Non linear SVM classifier
################################

# load data
load(url("http://statweb.stanford.edu/~tibs/ElemStatLearn/datasets/ESL.mixture.rda"))
names(ESL.mixture)
rm(x,y)
attach(ESL.mixture)

plot(x, col=y+1)
dat = data.frame(x,y=as.factor(y))

fit = svm(y~., data=dat, scale=FALSE, kernel="radial", cost=5)

xgrid = expand.grid(X1=px1, X2=px2)
ygrid = predict(fit, xgrid)
plot(xgrid, col=as.numeric(ygrid), pch=20, cex=.2)
points(x, col=y+1, pch=19)

# add predicted decision boundary 
func = predict(fit, xgrid, decision.values = TRUE)
func = attributes(func)$decision
contour(px1, px2, matrix(func, 69, 99), level=0, add=TRUE)

# add true decision boundary (the true probability is provided)
contour(px1, px2, matrix(prob, 69, 99), level=.5, add=TRUE, col="blue")
