library(glmnet)

# data
library(ISLR)
summary(Hitters)

# the "-1" is to remove the intercept
x = model.matrix(Salary~.-1,data=Hitters)
y = Hitters$Salary

####################
# Ridge Regression #
####################

fit.ridge = glmnet(x,y,alpha = 0)
plot(fit.ridge,xvar="lambda",label=TRUE)

# do cross validation
cv.ridge = cv.glmnet(x,y,alpha = 0)
plot(cv.ridge)

##################
# Lasso          #
##################

fit.lasso = glmnet(x,y) # default alpha=1 lasso
plot(fit.lasso, xvar="lambda",label=TRUE)
plot(fit.lasso, xvar="dev",label=TRUE)

# do cross validation
cv.lasso = cv.glmnet(x,y)
plot(cv.lasso)

# coeff for the best model
coef(cv.lasso)

# use train/validation 
set.seed(1)
train = sample(seq(nrow(Hitters)), 180, replace = FALSE)

# train lasso
lasso.tr = glmnet(x[train,],y[train])

# compute predictions on test set
pred = predict(lasso.tr,x[-train,])
dim(pred)

# compute test error
rmse = sqrt(apply((y[-train]-pred)^2,2,mean))
  
plot(log(lasso.tr$lambda), rmse, type="b", xlab="Log(lambda)")

# get best lambda looking for minimum test error
lam.best = lasso.tr$lambda[order(rmse)[1]]
lam.best

# get coefficients for best lambda
coef(lasso.tr, s=lam.best)