# Forward Stepwise Selection
############################
library(leaps)

# data
library(ISLR)
summary(Hitters)

regfit.fwd = regsubsets(Salary~.,data = Hitters, 
                        nvmax = ncol(Hitters)-1,
                        method = "forward")
summary(regfit.fwd)
plot(regfit.fwd,scale="Cp")

# Use a validation set to choose the best subset
#################################################
# 2/3 training (~180) and 1/3 test samples

set.seed(1)
train = sample(seq(nrow(Hitters)), 180, replace = FALSE)
train

# define subsets on train data
regfit.fwd = regsubsets(Salary~., data = Hitters[train,],
                        nvmax = ncol(Hitters)-1,
                        method = "forward")

# calculate test error
test.errors=rep(NA,19)
x.test = model.matrix(Salary~.,data = Hitters[-train,])

# test error = MSE 
for(i in 1:19){
  coefi = coef(regfit.fwd,id=i)
  pred = x.test[,names(coefi)]%*%coefi
  test.errors[i] = mean((Hitters$Salary[-train]-pred)^2)
}

# MSE on test data
plot(sqrt(test.errors), ylab = "Root MSE", ylim = c(300,400),
     pch=19, type="b")
# RSS on training data
points(sqrt(regfit.fwd$rss[-1]/180),col="blue",pch=19,type="b")
legend("topright",
       legend = c("Training","Test"),
       col = c("blue","black"),
       pch=19)
       
# function to compute preductions from regsubsets
predict.regsubsets=function(object,newdata,id,...) {
  form=as.formula(object$call[[2]])
  mat=model.matrix(form,newdata)
  coefi=coef(object,id=id)
  mat[,names(coefi)]%*%coefi
}

# Use Cross-Validation to choose best subset
#############################################

set.seed(11)
folds=sample(rep(1:10,length = nrow(Hitters)))
folds
table(folds)

cv.errors = matrix(NA,10,19)

# loop on folds
for(k in 1:10) {
  best.fit = regsubsets(Salary~., 
                        data=Hitters[folds!=k,],
                        nvmax = 19,
                        method = "forward")
  for(i in 1:19) {
    pred = predict(best.fit,Hitters[folds==k,],id=i)
    cv.errors[k,i] = mean((Hitters$Salary[folds==k]-pred)^2)
    }
}

rmse.cv = sqrt(apply(cv.errors,2,mean))
plot(rmse.cv,pch=19,type="b")






       