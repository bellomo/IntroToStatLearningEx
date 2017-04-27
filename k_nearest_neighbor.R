library(class)
library(ISLR)

?knn

attach(Smarket)
Xlag=cbind(Lag1,Lag2)

# training set
train=Year<2005

knnpred = function(kk) {
  cat("KNN k=",kk,"\n")
  knn.pred=knn(Xlag[train,],Xlag[!train,],Direction[train],k=kk)
  #show(table(knn.pred,Direction[!train]))
  cat("Test success rate  = ",mean(knn.pred==Direction[!train]),"\n")
}

knnpred(1)
knnpred(2)
knnpred(3)
knnpred(4)
knnpred(5)
knnpred(10)