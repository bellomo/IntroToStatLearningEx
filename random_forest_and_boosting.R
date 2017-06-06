###########################
# Random Forest           #
###########################

require(randomForest)
require(MASS)

# define train and test sets
set.seed(101)
dim(Boston)
train = sample(1:nrow(Boston), 300)

# check details of the dataset
?Boston
# response is "medv"

# Random Forest
rf.boston = randomForest(medv~., data=Boston, subset=train)
rf.boston

# tuning parameter : number of variables used at each split
# to decide how to split, "mtry"

oob.err=double(13)
test.err=double(13)

for(mtry in 1:13) {
  fit = randomForest(medv~., data=Boston, subset=train, mtry=mtry, ntree=400)
  oob.err[mtry] = fit$mse[400]
  pred = predict(fit,Boston[-train,])
  test.err[mtry] = with(Boston[-train,], mean((medv-pred)^2))
}

matplot(1:mtry, cbind(test.err, oob.err), 
        pch=19, col=c("red","blue"), 
        type="b", 
        ylab="Mean Squared Error")
legend("topright", legend=c("OOB","Test"), pch=19, col=c("red","blue"))


##################################
# Boosting                       #
##################################

require(gbm)

boost.boston = gbm(medv~., data=Boston[train,], 
                   distribution="gaussian", # squared error loss
                   n.trees = 10000, 
                   shrinkage = 0.01, # how much we shrink each tree back
                   interaction.depth = 4) # number of splits per tree

summary(boost.boston)

# plot relation of vars with response
plot(boost.boston, i="lstat")
plot(boost.boston, i="rm")

# shrinkage can be tuned with CV...

# make predictions
n.trees = seq(from=100, to=10000, by=100)
predmat = predict(boost.boston, newdata = Boston[-train,], 
                  n.trees = n.trees)

berr = with(Boston[-train,], apply( (predmat-medv)^2, 2, mean))
plot(n.trees, berr, pch=19, ylab = "Mean Squared Error", xlab = "# trees",
     main = "Boosting Test Error")

# draw a red line for the minimum test error from random forest
abline(h = min(test.err), col = "red")
