#####################################
# Decision Trees                    #
#####################################

require(ISLR)
require(tree)
attach(Carseats)

hist(Sales)

# create a binary variable
High = ifelse(Sales<=8,"No","Yes")

# adding the new variable to the data set
Carseats = data.frame(Carseats, High)

# deep level tree
tree.carseats = tree(High~.-Sales, data=Carseats)
summary(tree.carseats)
plot(tree.carseats)
text(tree.carseats, pretty=0)

tree.carseats

# create a train and test set
set.seed(1011)

# sample 250 indexes without replacement
train=sample(1:nrow(Carseats), 250)

tree.carseats=tree(High~.-Sales, data=Carseats, subset=train)
plot(tree.carseats)
text(tree.carseats, pretty=0)

# make predictions
tree.pred = predict(tree.carseats, Carseats[-train,], type="class")

# classification table
with(Carseats[-train,], table(tree.pred,High))

# use CV to prune the tree optimally
cv.carseats = cv.tree(tree.carseats, FUN=prune.misclass)
cv.carseats
plot(cv.carseats)

# pickup the best pruned tree, i.e. size=13
prune.carseats=prune.misclass(tree.carseats, best=13)
plot(prune.carseats) 
text(prune.carseats, pretty=0)

# check mis-classification
tree.pred = predict(prune.carseats, Carseats[-train,], type="class")
with(Carseats[-train,], table(tree.pred,High))



