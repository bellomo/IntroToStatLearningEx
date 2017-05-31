# Best subset regression
########################

library(leaps)
library(ISLR)
summary(Hitters)

# remove any row with a missing values in it
Hitters = na.omit(Hitters)

# check for missing values
with(Hitters,sum(is.na(Salary)))

regfit.full=regsubsets(Salary~.,data=Hitters, nvmax = 19)
regfit.summary=summary(regfit.full)
names(regfit.summary)

# plot CP error estimate to choose which subset is best
plot(regfit.summary$cp, xlab = "Number of variables", ylab = "Cp")
regfit.min=which.min(regfit.summary$cp)
points(10,regfit.summary$cp[regfit.min],pch=20,col="red")

# plot from regsubsets object showing which variables are used
plot(regfit.full,scale="Cp")

# showing coefficients for best subset
coef(regfit.full,regfit.min)