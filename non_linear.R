library(ISLR)
attach(Wage)

################
# Polynomials  #
################

# poly generates orthogonal polynomials, this allows to
# check the significance of each term separately.
# It works if we have a single predictor in the model!
fit = lm(wage~poly(age,4),data=Wage)
summary(fit)

# min and max values of age
agelims = range(age) 

# grid values on age
age.grid = seq(from=agelims[1],to=agelims[2])

# predic data from fitted function, "se" adding standard errors
preds = predict(fit, newdata = list(age=age.grid), se=TRUE)

# make error bands at 2 sigma
se.bands = cbind(preds$fit+2*preds$se, preds$fit-2*preds$se)

# plot data
plot(age, wage, col="darkgray")

# plot fitted function
lines(age.grid, preds$fit, lwd=2, col="blue")

# plot error bands
matlines(age.grid, se.bands, col = "blue", lty = 2)

# more predictors, how to test for their significance !!
fita = lm(wage~education, data=Wage)
fitb = lm(wage~education+age, data=Wage)
fitc = lm(wage~education+poly(age,2), data=Wage)
fitd = lm(wage~education+poly(age,3), data=Wage)
anova(fita,fitb,fitc,fitd)

# polynomial logistic regression
#################################

fit = glm(I(wage>250) ~ poly(age,3), data=Wage, family = binomial)
summary(fit)

preds = predict(fit, list(age=age.grid), se=TRUE)
se.bands = preds$fit + cbind(fit=0, lower=-2*preds$se, upper=2*preds$se)
se.bands[1:5,]

# computations done in the logit scale, transform to probability
# applying the inverse logit mapping p = e^y / (1 + e^y)

prob.bands = exp(se.bands)/(1+exp(se.bands))
matplot(age.grid, prob.bands, col = "blue", lwd = c(2,1,1), 
        lty = c(1,2,2), type = "l", ylim = c(0,.1))
points(jitter(age), I(wage>250)/10, pch = "|", cex = .5)
# jitter adds some uniform noise to better visualise density

##############
# Splines    # 
##############

require(splines)

# cubic spline
fit = lm(wage~bs(age, knots = c(25,40,60)), data = Wage)

plot(age,wage,col = "darkgray")
lines(age.grid, predict(fit, list(age=age.grid)), col = "blue", lwd=2)
abline(v=c(25,40,60), lty=2, col = "blue")

# smoothing spline (no need to specify knots, 
# use smoothing parameter, "df")
fit = smooth.spline(age,wage,df=16)
lines(fit,col = "red", lwd = 2)

# use leave one-out cross validation
fit = smooth.spline(age, wage, cv = TRUE)
lines(fit,col = "purple", lwd = 2)

################################
# Generalize Additive Models   #
################################
require(gam)

# regression
gam1 = gam(wage ~ s(age,df=4) + s(year,df=4) + education, data = Wage)
# s -> smoothing split in gam
par(mfrow=c(1,3))
plot(gam1,se=T)

# logistic regression
gam2 = gam(I(wage>250) ~ s(age,df=4) + s(year,df=4) + education, data = Wage)
par(mfrow=c(1,3))
plot(gam2,se=T)

# do we need a non-linear term for year??
gam2a = gam(I(wage>250) ~ s(age,df=4) + year + education, data = Wage)
anova(gam2a, gam2, test="Chisq")

# using gam plot function for lm fit
lm1 = lm(wage ~ ns(age,df=4) + ns(year,df=4) + education, data = Wage)
par(mfrow=c(1,3))
plot.gam(lm1,se=T)






