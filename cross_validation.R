library(ISLR)
library(boot)

?cv.glm

plot(mpg~horsepower,data=Auto)

# linear fit
glm.fit = glm(mpg~horsepower, data=Auto)
summary(glm.fit)

### Leave One Out Cross Validation (LOOCV)
cv.glm(Auto,glm.fit)$delta

### LOOCV using formula 5.2
loocv = function(fit) {
  h=lm.influence(fit)$h
  mean((residuals(fit)/(1-h))^2)
}
loocv(glm.fit)

### test with LOOCV different pol orders
cv.error = rep(0,5)
degree = 1:5
for(d in degree) {
  glm.fit=glm(mpg~poly(horsepower,d), data=Auto)
  cv.error[d] = loocv(glm.fit)
}
plot(degree,cv.error,type = "b")

### 10-fold CV
cv.error10 = rep(0,5)
degree = 1:5
for(d in degree) {
  glm.fit=glm(mpg~poly(horsepower,d), data=Auto)
  cv.error10[d] = cv.glm(Auto,glm.fit,K=10)$delta[1]
}

plot(degree,cv.error, type="b")
lines(degree,cv.error10, type="b", col="red")

