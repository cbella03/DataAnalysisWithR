#Resampling methods
# The Auto dataset in the ISLR package of R contains a list of features for a number of cars
# I will focus on a pair of variables: mpg and hoursepower

#first, I will divide the dataset into two subsets: a training set and a test set

set.seed(100)
train <- sample(nrow(Auto), nrow(Auto)/2)
train.set <- Auto[train]

#I'm going to use the training set to train a linear model (linear regression)
lm.fit.train <- lm(mpg~horsepower, data=Auto, subset=train)

#now I can use the test set to make an estimate of the test MSE for the given linear model. Additionally, I will consider a polynomial model

error <- rep(0,3)
error[1] <- mean((Auto$mpg - predict(lm.fit.train, Auto))[-train]^2)
print(error[1])

lm.fit.train2 <- lm(mpg~poly(horsepower, 2), data=Auto, subset=train)
error[2] <- mean((Auto$mpg - predict(lm.fit.train2, Auto))[-train]^2)

lm.fit.train3 <- lm(mpg~poly(horsepower, 3), data=Auto, subset=train)
error[3] <- mean((Auto$mpg - predict(lm.fit.train3, Auto))[-train]^2)

sapply(error, print)
#conclusions: a model that predicts mpg using a quadratic function of horsepower performs better than a model that involves only a linear function of horsepower, and there is little evidence in favour of a model that uses a cubic function of horsepower


#now I want to check the test MSE for regression models of degree from 1 to 10 and for a number of 10 different seeds
# first, I calculate the errors for the first seed. For the errors of the remaining seeds I use a matrix.
# I plot the variability on the results

seeds <- seq(1:10)
train <- sample(392, 196)
errors <- rep(0, 10)
for (i in 1:10){
  lm.fit.train <- lm(mpg~poly(horsepower, i), data=Auto, subset=train)
  errors[i] <- mean((Auto$mpg - predict(lm.fit, Auto))[-train]^2)
}

errorMatrix <- matrix(nrow=10, ncol=10)
errorMatrix[1] <- errors

plot(errors, col=1, pch=".", xlab="Degrees of Polynomial", ylab="Mean Squared Error", main="10 times random split", ylim=c(14,27), xlim=c(0,12))

for(i in 2:10){
  set.seed(i)
  train <- sample(392, 196)
  for(j in 1:10){
    lm.fit.train <- lm(mpg~poly(horsepower, j), data=Auto, subset=train)
    errorMatrix[i, j] <- mean((Auto$mpg - predict(lm.fit.train, Auto))[-train]^2)
  }
  lines(errorMatrix[i,], col=i)
}

legend("bottomright", c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10"), lty=rep(1,10), col=1:10)


#let's experiment on the LOOCV for increasingly complex polynomial fits (from 1 to 10) and record the LOOCV estimate for the test error for each degree.

library(boot)

cv.error <- rep(0, 10)
for (i in 1:10){
  glm.fit <- glm(mpg~poly(horsepower, i), data=Auto)
  cv.error[i] <- cv.glm(Auto, glm.fit)$delta[1]
}


print(cv.error)

plot(cv.error, col="blue", xlab = "Degrees of Polynomial", ylab="Mean Squared Error", main="LOOCV", ylim=c(15,28))
lines(cv.error, col="blue")

#let's implement k-fold CV (10 fold) for 10 different polynomial fits and plot the resulting errors

plot(1, type="l", main="K-fold CV", xlab="Degrees of Polynomial", ylab="Mean Squared Error", xlim=c(0,12), ylim=c(14,27))

cv.errors <- rep(0,10)
set.seed(100)
for (i in 1:10){
  glm.fit <- glm(mpg~poly(horsepower, i), data=Auto)
  cv.errors[i] <- cv.glm(Auto, glm.fit, K=10)$delta[1]
}
lines(cv.errors, col=i)
print(cv.errors)


# I can do the same for 10 different seeds

plot(1, type="l", main="K-fold CV", xlab="Degrees of Polynomial", ylab="Mean Squared Error", xlim=c(0,12), ylim=c(14,27))

cv.errors2 <- rep(0,10)
set.seed(100)
for (i in 1:10){
  set.seed(i)
  for (j in 1:10){
    glm.fit <- glm(mpg~poly(horsepower, j), data=Auto)
    cv.errors2[j] <- cv.glm(Auto, glm.fit, K=10)$delta[1]
  }
  lines(cv.errors2, col=i)
}








