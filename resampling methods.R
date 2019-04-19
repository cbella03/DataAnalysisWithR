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


#Decision Trees
#The used dataset (from GitHub) contains some data regarding the features of the passengers of Titanic.
# I want to use the decision tree technique to predict whether some feature affected the probability of persons to survive.

library(readr)
library(dplyr)

titanic3 <- "https://goo.gl/At238b" %>% read_csv %>% select(survived, embarked, sex, sibsp, parch, fare) %>% mutate(embarked = factor(embarked), sex = factor(sex))

titanic3$survived <- as.factor(titanic3$survived) #the survived variable is transformed in a categorical variable

#let's fit a classification tree


tree.titanic3 <- tree(survived ~ embarked+sex+sibsp+parch+fare, titanic3)
summary(tree.titanic3)
plot(tree.titanic3)
text(tree.titanic3, pretty=0)

# to evaluate the performance of the model I build a tree using a training set and I estimate its error rate on the test data

set.seed(2)
train <- sample(1:nrow(titanic3), nrow(titanic3)/2)
titanic3.test <- titanic3[-train,]
survived.test <- titanic3$survived[-train]

tree.titanic3.train <- tree(survived ~ embarked+sex+sibsp+parch+fare, titanic3, subset=train)
plot(tree.titanic3.train)
text(tree.titanic3.train, pretty=0)

tree.titanic3.pred <- predict(tree.titanic3.train, titanic3.test, type="class")
mean(tree.titanic3.pred != survived.test)

#check:
table(tree.titanic3.pred, survived.test)


#pruning the tree to reduce variance and to determine the optimal level of tree complexity

set.seed(3)
cv.titanic3 <- cv.tree(tree.titanic3.train, FUN=prune.misclass)
print(cv.titanic3)

#according to the results the best tree has 8 or 4 leaves. Since the tree has already 8 leaves, there is no need to prune.
#However, we can prune it to 4 leaves and calculate the new test error rate.

prune.titanic3 <- prune.misclass(tree.titanic3.train, best=4)
plot(prune.titanic3)
text(prune.titanic3, pretty=0)


tree.prune.titanic3.pred <- predict(prune.titanic3, titanic3.test, type="class")
mean(tree.prune.titanic3.pred!=survived.test)

#the error rate is the same as the tree with 8 leaves. However, considering the interpretability, the tree with 4 leaves is better.

#Bagging and Random Forests
#this analysis will be carried out on an updated version of the titanic dataset. The dataset is available in the Github repository.
#the dataset needs to be cleaned and transformed (categorical variables)
library(dplyr) 
titanic3 <- read.csv("C:/Users/carlo/Desktop/DataAnalysisWithR/titanic3.csv") 
titanic3 <- select(titanic3,-name, -ticket, -boat, -body, -home.dest, -cabin) %>% mutate(embarked = factor(embarked), sex = factor(sex), pclass=factor(pclass), survived01 = as.factor(titanic3$survived)) 
summary(titanic3)

nrow(titanic3)
titanic3 <- na.omit(titanic3)
nrow(titanic3)

library(randomForest)

#create a training set and a test set

set.seed(8)
train <- sample(1:nrow(titanic3), nrow(titanic3)/2)
test <- titanic3[-train,]

survived.test <- titanic3$survived[-train]
survived01.test <- titanic3$survived01[-train]


#the training set can be used to build a bagged model for: y: survived, x: all the features other than survived and survived01

set.seed(6)
bag.titanic <- randomForest(survived ~ .-survived01, data = titanic3, subset = train, mtry=7, importance=TRUE)
print(bag.titanic)

bag.pred <- predict(bag.titanic, newdata=test)
bag.pred.class <- ifelse(bag.pred <= 0.5, "0", "1")
mean(bag.pred.class!=survived.test) # this is the mean error rate on the test dataset

#a bagged model can be built also on: y: survived01, x: all the features other than survived01 and survived

bag.titanic01 <- randomForest (survived01 ~.-survived, data=titanic3, subset=train, mtry=7, importance=TRUE)
print(bag.titanic01)
bag.pred01 <- predict(bag.titanic01, newdata=test, type="class")
print(mean(bag.pred01!=survived.test))


#we check the variable importance for the two previous models

importance(bag.titanic)
varImpPlot(bag.titanic)

importance(bag.titanic01)
varImpPlot(bag.titanic01)

barplot(sort(importance(bag.titanic)[,1], decreasing=TRUE), xlab="Relative Importance", col="red", horiz = TRUE, las=1)

#as we can see from the %IncMSE, the importance of both models coincide.


x_test <- test[, -c(2,9)] #indicates the test set of all predictors without survived and survived01

bag.titanic01 = randomForest(survived01 ~ .-survived, data=titanic3, subset=train, importance=TRUE, xtest=x_test, ytest=survived01.test, mtry=7, ntree=200)
# we have inserted the test set when building the model

plot(1:200, bag.titanic01$test$err.rate[,1], type="l", xlab="Number of Bootstrap Data Sets", ylab="Test Error Rate", ylim=c(0.17, 0.30), xlim=c(0,205))
abline(h=bag.titanic01$test$err.rate[1,1], lty=2, col="red")

yhat.ter.ave <-rep(0,200)# a vector for Test Error Rate using averaging
for(j in 1:200){
  set.seed(6)
  bag.titanic <-randomForest(survived ~ .-survived01,data=titanic3,subset = train,mtry=7,importance = TRUE,ntree=j)
  bag.pred <-predict(bag.titanic, newdata = test)
  bag.pred.class <-ifelse(bag.pred<=0.5, "0", "1")
  yhat.ter.ave[j] <-mean(bag.pred.class!=survived.test)
}

lines(yhat.ter.ave,col="blue")
legend("topright",c("single tree","majority vote","averaging prob"),lty=c(2,1,1),col=c("red","black","blue"))

#the plot shows 3 lines:
#red dashed line: the test error rate of a single tree
#black curve: the test error rates for majority vote
#blue curve: the test error rates for averaging the probabilities







