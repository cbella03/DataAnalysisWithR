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

