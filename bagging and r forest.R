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



