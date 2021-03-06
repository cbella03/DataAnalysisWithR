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

#we can calculate and plot the test error rate with random forest of different mtry (number of variables sampled when splitting)
#we can call the test set directly in the function

testErrorRate <- rep(0,7) 
for(i in 1:7){ 
  set.seed(6) 
  bag.titanic01 <- randomForest(survived01 ~ .-survived, data=titanic3, subset=train, mtry=i, importance=TRUE, xtest=x_test, ytest=survived01.test, ntree=500) 
  testErrorRate[i] <- bag.titanic01$test$err.rate[500,1] 
} 

plot(testErrorRate,type="b",xlab="mtry",ylab="Test Error Rate")

#the last plot will display the test error rate of random forest of 7 different mtry value

plot(0, xlab="Number of Trees",ylab="Test Error Rate", xlim=c(1,540), ylim =c(0.18,0.28))


for(i in 1:7){ 
  bag.titanic01 <- randomForest(survived01 ~ .-survived, data=titanic3, subset=train, mtry=i, importance=TRUE, xtest=x_test, ytest=survived01.test, ntree=500)
  lines(bag.titanic01$test$err.rate[,1],col=i,type="l")
}

legend(title = "mtry", "topright", c("1","2","3","4","5","6","7"), lty=rep(1,7),col=1:7)



