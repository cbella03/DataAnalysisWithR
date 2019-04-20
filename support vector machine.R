#Support Vector Machine (SVM)

#I will generate a toy dataset on which to work on. My goal is to build idfferent SVM models to predict class labels and estimate their test erro rates.
# the toy dataset is designed to be almost entirely separable
#the toy dataset will contain 200 observations, will include two quantitative variables and one categorical response variable (binary)
set.seed(300) 
x.pos <- matrix(rnorm(100*2,mean=0), nrow = 100, ncol = 2) 
set.seed(300) 
x.neg <- matrix(rnorm(100*2,mean=3), nrow = 100, ncol = 2) 
y <- c(rep(1,100),rep(-1,100)) 
x <- rbind(x.pos,x.neg) 
dat <- data.frame(x = x, y = as.factor(y))
plot(x, col = ifelse(y>0,1,2)) 
legend("topleft",c("Positive","Negative"),col=seq(2),pch=1,text.col=seq(2))

#we need a training set and a test set 
set.seed(400) 
train <- sample(200, 200*0.7) 
dat.train <- dat[train,] 
x.test <- x[-train,] 
y.test <- y[-train]

#train an svm of linear type with cost 1.
library(e1071)
svmfit.train.C1 <- svm(y ~ ., data = dat.train, kernel = "linear", cost = 1, scale = FALSE)
plot(svmfit.train.C1, dat.train)
summary(svmfit.train.C1)

#the summary shows that there are 11 support vectors, 6 from the class of y= -1 and 5 from the class of y = 1

#let's predict the class label of y on the test set and estimate the test error rate

y.pred.c1 <- predict(svmfit.train.C1, newdata=x.test)
mean(y.pred.c1 != y.test)

# now we can tune the cost parameter to obtain the best model

set.seed(1)
tune.out <- tune(svm, y ~ ., data = dat.train, kernel = "linear", ranges = list(cost = c(0.001,0.01,0.1,1,5,10,100,1000,10000,1e5)))
summary(tune.out)

#we will examine another case, of a toy dataset that is linearly inseparable

set.seed(300)
x.pos1 <-matrix(rnorm(30*2, mean=0), nrow=30, ncol=2)
x.pos2 <- matrix(rnorm(30*2,mean=3), nrow=30, ncol=2) 
set.seed(300) 
x.neg1 <- matrix(c(rnorm(30,mean=0)+3,rnorm(30,mean=0)),nrow=30,ncol=2) 
x.neg2<- matrix(c(rnorm(30,mean=3)-3,rnorm(30,mean=3)),nrow=30,ncol=2) 
y <- c(rep(1,60),rep(-1,60)) 
x <- rbind(x.pos1, x.pos2, x.neg1, x.neg2) 
dat2 <- data.frame(x = x, y = as.factor(y))

plot(x, col=ifelse(y > 0, 1, 2), pch = ifelse(y > 0, 1, 2))
legend("topleft", c("Positive", "Negative"), col=seq(2), pch = 1:2, text.col = seq(2))

#training set and test set

set.seed(400)
train <- sample(120, 120*0.7)

dat2.train <- dat2[train,]
x.test <- x[-train,]
y.test <- y[-train]

#we build an SVM model with a radial kernel, gamma = 1 and cost = 1, and we estimate its test erro rate

svmfit.radial.G1C1 <- svm(y ~ ., data=dat2.train, kernel="radial", gamma=1, cost=1, scale = F)
summary(svmfit.radial.G1C1)

plot(svmfit.radial.G1C1, data = dat2.train)

y.pred.radial.G1C1 <- predict(svmfit.radial.G1C1, newdata=x.test)
mean(y.pred.radial.G1C1 != y.test)

#we can tune both cost and gamma parameters to find the best radial model

set.seed(1)
tune.out.radial <- tune(svm, y ~., data = dat2.train, kernel = "radial", ranges=list(cost= c(0.1,1,10,100,1000), gamma = c(0.5,1,2,3,4)))
summary(tune.out.radial)

y.pred.radial.best <- predict(tune.out.radial$best.model, newdata = x.test)
mean(y.pred.radial.best != y.test)
















