#Support Vector Machine (SVM)

#I will generate a toy dataset on which to work on. My goal is to build idfferent SVM models to predict class labels and estimate their test erro rates.

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
