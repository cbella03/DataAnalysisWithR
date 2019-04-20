
#Clustering

#K-means Clustering
#we are going to use the Chatterjee-Price attitude Data, taken from the datasets package of R.
# The data are aggregated from questionnaires of approximately 35 employees for each of 30 (randomly selected) departments. 
# The numbers give the percent proportion of favourable responses to seven questions in each department. 
# For more details, see ?attitude. When performing clustering if data contains multiple (or more than 2) variables, 
# In this exercise, we'll take a subset of the attitude dataset and consider only two variables privileges and 
# learning, that is we would like to cluster the attitude dataset with the responses from all 30 departments when it comes to
# privileges and learning.

library(datasets)
dat <- attitude[,c(3,4)]

#first let's plot it
plot(dat, main = "% of favourable responses to Learning and Privilege", pch=20, cex=2)

#in this first attempt we will set k = 2, and nstart = 1 to perform a k-means clustering 

km.out1 <- kmeans(dat, 2, nstart=1)
km.out1$tot.withinss
plot(dat, col= (km.out1$cluster +1), main="K-means result with 2 clusters", pch=20, cex=2)

#we can check the tot.withinss for several different k

totWithinss <- rep(0,15)
for(i in 1:15){
  set.seed(70)
  totWithinss[i] <- kmeans(dat, i, nstart=100)$tot.withinss
}

#alternatively, we can identify the best value of k by using the elbow method, by looking in the plot when the decrease of the within sum of sqaures begins to slow down

plot(1:15, totWithinss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares", main="Assessing the Optimal Number of Clusters with the elbow method", pch=20, cex=2)

# 6 is the best value for k. Let's plot the resulting clusters

k=6
set.seed(70)
km.out.k6 <- kmeans(dat, 6, nstart=100)
plot(dat, col = (km.out.k6$cluster +1), main="K-Means result with 6 clusters", pch=20, cex=2)


#hierarchical clustering

#The gene expression data set (Ch10Ex11.csv) consists of 40 tissue samples (40 columns) with measurements on 1,000 genes 
# (1000 rows). The ???rst 20 samples are from healthy patients, while the second 20 are from a diseased group. 
# The data set is saved into the Github repository. we need to transpose it such that the rows are for tissue samples, and
# the columns are for different variables.

DF <- read.csv("C:/Users/carlo/Desktop/DataAnalysisWithR/Ch10Ex11.csv")
DF <- t(DF)

#first, we calculate the dissimilarity metric between the samples, computed as 1 - correlation between the samples.
# since core computes the correlation of columns, we need to take the transpose of DF.

D <- as.dist(1 - cor(t(DF)))

#we can apply hierarchical clustering using different types of linkage

hclust.cor.comp <- hclust(D, method="complete") # Complete linkage
hclust.cor.ave <- hclust(D, method="average") #Average linkage 
hclust.cor.sing <- hclust(D, method="single") #Single linkage 
hclust.cor.cent <- hclust(D, method="centroid") #Centroid linkage 

# We can plot the four dendrograms 

par(mfrow=c(2,2))
plot(hclust.cor.comp, main="Complete Linkage", cex=0.9) 
plot(hclust.cor.ave, main="Average Linkage", cex=0.9) 
plot(hclust.cor.sing, main="Single Linkage", cex=0.9) 
plot(hclust.cor.cent, main="Centroid Linkage", cex=0.9)

# do the genes separate the samples into two groups (as it we may guess)? to answer we generate a confusion matrix
# on the predicted and true number of healthy/diseased patients.


print(table(predicted = cutree(hclust.cor.comp, k=2), truth=c(rep(0,20), rep(1,20))))
print(table(predicted = cutree(hclust.cor.ave, k=2), truth=c(rep(0,20), rep(1,20))))
print(table(predicted = cutree(hclust.cor.sing, k=2), truth=c(rep(0,20), rep(1,20))))
print(table(predicted = cutree(hclust.cor.cent, k=2), truth=c(rep(0,20), rep(1,20))))

#if we compare the predicted cluster label to the known truth label where we take 0 to be a healthy patient and 1 to be a diseased patient
# the predicted class label of 1 correspond to 10 healthy patients, while the predicted class of 2 corresponds to 10 healthy patients and 10 diseased patients.
# this might indicate that we want to look for three clusters where the extra cluster might split the 10 patients from the 20 diseased ones.




