 install.packages(c("cluster", "rattle.data","NbClust"))
 library(NbClust)
 View(wine)
 #Load the data and look at the first few rows
 data(wine, package="rattle.data")
 head(wine)
 #EXERCISE 1
 #remove the 1st column from data and scale it
 df <- scale(wine[-1])
#Graph method 1
 wssplot <- function(df, nc=15, seed=1234){
   wss <- (nrow(df)-1)*sum(apply(df,2,var))
   for (i in 2:nc){
     set.seed(seed)
     wss[i] <- sum(kmeans(df, centers=i)$withinss)}
   
   plot(1:nc, wss, type="b", xlab="Number of Clusters",
        ylab="Within groups sum of squares")}
  wssplot(df)
 
#EXERCISE 2
  #* This method seems to suggest 3 Clusters
  #*we notice a drop within groups sum of squares when moving from 1 to 3 clusters.
  #After 3 clusters, this decrease drops off, suggesting that a 3-cluster solution may be a good fit to the data
  
  #graph method 2
  library(NbClust)
  set.seed(1234)
  nc <- NbClust(df, min.nc=2, max.nc=15, method="kmeans")
  barplot(table(nc$Best.n[1,]),
          xlab="Numer of Clusters", ylab="Number of Criteria",
          main="Number of Clusters Chosen by 26 Criteria")
  
#EXERCISE 3
  #  15 of 26 criteria provided by the NbClust package suggest a 3 cluster solution.
  # According to the majority rule, the best number of clusters is  3 
  
#EXERCISE 4
  set.seed(1234)
  fit.km <- kmeans(df, 3, nstart=25)
#EXERCISE 5
  # Now we want to evaluate how well this clustering does.
  fit.km$size
  fit.km$centers                                              
  aggregate(wine, by=list(cluster=fit.km$cluster), mean)
  install.packages("flexclust")
   library(flexclust)
  ct.km <- table(wine$Type,fit.km$cluster)
  ct.km
  randIndex(ct.km)
  # This is a good clustering
#Exercise 6
  install.packages("cluster")
  library("cluster")
  clusplot(df, fit.km$cluster)
  
  #* This is a good clustering
  
  