# plot data to observe patterns
plot(cdf)

# use nbclust to determine number of clusters 
library(NbClust)
set.seed(1234)
nc <- NbClust(cdf, min.nc=2, max.nc=15, method="kmeans")
barplot(table(nc$Best.n[1,]),
        xlab="Numer of Clusters", ylab="Number of Criteria",
        main="Number of Clusters Chosen by 26 Criteria")

# fit kmeans model
set.seed(1234)
fit.km <- kmeans(df, centers=3,  nstart=25)

# bind cluster category to data frame
clustered_df <- cbind(cdf, clusterNum = fit.km$cluster)

# save df
write.csv(clustered_df, "CLUSTERED_DF.CSV")