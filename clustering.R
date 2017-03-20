# This mini-project is based on the K-Means exercise from 'R in Action'
# Go here for the original blog post and solutions
# http://www.r-bloggers.com/k-means-clustering-from-r-in-action/

# Exercise 0: Install these packages if you don't have them already

install.packages(c("cluster", "rattle","NbClust"))
library(cluster)
library(rattle)

# Now load the data and look at the first few rows
data(wine, package="rattle")
head(wine)

# Exercise 1: Remove the first column from the data and scale
# it using the scale() function

# Remove the Type column and scale it
df <- wine[2:14] 
scale(df)

# Now we'd like to cluster the data using K-Means. 
# How do we decide how many clusters to use if you don't know that already?
# We'll try two methods.

# Method 1: A plot of the total within-groups sums of squares against the 
# number of clusters in a K-means solution can be helpful. A bend in the 
# graph can suggest the appropriate number of clusters. 

wssplot <- function(data, nc=15, seed=1234){
	              wss <- (nrow(data)-1)*sum(apply(data,2,var))
               	      for (i in 2:nc){
		        set.seed(seed)
	                wss[i] <- sum(kmeans(data, centers=i)$withinss)}
	                
		      plot(1:nc, wss, type="b", xlab="Number of Clusters",
	                        ylab="Within groups sum of squares")
	   }

wssplot(df)

# Exercise 2:
#   * How many clusters does this method suggest?
#   * Why does this method work? What's the intuition behind it?
#   * Look at the code for wssplot() and figure out how it works

# From the wssplot method, we find that a cluster of 3 is most appopriate because
# as we see in the plot there is a sharp drop within the group's sum of squares
# from Cluster 1 to 3. Then from Cluster 4 and onward the difference is minimal.
# This suggest cluster of 3 will best fit group the different wine attributes.

# Method 2: Use the NbClust library, which runs many experiments
# and gives a distribution of potential number of clusters.

library(NbClust)
set.seed(1234)
nc <- NbClust(df, min.nc=2, max.nc=15, method="kmeans")
barplot(table(nc$Best.n[1,]),
	          xlab="Number of Clusters", ylab="Number of Criteria",
		            main="Number of Clusters Chosen by 26 Criteria")

# Exercise 3: How many clusters does this method suggest?

# From the NbClust plot, and conclusion statements, the best number of clusters is 2
# since 10 out of the 24 wine attributes suggested that to be the most appropriate.
# We also notice that 2 out of the original 26 attributes didn't fall under any cluster.

# Exercise 4: Once you've picked the number of clusters, run k-means 
# using this number of clusters. Output the result of calling kmeans()
# into a variable fit.km

set.seed(1234)
fit.km <- kmeans(df, 2, nstart = 25)

# Now we want to evaluate how well this clustering does.

# Exercise 5: using the table() function, show how the clusters in fit.km$clusters
# compares to the actual wine types in wine$Type. Would you consider this a good
# clustering?

clust_check <- table(wine$Type, fit.km$cluster)
clust_check

# Since in the original Wine type had levels of 3 and we ran k-means with a cluster of 2
# there was not a good clustering, since the wine type that were 3 were inaccurately placed,
# in cluster 2 through our method. It would have been more appropriate to do a cluster of 3,
# method 1 using the wssplot method.

# Exercise 6:
# * Visualize these clusters using  function clusplot() from the cluster library
# * Would you consider this a good clustering?

clusplot(wine, fit.km$cluster)

# The clusplot supports our previous claim that a cluster of 2 is not a good cluster for the 
# wine attributes. The plot shows a lot of overlapping between the elements and would have
# been better constructed with a cluster of 3 (left, top and right clusters)

