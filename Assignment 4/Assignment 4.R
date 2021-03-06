# Settingworking directry to current File location
setwd("D:/A_Sem_1/ML/Assignment 4")

# Importing required libraries
library(tidyverse)  # data manipulation
library(cluster)    # clustering algorithms
library(factoextra) # clustering algorithms & visualization

# A: Kmeans Algo on Data set.
# Importing the data set- Pharmaceuticals.csv

library(readr)
Pharmaceuticals <- read_csv("Pharmaceuticals.csv", 
                            col_types = cols(Market_Cap = col_number(), 
                                             Beta = col_number(), PE_Ratio = col_number(), 
                                             ROE = col_number(), ROA = col_number(), 
                                             Asset_Turnover = col_number(), Leverage = col_number(), 
                                             Rev_Growth = col_number(), Net_Profit_Margin = col_number()))
data_df <- data.frame(Pharmaceuticals[,3:10])


# Removing missing values from the dataset.

data_df <- na.omit(data_df)

# Standardizing all the values from column 3-9 (Market Cap - Net Profit Margin)

data_df_scaled <- scale(data_df)

# Now we are going to compute the distance between the rows of the data. 
# We will use Euclidean distance which is the default distance used.
# But we can also use any other distance

distance <- get_dist(data_df_scaled)
fviz_dist(distance, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))

# Run K-Means Clustering Algo
# Now we will run K-means algo, with intially 2 clusters and with an mutilple intial
# configuration of 25.

kmeans_model <- kmeans(data_df_scaled, centers = 2, nstart = 25)
str(kmeans_model)

# There are several parameters that we get as an output.
# The important one's are:
# Clusters : The vector of integers which tells us the cluster to which each point is allocated.
# centers : Matrix of cluster centers
# Size : The size of each cluster (In this we have 11 values in C1 and 10 in C2)

# When we print the model itself, we see the actual grouping within the clusters.
kmeans_model

# To view it and understand this model, we can plot this using fviz_cluster() method

fviz_cluster(kmeans_model, data = data_df_scaled)

# Since we know that K is a hyperparameter that the user needs to set, we will
# run several iterationd of Kmeans algo, each time changing the value of K.
# This will help us find the optimal value of K.

kmeans_model3 <- kmeans(data_df_scaled, centers = 3, nstart = 25)
kmeans_model4 <- kmeans(data_df_scaled, centers = 4, nstart = 25)
kmeans_model5 <- kmeans(data_df_scaled, centers = 5, nstart = 25)
kmeans_model6 <- kmeans(data_df_scaled, centers = 6, nstart = 25)
kmeans_model7 <- kmeans(data_df_scaled, centers = 7, nstart = 25)

# plots to compare
p1 <- fviz_cluster(kmeans_model, geom = "point", data = data_df_scaled) + ggtitle("k = 2")
p2 <- fviz_cluster(kmeans_model3, geom = "point",  data = data_df_scaled) + ggtitle("k = 3")
p3 <- fviz_cluster(kmeans_model4, geom = "point",  data = data_df_scaled) + ggtitle("k = 4")
p4 <- fviz_cluster(kmeans_model5, geom = "point",  data = data_df_scaled) + ggtitle("k = 5")
p5 <- fviz_cluster(kmeans_model6, geom = "point",  data = data_df_scaled) + ggtitle("k = 6")
p6 <- fviz_cluster(kmeans_model7, geom = "point",  data = data_df_scaled) + ggtitle("k = 7")

library(gridExtra)
grid.arrange(p1, p2, p3, p4, p5, p6, nrow = 4)

# Now as the graph depicts, we find ourself in new type of situation.
# This situation is to decide which value of k to consider?
# Thankfully, we have 3 techniques to determine this. 
# These techniques are:
# 1. Elbow method
# 2. Silhouette method
# 3. Gap statistic

# In this scenario we will use the Elbow method. We will consider the value
# of k, where there is bend in the plot.
# We use fviz_nbclust() method, which calculates this for us.

set.seed(123)
fviz_nbclust(data_df_scaled, kmeans, method = "wss")

# Here we will not use k=7 as the value goes up before going down.
# So we will choose k = 5.

# Similarly we can calculate Silhouette method and Gap statistic

# Silhouette method
# This method determines how well an object lies in a cluster.
# In this, a high average silhouette width indicates good clustering.
fviz_nbclust(data_df_scaled, kmeans, method = "silhouette")

# Gap Statistic
set.seed(123)
gap_stat <- clusGap(data_df_scaled, FUN = kmeans, nstart = 25,
                    K.max = 10, B = 50)
fviz_gap_stat(gap_stat)

# Now we can extract the final result for our kmeans method
# by setting value of k = 5

set.seed(123)
final_model <- kmeans(data_df_scaled, 5, nstart = 25)
print(final_model)

# And we can visualize the result using fviz_cluster()

fviz_cluster(final_model, data = data_df_scaled)

# We can also take k = 2 for our final result and see
# what result we get by that.
set.seed(123)
final_model2 <- kmeans(data_df_scaled, 2, nstart = 25)
print(final_model2)

# And we can visualize the result using fviz_cluster()

fviz_cluster(final_model2, data = data_df_scaled)


# B. We get 5 clusters with following rows from data.
# Cluster 1 : 2, 18 [AGN, PHA] [MOERATE BUY, HOLD]
# Cluster 2 : 6, 8, 12 [BAY, CHTT, IVX] [HOLD, MODERATE BUY,HOLD]
# Cluster 3 : 3, 7, 10, 16, 19, 21 [AHM, BMY, LLY, NVS, SGP, WYE] [STRONG BUY, MODERATE SELL, HOLD, HOLD, HOLD,HOLD]
# Cluster 4 : 11, 13, 15, 17 [GSK, JNJ, MRK, PFE] [HOLD,MODERATE BUY, HOLD, MODERATE BUY]
# Cluster 5 : 5, 9, 14, 20 [AVE, ELN, MRX, WPI] [MODERATE BUY,MODERATE SELL,MODERATE BUY,MODERATE SELL]

# C.  Yes. There appears to be a pattern emerging when one observes the cluster formation
#     the values from the Median_recommendation column.
#  For Instance, cluster 3 that has highest Market Cap, ROA and asset turnover does not
#  have strong sell recommendation, instead it has 1 STRONG BUY and 4 HOLD recommendations.

# D. The Clusters can be loosely named as:
#    Cluster 1. Moderate buy and hold cluster.
#    Cluster 2. Moderate buy and strong hold cluster (because of 2 Hold values in it)
#    Cluster 3. Solid hold and strong buy cluster
#    Cluster 4. Hold and moderate buy cluster(because of 2 entries for each of cluster.)
#    Cluster 5. Moderate buy and Sell Cluster.

# D. fig.width=10