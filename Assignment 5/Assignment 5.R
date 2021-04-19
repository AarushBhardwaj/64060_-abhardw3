# Setting working directory
setwd("D:/A_Sem_1/ML/Assignment 5")
set.seed(123)
# Importing required library
library(cluster)
library(caret)
library(dendextend)
library(knitr)
library(factoextra)
# Importing the cereals dataset.

library(readr)
Cereals <- read_csv("Cereals.csv", col_types = cols(calories = col_number(), 
                                                    protein = col_number(), fat = col_number(), 
                                                    sodium = col_number(), fiber = col_number(), 
                                                    carbo = col_number(), sugars = col_number(), 
                                                    potass = col_number(), vitamins = col_number(), 
                                                    shelf = col_number(), weight = col_number(), 
                                                    cups = col_number(), rating = col_number()))

data_df <- data.frame(Cereals[,4:16])

# Data Preprocessing.
# Removing all NA values

data_df <- na.omit(data_df)

# Normalizing the Data
data_df_scaled <- scale(data_df)

# Now we will perform Hierarchical Clustering using hclust().
# Before using the hclust() method, we will use dist() mthod to calculate the
# Dissimilarity matrix

d <- dist(data_df_scaled, method = "euclidean")

# Perfrom Hierarchical Clustering using complete linkage

hc1_complete <- hclust(d, method = "complete")

# Plotting the dendogram

plot(hc1_complete, cex = 0.6, hang = -1)

# We can also use agnes() function to perfrom clustering
# The only difference between agnes() and hclust() is that
# agnes provides with agglomerative coefficient
# This coefficient helps us to determine whether the clustering
# Structure is strong or weak.

# Performing clustering using agnes() with single, complete, average and ward.

hc_single <- agnes(data_df_scaled, method = "single")
hc_complete <- agnes(data_df_scaled, method = "complete")
hc_average <- agnes(data_df_scaled, method = "average")
hc_ward <- agnes(data_df_scaled, method = "ward")

# The only difference between ward. D & ward. D2 is the input parameter.
# The Ward2 criterion values are “on a scale of distances”
# whereas the Ward1 criterion values are “on a scale of distances squared

# Now we will compare the agglomerative coefficients

# For Single
print(hc_single$ac)

# For complete
print(hc_complete$ac)

# For average
print(hc_average$ac)

# For Ward
print(hc_ward$ac)

# Clearly the results show that the wards method is the best with the value 
# of 0.904

# Plotting the agnes using ward method
pltree(hc_ward, cex = 0.6, hang = -1, main = "Dendrogram of agnes (Using Ward)")

# Cutting the Dendogram. We will take k = 4 by observing the distance.
rect.hclust(hc_ward, k = 4, border = 1:4)
cluster1 <- cutree(hc_ward, k=4)
df2 <- as.data.frame(cbind(data_df_scaled,cluster1))

#3. Commenting on the structure and the stability of the clusters

# Creating Partitions

part_1 <- data_df[1:55,]
part_2 <- data_df[56:74,]

agnes_ward <- agnes(scale(part_1), method = "ward")
agnes_average <- agnes(scale(part_1), method = "average")
agnes_complete <- agnes(scale(part_1), method = "complete")
agnes_single <- agnes(scale(part_1), method = "single")

cbind(ward=agnes_ward$ac, average=agnes_average$ac, complete=agnes_complete$ac, 
      single=agnes_single$ac)
pltree(agnes_ward, cex = 0.6, hang = -1, main = "Dendogram of Agnes with Partitioned Data (Using Ward)")

rect.hclust(agnes_ward, k = 3, border = 2:5)

cut_2 <- cutree(agnes_ward, k = 4)

result <- as.data.frame(cbind(part_1, cut_2))
result[result$cut_2==1,]
center1 <- colMeans(result[result$cut_2==1,])

result[result$cut_2==2,]
center2 <- colMeans(result[result$cut_2==2,])

result[result$cut_2==3,]
center3 <- colMeans(result[result$cut_2==3,])

result[result$cut_2==4,]
center4 <- colMeans(result[result$cut_2==4,])

centers <- rbind(center1, center2, center3, center4)

x2 <- as.data.frame(rbind(centers[,-14], part_2))

# Calculating Distance

d1 <- get_dist(x2)

mat1 <- as.matrix(d1)

df1 <- data.frame(data=seq(1,nrow(part_2),1), clusters = rep(0,nrow(part_2)))

for(i in 1:nrow(part_2)) {
  
  df1[i,2] <- which.min(mat1[i+4, 1:4])
}

df1

cbind(df2$cluster1[56:74], df1$clusters)
table(df2$cluster1[56:74] == df1$clusters)

# Since there are 12 FALSE out of 19, so we can conclude that the model is not that stable.

# 4. Cluster of Healthy Cereals.

new_data <- Cereals
new_data_na <- na.omit(new_data)

Clust <- cbind(new_data_na, cluster1)

Clust[Clust$cluster1==1,]
Clust[Clust$cluster1==2,]
Clust[Clust$cluster1==3,]
Clust[Clust$cluster1==4,]

Calculating mean ratings to determine the best cluster.

mean(Clust[Clust$cluster1==1,"rating"])
mean(Clust[Clust$cluster1==2,"rating"])
mean(Clust[Clust$cluster1==3,"rating"])
mean(Clust[Clust$cluster1==4,"rating"])

# Since the mean rating for cluster 1 == 1 is the highest, so we will choose the first Cluster.