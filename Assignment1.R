# Assignment 1.
# Loading built-in datasets (iris)

library(datasets)

# Print descriptive stats for a selection of variables

summary(iris)


data1 <- data.frame(iris)
summary(data1)

data1$Species <- as.character(data1$Species)
data1[data1 == "setosa"] <- "0"
data1$Species <- as.factor(data1$Species)

data1$Species <- as.character(data1$Species)
data1[data1 == "versicolor"] <- "1"
data1$Species <- as.factor(data1$Species)

data1$Species <- as.character(data1$Species)
data1[data1 == "virginica"] <- "2"
data1$Species <- as.factor(data1$Species)

summary(data1)

# Plotting Quantitative Variable Petal.Length
plot(iris$Petal.Length)

# Plotting Petal.Length and Sepal.Length
plot(iris$Petal.Length, iris$Sepal.Length)
