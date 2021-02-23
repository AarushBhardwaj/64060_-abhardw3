library(caret)
library(FNN)
# Setting working directory
setwd("D:/A_Sem_1/ML/Assignment 2")

# Loading Data
bank_data <- read.csv("UniversalBank.csv")

#Converting column education into dummy variable
bank_data[, 8] <- as.data.frame(model.matrix(~ bank_data[, 8] - 1))

set.seed(123)
train_index <- sample(row.names(bank_data), 0.6*dim(bank_data)[1])
validation_index <- setdiff(row.names(bank_data), train_index)

train_df <- bank_data[train_index, ]
validation_df <- bank_data[validation_index, ]

## new data to be predicted
new_df <- data.frame(Age = 40, Experience = 10, Income = 84, Family = 2, CCAvg = 2,
                     Education = 2, Mortgage = 0, Securities.Account = 0, CD.Account = 0,
                     Online = 1, CreditCard = 1)

# Normalizing the data using caret package
train_norm_df <- train_df
validation_norm_df <- validation_df
bank_norm_df <- bank_data

norm_values <- preProcess(train_df[, 3:13], method=c("center", "scale"))
train_norm_df[, 3:13] <- predict(norm_values, train_df[, 3:13])
validation_norm_df[, 3:13] <- predict(norm_values, validation_df[, 3:13])
bank_norm_df[, 3:13] <- predict(norm_values, bank_data[, 3:13])
new_norm_df <- predict(norm_values, new_df)

# Training KNN for k-1
model1 <- knn(train = train_norm_df[, 3:13], test =validation_norm_df[, 3:13],
          cl = train_norm_df[, 14], k = 1)
row.names(train_df)[attr(model1, "model1.index")]

# PRedicting for new value using k = 1

knn_pred_new <- knn(bank_norm_df[, 3:13], new_norm_df,
                    cl = bank_norm_df[, 14], k = 1)
row.names(train_df)[attr(model1, "model1.index")]

# The new customer does not gets qualified. (output- 0)
############################################################################

#Code to measure accuracy of different K variables

# initializing a data frame with two columns: k, and accuracy.
accuracy_df <- data.frame(k = seq(1, 14, 1), accuracy = rep(0, 14))

# computing the best value of k on validation.
for(i in 1:14) {
  knn_pred <- knn(train_norm_df[, 3:13], validation_norm_df[, 3:13],
                  cl = train_norm_df[, 14], k = i)
  accuracy_df[i, 2] <- confusionMatrix(knn_pred, as.factor(validation_norm_df[, 14]))$overall[1]
}

# The best value of K is 3

#############################################################################

# PRedicting for new value using best k

knn_pred_new <- knn(bank_norm_df[, 3:13], new_norm_df,
                    cl = bank_norm_df[, 14], k = 3)
row.names(train_df)[attr(model1, "model1.index")]

############################################################################

# Showing confusion matrix

library("gmodels")
CrossTable(x = validation_norm_df[, 14], y = model1, prop.chisq = FALSE)

############################################################################

# Repartiton the data into train, validation and test set of (50,30,20)

train.rows <- sample(rownames(bank_data), dim(bank_data)[1] *0.50)
validation.rows <- sample(setdiff(rownames(bank_data), train.rows), dim(bank_data)[1] *0.30)
test.rows <- setdiff(rownames(bank_data), union(train.rows, validation.rows))

train_data <- bank_data[train.rows,]
validation_data <- bank_data[validation.rows,]
test_data <- bank_data[test.rows,]

# Normalizing the data

train_data_norm <- train_data
validation_data_norm <- validation_data
test_data_norm <- test_data

norm_values1 <- preProcess(train_data[, 3:13], method=c("center", "scale"))
train_data_norm[, 3:13] <- predict(norm_values1, train_data[, 3:13])
validation_data_norm[, 3:13] <- predict(norm_values1, validation_data[, 3:13])
test_data_norm[, 3:13] <- predict(norm_values1, test_data[, 3:13])

model2 <- knn(train = train_data_norm[, 3:13], test =validation_data_norm[, 3:13],
              cl = train_data_norm[, 14], k = 3)
row.names(train_data)[attr(model2, "model2.index")]

model3 <- knn(train = train_data_norm[, 3:13], test =test_data_norm[, 3:13],
              cl = train_data_norm[, 14], k = 3)
row.names(train_data)[attr(model3, "model3.index")]


# confusion matrix using validation set

CrossTable(x = validation_data_norm[, 14], y = model2, prop.chisq = FALSE)

# Confusion matrix using test set

CrossTable(x = test_data_norm[, 14], y = model3, prop.chisq = FALSE)