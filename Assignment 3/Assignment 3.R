# Settinig working directory to current directory
setwd("D:/A_Sem_1/ML/Assignment 3")

# Importing libraries
library(e1071)

# Importing Universal Bank dataset
bank_data <- read.csv("UniversalBank.csv")
bank_data1 <- bank_data
# Change numerical variables into categorical variables
bank_data1$Online <- factor(bank_data1$Online)
bank_data1$CreditCard <- factor(bank_data1$CreditCard)
bank_data1$Personal.Loan <- factor(bank_data1$Personal.Loan)

# Partitioning the data into training and testing set

train_index <- sample(c(1:dim(bank_data1)[1]), dim(bank_data1)[1]*0.6)
train_data <- bank_data1[train_index,]
validation_data <- bank_data1[-train_index,]


# 1. Creating pivot table

library(reshape)

pt4 = cast(train_data, CreditCard ~ Online, value = "Personal.Loan")


# Here we are creating a pivot table using 3 columns Online, CrediCard and Personal.Loan
# from our training data.

# The values in this table is for the Personal loan for the people who have and does
# not have online banking service (column of table) and who use and does not use 
#Credit Card (Row of the table).

#####################################################################################

# 2.  Now we are going to compute the probability of loan acceptance (Loan = 1) conditional on having
#     a bank credit card (CC = 1) and being an active user of online banking services (Online = 1)] 
#     by looking at the value of pivot table

Prob1 <- pt4[1,3]/ (pt4[1,3] + pt4[2,3])

# Here prob 1 is P(L=1|CC=1,O=1) = 0.694
# This is computed by dividing the probability of loan acceptance (Loan = 1) conditional on having
#     a bank credit card (CC = 1) and being an active user of online banking services (Online = 1)]
#  and probability of loan acceptance (Loan = 0) conditional on having
#     a bank credit card (CC = 1) and being an active user of online banking services (Online = 1)]

###################################################################################################

# 3.  Now we are going to create 2 seperate pivot tables.

# Loan (rows) as a function of Online (columns)
pt5 = cast(train_data, Personal.Loan ~ Online, value = "CreditCard")

# Another way of doing this:
# xtabs(~ train_data$Personal.Loan + train_data$Online)
# The result is the same

# e Loan (rows) as a function of CC (columns)
pt6 = cast(train_data, Personal.Loan ~ CreditCard , value = "Online")
#Similarly for this table as well
#xtabs(~ train_data$Personal.Loan + train_data$CreditCard)

###############################################################################################

# 4.  Compute the following
#Sum of all the values in 2 tables
total_sum <- pt5[1,2] + pt5[1,3] + pt5[2,2] + pt5[2,3] + pt6[2,2] + pt6[2,3] + pt6[1,2] + pt6[1,3]

# i. P(CC = 1 | Loan = 1) (the proportion of credit card holders among the loan acceptors)
prob2 <- pt6[2,3]/ (pt6[2,2] + pt6[2,3])

# ii. P(Online = 1 | Loan = 1)
prob3 <- pt5[2,3]/ (pt5[2,2] + pt5[2,3])

# iii. P(Loan = 1) (the proportion of loan acceptors)

  
prob4 <- (pt5[2,2] + pt5[2,3] + pt6[2,2] + pt6[2,3])/total_sum

# iv. P(CC = 1 | Loan = 0)
prob5 <- pt6[1,3]/ (pt6[1,3] + pt6[1,2])
# v. P(Online = 1 | Loan = 0)
prob6 <- pt5[1,3]/ (pt5[1,3] + pt5[1,2])

# vi. P(Loan = 0)
prob7 <- (pt5[1,2] + pt5[1,3] + pt6[1,2] + pt6[1,3])/total_sum

##################################################################################

# 5.  compute the naive Bayes probability P(Loan = 1 | CC = 1,Online = 1).

prob8 <- Prob1 / ((prob2 * prob4) + (prob3 * prob4) + (prob5 * prob7) + (prob6 * prob7))


# 6.  Which value is more aacurate?
Prob1
# The part B value is more accurate as it is computed via bayes theorem.
# The reason for the accuracy of the bayes theorem is the fact is calculates probability
# based on the given condition.
prob8
# Whereas naive bayes assumes conditional independence among the variables, which makes the
# calculation easier but looses the accuaracy of that calculation. However, it does preserve 
# the order of probability.

#######################################################################################

# 7.  Running naive bayes on actual data
model <- naiveBayes(Personal.Loan ~c("Online", "CreditCard") , data = train_data)
model
