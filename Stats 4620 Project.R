# read data
library(readr)
train.raw <- read_csv("School/Stats 4620/Ames/Ames/train.csv")
test.raw <- read_csv("School/Stats 4620/Ames/Ames/test.csv")


# factor test and train
num_cols_train <- unlist(lapply(train.raw, is.numeric))
num_cols_test <- unlist(lapply(test.raw, is.numeric))

train.num <- train.raw[,num_cols_train]
test.num <- test.raw[,num_cols_test]

train_fac <- train.raw[,num_cols_train==F]
test_fac <- test.raw[,num_cols_test==F]

train.fac.df <- as.data.frame(unclass(train_fac),stringsAsFactors=TRUE)
test.fac.df <- as.data.frame(unclass(test_fac),stringsAsFactors=TRUE)

train <- cbind(train.num, train.fac.df)
test <- cbind(test.num, test.fac.df)


# look at SalesPrice
hist(train$SalePrice)
hist(log(train$SalePrice))

#View(train)

# look at random pairs
pairs(train.num[,0:10])
pairs(train.num[,11:21])
pairs(train.num[,22:32])
pairs(train.num[,33:38])

  
# hist of all variables
library(Hmisc)
hist.data.frame(train.num)


#par(mar = c(1, 1, 1, 1))
#pairs(train_df)

#library(GGally)
#pm <- ggpairs(train_df, cardinality_threshold = NULL)
#pm


