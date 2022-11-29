# read data
library(readr)
train.raw <- read_csv("School/Stats 4620/Ames/Ames/train.csv")
test.raw <- read_csv("School/Stats 4620/Ames/Ames/test_new.csv")


# factor test and train
num_cols_train <- unlist(lapply(train.raw, is.numeric))
num_cols_test <- unlist(lapply(test.raw, is.numeric))

train.num <- train.raw[,num_cols_train]
test.num <- test.raw[,num_cols_test]

train_fac <- train.raw[,num_cols_train==F]
test_fac <- test.raw[,num_cols_test==F]

train_fac[is.na(train_fac)] = 'None'
test_fac[is.na(test_fac)] = 'None'

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

View(train)
nas <- which(is.na(train), arr.ind=TRUE)
unique(nas[,2])
# columns 3, 9, 26, 41, 56, 60, 61, 62, 63, 64, 68, 71, 72, 73, 74, 75, 77, 78, 79 have NaNs
colnames(train)[unique(nas[,2])]

# 3 = LotFrontage -> can set NaNs to 0
train[is.na(train$LotFrontage),]$LotFrontage = 0
test[is.na(test$LotFrontage),]$LotFrontage = 0
# 9 = MasVnrArea -> only 9 NaNs - so delete?
train <- train[is.na(train$MasVnrArea)==FALSE,]
test <- test[is.na(test$MasVnrArea)==FALSE,]
# 26 = GaragesYrBlt -> create splits
summary(train$GarageYrBlt)
hist(train$GarageYrBlt)

train$GarageYrBlt.fac <- cut(train$GarageYrBlt,
                       breaks=c(1900, 1961, 1978, 2002, 2010),
                       labels=c('1900-1961', '1961-1978', '1978-2002', '2002-2010'))
train$GarageYrBlt.fac <- addNA(train$GarageYrBlt.fac) 
train <- subset(train, select = -c(GarageYrBlt) )

test$GarageYrBlt.fac <- cut(test$GarageYrBlt,
                             breaks=c(1900, 1961, 1978, 2002, 2010),
                             labels=c('1900-1961', '1961-1978', '1978-2002', '2002-2010'))
test$GarageYrBlt.fac <- addNA(test$GarageYrBlt.fac) 
test <- subset(test, select = -c(GarageYrBlt) )

# 41 = Alley -> factorize NaNs
# 56 = MasVnrType -> facorize NaNs
# 60 = BsmtQual
# 61 = BsmtCond
# 62 = BsmtExposure
# 63 = BsmtFinType1
# 64 = BsmtFinType2
# 68 = Electrical
# 71 = FireplaceQu
# 72 = GarageType
# 73 = GarageFinish
# 74 = GarageQual
# 75 = GarageCond
# 77 = PoolQC
# 78 = Fence
# 79 = MiscFeature


for (col in colnames(test)){
  attributes(test[[col]]) <- attributes(train[[col]]) 
}


which(is.na(test), arr.ind=TRUE)
test <- test[-c(718), ]

dim(x.test)
length(y.test)


###
### Ridge
###
library(glmnet)
x=model.matrix(SalePrice~.,train)[,-1]  # remove the first column which is the intercept - glmnet will do that automatically for us.
y=train$SalePrice
x.test=model.matrix(SalePrice~.,test)[,-1]  # remove the first column which is the intercept - glmnet will do that automatically for us.
y.test=test$SalePrice

set.seed(4620)
ridge.cv = cv.glmnet(x,y,alpha=0)
plot(ridge.cv)  #plots on log-lambda scale.  The '19' at the top refers to effective df.
plot(ridge.cv$lambda,ridge.cv$cvm,xlim=c(0,5000))  # we can always plot things manually if we like
lambda.cv = ridge.cv$lambda.min  # the minimizing lambda
lambda.cv

fit.ridge = glmnet(x,y,alpha=0,lambda=lambda.cv)
pred.ridge = predict(fit.ridge,newx=x.test)
mean((y.test-pred.ridge)^2)

###
### Lasso
###
set.seed(4620)
lasso.cv = cv.glmnet(x,y,alpha=1)
plot(lasso.cv)  #plots on log-lambda scale.  The '19' at the top refers to effective df.
plot(lasso.cv$lambda,lasso.cv$cvm,xlim=c(0,50))  # we can always plot things manually if we like
lambda.cv = lasso.cv$lambda.min  # the minimizing lambda
lambda.cv

fit.lasso = glmnet(x,y,alpha=1,lambda=lambda.cv)
pred.lasso = predict(fit.lasso,newx=x.test)
mean((y.test-pred.lasso)^2)

###
###
###
x=model.matrix(log(SalePrice)~.,train)[,-1]  # remove the first column which is the intercept - glmnet will do that automatically for us.
y=log(train$SalePrice)
x.test=model.matrix(log(SalePrice)~.,test)[,-1]  # remove the first column which is the intercept - glmnet will do that automatically for us.
y.test=test$SalePrice

set.seed(4620)
ridge.cv = cv.glmnet(x,y,alpha=0)
plot(ridge.cv)  #plots on log-lambda scale.  The '19' at the top refers to effective df.
plot(ridge.cv$lambda,ridge.cv$cvm,xlim=c(0,5000))  # we can always plot things manually if we like
lambda.cv = ridge.cv$lambda.min  # the minimizing lambda
lambda.cv

fit.ridge = glmnet(x,y,alpha=0,lambda=lambda.cv)
pred.ridge = predict(fit.ridge,newx=x.test)
mean((y.test-exp(pred.ridge))^2)