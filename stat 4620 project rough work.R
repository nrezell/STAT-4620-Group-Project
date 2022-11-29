# Vidhya Kewale (kewale.1)
# STAT 4620 Project EDA

# working directory, remove from final report
setwd("C:/Users/nkewa/Documents/ed/stat 4620")

# required packages
library(readr)
library(glmnet)
library(ISLR)
library(pls)

# obtain data
train.raw <- read.csv("train.csv")
test.raw <- read.csv("test_new.csv")

# factor test and train
num_cols_train <- unlist(lapply(train.raw, is.numeric))
num_cols_test <- unlist(lapply(test.raw, is.numeric))

# get numeric columns only
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
pairs(train.num[,0:11])
pairs(train.num[,11:21])
pairs(train.num[,22:32])
pairs(train.num[,33:38])


# hist of all variables
library(Hmisc)
hist.data.frame(train.num[,0:6])


#par(mar = c(1, 1, 1, 1))
#pairs(train_df)

#library(GGally)
#pm <- ggpairs(train_df, cardinality_threshold = NULL)
#pm

################################################################################

# correlation matrix
n <- cor(train.num)
n[is.na(n)] <- 0
n[n == 1] <- 0
# apply(n, 1, max, na.rm = TRUE)
(m <- max.col(`diag<-`(n,0)))
colnames(n)[m]
max(n[cbind(seq_len(nrow(n)),m)])

# omit columns with NA values; left with 1121 rows
train.num <- na.omit(train.num)


# RIDGE REGRESSION ###
x = model.matrix(SalePrice ~ ., data = train.num)  # remove the first column which is the intercept - glmnet will do that automatically for us.
y=train.num$SalePrice
# fit.ridge = glmnet(x,y,alpha=0,lambda=10) # try a particular lambda value and see what we get
# pred.ridge = predict(fit.ridge,newx=x)
# mean((y-pred.ridge)^2)
# coef(fit.ridge)
# lambda from cross validation
ridge.cv = cv.glmnet(x,y,alpha=0)
plot(ridge.cv)  #plots on log-lambda scale.  The '19' at the top refers to effective df.
plot(ridge.cv$lambda,ridge.cv$cvm)  # we can always plot things manually if we like
lambda.cv = ridge.cv$lambda.min  # the minimizing lambda
lambda.cv # 29328
# fit model using CV estimate of lambda
fit.ridge = glmnet(x,y,alpha=0,lambda=lambda.cv)
pred.ridge = predict(fit.ridge,newx = x)
mean((y-pred.ridge)^2)

# LASSO ###
lasso.cv = cv.glmnet(x,y,alpha=1)
plot(lasso.cv)  #plots on log-lambda scale.  The '19' at the top refers to effective df.
plot(lasso.cv$lambda,lasso.cv$cvm)  # we can always plot things manually if we like
lambda.cv = lasso.cv$lambda.min  # the minimizing lambda
lambda.cv
fit.lasso = glmnet(x,y,alpha=1,lambda=lambda.cv)
pred.lasso = predict(fit.lasso,newx=x)
mean((y-pred.lasso)^2)

mean((y-pred.ridge)^2)  # Ridge MSE
mean((y-pred.lasso)^2)  # LASSO MSE
coef(fit.ridge)
coef(fit.lasso)

# PCR ###
fit.pcr = pcr(SalePrice ~ ., data = train.num, scale = TRUE, validation = "CV")
summary(fit.pcr)
validationplot(fit.pcr, val.type = "MSEP") # 30 components i believe?
MSEP_object <- MSEP(fit.pcr)
paste( "Minimum MSE of ",  
       MSEP_object$val[1,1, ][ which.min(MSEP_object$val[1,1, ] )], 
       " was produced with ", 
       sub(" comps","", names(which.min(MSEP_object$val[1,1, ] ))), 
       " components")

# PLS ###
fit.pls = plsr(SalePrice ~ ., data = train.num, scale = TRUE, validation = "CV")
summary(fit.pls)
validationplot(fit.pls, val.type = "MSEP")
MSEP_object <- MSEP(fit.pls)
paste( "Minimum MSE of ",  
       MSEP_object$val[1,1, ][ which.min(MSEP_object$val[1,1, ] )], 
       " was produced with ", 
       sub(" comps","", names(which.min(MSEP_object$val[1,1, ] ))), 
       " components")

fit.pcr2 = pcr(SalePrice ~ ., data = train.num, scale = TRUE, ncomp = 30)
fit.pcr2b = pcr(SalePrice ~ ., data = train.num, scale = TRUE, ncomp = 7)
fit.pls2 = plsr(SalePrice ~ ., data = train.num, scale = TRUE, ncomp = 7)
pcr.pred = predict(fit.pcr2, ncomp = 30)
pcr.pred.b = predict(fit.pcr2, ncomp = 7)
pls.pred = predict(fit.pls2, ncomp = 7)
vec <- train.num$SalePrice
vec <- sample(vec, 1121, replace = TRUE)
mean((as.vector(pcr.pred) - vec)^2)
mean((as.vector(pcr.pred.b) - vec)^2)
mean((as.vector(pls.pred) - vec)^2)
