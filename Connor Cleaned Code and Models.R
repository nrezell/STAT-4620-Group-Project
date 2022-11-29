# read data
library(tidyverse)
train.raw <- read_csv("train.csv")
test.raw <- read_csv("test_new.csv")

# handle missing values
train.raw <- train.raw %>% 
  mutate(PoolQC = replace_na(PoolQC, "None"),
         MiscFeature = replace_na(MiscFeature, "None"),
         Alley = replace_na(Alley, "None"),
         Fence = replace_na(Fence, "None"),
         FireplaceQu = replace_na(FireplaceQu, "None"),
         LotFrontage = replace_na(LotFrontage, 0),
         GarageType = replace_na(GarageType, "None"),
         GarageFinish = replace_na(GarageFinish, "None"),
         GarageQual = replace_na(GarageQual, "None"),
         GarageCond = replace_na(GarageCond, "None"),
         BsmtExposure = replace_na(BsmtExposure, "None"),
         BsmtFinType2 = replace_na(BsmtFinType2, "None"),
         BsmtQual = replace_na(BsmtQual, "None"),
         BsmtCond = replace_na(BsmtCond, "None"),
         BsmtFinType1 = replace_na(BsmtFinType1, "None"),
         GarageYrBlt = case_when(
           is.na(GarageYrBlt) ~ 0,
           GarageYrBlt <=1961 ~ 1,
           GarageYrBlt <=1980 ~ 2,
           GarageYrBlt <=2002 ~ 3,
           GarageYrBlt <=2010 ~ 4
         ),
         MasVnrType = replace_na(MasVnrType, "None"),
         MasVnrArea = replace_na(MasVnrArea, 0),
         MSSubClass = as.character(MSSubClass)
  )

test.raw <- test.raw %>% 
  mutate(PoolQC = replace_na(PoolQC, "None"),
         MiscFeature = replace_na(MiscFeature, "None"),
         Alley = replace_na(Alley, "None"),
         Fence = replace_na(Fence, "None"),
         FireplaceQu = replace_na(FireplaceQu, "None"),
         LotFrontage = replace_na(LotFrontage, 0),
         GarageType = replace_na(GarageType, "None"),
         GarageFinish = replace_na(GarageFinish, "None"),
         GarageQual = replace_na(GarageQual, "None"),
         GarageCond = replace_na(GarageCond, "None"),
         BsmtExposure = replace_na(BsmtExposure, "None"),
         BsmtFinType2 = replace_na(BsmtFinType2, "None"),
         BsmtQual = replace_na(BsmtQual, "None"),
         BsmtCond = replace_na(BsmtCond, "None"),
         BsmtFinType1 = replace_na(BsmtFinType1, "None"),
         GarageYrBlt = case_when(
           is.na(GarageYrBlt) ~ 0,
           GarageYrBlt <=1961 ~ 1,
           GarageYrBlt <=1980 ~ 2,
           GarageYrBlt <=2002 ~ 3,
           GarageYrBlt <=2010 ~ 4
         ),
         MasVnrType = replace_na(MasVnrType, "None"),
         MasVnrArea = replace_na(MasVnrArea, 0),
         MSSubClass = as.character(MSSubClass)
  )

# determine numeric variable columns
numeric_cols <- unlist(lapply(train.raw, is.numeric))

# subset numeric variables
train.num <- train.raw[,numeric_cols]
test.num <- test.raw[,numeric_cols]

# subset categorical variables
train_fac <- train.raw[,numeric_cols==F]
test_fac <- test.raw[,numeric_cols==F]

# factor the categorical variables
train.fac.df <- as.data.frame(unclass(train_fac),stringsAsFactors=TRUE)
test.fac.df <- as.data.frame(unclass(test_fac),stringsAsFactors=TRUE)

# recombine the data
train <- cbind(train.num, train.fac.df)
test <- cbind(test.num, test.fac.df)

train = na.omit(train)
test = na.omit(test)

# Remove ID variable
train = train[,-1]
test = test[, -1]

# Remove Utilities variables
train = train[,-43]
test = test[,-43]

summary(train)

# MODELS

# LINEAR REGRESSION
model.lm <- lm(SalePrice ~ ., data=train)
library(MASS)
stepAIC(model.lm)

# LASSO
library(glmnet)
x.train=model.matrix(SalePrice~., train)[,-1]
y.train=train$SalePrice
x.test=model.matrix(SalePrice~., test)[,-1]

set.seed(4620)
lasso.cv = cv.glmnet(x.train, y.train, alpha=1)
plot(lasso.cv)

lambda.cv = lasso.cv$lambda.min

fit.lasso = glmnet(x.train, y.train, alpha=1, lambda=lambda.cv)
coef(fit.lasso)
pred.lasso = predict(fit.lasso, newx=x.test)