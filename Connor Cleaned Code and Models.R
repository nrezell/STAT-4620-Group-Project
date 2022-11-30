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
         MSSubClass = as.character(MSSubClass),
         ExterQual = factor (ExterQual, levels = c ("Po", "Fa", "TA", "Gd", "Ex"), ordered = TRUE),
         ExterCond = factor (ExterCond, levels = c ("Po", "Fa", "TA", "Gd", "Ex"), ordered = TRUE),
         BsmtQual = factor (BsmtQual, levels = c ("None", "Po", "Fa", "TA", "Gd", "Ex"), ordered = TRUE),
         BsmtCond = factor (BsmtCond, levels = c ("None", "Po", "Fa", "TA", "Gd", "Ex"), ordered = TRUE),
         BsmtExposure = factor(BsmtExposure, levels = c ("None", "No", "Mn", "Av", "Gd"), ordered = TRUE),
         BsmtFinType1 = factor (BsmtFinType1, levels = c ("None", "Unf", "LwQ", "Rec", "BLQ", "ALQ", "GLQ"), ordered = TRUE),
         BsmtFinType2 = factor (BsmtFinType2, levels = c ("None", "Unf", "LwQ", "Rec", "BLQ", "ALQ", "GLQ"), ordered = TRUE),
         HeatingQC = factor (HeatingQC, levels = c ("Po", "Fa", "TA", "Gd", "Ex"), ordered = TRUE),
         CentralAir = factor (CentralAir, levels = c ("N", "Y"), ordered = TRUE),
         Electrical = factor (Electrical, levels = c ("None", "Mix", "FuseP", "FuseF", "FuseA", "SBrkr"), ordered = TRUE),
         KitchenQual = factor (KitchenQual, levels = c ("None", "Po", "Fa", "TA", "Gd", "Ex"), ordered = TRUE),
         Functional = factor (Functional, levels = c ("None", "Sal", "Sev", "Maj2", "Maj1", "Mod", "Min2", "Min1", "Typ"), ordered = TRUE),
         FireplaceQu = factor (FireplaceQu, levels = c ("None", "Po", "Fa", "TA", "Gd", "Ex"), ordered = TRUE),
         GarageFinish = factor (GarageFinish, levels = c ("None", "Unf", "RFn", "Fin"), ordered = TRUE),
         GarageQual = factor (GarageQual, levels = c ("None", "Po", "Fa", "TA", "Gd", "Ex"), ordered = TRUE),
         GarageCond = factor (GarageCond, levels = c ("None", "Po", "Fa", "TA", "Gd", "Ex"), ordered = TRUE),
         PavedDrive = factor (PavedDrive, levels = c ("N", "P", "Y"), ordered = TRUE),
         PoolQC = factor (PoolQC, levels = c ("None", "Fa", "TA", "Gd", "Ex"), ordered = TRUE),
         Fence = factor (Fence, levels = c ("None", "MnWw", "GdWo", "MnPrv", "GdPrv"), ordered = TRUE)
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
         MSSubClass = as.character(MSSubClass),
         ExterQual = factor (ExterQual, levels = c ("Po", "Fa", "TA", "Gd", "Ex"), ordered = TRUE),
         ExterCond = factor (ExterCond, levels = c ("Po", "Fa", "TA", "Gd", "Ex"), ordered = TRUE),
         BsmtQual = factor (BsmtQual, levels = c ("None", "Po", "Fa", "TA", "Gd", "Ex"), ordered = TRUE),
         BsmtCond = factor (BsmtCond, levels = c ("None", "Po", "Fa", "TA", "Gd", "Ex"), ordered = TRUE),
         BsmtExposure = factor(BsmtExposure, levels = c ("None", "No", "Mn", "Av", "Gd"), ordered = TRUE),
         BsmtFinType1 = factor (BsmtFinType1, levels = c ("None", "Unf", "LwQ", "Rec", "BLQ", "ALQ", "GLQ"), ordered = TRUE),
         BsmtFinType2 = factor (BsmtFinType2, levels = c ("None", "Unf", "LwQ", "Rec", "BLQ", "ALQ", "GLQ"), ordered = TRUE),
         HeatingQC = factor (HeatingQC, levels = c ("Po", "Fa", "TA", "Gd", "Ex"), ordered = TRUE),
         CentralAir = factor (CentralAir, levels = c ("N", "Y"), ordered = TRUE),
         Electrical = factor (Electrical, levels = c ("None", "Mix", "FuseP", "FuseF", "FuseA", "SBrkr"), ordered = TRUE),
         KitchenQual = factor (KitchenQual, levels = c ("None", "Po", "Fa", "TA", "Gd", "Ex"), ordered = TRUE),
         Functional = factor (Functional, levels = c ("None", "Sal", "Sev", "Maj2", "Maj1", "Mod", "Min2", "Min1", "Typ"), ordered = TRUE),
         FireplaceQu = factor (FireplaceQu, levels = c ("None", "Po", "Fa", "TA", "Gd", "Ex"), ordered = TRUE),
         GarageFinish = factor (GarageFinish, levels = c ("None", "Unf", "RFn", "Fin"), ordered = TRUE),
         GarageQual = factor (GarageQual, levels = c ("None", "Po", "Fa", "TA", "Gd", "Ex"), ordered = TRUE),
         GarageCond = factor (GarageCond, levels = c ("None", "Po", "Fa", "TA", "Gd", "Ex"), ordered = TRUE),
         PavedDrive = factor (PavedDrive, levels = c ("N", "P", "Y"), ordered = TRUE),
         PoolQC = factor (PoolQC, levels = c ("None", "Fa", "TA", "Gd", "Ex"), ordered = TRUE),
         Fence = factor (Fence, levels = c ("None", "MnWw", "GdWo", "MnPrv", "GdPrv"), ordered = TRUE)
  )

# determine character variable columns
char_cols <- unlist(lapply(train.raw, is.character))

# subset already factored and numeric variables
train.num <- train.raw[,char_cols==F]
test.num <- test.raw[,char_cols==F]

# subset categorical variables
train_fac <- train.raw[,char_cols]
test_fac <- test.raw[,char_cols]

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
train = train[,-62]
test = test[,-62]

summary(test)

for (col in colnames(test)){
  attributes(test[[col]]) <- attributes(train[[col]]) 
}

# MODELS

# LINEAR REGRESSION
model.lm <- lm(SalePrice ~ ., data=train)
summary(model.lm)
library(MASS)
stepAIC(model.lm)
stepAIC(model.lm, direction="forward")

# LASSO
library(glmnet)
x.train=model.matrix(SalePrice~., train)[,-1]
y.train=train$SalePrice
x.test=model.matrix(SalePrice~., test)[,-1]
y.test=test$SalePrice

Missing1 <- setdiff(colnames(x.test), colnames(x.train))
Missing2 <- setdiff(colnames(x.train), colnames(x.test))
missing <- c(Missing1, Missing2)

set.seed(4620)
lasso.cv = cv.glmnet(x.train, y.train, alpha=1)
plot(lasso.cv)

lambda.cv = lasso.cv$lambda.min

fit.lasso = glmnet(x.train, y.train, alpha=1, lambda=lambda.cv)

pred.lasso = predict(fit.lasso, newx=x.test)
mean((y.test - pred.lasso)^2)

# Ridge
set.seed(4620)
ridge.cv = cv.glmnet(x.train, y.train, alpha=0)
plot(ridge.cv)

ridge.lambda.cv = ridge.cv$lambda.min

fit.ridge = glmnet(x.train, y.train, alpha=0, lambda=ridge.lambda.cv)
pred.ridge = predict(fit.ridge, newx=x.test)
mean((y.test - pred.ridge)^2)

# PCR
library(pls)
set.seed(4620)
fit.pcr = pcr(SalePrice~., data=train, scale=TRUE, validation="CV")

# PLS