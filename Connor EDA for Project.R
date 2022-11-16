# Connor's R Code for Exploratory Data Analysis

# IMPORT LIBRARIES
library(tidyverse)
train <- read_csv("train.csv")
View(train)

# first start with numerical summary

summary(train)
missing <- sapply(train, function(x) sum(is.na(x)))
sort(missing, decreasing=TRUE)

train_upd <- train %>% 
  mutate(PoolQC = replace_na("None"),
         MiscFeature = replace_na("None"),
         Alley = replace_na("None"),
         Fence = replace_na("None"),
         FireplaceQu = replace_na("None"),
         LotFrontage = replace_na(0),
         GarageType = replace_na("None"),
         GarageFinish = replace_na("None"),
         GarageQual = replace_na("None"),
         GarageCond = replace_na("None"),
         BsmtExposure = replace_na("None"),
         BsmtFinType2 = replace_na("None"),
         BsmtQual = replace_na("None"),
         BsmtCond = replace_na("None"),
         BsmtFinType1 = replace_na("None"),
         GarageYrBlt = case_when(
           is.na(GarageYrBlt) ~ 0,
           GarageYrBlt <=1961 ~ 1,
           GarageYrBlt <=1980 ~ 2,
           GarageYrBlt <=2002 ~ 3,
           GarageYrBlt <=2010 ~ 4
         ),
         MasVnrType = replace_na("None"),
         MasVnrArea = replace_na(0)
  )

train_upd %>% remove_missing() %>% count()
missing2 <- sapply(train_upd, function(x) sum(is.na(x)))
sort(missing2, decreasing=TRUE)

train_upd <- remove_missing(train_upd)

train_upd %>% 
  ggplot() +
  geom_boxplot(aes(y=Neighborhood, x=SalePrice, fill=Neighborhood), show.legend=FALSE)

train_upd %>% 
  ggplot() +
  geom_boxplot(aes(y=Functional, x=SalePrice, fill=Functional), show.legend=FALSE)

train_upd %>% 
  ggplot() +
  geom_boxplot(aes(x=OverallQual, y=SalePrice, fill=OverallQual, group=OverallQual))

train_upd %>% 
  ggplot() +
  geom_boxplot(aes(x=OverallCond, y=SalePrice, fill=OverallCond, group=OverallCond))

train_upd %>% 
  ggplot() +
  geom_jitter(aes(x=OverallCond, y=OverallQual))
