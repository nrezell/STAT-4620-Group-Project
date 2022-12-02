# Connor's R Code for Exploratory Data Analysis

# IMPORT LIBRARIES
load("train_db.RData")

# first start with numerical summary

summary(train)
library(forcats)

train %>% 
  mutate(Neighborhood = fct_reorder(Neighborhood, SalePrice, .fun='median')) %>% 
  ggplot() +
  geom_boxplot(aes(x=SalePrice, y=Neighborhood, fill=Neighborhood), show.legend=FALSE)
train %>% 
  ggplot() +
  geom_boxplot(aes(x=OverallQual, y=Neighborhood, fill=Neighborhood), show.legend=FALSE)

train %>% 
  ggplot() +
  geom_boxplot(aes(x=OverallQual, y=SalePrice, group=OverallQual, color=OverallQual), show.legend=FALSE) +
  scale_x_continuous(breaks=1:10) +
  labs(x="Overall Quality", y="Sale Price ($)", title="Overall Quality vs. Sale Price")

train_upd %>% 
  ggplot() +
  geom_boxplot(aes(y=Functional, x=SalePrice, fill=Functional), show.legend=FALSE)

train %>% 
  ggplot() +
  geom_boxplot(aes(x=OverallQual, y=SalePrice, fill=OverallQual, group=OverallQual))

train %>% 
  ggplot() +
  geom_boxplot(aes(x=OverallCond, y=SalePrice, fill=OverallCond, group=OverallCond))

train %>% 
  ggplot() +
  geom_jitter(aes(x=OverallCond, y=OverallQual))

train %>%
  group_by(OverallCond) %>% 
  summarise(med_qual = median(OverallQual)) %>% 
  ggplot() +
  geom_line(aes(x=OverallCond, y=med_qual)) +
  labs(x="Overall Condition", y="Med. Overall Quality")

train %>% 
  ggplot() +
  geom_histogram(aes(x=SalePrice)) +
  scale_x_log10()

num_cols <- unlist(lapply(train, is.numeric))
data_num <- train[,num_cols]

data_fac <- train[,num_cols==F]

DF <- as.data.frame(unclass(data_fac),stringsAsFactors=TRUE)
train_df <- cbind(data_num, DF)

traindf_upd <- train_df %>% 
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

library(glmnet)

# LASSO
x = model.matrix(SalePrice~.,traindf_upd)[,-1]
