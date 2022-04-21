rm(list=ls())
setwd("C:/Users/north/Downloads")
library(dplyr)
library(plyr)
library(caret)
library(fastDummies)
library(tree)
library (gbm)
library(Metrics)
library(glmnet)
library(ggplot2)
library(randomForest)
library(foreach)
library(doParallel)

set.seed(123)

#Set Training Index: Now using log
log <- read.csv("final_data.csv", header=TRUE, sep=',', stringsAsFactors = TRUE)
summary(log)

#Get Rid of Sneaker.Name Because we extracted the variables. 
log <- log[,-5]

#Turn Categorical Variables into Factors
log$Shoe.Size <- as.factor(log$Shoe.Size)
str(log$Shoe.Size)
log$Brand <- as.factor(log$Brand)
str(log$Brand)
log$color <- as.factor(log$color)
str(log$color)


#Log Transformation 
log$Sale.Price <- log(log$Sale.Price)

#X and Y Variable 
x<-model.matrix(Sale.Price~., log)[,-1] # x matrix all numbers but changes all "words" in to a dummy variable
y<-log$Sale.Price # y vector

#Split Train and Test 
train <- sample(1:nrow(x), size=nrow(x)*.7)
test<-(-train)

#Make Grid for Best Lambda
value<-seq(from=10, to=-2, length=100) 
grid<-10^value


#Random Forest

set.seed(123)

# setting train and test
train_idx <- sample(1:nrow(x), size=nrow(x)*.7)
train_rf <- log[train_idx,]
test_rf <- log[-train_idx,]


# This is where packages foreach and doParallel shine. A model that takes 
# 45 minutes to run now takes around ~20 seconds to run

# The standard number of trees is 500, however, our accuracy levels off at 
# ~100

nb_trees <- 50 
ptm <- proc.time()  # this is used to measure the runtime of the algorithm

fit <- randomForest(Sale.Price~., 
                    data = train_rf,
                    ntree = nb_trees,
                    type = "regression")
proc.time() - ptm

# user  system elapsed 
# 9.48    0.11    9.73

fit
# Type of random forest: regression
# Number of trees: 50
# No. of variables tried at each split: 2
# 
# Mean of squared residuals: 0.02828426
# % Var explained: 85.35

# Variable Importance Plot
varImpPlot(fit)

# MSE / RMSE
yhat <- predict(fit, newdata=test_rf)
mean((yhat-log$Sale.Price[-train_idx])^2)
  # MSE: 0.02923673
  # MSE with mtry = 3: 0.01818866 

sqrt(mean((yhat-log$Sale.Price[-train_idx])^2))
  # RMSE indicates average model prediction error.
    # RMSE: 0.1709875
    # RMSE with mtry = 3: 0.1348654

# With this Random Forest model, the data is concentrated
# around the line of best fit, indicating that model and color can
# be the most influential in determining the Sales price of a shoe
