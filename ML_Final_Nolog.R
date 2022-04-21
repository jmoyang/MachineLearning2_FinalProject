rm(list=ls())
setwd("C:/Users/alexd")
library(dplyr)
library(plyr)
library(caret)
library(fastDummies)
library(tree)
library (gbm)
library(Metrics)
library(glmnet)
library(ggplot2)
set.seed(123)

#Set Training Index:
No_log <- read.csv("final_data.csv", header=TRUE, sep=',', stringsAsFactors = TRUE)
summary(No_log)

#Get Rid of Sneaker.Name Because we extracted the variables. 
No_log <- No_log[,-5]

#Turn Categorical Variables into Factors
No_log$Shoe.Size <- as.factor(No_log$Shoe.Size)
str(No_log$Shoe.Size)
No_log$Brand <- as.factor(No_log$Brand)
str(No_log$Brand)
No_log$color <- as.factor(No_log$color)
str(No_log$color)


#X and Y Variable 
x<-model.matrix(Sale.Price~., No_log)[,-1] # x matrix all numbers but changes all "words" in to a dummy variable
y<-No_log$Sale.Price # y vector

#Split Train and Test 
train <- sample(1:nrow(x), size=nrow(x)*.7)
test<-(-train)

#Make Grid for Best Lambda
value<-seq(from=10, to=-2, length=100) 
grid<-10^value

########################################
########### RIDGE REGRESSION ###########
########################################

# Initial Model 
ridge.mod<-glmnet(x[train, ], y[train], alpha =0, lambda = grid, thresh = 1e-12)
#summary(ridge.mod)


# Cross Validation
cv.out<-cv.glmnet(x[train, ], y[train], alpha=0)
bestlam<-cv.out$lambda.min
bestlam # 14.60722

# Predict using initial model, and best lambda
ridge.pred<-predict(ridge.mod, s=bestlam, newx=x[test,])
mse <- mean((ridge.pred-y[test])^2)
mse # 21655.74
rmse <- sqrt(mse)
rmse # 147.1589

# Fit the Model Using Best Lambda Value 
out <-glmnet(x,y,alpha=0)
ridge.coef <- predict(out, type="coefficients", s=bestlam)
ridge.coef # Coef for model seem larger than color, same as GBM

plot(ridge.mod, xvar = "norm", label = TRUE)

# Linear Model because lambda = 0
lm.pred<-predict(ridge.mod, s=0, newx=x[test,], exact=T, x=x[train, ], y=y[train])
mse <- mean((lm.pred-y[test])^2)
mse #20556.83 Lower MSE than ridge model, but not by much.
rmse <- sqrt(mse)
rmse #143.3765

#########################################
########### LASSO REGRESSION  ########### --> Check if other variables are impactful but zeroing out
#########################################     unnecessary variables using Lasso

#Initial Model
lasso.mod<-glmnet(x[train, ], y[train], alpha=1, lambda=grid)  # Alpha = 1 for Lasso
#summary(lasso.mod)

# Cross-Validation
cv.out<-cv.glmnet(x[train, ], y[train], alpha=1)
bestlam <- cv.out$lambda.min
bestlam #0.04895759

# Predict using initial model, and best lambda
lasso.pred<-predict(lasso.mod, s=bestlam,newx=x[test,])
mse <- mean((lasso.pred-y[test])^2) 
mse #20559.44
rmse <- sqrt(mse)
rmse #143.3856

# Fit the Model Using Best Lambda Value
out<-glmnet(x,y, alpha=1, lambda=grid)
lasso.coef<-predict(out, type="coefficients", s=bestlam)
lasso.coef #--> Most sizes, Brand and color has been eliminate, 
           #    still high coef for models and color

plot(lasso.mod, xvar = "norm", label = TRUE)
########################################################################