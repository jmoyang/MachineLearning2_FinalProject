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

########################################
########### RIDGE REGRESSION ###########
########################################

# Initial Model 
ridge.mod<-glmnet(x[train, ], y[train], alpha =0, lambda = grid, thresh = 1e-12)
#summary(ridge.mod)


# Cross Validation
cv.out<-cv.glmnet(x[train, ], y[train], alpha=0)
bestlam<-cv.out$lambda.min
bestlam #0.02706142 --> Extremely Low, Hence it is producing closer to OLS

# Predict using initial model, and best lambda
ridge.pred<-predict(ridge.mod, s=bestlam, newx=x[test,])
mse <- mean((ridge.pred-y[test])^2)
mse #0.06179721
rmse <- sqrt(mse)
rmse #0.2485905 # better than Lasso

# Fit the Model Using Best Lambda Value 
out <-glmnet(x,y,alpha=0)
ridge.coef <- predict(out, type="coefficients", s=bestlam)
ridge.coef # Coef for model are now smaller. Color remains negative
           # Model and size also remain positive

plot(ridge.mod, xvar = "norm", label = TRUE)

# Linear Model because lambda = 0
lm.pred<-predict(ridge.mod, s=0, newx=x[test,], exact=T, x=x[train, ], y=y[train])
mse <- mean((lm.pred-y[test])^2)
mse #0.05658299 Lower MSE than ridge model, but not by much.
rmse <- sqrt(mse)
rmse #0.2378718 Better than both ridge and lasso


#########################################
########### LASSO REGRESSION  ########### --> Check if other variables are impactful but zeroing out
#########################################     unnecessary variables using Lasso

#Initial Model
lasso.mod<-glmnet(x[train, ], y[train], alpha=1, lambda=grid)  # Alpha = 1 for Lasso
#summary(lasso.mod)

# Cross-Validation
cv.out<-cv.glmnet(x[train, ], y[train], alpha=1)
bestlam <- cv.out$lambda.min
bestlam #8.264167e-05

# Predict using initial model, and best lambda
lasso.pred<-predict(lasso.mod, s=bestlam,newx=x[test,])
mse <- mean((lasso.pred-y[test])^2) 
mse #0.0672698
rmse <- sqrt(mse)
rmse #0.2593642 not as good as rdige or linear

# Fit the Model Using Best Lambda Value
out<-glmnet(x,y, alpha=1, lambda=grid)
lasso.coef<-predict(out, type="coefficients", s=bestlam)
lasso.coef #--> Most sizes, Brand, and Retail Price have been eliminated, 
           #    coef for models and color maintain their positive and negative signs

plot(lasso.mod, xvar = "norm", label = TRUE)
########################################################################

