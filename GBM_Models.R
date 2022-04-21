library(dplyr)
library(plyr)
library(caret)
library(fastDummies)
library(tree)
library (gbm)
library(Metrics)
library(ggplot2)
setwd("C:/Users/jeong/Downloads")

#Set Training Index
dt <- read.csv("final_data.csv", header=TRUE, sep=',', stringsAsFactors = TRUE)
summary(dt)

dt <- dt[,-5]

#Take the Log of our Target Variable
dt$Sale.Price <- log(dt$Sale.Price)

#Get ready for model
#Turn shoe size into a factor
dt$Shoe.Size <- as.factor(dt$Shoe.Size)
str(dt$Shoe.Size)

#Turn Model into a factor 
dt$model <- as.factor(dt$model)
str(dt$model)

#Turn color into a factor
dt$color<- as.factor(dt$color)
str(dt$color)

#Turn Brand into a factor 
dt$Brand <- as.factor(dt$Brand)
str(dt$Brand)

n = nrow(dt)
trainIndex = sample(1:n, size = n*0.7)  
length(trainIndex)
set.seed(123)

#Make Model (Takes Forever to Run)
mse.boost<-rep(0,4*5)
counter <-0
for (i in 1:4) {
  for (j in 1:5) {
    counter <- counter +1
    boost<-gbm(Sale.Price~.,
               data= dt[trainIndex,],
               distribution = "gaussian",
               n.trees = 500,
               interaction.depth= i, # 1- 4 splits
               shrinkage = 0.01*j, #learning rate = 0.01 to 0.05
               verbose = F)
    
    yhat<-predict(boost,
                  newdata=dt[-trainIndex,],
                  n.trees=500)
    mse.boost[counter]<-mean((yhat-dt$Sale.Price[-trainIndex])^2) #Q04-3
  }
}

mse.boost 

#[1] 0.0005309474 0.0004691109 0.0004430003 0.0004273590 0.0004163625 0.0003896201 0.0002916414
#[8] 0.0002579118 0.0002367718 0.0002217208 0.0003257089 0.0002433054 0.0002193131 0.0002025752
#[15] 0.0001947522 0.0002864678 0.0002215026 0.0001996068 0.0001872039 

## Lowest: 0.0001786281 --> best output is with 4 interaction depth and 0.05 learning rate

#############################
######## LOG Y Model ########
#############################
logY<-gbm(Sale.Price~.,
           data= dt[trainIndex,],
           distribution = "gaussian",
           n.trees = 500,
           interaction.depth= 4, # 1- 4 splits
           shrinkage = 0.05, #learning rate = 0.01 to 0.05
           verbose = F)

yhat<-predict(boost,
              newdata=dt[-trainIndex,],
              n.trees=500)
mse.boost<-mean((yhat-dt$Sale.Price[-trainIndex])^2) 
mse.boost


summary(logY)
#                   var     rel.inf
#model               model  62.8496021
#color               color  24.1806037
#weeks_passed weeks_passed  10.5197879
#Shoe.Size       Shoe.Size  1.3536046
#Retail.Price Retail.Price  0.6439537
#Brand               Brand  0.4524481



