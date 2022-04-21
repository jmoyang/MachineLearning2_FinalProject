library(dplyr)
library(plyr)
library(caret)
library(fastDummies)
library(tree)
library (gbm)
library(Metrics)
library(ggplot2)

rm(list=ls())
setwd("C:/Users/jeong/Downloads")

csv <- read.csv("StockX-Data-Contest-2019-3.csv")
summary(csv)
attach(csv)

### Ready Data ###
#Extract Model Name from Sneaker.Name 
csv$model = case_when(
  startsWith(csv$Sneaker.Name, "Adidas-Yeezy-Boost-350-V2-Beluga" ) ~ "v2",
  startsWith(csv$Sneaker.Name, "Adidas-Yeezy-Boost-350-Low-V2-Beluga") ~ "v2",
  startsWith(csv$Sneaker.Name, "Adidas-Yeezy-Boost-350-Low-V2-Beluga-2pt0") ~ "v2",
  startsWith(csv$Sneaker.Name, "Adidas-Yeezy-Boost-350-V2-Moonrock") ~ "v2",
  startsWith(csv$Sneaker.Name, "Adidas-Yeezy-Boost-350-Low-Moonrock" ) ~ "og",
  startsWith(csv$Sneaker.Name, "Adidas-Yeezy-Boost-350-V2-Oxford-Tan" ) ~ "yeezy",
  startsWith(csv$Sneaker.Name, "Adidas-Yeezy-Boost-350-Low-Oxford-Tan" ) ~ "og",
  startsWith(csv$Sneaker.Name, "Adidas-Yeezy-Boost-350-Low-Pirate-Black" ) ~ "og",
  startsWith(csv$Sneaker.Name, "Adidas-Yeezy-Boost-350-Low-Turtledove" ) ~ "og",
  startsWith(csv$Sneaker.Name, "Adidas-Yeezy-Boost-350-V2-Semi-Frozen-Yellow" ) ~ "v2",
  startsWith(csv$Sneaker.Name, "Adidas-Yeezy-Boost-350-V2-Blue-Tint" ) ~ "v2",
  startsWith(csv$Sneaker.Name, "Adidas-Yeezy-Boost-350-V2-Core-Black-Copper" ) ~ "v2",
  startsWith(csv$Sneaker.Name, "Adidas-Yeezy-Boost-350-V2-Core-Black-Green" ) ~ "v2",
  startsWith(csv$Sneaker.Name, "Adidas-Yeezy-Boost-350-V2-Core-Black-Red" ) ~ "v2",
  startsWith(csv$Sneaker.Name, "Adidas-Yeezy-Boost-350-V2-Core-Black-White" ) ~ "v2",
  startsWith(csv$Sneaker.Name, "Adidas-Yeezy-Boost-350-V2-Cream-White" ) ~ "v2",
  startsWith(csv$Sneaker.Name, "Adidas-Yeezy-Boost-350-V2-Zebra" ) ~ "v2",
  startsWith(csv$Sneaker.Name, "adidas-Yeezy-Boost-350-V2-Butter" ) ~ "v2",
  startsWith(csv$Sneaker.Name, "adidas-Yeezy-Boost-350-V2-Static" ) ~ "v2",
  startsWith(csv$Sneaker.Name, "adidas-Yeezy-Boost-350-V2-Static-Reflective" ) ~ "v2",
  startsWith(csv$Sneaker.Name, "Adidas-Yeezy-Boost-350-V2-Sesame" ) ~ "v2",
  startsWith(csv$Sneaker.Name, "Nike-Zoom-Fly-Off-White-Pink" ) ~ "zoomFly",
  startsWith(csv$Sneaker.Name, "Nike-Zoom-Fly-Off-White-Black-Silver" ) ~ "zoomFly",
  startsWith(csv$Sneaker.Name, "Nike-Zoom-Fly-Off-White" ) ~ "zoomFly",
  startsWith(csv$Sneaker.Name, "Nike-Air-Force-1-Low-Off-White" ) ~ "AF1",
  startsWith(csv$Sneaker.Name, "Nike-Air-Max-97-Off-White" ) ~ "AirMax97",
  startsWith(csv$Sneaker.Name, "Nike-Air-Force-1-Low-Virgil-Abloh-Off-White-AF100" ) ~ "AF1",
  startsWith(csv$Sneaker.Name, "Nike-React-Hyperdunk-2017-Flyknit-Off-White" ) ~ "Hyper",
  startsWith(csv$Sneaker.Name, "Nike-Air-Max-90-Off-White" ) ~ "AirMax90",
  startsWith(csv$Sneaker.Name, "Nike-Air-Presto-Off-White" ) ~ "Presto",
  startsWith(csv$Sneaker.Name, "Nike-Air-VaporMax-Off-White" ) ~ "Vapor",
  startsWith(csv$Sneaker.Name, "Air-Jordan-1-Retro-High-Off-White-Chicago" ) ~ "AJ1",
  startsWith(csv$Sneaker.Name, "Nike-Blazer-Mid-Off-White" ) ~ "Blazer",
  startsWith(csv$Sneaker.Name, "Nike-Air-VaporMax-Off-White-2018" ) ~ "Vapor",
  startsWith(csv$Sneaker.Name, "Air-Jordan-1-Retro-High-Off-White-White" ) ~ "AJ1", 
  startsWith(csv$Sneaker.Name, "Nike-Air-VaporMax-Off-White-Black" ) ~ "Vapor", 
  startsWith(csv$Sneaker.Name, "Air-Jordan-1-Retro-High-Off-White-University-Blue" ) ~ "AJ1", 
  startsWith(csv$Sneaker.Name, "Nike-Air-Presto-Off-White-Black-2018" ) ~ "Presto", 
  startsWith(csv$Sneaker.Name, "Nike-Air-Presto-Off-White-White-2018" ) ~ "Presto", 
  startsWith(csv$Sneaker.Name, "Nike-Zoom-Fly-Mercurial-Off-White-Black" ) ~ "zoomFly", 
  startsWith(csv$Sneaker.Name, "Nike-Zoom-Fly-Mercurial-Off-White-Total-Orange" ) ~ "zoomFly", 
  startsWith(csv$Sneaker.Name, "Nike-Air-Max-97-Off-White-Elemental-Rose-Queen" ) ~ "AirMax97",
  startsWith(csv$Sneaker.Name, "Nike-Blazer-Mid-Off-White-All-Hallows-Eve" ) ~ "Blazer",
  startsWith(csv$Sneaker.Name, "Nike-Blazer-Mid-Off-White-Grim-Reaper" ) ~ "Blazer",
  startsWith(csv$Sneaker.Name, "Nike-Blazer-Mid-Off-White-Wolf-Grey" ) ~ "Blazer",
  startsWith(csv$Sneaker.Name, "Nike-Air-Max-97-Off-White-Menta" ) ~ "AirMax97",
  startsWith(csv$Sneaker.Name, "Nike-Air-Max-97-Off-White-Black" ) ~ "AirMax97",
  startsWith(csv$Sneaker.Name, "Nike-Air-Force-1-Low-Off-White-Volt" ) ~ "AF1",
  startsWith(csv$Sneaker.Name, "Nike-Air-Force-1-Low-Off-White-Black-White" ) ~ "AF1",
  startsWith(csv$Sneaker.Name, "Nike-Air-Max-90-Off-White-Black" ) ~ "AF1",
  startsWith(csv$Sneaker.Name, "Nike-Air-Max-90-Off-White-Desert-Ore" ) ~ "AF1", 
)

#Plot Model 
ggplot(csv, aes(x = model)) + theme(
  axis.text.x = element_text(angle = 45))+
  geom_bar()

#Extract Color From Sneaker.Name
csv$color = case_when(
  startsWith(csv$Sneaker.Name, "Adidas-Yeezy-Boost-350-V2-Beluga" ) ~ "tan",
  startsWith(csv$Sneaker.Name, "Adidas-Yeezy-Boost-350-Low-V2-Beluga") ~ "tan",
  startsWith(csv$Sneaker.Name, "Adidas-Yeezy-Boost-350-Low-V2-Beluga-2pt0") ~ "tan",
  startsWith(csv$Sneaker.Name, "Adidas-Yeezy-Boost-350-V2-Moonrock") ~ "grey",
  startsWith(csv$Sneaker.Name, "Adidas-Yeezy-Boost-350-Low-Moonrock" ) ~ "grey",
  startsWith(csv$Sneaker.Name, "Adidas-Yeezy-Boost-350-V2-Oxford-Tan" ) ~ "tan",
  startsWith(csv$Sneaker.Name, "Adidas-Yeezy-Boost-350-Low-Oxford-Tan" ) ~ "tan",
  startsWith(csv$Sneaker.Name, "Adidas-Yeezy-Boost-350-Low-Pirate-Black" ) ~ "black",
  startsWith(csv$Sneaker.Name, "Adidas-Yeezy-Boost-350-Low-Turtledove" ) ~ "grey",
  startsWith(csv$Sneaker.Name, "Adidas-Yeezy-Boost-350-V2-Semi-Frozen-Yellow" ) ~ "yellow",
  startsWith(csv$Sneaker.Name, "Adidas-Yeezy-Boost-350-V2-Blue-Tint" ) ~ "blue",
  startsWith(csv$Sneaker.Name, "Adidas-Yeezy-Boost-350-V2-Core-Black-Copper" ) ~ "black",
  startsWith(csv$Sneaker.Name, "Adidas-Yeezy-Boost-350-V2-Core-Black-Green" ) ~ "black",
  startsWith(csv$Sneaker.Name, "Adidas-Yeezy-Boost-350-V2-Core-Black-Red" ) ~ "black",
  startsWith(csv$Sneaker.Name, "Adidas-Yeezy-Boost-350-V2-Core-Black-White" ) ~ "black",
  startsWith(csv$Sneaker.Name, "Adidas-Yeezy-Boost-350-V2-Cream-White" ) ~ "white",
  startsWith(csv$Sneaker.Name, "Adidas-Yeezy-Boost-350-V2-Zebra" ) ~ "mixed",
  startsWith(csv$Sneaker.Name, "adidas-Yeezy-Boost-350-V2-Butter" ) ~ "white",
  startsWith(csv$Sneaker.Name, "adidas-Yeezy-Boost-350-V2-Static" ) ~ "white",
  startsWith(csv$Sneaker.Name, "adidas-Yeezy-Boost-350-V2-Static-Reflective" ) ~ "white",
  startsWith(csv$Sneaker.Name, "Adidas-Yeezy-Boost-350-V2-Sesame" ) ~ "grey",
  startsWith(csv$Sneaker.Name, "Nike-Zoom-Fly-Off-White-Pink" ) ~ "pink",
  startsWith(csv$Sneaker.Name, "Nike-Zoom-Fly-Off-White-Black-Silver" ) ~ "black",
  startsWith(csv$Sneaker.Name, "Nike-Zoom-Fly-Off-White" ) ~ "white",
  startsWith(csv$Sneaker.Name, "Nike-Air-Force-1-Low-Off-White" ) ~ "white",
  startsWith(csv$Sneaker.Name, "Nike-Air-Max-97-Off-White" ) ~ "white",
  startsWith(csv$Sneaker.Name, "Nike-Air-Force-1-Low-Virgil-Abloh-Off-White-AF100" ) ~ "white",
  startsWith(csv$Sneaker.Name, "Nike-React-Hyperdunk-2017-Flyknit-Off-White" ) ~ "white",
  startsWith(csv$Sneaker.Name, "Nike-Air-Max-90-Off-White" ) ~ "white",
  startsWith(csv$Sneaker.Name, "Nike-Air-Presto-Off-White" ) ~ "white",
  startsWith(csv$Sneaker.Name, "Nike-Air-VaporMax-Off-White" ) ~ "white",
  startsWith(csv$Sneaker.Name, "Air-Jordan-1-Retro-High-Off-White-Chicago" ) ~ "Red",
  startsWith(csv$Sneaker.Name, "Nike-Blazer-Mid-Off-White" ) ~ "white",
  startsWith(csv$Sneaker.Name, "Nike-Air-VaporMax-Off-White-2018" ) ~ "white",
  startsWith(csv$Sneaker.Name, "Air-Jordan-1-Retro-High-Off-White-White" ) ~ "white", 
  startsWith(csv$Sneaker.Name, "Nike-Air-VaporMax-Off-White-Black" ) ~ "black", 
  startsWith(csv$Sneaker.Name, "Air-Jordan-1-Retro-High-Off-White-University-Blue" ) ~ "blue", 
  startsWith(csv$Sneaker.Name, "Nike-Air-Presto-Off-White-Black-2018" ) ~ "black", 
  startsWith(csv$Sneaker.Name, "Nike-Air-Presto-Off-White-White-2018" ) ~ "white", 
  startsWith(csv$Sneaker.Name, "Nike-Zoom-Fly-Mercurial-Off-White-Black" ) ~ "black", 
  startsWith(csv$Sneaker.Name, "Nike-Zoom-Fly-Mercurial-Off-White-Total-Orange" ) ~ "orange", 
  startsWith(csv$Sneaker.Name, "Nike-Air-Max-97-Off-White-Elemental-Rose-Queen" ) ~ "pink",
  startsWith(csv$Sneaker.Name, "Nike-Blazer-Mid-Off-White-All-Hallows-Eve" ) ~ "tan",
  startsWith(csv$Sneaker.Name, "Nike-Blazer-Mid-Off-White-Grim-Reaper" ) ~ "black",
  startsWith(csv$Sneaker.Name, "Nike-Blazer-Mid-Off-White-Wolf-Grey" ) ~ "grey",
  startsWith(csv$Sneaker.Name, "Nike-Air-Max-97-Off-White-Menta" ) ~ "grey",
  startsWith(csv$Sneaker.Name, "Nike-Air-Max-97-Off-White-Black" ) ~ "black",
  startsWith(csv$Sneaker.Name, "Nike-Air-Force-1-Low-Off-White-Volt" ) ~ "yellow",
  startsWith(csv$Sneaker.Name, "Nike-Air-Force-1-Low-Off-White-Black-White" ) ~ "black",
  startsWith(csv$Sneaker.Name, "Nike-Air-Max-90-Off-White-Black" ) ~ "black",
  startsWith(csv$Sneaker.Name, "Nike-Air-Max-90-Off-White-Desert-Ore" ) ~ "tan", 
)

#Plot color
ggplot(csv, aes(x = color)) +
  geom_bar()

#Plot our Y target
ggplot(csv, aes(x = Sale.Price)) +
  geom_bar()
#Our Y target is highly skewed, Highest Sale Price was $4050 (1 data Point)

#Take log of Y target to "normalize"
csv$log <- log(csv$Sale.Price)
ggplot(csv, aes(x = csv$log)) +
  geom_bar()
#Somewhat more normalized than before. 


  


#Change Name & Change into Date Data
names(csv)[names(csv) == 'ï..Order.Date'] <- 'Resale_Date'
csv$Resale_Date<- as.Date(csv$Resale_Date, "%m/%d/%Y")
names(csv)[names(csv) == 'Release.Date'] <- 'Release_Date'
csv$Release_Date<- as.Date(csv$Release_Date, "%m/%d/%Y")


#Calculate How Many Weeks Has Passed Since Release Date to Resale Date 
csv$weeks_passed = as.numeric(difftime(csv$Resale_Date, csv$Release_Date, units = "weeks"))
csv



final<- csv[,c('color', 'Brand', 'weeks_passed','model','Sneaker.Name', 'Shoe.Size', 'Sale.Price', 'Retail.Price')]

summary(csv$color)



write.csv(final, "C:/Users/jeong/Downloads/final_data.csv", row.names=FALSE)
