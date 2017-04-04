#A2
### Preamble #############################
library(stats)
library(tseries)
library(forecast)
library(xtable)
library(fGarch)
library(FinTS)
library(ggplot2)
setwd("~/Documents/Mathematical_Finance")

### Question 1 #############################




### Question 2 #############################

bell <- read.csv("bell_stocks_prices.csv")
dbell_Adj.Close<-diff(bell$Adj.Close)
sd(dbell_Adj.Close)
