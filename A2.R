#A2
### Preamble #############################
library(stats)
library(tseries)
library(forecast)
library(xtable)
library(fGarch)
library(FinTS)
library(ggplot2)
library(lubridate)
setwd("~/Documents/Mathematical_Finance")

### Question 1 #############################

#Import and make the data nice to use
yield <- read.csv("yield_apr4.csv")
yield$Country<-as.character(yield$Country)
yield$Eff..Maturity<-as.character(yield$Eff..Maturity)
yield$Eff..Maturity<-as.Date(yield$Eff..Maturity, "%Y-%B-%d")
yield<-yield[c(1:43),]
View(yield)

#Compute all yields via bootstrapping
todaysdate<-as.Date("2017-04-04")
todaysdate

r<-rep(1, 10)
R<-data.frame(r, r, r, r, r, r, r, r, r, r)
for (j in c(1:10)) {
  R[1,j]<-((subbonds[(10*j-9),4])/((subbonds[(10*j-9),2])/2+100))^-1-1
}
View(R)

for (j in c(1:10)) {
  for (i in c(2:10)) {
    R[i,j]<-((subbonds[((10*j-10)+i),4]-
                sum((subbonds[((10*j-10)+i),2]/2)
                    /((1+R[c(1:(i-1)),j])^(c(1:(i-1))))))
             *(100+subbonds[((10*j-10)+i),2]/2)^(-1))^(-1/i)-1
  }
}

### Question 2 #############################

bell <- read.csv("bell_stocks_prices.csv")
dbell_Adj.Close<-diff(bell$Adj.Close)
sd(dbell_Adj.Close)
