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

dt <- as.Date("2010-02-10")
new.dt <- dt - todaysdate
new.dt

R_nocoupon<-rep(1,4)
for (i in c(1:4)) {
  R_nocoupon[i]<- -(365/(as.numeric(yield$Eff..Maturity[i]-todaysdate)))*log((yield$Price[i]+(as.numeric(todaysdate-yield$Eff..Maturity[i] %m-% months(6)))/365*yield$Coupon[i])/(yield$Coupon[i]/2+100))
}
R_nocoupon


R1_nocoupon<-rep(1,3)
for (i in c(5:7)) {
  R1_nocoupon[i-4]<--(365/(as.numeric(yield$Eff..Maturity[i]-todaysdate)))*log((yield$Price[i]-yield$Coupon[i]/2*exp(-R_nocoupon[4]*1/2)+(as.numeric(todaysdate-yield$Eff..Maturity[i] %m-% months(12)))/365*yield$Coupon[i])/(yield$Coupon[i]/2+100))
}
R1_nocoupon
R12_nocoupon<-rep(1,4)
for (i in c(8:11)) {
  R12_nocoupon[i-7]<--(365/(as.numeric(yield$Eff..Maturity[i]-todaysdate)))*log((yield$Price[i]-yield$Coupon[i]/2*exp(-r612*1/2)-yield$Coupon[i]/2*exp(-R1_nocoupon[3])+(as.numeric(todaysdate-yield$Eff..Maturity[i] %m-% months(18)))/365*yield$Coupon[i])/(yield$Coupon[i]/2+100))
}
R12_nocoupon

r612<-(R1_nocoupon[1]+R_nocoupon[4])/2
r612
r1<-(R1_nocoupon[3]+R12_nocoupon[1])/2
r1

# So r_1=r1 = 0.006400881

#calculate r_1/2 and r_1 for Bell now...

#Import Data for Bell

bell <- read.csv("bell_bonds.csv")
View(bell)
bell$Company<-as.character(bell$Company)
bell$Eff..Maturity<-as.character(bell$Eff..Maturity)
bell$Eff..Maturity<-as.Date(bell$Eff..Maturity, "%Y-%m-%d")

B_nocoupon<-rep(1,1)
for (i in c(1:1)) {
  B_nocoupon[i]<- -(365/(as.numeric(bell$Eff..Maturity[i]-todaysdate)))*log((bell$Price[i]+(as.numeric(todaysdate-bell$Eff..Maturity[i] %m-% months(6)))/365*bell$Coupon[i])/(bell$Coupon[i]/2+100))
}
B_nocoupon



R1_nocoupon<-rep(1,3)
for (i in c(5:7)) {
  R1_nocoupon[i-4]<--(365/(as.numeric(yield$Eff..Maturity[i]-todaysdate)))*log((yield$Price[i]-yield$Coupon[i]/2*exp(-R_nocoupon[4]*1/2)+(as.numeric(todaysdate-yield$Eff..Maturity[i] %m-% months(12)))/365*yield$Coupon[i])/(yield$Coupon[i]/2+100))
}
R1_nocoupon
R12_nocoupon<-rep(1,4)
for (i in c(8:11)) {
  R12_nocoupon[i-7]<--(365/(as.numeric(yield$Eff..Maturity[i]-todaysdate)))*log((yield$Price[i]-yield$Coupon[i]/2*exp(-r612*1/2)-yield$Coupon[i]/2*exp(-R1_nocoupon[3])+(as.numeric(todaysdate-yield$Eff..Maturity[i] %m-% months(18)))/365*yield$Coupon[i])/(yield$Coupon[i]/2+100))
}
R12_nocoupon



### Question 2 #############################

bell <- read.csv("bell_stocks_prices.csv")
dbell_Adj.Close<-diff(bell$Adj.Close)
sd(dbell_Adj.Close)
