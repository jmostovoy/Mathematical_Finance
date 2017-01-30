install.packages("lubridate")
library(lubridate)

setwd("~/Documents/Mathematical_Finance")
Day_10<-read.csv("Day_10.csv", header=F)
Day_10<-as.data.frame(Day_10)
View(Day_10)
day_10_can_true<-Day_10[,1]=="Canada"
Day_10<-Day_10[day_10_can_true,1:5]
Day_10[,3]<-as.Date(Day_10[,3], "%Y-%B-%d")
rownames(Day_10) <- seq(length=nrow(Day_10))
colnames(Day_10) <- c("country","coupon", "date", "price", "ytm")


d <- as.Date('2004-01-01')
month(d) <- month(d) + 1
d

x<-rep(as.Date("2017-01-20"), 10)

for (i in c(1:10)) {
  month(x[i])<-month(x[i])+6*i
}
x<-as.Date(x)
x
y<-rep(1, 10)
for (i in c(1:10)) {
  y[i]<-which(abs(Day_10[,3]-x[i]) == min(abs(Day_10[,3] - x[i])))
}
y

gucci10<-Day_10[y,]
View(gucci10)
rownames(gucci10) <- seq(length=nrow(gucci10))


