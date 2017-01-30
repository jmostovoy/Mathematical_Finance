#Install and load packages to be used

install.packages("lubridate")
library(lubridate)


#Set working directory
setwd("~/Documents/Mathematical_Finance")

#Import data
Day_10<-read.csv("Day_10.csv", header=F)

#Manipulate and select relevent data
Day_10<-as.data.frame(Day_10)
View(Day_10)
day_10_can_true<-Day_10[,1]=="Canada"
Day_10<-Day_10[day_10_can_true,1:5]
Day_10[,3]<-as.Date(Day_10[,3], "%Y-%B-%d")
rownames(Day_10) <- seq(length=nrow(Day_10))
colnames(Day_10) <- c("country","coupon", "date", "price", "ytm")


#Initialize a date vector
x<-rep(as.Date("2017-01-20"), 10)

#Loop to set dates of interest
for (i in c(1:10)) {
  month(x[i])<-month(x[i])+6*i
}
x<-as.Date(x)
x

#Intialize another date vector, and perform a loop
#that selects the closest date to that of our interest
#In 6 month intervals
y<-rep(1, 10)
for (i in c(1:10)) {
  y[i]<-which(abs(Day_10[,3]-x[i]) == min(abs(Day_10[,3] - x[i])))
}
y

#reduce dataframe to data of interest
gucci10<-Day_10[y,]
View(gucci10)
rownames(gucci10) <- seq(length=nrow(gucci10))


#Compute yields
r<-rep(1, 10)
r[1]<-((gucci10[1,4])/((gucci10[1,2])/2+100))^-1-1
for (i in c(2:10)) {
  r[i]<-((gucci10[i,4]-sum((gucci10[i,2]/2)/((1+r[c(1:(i-1))])^(c(1:(i-1))))))/(100+gucci10[i,2]/2))^(-(1/i))-1
}
r

gucci10<-data.frame(gucci10, 2*r)
colnames(gucci10) <- c("country","coupon", "date", "price", "ytm", "yield")

plot(gucci10[,3], gucci10[,6])

plot_colours <- c("blue", "red", "forestgreen", "yellow")
plot_colours1 <- plot_colours[c(1,2)]

plot(gucci10[,3], 100*gucci10[,6], type="l", col=plot_colours1[1], ann=FALSE)
title(main="Yiled Curve for 2017-01-20", col.main="forestgreen", font.main=4)
title(xlab="Date", col.lab=rgb(0,0.6,.7))
title(ylab="Yield (in %)", col.lab=rgb(0,0.6,.7))




#Working Area
sum((gucci10[3,2]/2)/(1+r[c(1:1)])^(c(1:1)))
sum((gucci10[2,2]/2)/((1+r[c(1:3)]^(c(1:3)))))

sum((gucci10[2,2]/2)/((1+r[1]^((1)))))+sum((gucci10[2,2]/2)/((1+r[2]^((2)))))+sum((gucci10[2,2]/2)/((1+r[3]^((3)))))

cool<-rep(1, 9)

for (i in length(cool)) {
  cool[i]<-as.numeric(month(x[i+1])-month(x[i]))
}



cool

month(x[10])-month(x[9])



